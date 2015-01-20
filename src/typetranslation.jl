# Translating between clang and julia types

# # # # Section 1: Mapping Julia types to clang types
#
# The main function defined by this section is cpptype(T), which for a given
# julia type `T` will return a QualType, representing clang's interpretation
# of this type

# # # Built-in clang types
chartype() = QualType(pcpp"clang::Type"(unsafe_load(cglobal((:cT_cchar,libcxxffi),Ptr{Void}))))
winttype() = QualType(pcpp"clang::Type"(unsafe_load(cglobal((:cT_wint,libcxxffi),Ptr{Void}))))
cpptype(::Type{Uint8}) = chartype()

# Mapping some basic julia bitstypes to C's intrinsic types
# We could use Cint, etc. here, but we actually already do that
# in the C++ side, which makes this easy:
for (jlt, sym) in
    ((Void, :cT_void),
     (Int8, :cT_int8),
     (Uint32, :cT_uint32),
     (Int32, :cT_int32),
     (Uint64, :cT_uint64),
     (Int64, :cT_int64),
     (Bool, :cT_int1),
     (Float32, :cT_float32),
     (Float64, :cT_float64))

    # For each type, do the mapping by loading the appropriate global
    @eval cpptype(::Type{$jlt}) = QualType(pcpp"clang::Type"(
        unsafe_load(cglobal(
            ($(quot(sym)), libcxxffi),
            Ptr{Void} ))
        ))

end

# # # Mapping the C++ object hierarchy types back to clang types
#
# This is more complicated than simply mapping builtin types and proceeds in
# stages.
#
# 1. Perform the name lookup of the inner most declaration (cppdecl)
#   - While doing this we also need to perform template instantiation
# 2. Resolve pointer/reference types in clang/land
# 3. Add CVR qualifiers
#
# Steps 2/3 are performed in the actual definition of cpptype at the end of
# this subsection. But first, we need utilities for name lookup, templates, etc.
#

# # Name Lookup

#
# Name lookup was rather frustrating to figure out and I'm sure this is still
# not the ideal way to do this. The main entry points to this section are
# lookup_name and lookup_ctx.
#
# lookup_name takes a list of parts, where each part is the name of the next
# Decl to lookup, e.g. foo::bar would be looked up as ("foo","bar").
# However, this isn't necessarily enough as some other parts of the clang API
# require not only the looked up type, but also how it was qualified, e.g.
# to differentiate
#
# ```
#   using namespace foo;
#   bar
# ```
#
# from `foo::bar`
#
# This is achieved using the CxxScopeSpec type. Where required, a CxxScopeSpec
# can be allocated using newCXXScopeSpec (and deleted using deleteCXXScopeSpec)
# and passed in as the `cxxscope` argument to lookup_name, which will be
# automatically built up as expressions are looked up.
#
# Note however, that e.g. in `foo::bar::baz`, the CxxScopeSpec only covers
# `foo::bar`, but not the actual decl `baz`. To this end, lookup_name does
# not add the last part to the CxxScopeSpec by default. This behavior can be
# overriden using the `addlast` parameter, which is useful when looking up a
# prefix to a decl context, rather than a Decl itself.
#
# TODO: Figure out what we need to do if there's a template in the NNS.
# Originally there was code to handle this, but it was incorrect and apparently
# never triggered in my original tests. Better to address that point when
# we have a failing test case to verify we did it correctly.
#

#
# Extend the CxxScopeSpec * by the type or namespace with the name of `part`.
# E.g. if the CxxScopeSpec already contains ::foo, it could be extended to
# ::foo::bar by calling nnsexpend(S,"bar")
#
function nnsextend(cxxscope,part)
    if cxxscope != C_NULL
        errorOccured = ccall((:BuildNNS,libcxxffi),Bool,
            (Ptr{Void},Ptr{Uint8}),cxxscope,part)
        if errorOccured
            error("Failed to extend NNS with part $part")
        end
    end
end

function lookup_name(parts, cxxscope = C_NULL, start=translation_unit(), addlast=false)
    cur = start
    for (i,fpart) in enumerate(parts)
        lastcur = cur
        (addlast || (i != length(parts))) && nnsextend(cxxscope,fpart)
        cur = _lookup_name(fpart,primary_ctx(toctx(cur)))
        if cur == C_NULL
            if lastcur == translation_unit()
                error("Could not find $fpart in translation unit")
            else
                error("Could not find $fpart in context $(_decl_name(lastcur))")
            end
        end
    end
    cur
end

# Convenience method for splitting a qualified name into actual parts
function lookup_ctx(fname::String;
    cxxscope=C_NULL, cur=translation_unit(), addlast = false)
    lookup_name(split(fname,"::"),cxxscope,cur, addlast)
end
lookup_ctx(fname::Symbol; kwargs...) = lookup_ctx(string(fname); kwargs...)

# # Template instantiations for decl lookups

# Specialize a ClassTemplateDecl cxxt with a list of template arguments given
# by `targs`
function specialize_template(cxxt::pcpp"clang::ClassTemplateDecl",targs)
    @assert cxxt != C_NULL
    integralValues = zeros(Uint64,length(targs))
    integralValuesPresent = zeros(Uint8,length(targs))
    bitwidths = zeros(Uint32,length(targs))
    ts = Array(QualType,length(targs))
    for (i,t) in enumerate(targs)
        if isa(t,Type)
            ts[i] = cpptype(t)
        elseif isa(t,Integer) || isa(t,CppEnum)
            ts[i] = cpptype(typeof(t))
            integralValues[i] = convert(Uint64,isa(t,CppEnum) ? t.val : t)
            integralValuesPresent[i] = 1
            bitwidths[i] = isa(t,Bool) ? 8 : sizeof(typeof(t))
        else
            error("Unhandled template parameter type ($t)")
        end
    end
    d = pcpp"clang::ClassTemplateSpecializationDecl"(ccall((:SpecializeClass,libcxxffi),Ptr{Void},
            (Ptr{Void},Ptr{Void},Ptr{Uint64},Ptr{Uint32},Ptr{Uint8},Uint32),
            cxxt.ptr,[p.ptr for p in ts],integralValues,bitwidths,integralValuesPresent,length(ts)))
    d
end

function specialize_template_clang(cxxt::pcpp"clang::ClassTemplateDecl",targs,cpptype)
    @assert cxxt != C_NULL
    integralValues = zeros(Uint64,length(targs))
    integralValuesPresent = zeros(Uint8,length(targs))
    bitwidths = zeros(Uint32,length(targs))
    ts = Array(QualType,length(targs))
    for (i,t) in enumerate(targs)
        if isa(t,pcpp"clang::Type") || isa(t,QualType)
            ts[i] = QualType(t)
        elseif isa(t,Integer) || isa(t,CppEnum)
            ts[i] = cpptype(typeof(t))
            integralValues[i] = convert(Uint64,isa(t,CppEnum) ? t.val : t)
            integralValuesPresent[i] = 1
            bitwidths[i] = isa(t,Bool) ? 8 : sizeof(typeof(t))
        else
            error("Unhandled template parameter type ($t)")
        end
    end
    d = pcpp"clang::ClassTemplateSpecializationDecl"(ccall((:SpecializeClass,libcxxffi),Ptr{Void},
            (Ptr{Void},Ptr{Void},Ptr{Uint64},Ptr{Uint32},Ptr{Uint8},Uint32),
            cxxt.ptr,[p.ptr for p in ts],integralValues,bitwidths,integralValuesPresent,length(ts)))
    d
end

# # The actual decl lookup
# since we split out all the actual meat above, this is now simple

function cppdecl{fname}(T::Type{CppBaseType{fname}})
    # Let's get a clang level representation of this type
    lookup_ctx(fname)
end
cppdecl{s}(::Type{CppEnum{s}}) = lookup_ctx(s)

function cppdecl{T,targs}(TT::Type{CppTemplate{T,targs}})
    ctx = cppdecl(T)

    # Do the acutal template resolution
    cxxt = cxxtmplt(ctx)
    @assert cxxt != C_NULL
    deduced_class = specialize_template(cxxt,targs)
    ctx = deduced_class

    ctx
end

# For getting the decl ignore the CVR qualifiers, pointer qualification, etc.
# and just do the lookup on the base decl
function cppdecl{T,CVR}(::Union(
    Type{CppPtr{T,CVR}}, Type{CppValue{T,CVR}}, Type{CppRef{T,CVR}}))
    cppdecl(T)
end

# Get a clang Type * for the decl we have looked up.
# @cxx (@cxx dyn_cast{vcpp"clang::TypeDecl"}(d))->getTypeForDecl()
function typeForDecl(d::Union(
        # Luckily Decl is the first base for these, so we can get away
        # with only one function on the C++ side that takes a Decl*
        pcpp"clang::Decl",pcpp"clang::CXXRecordDecl",
        pcpp"clang::ClassTemplateSpecializationDecl"))
    @assert d != C_NULL
    pcpp"clang::Type"(ccall((:typeForDecl,libcxxffi),Ptr{Void},(Ptr{Void},),d))
end

for sym in (:withConst, :withVolatile, :withRestrict)
    @eval ($sym)(T::QualType) =
        QualType(ccall(($(quot(sym)),libcxxffi),Ptr{Void},(Ptr{Void},),T))
end

function addQualifiers(clangT::QualType,CVR)
    C,V,R = CVR
    if C
        clangT = withConst(clangT)
    end
    if V
        clangT = withVolatile(clangT)
    end
    if R
        clangT = withRestrict(clangT)
    end
    clangT
end
addQualifiers(clangT::pcpp"clang::Type",CVR) = addQualifiers(QualType(clangT.ptr),CVR)

# And finally the actual definition of cpptype

cpptype{s}(p::Type{CppEnum{s}}) = QualType(typeForDecl(cppdecl(p)))
function cpptype{T,CVR}(p::Type{CppPtr{T,CVR}})
    addQualifiers(pointerTo(cpptype(T)),CVR)
end
function cpptype{T,CVR}(p::Type{CppValue{T,CVR}})
    addQualifiers(typeForDecl(cppdecl(T)),CVR)
end
function cpptype{T,CVR}(p::Type{CppRef{T,CVR}})
    referenceTo(addQualifiers(typeForDecl(cppdecl(T)),CVR))
end

cpptype{T}(p::Type{Ptr{T}}) = pointerTo(cpptype(T))

function cpptype{base,fptr}(p::Type{CppMFptr{base,fptr}})
    makeMemberFunctionType(cpptype(base), cpptype(fptr))
end
function cpptype{rt, args}(p::Type{CppFunc{rt,args}})
    makeFunctionType(cpptype(rt),QualType[cpptype(arg) for arg in args])
end
cpptype{f}(p::Type{CppFptr{f}}) = pointerTo(cpptype(f))

cpptype(F::Type{Function}) = cpptype(pcpp"jl_function_t")

# # # # Section 2: Mapping Julia types to clang types
#
# Somewhat simpler than the above, because we simply need to call the
# appropriate Type * methods and marshal everything into the Julia-side
# hierarchy
#

function _decl_name(d)
    @assert d != C_NULL
    s = ccall((:decl_name,libcxxffi),Ptr{Uint8},(Ptr{Void},),d)
    ret = bytestring(s)
    c_free(s)
    ret
end

function simple_decl_name(d)
    s = ccall((:simple_decl_name,libcxxffi),Ptr{Uint8},(Ptr{Void},),d)
    ret = bytestring(s)
    c_free(s)
    ret
end

get_pointee_name(t) = _decl_name(getPointeeCXXRecordDecl(t))
function get_name(t)
    d = getAsCXXRecordDecl(t)
    if d != C_NULL
        return _decl_name(d)
    end
    d = isIncompleteType(t)
    @assert d != C_NULL
    return _decl_name(d)
end

const KindNull              = 0
const KindType              = 1
const KindDeclaration       = 2
const KindNullPtr           = 3
const KindIntegral          = 4
const KindTemplate          = 5
const KindTemplateExpansion = 6
const KindExpression        = 7
const KindPack              = 8

function getTemplateParameters(cxxd)
    targt = ()
    if isaClassTemplateSpecializationDecl(pcpp"clang::Decl"(cxxd.ptr))
        tmplt = dcastClassTemplateSpecializationDecl(pcpp"clang::Decl"(cxxd.ptr))
        targs = getTemplateArgs(tmplt)
        args = Any[]
        for i = 0:(getTargsSize(targs)-1)
            kind = getTargKindAtIdx(targs,i)
            if kind == KindType
                push!(args,juliatype(getTargTypeAtIdx(targs,i)))
            elseif kind == KindIntegral
                val = getTargAsIntegralAtIdx(targs,i)
                t = getTargIntegralTypeAtIdx(targs,i)
                push!(args,convert(juliatype(t),val))
            else
                error("Unhandled template argument kind ($kind)")
            end
        end
        targt = tuple(args...)
    end
    targt
end

# TODO: Autogenerate this from the appropriate header
const cVoid      = 0
const cBool      = 1
const cChar_U    = 2
const cUChar     = 3
const cWChar_U   = 4
const cChar16    = 5
const cChar32    = 6
const cUShort    = 7
const cUInt      = 8
const cULong     = 9
const cULongLong = 10
const CUInt128   = 11
const cChar_S    = 12
const cSChar     = 13
const cWChar_S   = 14
const cShort     = 15
const cInt       = 16
const cLong      = 17
const cLongLong  = 18
const cInt128    = 19
const cHalf      = 20
const cFloat     = 21
const cDouble    = 22

# Decl::Kind
const LinkageSpec = 9


getPointeeType(t::pcpp"clang::Type") = QualType(ccall((:getPointeeType,libcxxffi),Ptr{Void},(Ptr{Void},),t.ptr))
canonicalType(t) = pcpp"clang::Type"(ccall((:canonicalType,libcxxffi),Ptr{Void},(Ptr{Void},),t))

function toBaseType(t::pcpp"clang::Type")
    T = CppBaseType{symbol(get_name(t))}
    rd = getAsCXXRecordDecl(t)
    if rd.ptr != C_NULL
        targs = getTemplateParameters(rd)
        if !isempty(targs)
            T = CppTemplate{T,targs}
        end
    end
    T
end

function juliatype(t::QualType)
    CVR = extractCVR(t)
    t = extractTypePtr(t)
    t = canonicalType(t)
    if isVoidType(t)
        return Void
    elseif isBooleanType(t)
        return Bool
    elseif isPointerType(t)
        pt = getPointeeType(t)
        tt = juliatype(pt)
        if tt <: CppFunc
            return CppFptr{tt}
        elseif CVR != NullCVR || tt <: CppValue || tt <: CppPtr || tt <: CppRef
            return CppPtr{tt,CVR}
        else
            return Ptr{tt}
        end
    elseif isFunctionPointerType(t)
        error("Is Function Pointer")
    elseif isFunctionType(t)
        if isFunctionProtoType(t)
            t = pcpp"clang::FunctionProtoType"(t.ptr)
            rt = getReturnType(t)
            args = QualType[]
            for i = 0:(getNumParams(t)-1)
                push!(args,getParam(t,i))
            end
            f = CppFunc{juliatype(rt), tuple(map(juliatype,args)...)}
            return f
        else
            error("Function has no proto type")
        end
    elseif isMemberFunctionPointerType(t)
        cxxd = QualType(getMemberPointerClass(t))
        pointee = getMemberPointerPointee(t)
        return CppMFptr{juliatype(cxxd),juliatype(pointee)}
    elseif isReferenceType(t)
        t = getPointeeType(t)
        jt = juliatype(t)
        if jt <: CppValue
            return CppRef{toBaseType(extractTypePtr(t)),CVR}
        else
            return CppRef{jt,CVR}
        end
    elseif isCharType(t)
        return Uint8
    elseif isEnumeralType(t)
        return CppEnum{symbol(get_name(t))}
    elseif isIntegerType(t)
        kind = builtinKind(t)
        if kind == cLong || kind == cLongLong
            return Int64
        elseif kind == cULong || kind == cULongLong
            return Uint64
        elseif kind == cUInt
            return Uint32
        elseif kind == cInt
            return Int32
        elseif kind == cChar_U || kind == cChar_S
            return Uint8
        elseif kind == cSChar
            return Int8
        end
        #@show kind
        dump(t)
        error("Unrecognized Integer type")
    elseif isFloatingType(t)
        kind = builtinKind(t)
        if kind == cHalf
            return Float16
        elseif kind == cFloat
            return Float32
        elseif kind == cDouble
            return Float64
        end
        error("Unrecognized floating point type")
    else
        return CppValue{toBaseType(t),CVR}
    end
    return Ptr{Void}
end
