import Base: String, unsafe_string
using Base.@propagate_inbounds

cxxparse("""
#include <string>
#include <vector>
#include <stdexcept>
#include <memory>
#include <map>
""")

const StdString = cxxt"std::string"
const StdStringR = cxxt"std::string&"
typealias StdVector{T} Union{cxxt"std::vector<$T>",cxxt"std::vector<$T>&"}
typealias StdMap{K,V} cxxt"std::map<$K,$V>"

unsafe_string(str::Union{StdString,StdStringR}) = unsafe_string((@cxx str->data()),@cxx str->size())
String(str::Union{StdString,StdStringR}) = unsafe_string(str)
Base.convert(::Type{String}, x::Union{StdString,StdStringR}) = String(x)
Base.convert(StdString, x::AbstractString) = icxx"std::string s($(pointer(x)), $(sizeof(x))); s;"

import Base: showerror
import Cxx: CppValue
for T in Cxx.CxxBuiltinTypes.types
    @eval @exception function showerror(io::IO, e::$(T.parameters[1]))
        print(io, e)
    end
end
@exception function showerror(io::IO, e::cxxt"std::length_error&")
    try
        @show e
        print(io, bytestring(icxx"$e.what();"))
    catch w
        @show w
    end
end

Base.start(v::StdVector) = 0
Base.next(v::StdVector,i) = (v[i], i+1)
Base.done(v::StdVector,i) = i >= length(v)
Base.length(v::StdVector) = Int(icxx"$(v).size();")
Base.size(v::StdVector) = (length(v),)
Base.eltype{T}(v::StdVector{T}) = T
@inline Base.indices(v::StdVector) = (0:(length(v) - 1),)
@inline Base.linearindices(v::StdVector) = indices(v)[1]
@inline function Base.checkbounds(v::StdVector, I...)
    Base.checkbounds_indices(Bool, indices(v), I) || Base.throw_boundserror(v, I)
    nothing
end

@inline Base.getindex(v::StdVector,i) = (@boundscheck checkbounds(v, i); icxx"($(v))[$i];")
@inline Base.getindex{T<:Cxx.CxxBuiltinTs}(v::StdVector{T}, i) = (@boundscheck checkbounds(v, i); icxx"auto x = ($(v))[$i]; x;")

@inline function _generic_setindex!(v::StdVector, val, i::Integer)
    @boundscheck checkbounds(v, i)
    icxx"($(v))[$i] = $val; void();"
end
@propagate_inbounds Base.setindex!{T}(v::StdVector{T}, val::T, i) = _generic_setindex!(v, val, i)
@propagate_inbounds Base.setindex!{T}(v::StdVector{T}, val::Cxx.CppValue{T}, i) = _generic_setindex!(v, val, i)
@propagate_inbounds Base.setindex!{T}(v::StdVector{T}, val, i) = _generic_setindex!(v, convert(T, val), i)

Base.deleteat!(v::StdVector,idxs::UnitRange) =
    icxx"$(v).erase($(v).begin()+$(first(idxs)),$(v).begin()+$(last(idxs)));"
Base.push!(v::StdVector,i) = icxx"$v.push_back($i);"
Base.resize!(v::StdVector, n) = icxx"$v.resize($n);"

Base.start(map::StdMap) = icxx"$map.begin();"
function Base.next(map::StdMap,i)
    v = icxx"$i->first;" => icxx"$i->second;"
    icxx"++$i;"
    (v,i)
end
Base.done(map::StdMap,i) = icxx"$i == $map.end();"
Base.length(map::StdMap) = icxx"$map.size();"
Base.eltype{K,V}(::Type{StdMap{K,V}}) = Pair{K,V}

function Base.filter!(f, a::StdVector)
    insrt = start(a)
    for curr = start(a):length(a)
        if f(a[curr])
            icxx"$a[$insrt] = $a[$curr];"
            insrt += 1
        end
    end
    if insrt < length(a)
        deleteat!(a, insrt:length(a))
    end
    return a
end


Base.pointer(v::StdVector, i::Integer) = icxx"&$v[$i];"
Base.pointer(v::StdVector) = pointer(v, 0)

Base.unsafe_wrap{T}(::Type{DenseArray}, v::StdVector{T}) = WrappedCppObjArray(pointer(v), length(v))
Base.unsafe_wrap{T<:Cxx.CxxBuiltinTs}(::Type{DenseArray}, v::StdVector{T}) = WrappedCppPrimArray(pointer(v), length(v))

Base.copy!(dest::StdVector, src) = copy!(unsafe_wrap(DenseArray, dest), src)
Base.copy!(dest::AbstractArray, src::StdVector) = copy!(dest, unsafe_wrap(DenseArray, src))
# Base.copy!(dest::StdVector, src::StdVector) = ...

Base.copy!(dest::StdVector, doffs::Integer, src, soffs::Integer, n::Integer) =
    copy!(unsafe_wrap(DenseArray, dest), doffs + 1, src, soffs, n)
Base.copy!(dest::AbstractArray, doffs::Integer, src::StdVector, soffs::Integer, n::Integer) =
    copy!(dest, doffs + 1, unsafe_wrap(DenseArray, src), soffs, n)
# Base.copy!(dest::StdVector, doffs::Integer, src::StdVector, soffs::Integer, n::Integer) = ...

Base.convert{CT<:AbstractArray}(::Type{CT}, v::StdVector) = convert(CT, unsafe_wrap(DenseArray, v))

function Base.convert{T}(::Type{cxxt"std::vector<$T>"}, x::AbstractArray)
    n = length(linearindices(x))
    result = icxx"std::vector<$T> v($n); v;"
    copy!(result, x)
    result
end


immutable WrappedCppObjArray{T, CVR} <: DenseArray{T,1}
    ptr::Cxx.CppPtr{T,CVR}
    len::Int
end

immutable WrappedCppPrimArray{T<:Cxx.CxxBuiltinTs} <: DenseArray{T,1}
    ptr::Ptr{T}
    len::Int
end

typealias WrappedArray{T} Union{WrappedCppObjArray{T}, WrappedCppPrimArray{T}}

Base.length(A::WrappedArray) = A.len
Base.size(A::WrappedArray) = (A.len,)
Base.linearindexing(::WrappedArray) = Base.LinearFast()

Base.pointer(A::WrappedArray) = A.ptr
Base.pointer{T}(A::WrappedCppPrimArray{T}, i::Integer) = A.ptr + sizeof(T) * (i - 1)
Base.pointer{T}(A::WrappedCppObjArray{T}, i::Integer) = icxx"&$(A.ptr)[$(i - 1)];"

@inline Base.getindex(A::WrappedCppObjArray, i::Integer) =
    (@boundscheck checkbounds(A, i); icxx"($(A.ptr))[$(i - 1)];")

@inline Base.getindex(A::WrappedCppPrimArray, i::Integer) =
    (@boundscheck checkbounds(A, i); unsafe_load(A.ptr, i))

@inline _generic_setindex!(A::WrappedCppObjArray, val, i::Integer) =
    (@boundscheck checkbounds(A, i); icxx"($(A.ptr))[$(i - 1)] = $val; void();")

@inline _generic_setindex!(A::WrappedCppPrimArray, val, i::Integer) =
    (@boundscheck checkbounds(A, i); unsafe_store!(A.ptr, val, i) )

@propagate_inbounds Base.setindex!(A::WrappedCppObjArray, val::Union{Cxx.CppValue, Cxx.CppRef}, i::Integer) = _generic_setindex!(A, val, i)
@propagate_inbounds Base.setindex!{T<:Cxx.CxxBuiltinTs}(A::WrappedCppPrimArray{T}, val::T, i::Integer) = _generic_setindex!(A, val, i)
@propagate_inbounds Base.setindex!{T}(A::WrappedArray{T}, val, i::Integer) = _generic_setindex!(A, convert(T, val), i)


function Base.show{T}(io::IO,
    ptr::Union{cxxt"std::shared_ptr<$T>",cxxt"std::shared_ptr<$T>&"})
    println(io,"shared_ptr<",typename(T),"> @",convert(UInt,icxx"(void*)$ptr.get();"))
end

#Cxx.cpptype{T<:Union{ASCIIString,UTF8String}}(C,::Type{T}) = Cxx.cpptype(C,Ptr{UInt8})
Cxx.cxxtransform(::Type{String},ex) = (Ptr{UInt8},:(pointer($ex)))
