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
@inline Base.indices(v::StdVector) = (0:(length(v) - 1),)
@inline Base.linearindices(v::StdVector) = indices(v)[1]
@inline function Base.checkbounds(v::StdVector, I...)
    Base.checkbounds_indices(Bool, indices(v), I) || Base.throw_boundserror(v, I)
    nothing
end

@inline Base.getindex(v::StdVector,i) = (@boundscheck checkbounds(v, i); icxx"($(v))[$i];")
@inline Base.getindex{T<:Cxx.CxxBuiltinTs}(v::StdVector{T}, i) = (@boundscheck checkbounds(v, i); icxx"auto x = ($(v))[$i]; x;")

@inline function _std_setindex!(v::StdVector, val, i)
    @boundscheck checkbounds(v, i)
    icxx"($(v))[$i] = $val; void();"
end
@propagate_inbounds Base.setindex!{T}(v::StdVector{T}, val::T, i) = _std_setindex!(v, val, i)
@propagate_inbounds Base.setindex!{T}(v::StdVector{T}, val::Cxx.CppValue{T}, i) = _std_setindex!(v, val, i)
@propagate_inbounds Base.setindex!{T}(v::StdVector{T}, val, i) = _std_setindex!(v, convert(T, val), i)

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

Base.pointer(v::StdVector, i::Integer) = icxx"&$v[$i];"
Base.pointer(v::StdVector) = pointer(v, 0)

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

#=

immutable WrappedCppObjArray{T, CVR} <: DenseArray{T,1}
    ptr::CppPtr{T,CVR}
    len::Int
end

immutable WrappedCppPrimArray{T<:Cxx.CxxBuiltinTs} <: DenseArray{T,1}
    ptr::Ptr{T}
    len::Int
end

typealias WrappedArray{T} Union{WrappedCppObjArray{T}, WrappedCppPrimArray{T}}

Base.unsafe_wrap{T}(::Type{DenseArray}, v::StdVector{T}) = WrappedCppObjArray(pointer(v), length(v))
Base.unsafe_wrap{T<:Cxx.CxxBuiltinTs}(::Type{DenseArray}, v::StdVector{T}) = WrappedCppPrimArray(pointer(v), length(v))
Base.unsafe_wrap{T<:Cxx.CxxBuiltinTs}(::Type{Array}, v::StdVector{T}) = unsafe_wrap(Array, pointer(v), length(v), false)

@inline Base.length(A::WrappedArray) = length(A.len)
@inline Base.size(A::WrappedArray) = (A.len,)
@inline Base.linearindexing(::WrappedArray) = Base.LinearFast()

@inline Base.getindex(A::WrappedCppObjArray,i) = (@boundscheck checkbounds(v, i); icxx"($(A.ptr))[$(i - 1)];")

@inline function _std_setindex!(v::{StdVector, val, i)
    @boundscheck checkbounds(v, i)
    icxx"($(v))[$(i - 1)] = $val; void();"
end
@propagate_inbounds Base.setindex!{T}(v::StdVector{T}, val::T, i) = _std_setindex!(v, val, i)
@propagate_inbounds Base.setindex!{T}(v::StdVector{T}, val::Cxx.CppValue{T}, i) = _std_setindex!(v, val, i)


Base.unsafe_wrap{T}(::Type{Array}, v::StdVector{T}) = WrappedCppObjArray(pointer(v), length(v))
Base.unsafe_wrap{T<:Cxx.CxxBuiltinTs}(::Type{Array}, v::StdVector{T}) = unsafe_wrap(Array, pointer(v), length(v), false)

# Generic copy from iterable collection to StdVector. Reverse direction is
# already covered by Julia's default copy!(dest::AbstractArray, src).
Base.copy!(dest::StdVector, src) = copy!(unsafe_wrap(Array, dest), src)
Base.copy!(dest, src::StdVector) = copy!(dest, unsafe_wrap(Array, src))
# Base.copy!(dest::StdVector, src::StdVector) = ...

Base.copy!(dest::StdVector, doffs::Integer, src, soffs::Integer, n::Integer) =
    copy!(unsafe_wrap(Array, dest), doffs + 1, src, soffs, n)

Base.copy!(dest, doffs::Integer, src::StdVector, soffs::Integer, n::Integer) =
    copy!(unsafe_wrap(Array, dest), doffs + 1, src, soffs, n)

Base.copy!(dest::StdVector, doffs::Integer, src, soffs::Integer, n::Integer) =
    copy!(unsafe_wrap(Array, dest), doffs + 1, src, soffs, n)

=#

function _check_copy_len(dest, doffs::Integer, src, soffs::Integer, n::Integer)
    n > 0 || throw(ArgumentError(string("tried to copy n=", n, " elements, but n should be nonnegative")))
    dest_linidx = linearindices(dest)
    src_linidx = linearindices(src)
    (
        soffs < first(src_linidx) || doffs < first(dest_linidx) ||
            soffs + n - 1 > last(src_linidx) || doffs + n - 1 > last(dest_linidx)
    ) && throw(BoundsError())
    nothing
end

function _dense_unsafe_copy!(dest, doffs::Integer, src, soffs::Integer, n::Integer)
    n == 0 && return dest
    _check_copy_len(dest, doffs, src, soffs, n)
    unsafe_copy!(pointer(dest, doffs), pointer(src, soffs), n)
    dest
end

Base.copy!{T<:Cxx.CxxBuiltinTs,N}(dest::DenseArray{T,N}, doffs::Integer, src::StdVector{T}, soffs::Integer, n::Integer) =
    _dense_unsafe_copy!(dest, doffs, src, soffs, n)

Base.copy!{T<:Cxx.CxxBuiltinTs,N}(dest::StdVector{T}, doffs::Integer, src::DenseArray{T,N}, soffs::Integer, n::Integer) =
    _dense_unsafe_copy!(dest, doffs, src, soffs, n)

Base.copy!{T<:Cxx.CxxBuiltinTs,N}(dest::DenseArray{T,N}, src::StdVector{T}) =
    _dense_unsafe_copy!(dest, first(linearindices(dest)), src, 0, length(src))

Base.copy!{T<:Cxx.CxxBuiltinTs,N}(dest::StdVector{T}, src::DenseArray{T,N}) =
    _dense_unsafe_copy!(dest, 0, src, first(linearindices(src)), length(src))


function Base.convert{T}(V::Type{Vector{T}}, x::StdVector)
    result = V(length(x))
    copy!(result, x)
    result
end

function Base.convert{T}(::Type{cxxt"std::vector<$T>"}, x::AbstractVector)
    n = length(linearindices(x))
    result = icxx"std::vector<$T> v($n); v;"
    copy!(result, x)
    result
end

function Base.show{T}(io::IO,
    ptr::Union{cxxt"std::shared_ptr<$T>",cxxt"std::shared_ptr<$T>&"})
    println(io,"shared_ptr<",typename(T),"> @",convert(UInt,icxx"(void*)$ptr.get();"))
end

#Cxx.cpptype{T<:Union{ASCIIString,UTF8String}}(C,::Type{T}) = Cxx.cpptype(C,Ptr{UInt8})
Cxx.cxxtransform(::Type{String},ex) = (Ptr{UInt8},:(pointer($ex)))
