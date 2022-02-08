export EdgeCollection

export BufferedEdgeDict

abstract type EdgeCollection{ T <: AbstractEdge } end
# EdgeCollection interface:
# push!
# getindex
# length? 

# must have fields:
#   id_counter::EdgeNr

# function Base.setindex!(::T, _, _) where { T <: EdgeCollection }
#     println(T)
#     @assert false "setIndex! is not defined for this EdgeCollection type"
# end

# function Base.getindex(::EdgeCollection{T}, _) where { T }
#     @assert false "getIndex is not defined for this EdgeCollection type"
# end

################################################## BufferedEdgeDict

# TODO: Disable all constructures beside ()
Base.@kwdef mutable struct BufferedEdgeDict{T} <: EdgeCollection{T}
    containers = [ Dict{AgentID, Vector{T}}() for i = 1:NUM_BUFFERS ]
    read = 1
    write = 1
end

function Base.show(io::IO, mime::MIME"text/plain", bed::BufferedEdgeDict{T}) where { T }
    show_buffered_collection(io, mime, bed)
end

# function Base.setindex!(coll::BufferedEdgeDict{T}, value::T, key::EdgeID) where { T }
#     # TODO: Understand LSP warning
#     Base.setindex!(coll.containers[coll.write], value, key)
# end

function Base.push!(coll::BufferedEdgeDict{T}, edge::T ) where { T <: AbstractEdge }
    c = get!(coll.containers[coll.write], edge.to, Vector{T}())
    push!(c, edge)
end

function Base.getindex(coll::BufferedEdgeDict{T}, key::AgentID) where { T <: AbstractEdge } 
    Base.getindex(coll.containers[coll.read], key)
end

function Base.iterate(coll::BufferedEdgeDict{T}) where { T }
    Base.iterate(coll.containers[coll.read])
end

function Base.iterate(coll::BufferedEdgeDict{T}, state) where { T }
    Base.iterate(coll.containers[coll.read], state)
end

function Base.length(coll::BufferedEdgeDict{T}) where { T }
    Base.length(coll.containers[coll.read])
end
