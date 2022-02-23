export EdgeCollection

export BufferedEdgeDict

abstract type EdgeCollection{ T <: AbstractEdge } end
# EdgeCollection interface:
# Base.push!
# Base.getindex
# Base.length
# Base.iterate
# finish_init!
# prepare_write!
# finish_write!

# must have fields:
#   id_counter::EdgeNr

function finish_init!(::EdgeCollection)
end

function Base.getindex(coll::EdgeCollection, key::AgentID)
    Base.getindex(read_container(coll), key)
end

function Base.iterate(coll::EdgeCollection) 
    Base.iterate(read_container(coll))
end

function Base.iterate(coll::EdgeCollection, state) 
    Base.iterate(read_container(coll), state)
end

function Base.length(coll::EdgeCollection) 
    Base.length(read_container(coll))
end


################################################## BufferedEdgeDict

# TODO: Disable all constructures beside ()
Base.@kwdef mutable struct BufferedEdgeDict{T} <: EdgeCollection{T}
    containers::Vector{Dict{AgentID, Vector{T}}} =
        [ Dict{AgentID, Vector{T}}() for i = 1:NUM_BUFFERS ]
    read::Int8 = 2
    write::Int8 = 1
end

function Base.push!(coll::BufferedEdgeDict{T}, edge::T ) where { T <: AbstractEdge }
    @mayassert coll.read != coll.write "Can not add edges of type " *
        "$(statetype(edge)). Maybe you must add $(statetype(edge)) " *
        "to the list of variant types in apply_transition!"
    c = get!(coll.containers[coll.write], edge.to, Vector{T}())
    push!(c, edge)
end

function read_container(coll::BufferedEdgeDict{T}) where { T <: AbstractEdge }
    coll.containers[coll.read]
end
 
# function Base.getindex(coll::BufferedEdgeDict{T}, key::AgentID) where { T <: AbstractEdge } 
#     Base.getindex(coll.containers[coll.read], key)
# end

# function Base.iterate(coll::BufferedEdgeDict{T}) where { T }
#     Base.iterate(coll.containers[coll.read])
# end

# function Base.iterate(coll::BufferedEdgeDict{T}, state) where { T }
#     Base.iterate(coll.containers[coll.read], state)
# end

# function Base.length(coll::BufferedEdgeDict{T}) where { T }
#     Base.length(coll.containers[coll.read])
# end

statetype(::BufferedEdgeDict{T}) where { T } = T

function finish_init!(coll::BufferedEdgeDict{T}) where { T }
    coll.read = 1
end

function prepare_write!(coll::BufferedEdgeDict{T}) where { T }
    prepare_buffered_write!(coll)
end

function finish_write!(coll::BufferedEdgeDict{T}) where { T }
    finish_buffered_write!(coll)
end
