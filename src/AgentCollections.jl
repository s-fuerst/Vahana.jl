include("Collections.jl")

export AgentCollection

export BufferedAgentDict

abstract type AgentCollection{ T } end
# AgentCollection interface:
# Base.setindex!
# Base.getindex
# Base.length
# Base.iterate
# finish_init!
# show_length for pretty printing

# must have fields:
#   id_counter::AgentNr

is_agentcollection(coll) = eltype(coll) == AgentCollection

function Base.setindex!(::AgentCollection{T}, _, _) where { T }
    @assert false "setIndex! is not defined for this AgentCollection type"
end

function Base.getindex(::AgentCollection{T}, _) where { T }
    @assert false "getIndex is not defined for this AgentCollection type"
end

function finish_init!(::AgentCollection)
end

################################################## BufferedAgentDict

# TODO: Disable all constructures beside ()
Base.@kwdef mutable struct BufferedAgentDict{T} <: AgentCollection{T}
    containers = [ Dict{AgentID, T}() for i = 1:NUM_BUFFERS ]
    read = 1
    write = 1
    id_counter::AgentNr = 0
end


function Base.setindex!(coll::BufferedAgentDict{T}, value::T, key::AgentID) where { T }
    # TODO: Understand LSP warning
    Base.setindex!(coll.containers[coll.write], value, key)
end

function Base.getindex(coll::BufferedAgentDict{T}, key::AgentID) where { T }
    Base.getindex(coll.containers[coll.read], key)
end

function Base.iterate(coll::BufferedAgentDict{T}) where { T }
    Base.iterate(coll.containers[coll.read])
end

function Base.iterate(coll::BufferedAgentDict{T}, state) where { T }
    Base.iterate(coll.containers[coll.read], state)
end

function Base.length(coll::BufferedAgentDict{T}) where { T }
    Base.length(coll.containers[coll.read])
end

function prepare_write!(coll::BufferedAgentDict{T}) where { T }
    prepare_buffered_write!(coll)
end

function finish_write!(coll::BufferedAgentDict{T}) where { T }
    finish_buffered_write!(coll)
end

