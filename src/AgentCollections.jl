abstract type AgentCollection{ T } end
# AgentCollection interface:
# Base.setindex!
# Base.getindex
# Base.length
# Base.iterate
# finishinit!
# prepare_write!
# finish_write!

# show_length for pretty printing

# must have fields:
#   id_counter::AgentNr

is_agentcollection(coll) = eltype(coll) == AgentCollection

finishinit!(::AgentCollection) = nothing

function Base.getindex(coll::AgentCollection, key::AgentID)
    Base.getindex(read_container(coll), key)
end

function Base.iterate(coll::AgentCollection) 
    Base.iterate(read_container(coll))
end

function Base.iterate(coll::AgentCollection, state) 
    Base.iterate(read_container(coll), state)
end

function Base.length(coll::AgentCollection) 
    Base.length(read_container(coll))
end

################################################## BufferedAgentDict

# TODO: Disable all constructures beside ()
Base.@kwdef mutable struct BufferedAgentDict{T} <: AgentCollection{T}
    containers = [ Dict{AgentID, T}() for i = 1:NUM_BUFFERS ]
    read = 2
    write = 1
    id_counter::AgentNr = 0
end

function read_container(coll::BufferedAgentDict{T}) where { T <: Agent }
    coll.containers[coll.read]
end


function Base.setindex!(coll::BufferedAgentDict{T}, value::T, key::AgentID) where { T }
    # TODO: Understand LSP warning
    @mayassert coll.read != coll.write "Can not add agents of type $T. " *
        "Maybe you must add $T to the list of rebuild types in apply_transition!"
    Base.setindex!(coll.containers[coll.write], value, key)
end

# function Base.getindex(coll::BufferedAgentDict{T}, key::AgentID) where { T }
#     Base.getindex(coll.containers[coll.read], key)
# end

# function Base.iterate(coll::BufferedAgentDict{T}) where { T }
#     Base.iterate(coll.containers[coll.read])
# end

# function Base.iterate(coll::BufferedAgentDict{T}, state) where { T }
#     Base.iterate(coll.containers[coll.read], state)
# end

# function Base.length(coll::BufferedAgentDict{T}) where { T }
#     Base.length(coll.containers[coll.read])
# end

function finishinit!(coll::BufferedAgentDict{T}) where { T }
    coll.read = 1
    nothing
end

function prepare_write!(coll::BufferedAgentDict{T}) where { T }
    prepare_buffered_write!(coll)
    coll.containers[coll.write] = Dict{AgentID, T}()
end

function finish_write!(coll::BufferedAgentDict{T}) where { T }
    finish_buffered_write!(coll)
end

