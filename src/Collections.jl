export AgentCollection

export BufferedAgentDict

abstract type AgentCollection{ T } end
# AgentCollection interface:
# setindex!
# getindex
# length

# must have fields:
#   id_counter::AgentNr

function Base.setindex!(::AgentCollection{T}, _, _) where { T }
    @assert false "setIndex! is not defined for this AgentCointainer type"
end

function Base.getindex(::AgentCollection{T}, _) where { T }
    @assert false "getIndex is not defined for this AgentCointainer type"
end

################################################## BufferedAgentDict

# TODO: Disable all constructures beside ()
Base.@kwdef mutable struct BufferedAgentDict{T} <: AgentCollection{T}
    containers = [ Dict{AgentID, T}() for i = 1:NUM_BUFFERS ]
    read = 1
    write = 1
    id_counter::AgentNr = 0
end

function Base.show(io::IO, mime::MIME"text/plain", bad::BufferedAgentDict{T}) where { T }
    printstyled(io, "BufferedAgentDict for Type ", T, "\n"; color = :blue)
    printstyled(io, "Read:\n"; color = :cyan)
    for (k::AgentID, v) in first(bad.containers[bad.read], 5)
        show(io, mime, k)
        println(" => ", v)
    end
    if length(bad.containers[bad.read]) > 5
        println("...")
    end
    printstyled(io, "Write:"; color = :cyan)
    for (k::AgentID, v) in first(bad.containers[bad.write], 5)
        println()
        show(io, mime, k)
        print(" => ", v)
    end
    if length(bad.containers[bad.write]) > 5
        println("\n...")
    end
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
