using Vahana

struct Agent
    i::Int64
    f::Float64
end

struct StatelessAgent end

struct EdgeState
    from::AgentID
    to::AgentID
end

struct StatelessEdge end

struct Globals
    ga::Int64
    gb::Int64
end

struct Params
    pa::Int64
    pb::Int64
end

model = ModelTypes() |>
    register_agenttype!(Agent) |>
    register_edgetype!(EdgeState) |>
    construct_model("hdf5_default")

#function test(model)
sim = model |>
    new_simulation(Params(1,2), Globals(1,2))

ids = add_agents!(sim, [ Agent(i,i) for i in 1:(5 * mpi.size) ])

for id in ids
    add_edge!(sim, id, id, EdgeState(id, id))
    add_edge!(sim, id, ids[2], EdgeState(id, ids[2]))
end

finish_init!(sim;
             partition_algo = :EqualAgentNumbers)

write_globals(sim)
write_agents(sim)
write_edges(sim)

flush(sim.h5file)

sim
#end

#test(model)
