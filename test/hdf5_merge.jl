using Revise

include("hdf5_common.jl")

import Vahana: has_trait, type_nr, agent_id, agent_nr, AgentID, AgentNr

function test_merge(model)
    sim = runsim(model, false)
    restored = restore(sim)

    fids = open_h5file(restored, sim.name)
    @assert mpi.size == 1 && length(fids) > 1 """
        this should test the merge functionality (reading a distributed
        sim back to a single process), so the test should not be run 
        with mpi but the files should be written from a mpi simulation 
    """
    foreach(close, fids)

    function checkedges(sim_edges, restored_edges, T)
        for to in keys(sim_edges)
            to = AgentID(to)
            AT = has_trait(sim, T, :SingleAgentType) ?
                Agent :
                sim.typeinfos.nodes_id2type[type_nr(to)]
            if AT == Agent
                rto = filter(enumerate(restored.Agent.read.state) |>
                    collect) do (i, state)
                        state.f == sim.Agent.read.state[agent_nr(to)].f 
                    end |> first |> first
                rto = agent_id(sim, AgentNr(rto), Agent)
                if has_trait(sim, T, :IgnoreFrom) &&
                    has_trait(sim, T, :Stateless)
                    if has_trait(sim, T, :SingleAgentType)
                        @test sim_edges[agent_nr(to)] ==
                            restored_edges[agent_nr(rto)]
                    else
                        @test sim_edges[to] == restored_edges[rto]
                    end      
                else 
                    for edge in sim_edges[to]
                        if has_trait(sim, T, :IgnoreFrom)
                            @test edge in restored_edges[rto]
                        else
                            states = map(e -> e.state, restored_edges[rto])
                            @test edge.state in states
                        end
                    end
                end
            end
        end
    end

    checkedges(sim.EdgeState.read, restored.EdgeState.read, EdgeState)

    checkedges(sim.RasterEdge.read, restored.RasterEdge.read, RasterEdge)

    checkedges(sim.StatelessEdge.read, restored.StatelessEdge.read, StatelessEdge)
end

model = ModelTypes() |>
    register_agenttype!(Agent) |>
    register_agenttype!(RasterAgent) |>
    register_edgetype!(EdgeState) |>
    register_edgetype!(RasterEdge) |>
    register_edgetype!(StatelessEdge) |>
    construct_model("hdf5_default")

test_merge(model)

model = ModelTypes() |>
    register_agenttype!(Agent, :Immortal) |>
    register_agenttype!(RasterAgent) |>
    register_edgetype!(EdgeState, :IgnoreFrom) |>
    register_edgetype!(RasterEdge) |>
    register_edgetype!(StatelessEdge) |>
    construct_model("hdf5_ignore_immortal")

test_merge(model)


model = ModelTypes() |>
    register_agenttype!(Agent, :Immortal) |>
    register_agenttype!(RasterAgent) |>
    register_edgetype!(EdgeState, :NumNeighborsOnly) |>
    register_edgetype!(RasterEdge) |>
    register_edgetype!(StatelessEdge, :HasNeighborOnly, :SingleAgentType;
                       to_agenttype = Agent) |>
                           construct_model("hdf5_neighbors")

test_merge(model)

