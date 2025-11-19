include("hdf5_common.jl")

import Vahana: has_hint, type_nr, agent_id, agent_nr, AgentID, AgentNr

set_hdf5_path(joinpath(dirname(@__FILE__), "h5"))

function test_merge(model)
    sim = runsim(model, false)
    restored = restore(model, sim)

    fids = Vahana.open_h5file(restored, sim.name)
    @assert mpi.size == 1 && length(fids) > 1 """\n
        This tests the merge functionality (reading distributed simulation 
        files back into a single process). The test itself should run without
        MPI, but should use files previously written by an MPI simulation.
    """
    foreach(close, fids)

    @test sim.params.pa == restored.params.pa
    @test sim.params.pb == restored.params.pb
    @test sim.params.pos == restored.params.pos
    #    @test sim.globals == restored.globals
    @test sim.Agent.last_change == restored.Agent.last_change
    @test sim.RasterAgent.last_change == restored.RasterAgent.last_change

    function checkedges(sim_edges, restored_edges, T)
        @test getproperty(sim, Symbol(T)).last_change ==
            getproperty(restored, Symbol(T)).last_change

        @test num_edges(sim, T) == num_edges(restored, T)
        
        for to in keys(sim_edges)
            to = AgentID(to)
            AT = has_hint(sim, T, :SingleType) ?
                Agent :
                sim.typeinfos.nodes_id2type[type_nr(to)]
            if AT == Agent
                rto = filter(enumerate(restored.Agent.read.state) |>
                    collect) do (i, state)
                        state.f == sim.Agent.read.state[agent_nr(to)].f 
                    end |> first |> first
                rto = Vahana.agent_id(sim, AgentNr(rto), Agent)
                if has_hint(sim, T, :IgnoreFrom) &&
                    has_hint(sim, T, :Stateless)
                    if has_hint(sim, T, :SingleType)
                        @test sim_edges[agent_nr(to)] ==
                            restored_edges[agent_nr(rto)]
                    else
                        @test sim_edges[to] == restored_edges[rto]
                    end      
                elseif ! has_hint(sim, T, :Stateless)
                    for edge in sim_edges[to]
                        if has_hint(sim, T, :IgnoreFrom)
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

@testset "HDF5 merge" begin
    model_default = ModelTypes() |>
        register_agenttype!(Agent) |>
        register_agenttype!(RasterAgent) |>
        register_edgetype!(EdgeState) |>
        register_edgetype!(RasterEdge) |>
        register_edgetype!(StatelessEdge) |>
        register_agenttype!(EmptyAgentVector) |>
        register_edgetype!(EmptyEdgeVector) |> 
        create_model("hdf5_default")

    test_merge(model_default)

    model_immortal = ModelTypes() |>
        register_agenttype!(Agent, :Immortal) |>
        register_agenttype!(RasterAgent) |>
        register_edgetype!(EdgeState, :IgnoreFrom) |>
        register_edgetype!(RasterEdge) |>
        register_edgetype!(StatelessEdge, :Stateless) |>
        create_model("hdf5_ignore_immortal")

    test_merge(model_immortal)
    
    model_neighbors = ModelTypes() |>
        register_agenttype!(Agent, :Immortal) |>
        register_agenttype!(RasterAgent) |>
        register_edgetype!(EdgeState, :NumEdgesOnly) |>
        register_edgetype!(RasterEdge) |>
        register_edgetype!(StatelessEdge, :HasEdgeOnly, :SingleType;
                           target = Agent) |>
                               create_model("hdf5_neighbors")

    test_merge(model_neighbors)

    # this hack should help that the output is not scrambled
    sleep(mpi.rank * 0.05)
end
