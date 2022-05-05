# function createAgentBars(sim)
#     [ AgentBar(11), AgentBar(12) ]
# end

# function get_write_agent(sim, id::AgentID)
#     sim.agents[type_nr(id)].containers[sim.agents[type_nr(id)].write][id]
# end

# function get_write_edges(sim, T::DataType, id::AgentID)
#     sim.edges[T].containers[sim.edges[T].write][id]
# end

# function numAgents(sim, ::Type{T}) where {T <: Agent}
#     sim.agents[typeid(sim, T)].containers[sim.agents[typeid(sim, T)].write] |>
#         length
# end


# function transfoo(p::AgentBar, id, sim)
#     AgentBar(p.foo + 10)
# end

# function transfoo2(p::AgentBar, id, sim)
#     s = reduce((s,e) ->
#         s = s + e.state.foo, edges_to(sim, id, FooEdgeState); init = 0) 
#     AgentBar(s)
# end

# function transfoo3(p::AgentBar, id, sim)
#     n = edges_to(sim, id, FooEdgeState)
#     if length(n) > 0
#         s = agentstate(sim, n[1].from).foo
#     else
#         s = -1
#     end
#     AgentBar(s)
# end

# function transnothing(p::AgentBar, _, _)
#     nothing
# end

# function transstateless(h::AgentFoo, id, sim)
#     n = edges_to(sim, id, StatelessEdgeType) |> neighbors
#     if length(n) > 0
#         s = agentstate(sim, n[1]).foo
#     else
#         s = -1
#     end
#     AgentFoo(s)
# end

# function transaddedges(p::AgentBar, id, sim)
#     add_edges!(sim, id, edges_to(sim, id, FooEdgeState))
#     p
# end




@testset "Core" begin
    sim = construct(model, "Test", nothing, nothing)
    
    (a1id, a2id, a3id, avids, avfids) = add_example_network!(sim)
    
    finish_init!(sim)

    @testset "agentstate" begin
        @test agentstate_flexible(sim, a1id) == ADict(1)
        @test agentstate(sim, a2id, Val(ADict)) == ADict(2)
        @test agentstate(sim, avids[1], Val(AVec)) == AVec(1)
        @test agentstate(sim, avfids[1], Val(AVecFixed)) == AVecFixed(1)
        @test agentstate(sim, avids[10], Val(AVec)) == AVec(10)
        @test agentstate(sim, avfids[10], Val(AVecFixed)) == AVecFixed(10)
        # calling agentstate with the wrong typ should throw an AssertionError
        @test_throws AssertionError agentstate(sim, a2id, Val(AVec))
        @test_throws AssertionError agentstate(sim, avids[1], Val(ADict))
    end

    @testset "edges_to" begin
        @test size(edges_to(sim, a1id, Val(ESDict)), 1) == 4
        @test size(edges_to(sim, a1id, Val(ESLDict1)), 1) == 10
        @test size(edges_to(sim, avids[1], Val(ESLDict2)), 1) == 1
        @test size(edges_to(sim, avids[10], Val(ESLDict2)), 1) == 1
        # Check that we can call edges_to also for an empty set of neighbors
        @test size(edges_to(sim, a2id, Val(ESDict)), 1) == 0
        @test size(edges_to(sim, a2id, Val(ESLDict1)), 1) == 0
        @test size(edges_to(sim, a2id, Val(ESLDict2)), 1) == 0
    end

    @testset "neighbors & edgestates" begin
        edges = edges_to(sim, a1id, Val(ESLDict1))
        @test neighbors(edges)[1] == avids[1]
        @test neighbors(edges)[10] == avids[10]
        edges = edges_to(sim, avids[10], Val(ESLDict2))
        @test neighbors(edges)[1] == avfids[10]
    end

    @testset "neighborstates" begin
        @test neighborstates(sim, a1id, Val(ESLDict1), Val(AVec))[1] == AVec(1)
        @test neighborstates_flexible(sim, a1id, Val(ESDict))[2] == ADict(3)
    end

    @testset "transition" begin
        # we need this for each node factory
        apply_transition!(sim, create_sum_state_neighbors(Val(ESLDict1)),
                          [ ADict ], [ ESLDict1 ], [])
        @test agentstate(sim, a1id, Val(ADict)) == ADict(sum(1:10))
    end
end



