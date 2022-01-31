using Vahana
using Test

struct Person <: Agent
    id::T_AgentID
    foo::Int64
end

struct HH <: Agent
    id::T_AgentID
    bar::Int64
end

@testset "Vahana.jl" begin
    sim = Simulation("Example", ())

    p1 = Person(1, 1)
    p2 = Person(2, 1)

    h1 = HH(1, 1)
    h2 = HH(2, 1)

    add_agenttype!(sim, Person)
    @test length(sim.agent_data_t) == 1
    @test sim.type2number[Person] == 1
    @test sim.number2type[1] == Person

    add_agenttype!(sim, HH)
    @test length(sim.agent_data_t) == 2
    @test sim.type2number[HH] == 2
    @test sim.number2type[2] == HH

    add_agents!(sim, p1)
    @test length(sim.agent_data_tp1[Person]) == 1
    @test sim.agent_data_tp1[Person][1] == p1

    add_agents!(sim, p1, p2)
    @test length(sim.agent_data_tp1[Person]) == 2
    @test sim.agent_data_tp1[Person][2] == p2

    add_agents!(sim, [[p1, p2], h1])
    @test length(sim.agent_data_tp1[Person]) == 2
    @test length(sim.agent_data_tp1[HH]) == 1
    @test sim.agent_data_tp1[HH][1] == h1
end
