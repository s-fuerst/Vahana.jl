

@testset "Edges Iter" begin
    eisim = new_simulation(model_edges, nothing, nothing)

    aids = add_agents!(eisim, [ Agent(i) for i in 1:10 ])
    for id in aids
        add_edge!(eisim, aids[1], id, EdgeD(id))
        add_edge!(eisim, id, aids[1], EdgeD(id))
        add_edge!(eisim, aids[1], id, EdgeS())
        add_edge!(eisim, id, aids[1], EdgeS())
        add_edge!(eisim, aids[1], id, EdgeE(id))
        add_edge!(eisim, aids[1], id, EdgeT(id))
        add_edge!(eisim, id, aids[1], EdgeT(id))
        add_edge!(eisim, aids[1], id, EdgeI(id))
        add_edge!(eisim, id, aids[1], EdgeI(id))
    end

    @test Vahana.edges_iterator(eisim, EdgeD, false) |> length == 20
    @test Vahana.edges_iterator(eisim, EdgeD, false) |> collect |> length == 20

    finish_init!(eisim)

    
    @test Vahana.edges_iterator(eisim, EdgeD) |> length == 20
    @test Vahana.edges_iterator(eisim, EdgeD) |> collect |> length == 20
    @test Vahana.edges_iterator(eisim, EdgeS) |> length == 20
    @test Vahana.edges_iterator(eisim, EdgeS) |> collect |> length == 20
    @test Vahana.edges_iterator(eisim, EdgeE) |> length == 10
    @test Vahana.edges_iterator(eisim, EdgeE) |> collect |> length == 10
    @test Vahana.edges_iterator(eisim, EdgeT) |> length == 20
    @test Vahana.edges_iterator(eisim, EdgeT) |> collect |> length == 20
    @test Vahana.edges_iterator(eisim, EdgeI) |> length == 20
    @test Vahana.edges_iterator(eisim, EdgeI) |> collect |> length == 20

    @test Vahana.edges_iterator(eisim, EdgeSE) |> length == 0
    @test Vahana.edges_iterator(eisim, EdgeSE) |> collect |> length == 0
end
