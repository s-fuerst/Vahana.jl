using Vahana

using Random

import Base.+

struct Buyer <: AbstractAgent
    α::Float64
    B::Int64
end
Buyer() = Buyer(rand(), rand((1:100)))


struct Seller <: AbstractAgent
    p::Float64
    sum_y::Float64
end
Seller() = Seller(rand() + 0.5, 0)

struct KnownSellers <: AbstractEdge end

struct Bought <: AbstractEdge
    x::Float64
    y::Float64
end

struct ExcessDemand <: AbstractGlobal
    x_minus_y::Float64
end

struct AveragePrice <: AbstractGlobal
    p::Float64
end


import Base.+
    
+(a::Bought, b::Bought) = Bought(a.x + b.x, a.y + b.y)


function init_simulation(sim)
    add_agenttype!(sim, Buyer)
    add_agenttype!(sim, Seller)
    add_edgetype!(sim, KnownSellers)
    add_edgetype!(sim, Bought)

    add_globalseriestype!(sim, ExcessDemand)
    add_globalseriestype!(sim, AveragePrice)
    
    buyerids = add_agents!(sim, [ Buyer() for _ in 1:param(sim, :numBuyer)])

    sellerids = add_agents!(sim, [ Seller() for _ in 1:param(sim, :numSeller)])

    for b in buyerids
        for s in rand(sellerids, param(sim, :knownSellers))
            add_edge!(sim, s, b, KnownSellers)
        end
    end

    finish_init!(sim)
end

function calc_demand(b::Buyer, id, sim)
    seller_edge = rand(edges_to(sim, id, KnownSellers))
    s = agentstate_from(sim, seller_edge)
    x = b.B * b.α
    y = b.B * (1 - b.α) / s.p
    add_edge!(sim, id, seller_edge.from, Bought(x, y))
    b
end

function calc_price(s::Seller, id, sim)
    edges = edges_to(sim, id, Bought)
    if length(edges) == 0
        return s
    end
    q = reduce(+, states(edges))
    Seller(q.y / q.x * s.p, q.y)
end

function calc_average_price(sim)
    m = aggregate(sim, Seller, s -> s.p * s.sum_y, +)
    q = aggregate(sim, Seller, s -> s.sum_y, +)
    m / q
end

function run_simulation(steps, params)
    sim = Simulation("Tutorial1", params)

    init_simulation(sim)

    for _ in 1:steps
        apply_transition!(sim, calc_demand, [ Buyer ], [ KnownSellers], [ Bought ])
        add_globalstate!(sim, ExcessDemand(
            aggregate(sim, Bought, b -> b.x - b.y, +)))
        apply_transition!(sim, calc_price, [ Seller ], [ Bought ], [ Bought ])
        add_globalstate!(sim, AveragePrice(calc_average_price(sim)))
    end

    sim
end

