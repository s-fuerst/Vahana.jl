using Base: nothing_sentinel
using Makie: axislines!
using GraphMakie, Makie, Colors

import Graphs, Graphs.SimpleGraphs, NetworkLayout

export plot, create_plot, update_plot!
export set_agent_positions!, set_agent_colors!
export set_edge_colors!

Base.@kwdef mutable struct VahanaPlot
    sim::Simulation
    vg::VahanaGraph
    figure
    axis
    plot
    last_clicked_agent = nothing
    pos_jitter = Dict{DataType, Float64}()
    jitter_val = Dict{AgentID, NTuple{2, Float64}}()
    node_pos_fn = nothing
    node_color_fn = nothing
    edge_color_fn = nothing
end

function _set_agent_plot_properties!(g, f, vp, prop)
    disable_transition_checks(true)
    for T in vp.vg.agenttypes
        for id in 1:length(readstate(vp.sim, T))
            if length(readdied(vp.sim, T)) == 0 || readdied(vp.sim, T)[id] == false
                aid = agent_id(vp.sim, AgentNr(id), T)
                state = agentstate(vp.sim, aid, T)
                new = g(f(state, aid, vp.sim), aid, T)
                if new !== nothing
                    vp.plot[prop][][vp.vg.v2g[aid]] = new
                end
            end
        end
    end
    vp.plot[prop][] = vp.plot[prop][];
    disable_transition_checks(false)
    nothing
end

function _set_edge_plot_properties!(g, f, vp, prop)
    disable_transition_checks(true)
    for i in 1:Graphs.ne(vp.vg)
        tid = vp.vg.edgetypeidx[i]
        T = vp.sim.typeinfos.edges_types[tid]
        vedge = vp.vg.edges[i]
        from = vp.vg.g2v[vedge.src]
        to = vp.vg.g2v[vedge.dst]
        edges = edges_to(vp.vg.sim, to, T)
        if isnothing(edges) # this can happen after a state change of sim
            continue
        end
        es = has_trait(vp.sim, T, :Stateless) ? T() : (filter(edges) do edge
                                                           edge.from == from
                                                       end |> only).state
        new = g(f(es, from, to, vp.last_clicked_agent, vp.sim), to, T)
        if new !== nothing
            vp.plot[prop][][i] = new
        end
    end
    vp.plot[prop][] = vp.plot[prop][]
    disable_transition_checks(false)
    nothing
end

function set_agent_colors!(f, vp::VahanaPlot)
    _set_agent_plot_properties!(f, vp, :node_color) do color, _, _
        typeof(color) == String ? parse(Colorant, color) : color
    end
end

set_agent_colors!(vp::VahanaPlot, f) = set_agent_colors!(f, vp)

function set_edge_colors!(f, vp::VahanaPlot)
    _set_edge_plot_properties!(f, vp, :edge_color) do color, to, _
        typeof(color) == String ? parse(Colorant, color) : color
    end
end

function set_agent_positions!(f, vp; jitter = nothing)
    _set_agent_plot_properties!(f, vp, :node_pos) do pos, id, T
        j = isnothing(jitter) ? get(vp.pos_jitter, T, 0.0) : jitter
        jit = get!(vp.jitter_val, id, ( rand() * j - j/2, rand() * j - j/2 ))
        [ pos[1] + jit[1], pos[2] + jit[2]]
    end
end


import Base.display

display(vp::VahanaPlot) = Base.display(vp.figure)

function create_plot(sim::Simulation;
              agenttypes::Vector{DataType} = sim.typeinfos.nodes_types,
              edgetypes::Vector{DataType} = sim.typeinfos.edges_types,
              agent_pos_fn = nothing,
              pos_jitter = nothing,
              agent_color_fn = nothing,
              edge_color_fn = nothing)
    vg = vahanagraph(sim; agenttypes = agenttypes, edgetypes = edgetypes)

    f, ax, p = plot(vg)
    vp = VahanaPlot(sim = sim,
                    vg = vg,
                    figure = f,
                    axis = ax,
                    plot = p,
                    node_pos_fn = agent_pos_fn,
                    node_color_fn = agent_color_fn,
                    edge_color_fn = edge_color_fn)

    if !isnothing(pos_jitter)
        vp.pos_jitter = Dict(pos_jitter)
    end

    function _node_click_action(idx, _, _)
        vp.last_clicked_agent = vg.g2v[idx]
        update_plot!(vp)
    end
    register_interaction!(vp.axis,
                          :nodeclick, NodeClickHandler(_node_click_action))
        
    update_plot!(vp)
end

function update_plot!(vp::VahanaPlot)
    isnothing(vp.node_color_fn) ? nothing :
        set_agent_colors!(vp.node_color_fn, vp)
    isnothing(vp.node_pos_fn) ? nothing :
        set_agent_positions!(vp.node_pos_fn, vp)
    isnothing(vp.edge_color_fn) ? nothing :
        set_edge_colors!(vp.edge_color_fn, vp)
    vp
end

function _agenttostring(vg, idx)
    id = vg.g2v[idx]
    disable_transition_checks(true)
    as = agentstate_flexible(vg.sim, id)
    disable_transition_checks(false)
    str = " $(typeof(as)) (Nr: $(Vahana.agent_nr(id)))"
    if nfields(as) > 0
        fnames = as |> typeof |> fieldnames
        for f in fnames
            str *= "\n $f=$(getfield(as, f))"
        end
    end
    str
end

function _edgetostring(vg, idx) # from and to are vahanagraph indicies
    vedge = vg.edges[idx]
    from = vg.g2v[vedge.src]
    to = vg.g2v[vedge.dst]
    totype = vg.edgetypes[vg.edgetypeidx[idx]]

    if :Stateless in vg.sim.typeinfos.edges_attr[totype][:traits]
        return string(totype)
    end
    disable_transition_checks(true)
    edges = edges_to(vg.sim, to, totype)
    disable_transition_checks(false)
    if ! isnothing(edges)
        es = (filter(edges) do edge
                  edge.from == from
              end |> only).state
        str = " $(typeof(es))"
        if nfields(es) > 0
            fnames = es |> typeof |> fieldnames
            for f in fnames
                str *= "\n $f=$(getfield(es, f))"
            end
        end
        str
    else
        ""
    end
end

"""
    plot(vg::VahanaGraph)

Creates an interactive Makie plot for the VahanaGraph `vg`.

Returns the usual Makie combination of figure, axis, plot.

!!! info 

    `plot` is only available when the GraphMakie package and a
    Makie backend is imported by the client.

!!! warning

    For the mouse hover interaction, the current state of the
    simulation used to construct the VahanaGraph is accessed. In the
    case that a transition function is called after the construction
    of the VahanaGraph, and this transition function modifies the
    structure of the Graph (add/removes nodes or edges), then this
    changes are not reflected by the current implementation and can
    cause errors.

See also [`vahanagraph`](@ref)
"""
function plot(vg::VahanaGraph)
    agentcolors = distinguishable_colors(256,
                                         [RGB(1,1,1), RGB(0,0,0)],
                                         dropseed = true)
    edgecolors = distinguishable_colors(256,
                                        [RGB(0.2,0.3,0.3), RGB(0,0,0)],
                                        dropseed = true)    


    rv = 1:nv(vg)
    re = 1:ne(vg)
    p = graphplot(vg,
                  node_size = [ 10 for _ in rv],
                  node_color = [ agentcolors[type_nr(vg.g2v[i]) + 1]
                                 for i in rv ],
                  nlabels = [ "" for _ in rv ],
                  edge_color = [ edgecolors[vg.edgetypeidx[i] + 1]
                                 for i in re ],
                  elabels = [ "" for _ in re ])
    
    p.plot.elabels_align = (:left, :bottom)
    p.plot.elabels_rotation = 0

    # hidedecorations!(p.axis)
    hidespines!(p.axis)

    function _node_hover_action(state, idx, _, axis)
        p.plot.nlabels[][idx] = state ? string(_agenttostring(vg, idx)) : ""
        p.plot.nlabels[] = p.plot.nlabels[]
    end
    register_interaction!(p.axis, :nhover, NodeHoverHandler(_node_hover_action))
    
    function _edge_hover_action(state, idx, _, axis)
        p.plot.elabels[][idx] = state ? string(_edgetostring(vg, idx)) : ""
        p.plot.elabels[] = p.plot.elabels[]
    end
    register_interaction!(p.axis, :ehover, EdgeHoverHandler(_edge_hover_action))
    
    p.figure, p.axis, p.plot
end

function nodestate(vg::VahanaGraph, idx::Int64)
    disable_transition_checks(true)
    s = agentstate_flexible(vg.sim, vg.g2v[idx])
    disable_transition_checks(false)
    s
end
