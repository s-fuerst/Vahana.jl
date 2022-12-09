export plot, nodestate

using GraphMakie, Makie, Colors

import Graphs, Graphs.SimpleGraphs, NetworkLayout

function _agenttostring(ax, idx)
    vg = ax.scene.plots[2].input_args[1].val
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

function _edgetostring(ax, idx) # from and to are vahanagraph indicies
    vg = ax.scene.plots[2].input_args[1].val
    vedge = vg.edges[idx]
    from = vg.g2v[vedge.src]
    to = vg.g2v[vedge.dst]
    totype = vg.edgetypes[vg.edgetypeidx[idx]]

    if :Stateless in vg.sim.typeinfos.edges_attr[totype][:traits]
        return string(totype)
    end
    edges = edges_to(vg.sim, to, totype)
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

    p.plot.layout[] = NetworkLayout.Stress()
    
    p.plot.elabels_align = (:left, :bottom)
    p.plot.elabels_rotation = 0

    hidedecorations!(p.axis)
    hidespines!(p.axis)

    function _node_hover_action(state, idx, _, axis)
        p.plot.nlabels[][idx] = state ? string(_agenttostring(axis, idx)) : ""
        p.plot.nlabels[] = p.plot.nlabels[]
    end
    register_interaction!(p.axis, :nhover, NodeHoverHandler(_node_hover_action))
    
    function _edge_hover_action(state, idx, _, axis)
        p.plot.elabels[][idx] = state ? string(_edgetostring(axis, idx)) : ""
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
