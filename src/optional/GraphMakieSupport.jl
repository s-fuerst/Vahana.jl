export plotsubgraph

println("Including GraphMakieSupport.jl")

using GraphMakie, Makie, Colors

import Graphs, Graphs.SimpleGraphs

function _agenttostring(ax, idx)
    vg = ax.scene.plots[2].input_args[1].val
    id = vg.g2v[idx]
    as = agentstate_flexible(vg.sim, id)
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
        return str
    end
    ""
end

function plotsubgraph(sim, agenttypes, edgetypes)
    agentcolors = distinguishable_colors(256,
                                         [RGB(1,1,1), RGB(0,0,0)],
                                         dropseed = true)
    edgecolors = distinguishable_colors(256,
                                        [RGB(0.2,0.3,0.3), RGB(0,0,0)],
                                        dropseed = true)    


    vg = vahanagraph(sim, agenttypes, edgetypes)
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
