using GraphMakie, Makie, ColorSchemes, Colors

import Graphs, Graphs.SimpleGraphs

export create_graphplot, update_graphplot!
export plot, axis, figure
export clicked_agent

Base.@kwdef mutable struct VahanaPlot
    sim::Simulation
    vg::VahanaGraph
    # a Vahanaplot encapsulate a GraphMakie plot
    # figure, axis, plot are all returned by the graphplot function
    figure
    axis
    plot
    # the next field are AgentIDs or Edges that where clicked 
    # or that are actually hovered
    clicked_agent = Vector{AgentID}()
    # all the agenttypes where the position should be jittered
    pos_jitter = Dict{DataType, Float64}()
    # the offset of an agent is keeped constant, and stored in this dict
    jitter_val = Dict{AgentID, NTuple{2, Float64}}()
    # the function to call for updating the attributes of the graph
    update_fn = nothing
    # the edgetypes used for creating the vahanagraph
    edgetypes
end

function _set_plot_attrs!(vp, idx, attrs, aid, T)
    for (k, v) in attrs
        # jitter node positions
        if k == :node_pos
            j = get(vp.pos_jitter, T, 0.0) 
            jit = get!(vp.jitter_val, aid, ( rand() * j - j/2, rand() * j - j/2 ))
            v = [ v[1] + jit[1], v[2] + jit[2]]
            # convert name colors to color values
        elseif (k == :node_color || k == :edge_color) &&
            (typeof(v) == String || typeof(v) == Symbol)
            v = parse(Colorant, v)
            # elseif k == :edge_transparency
            #     # change the 
            #     k = :edge_color
            #     v = HSLA(HSL(vp.plot[:edge_color][][idx]), v)
        end

        vp.plot[k][][idx] = v
    end
end

function _set_agents_plot_attrs!(f, vp)
    disable_transition_checks(vp.sim, true)
    changed_attrs = Set{Symbol}()
    for T in vp.vg.agenttypes
        if hasmethod(f, (T, AgentID, VahanaPlot))
            for id in 1:length(readstate(vp.sim, T))
                if length(readdied(vp.sim, T)) == 0 || readdied(vp.sim, T)[id] == false
                    aid = agent_id(vp.sim, AgentNr(id), T)
                    state = agentstate(vp.sim, aid, T)
                    new::Dict{Symbol, Any} = f(state, aid, vp)
                    _set_plot_attrs!(vp, vp.vg.v2g[aid], new, aid, T)
                    foreach(a -> push!(changed_attrs, a), keys(new))
                end
            end
        end
    end
    for a in changed_attrs 
        vp.plot[a][] = vp.plot[a][]
    end
    disable_transition_checks(vp.sim, false)
    nothing
end

function _set_edge_plot_properties!(f, vp)
    disable_transition_checks(vp.sim, true)
    changed_attrs = Set{Symbol}()
    for i in 1:Graphs.ne(vp.vg)
        tid = vp.vg.edgetypeidx[i]
        T = vp.edgetypes[tid]
        if hasmethod(f, (T, AgentID, AgentID, VahanaPlot))
            vedge = vp.vg.edges[i]
            from = vp.vg.g2v[vedge.src]
            to = vp.vg.g2v[vedge.dst]
            es = has_hint(vp.sim, T, :Stateless) ? T() :
                (filter(edges(vp.sim, to, T)) do edge
                     edge.from == from
                 end |> only).state
            new = f(es, from, to, vp)
            _set_plot_attrs!(vp, i, new, 0, T)
            foreach(a -> push!(changed_attrs, a), keys(new))
        end
    end
    for a in changed_attrs
        vp.plot[a][] = vp.plot[a][]
    end
    disable_transition_checks(vp.sim, false)
    nothing
end

import Base.display

display(vp::VahanaPlot) = Base.display(vp.figure)

obsrange(x) = 1:length(x)

"""
    create_graphplot(sim; [agenttypes, edgetypes, update_fn, pos_jitter])

Creates an interactive Makie plot for the the simulation `sim`.

The graph can be restricted to a subgraph with the given `agentypes`
and `edgestatetypes`, see [`vahanagraph`](@ref) for details.

To modify the properties of the edges and agents a `update_fn` can be
defined. This function is called for each agent and edge (of types in agenttypes
and edgetypes) and must have for agents the signature

    f(state, id, vp::VahanaPlot)

and for edges

    f(state, source, target, vp::VahanaPlot)

The arguments of this functions are `state` for the agent or edgestate,
ID for the `id` of the agent called, `from` and `to` for the ID of the
agents at the source or target of the edge. `vp` is the struct
returned by `create_graphplot` and can be used to determine the id of
last clicked agent by calling `clicked_agent(vp)`. The function must
return a Dict with property names (as Symbol) as keys and their values
as values. The following properties are available: `node_pos`,
`node_color`, `node_size`, `node_marker`, `nlabels`, `nlabels_align`,
`nlabels_color`, `nlabels_distance`, `nlabels_offset`,
`nlabels_textsize`, `edge_color`, `edge_width`, `elables`,
`elables_align`, `elables_color`, `elables_distance`,
`elables_offset`, `elables_rotation`, `elables_shift`, `elables_side`,
`elabes_textsize`, `arrow_shift`, and `arrow_size`.

In the case that create_graphplot is not called from a parallel
simulation, it is possible to call inside `update_fn` functions that
are normally only available inside of a transition function (like
[`agentstate`](@ref), [`neighborstates`](@ref) etc.).

The `pos_jitter` argument must be a dictonary of agentstypes to float
values. The node positions will be then (x + rand() * jitter - jitter/2,
y + rand() * jitter - jitter/2).

The `edge_plottype` keyword is forwarded to GraphMakie.graphplot.

Returns a VahanaPlot structure that can be used to access the Makie
figure, axis and plot via call to the methods `figure`, `axis` and
`plot` with this struct as (single) argument.

!!! info 

    `create_graphplot` is only available when the GraphMakie package and a
    Makie backend is imported by the client.

!!! warning

    For the mouse click and hover interaction, the current state of
    the simulation used to construct the VahanaGraph is accessed. In
    the case that a transition function is called after the
    construction of the VahanaGraph, and this transition function
    modifies the structure of the Graph (add/removes nodes or edges),
    then this changes are not reflected by the current implementation
    and can cause errors.

See also [`vahanagraph`](@ref)
"""
function create_graphplot(sim::Simulation;
                   agenttypes::Vector{DataType} = sim.typeinfos.nodes_types,
                   edgetypes::Vector{DataType} = sim.typeinfos.edges_types,
                   update_fn = nothing,
                   pos_jitter = nothing,
                   edge_plottype = Makie.automatic)
    vg = vahanagraph(sim;
                     agenttypes = agenttypes,
                     edgetypes = edgetypes,
                     drop_multiedges = true)

    f, ax, p = _plot(vg, edge_plottype)

    vp = VahanaPlot(sim = sim,
                    vg = vg,
                    figure = f,
                    axis = ax,
                    plot = p,
                    update_fn = update_fn,
                    edgetypes = edgetypes)

    if !isnothing(pos_jitter)
        vp.pos_jitter = Dict(pos_jitter)
    end

    function _node_click_action(idx, _, _)
        vp.clicked_agent = vg.g2v[idx]
        update_graphplot!(vp)
    end
    register_interaction!(vp.axis,
                          :nodeclick, NodeClickHandler(_node_click_action))

    # if step_fn !== nothing
    #     f[2, 1] = buttongrid = GridLayout(tellwidth = false)

    #     buttongrid[1, 1] = resetbutton = Button(f, label="Reset")
    #     on(resetbutton.clicks) do _
    #         @info "reset"
    #         finish_simulation!(vp.sim)
    #         vp.sim = copy_simulation(vp.original)
    #         vp.vg = vahanagraph(vp.sim; agenttypes = agenttypes, edgetypes = edgetypes)
    #         update_graphplot!(vp)
    #     end
        
    #     buttongrid[1, 2] = stepbutton = Button(f, label="Step")
    #     on(stepbutton.clicks) do _
    #         step_fn(vp.sim)
    #         vp.vg = vahanagraph(vp.sim; agenttypes = agenttypes, edgetypes = edgetypes)
    #         update_graphplot!(vp)
    #     end
    # end

    if update_fn === nothing && pos_jitter === nothing
        hidedecorations!(vp.axis)
    end


    update_graphplot!(vp)
end

function update_graphplot!(vp::VahanaPlot)
    if ! isnothing(vp.update_fn)
        _set_agents_plot_attrs!(vp.update_fn, vp)
        _set_edge_plot_properties!(vp.update_fn, vp)
    end

    autolimits!(vp.axis)
    
    vp
end

figure(vp::VahanaPlot) = vp.figure

axis(vp::VahanaPlot) = vp.axis

plot(vp::VahanaPlot) = vp.plot

clicked_agent(vp::VahanaPlot) = vp.clicked_agent

function _agenttostring(vg, idx)
    id = vg.g2v[idx]
    disable_transition_checks(vg.sim, true)
    as = agentstate_flexible(vg.sim, id)
    disable_transition_checks(vg.sim, false)
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

    if :Stateless in vg.sim.typeinfos.edges_attr[totype][:hints]
        return string(totype)
    end
    disable_transition_checks(vg.sim, true)
    es = edges(vg.sim, to, totype)
    disable_transition_checks(vg.sim, false)
    if ! isnothing(es)
        es = (filter(es) do edge
                  edge.from == from
              end |> first).state
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

function _plot(vg::VahanaGraph, edge_plottype)
    edgecolors = ColorSchemes.glasbey_bw_minc_20_maxl_70_n256
    agentcolors = ColorSchemes.glasbey_bw_minc_20_hue_150_280_n256

    rv = 1:nv(vg)
    re = 1:ne(vg)
    p = graphplot(vg,
                  edge_plottype = edge_plottype,
                  node_color = [ agentcolors[type_nr(vg.g2v[i])]
                                 for i in rv ],
                  node_size = [ 12 for _ in rv ],
                  node_marker = [ :circle for _ in rv ],
                  #                  node_attr = [ Dict{Symbol, Observable}() for _ in rv ],
                  nlabels = [ "" for _ in rv ],
                  nlabels_align = [ (:left, :bottom) for _ in rv ],
                  nlabels_color = [ :black for _ in rv ],
                  nlabels_distance = [ 0.0 for _ in rv ],
                  nlabels_offset = [ Makie.Point(0, 0) for _ in rv ],
                  nlabels_textsize = [ 14 for _ in rv ],
                  edge_color = [ edgecolors[vg.edgetypeidx[i]]
                                 for i in re ],
                  edge_width = [ 1.0 for _ in re ],
                  elabels = [ "" for _ in re ],
                  elabels_align = [ (:left, :bottom) for _ in re ],
                  elabels_color = [ :black for _ in re ],
                  elabels_distance = [ 0.0 for _ in re ],
                  elabels_offset = [ Makie.Point(0, 0) for _ in re ],
                  elabels_rotation = Vector{Any}([ Makie.automatic for _ in re ]),
                  elabels_shift = [ 0.5 for _ in re ],
                  elabels_side = [ :left for _ in re ],
                  elabels_textsize = [ 14 for _ in re ],
                  arrow_shift = [ 0.5 for _ in re ],
                  #arrow_show = [ true for _ in re ], ! supported by Makie
                  arrow_size = [ 12.0 for _ in re ]
                  )

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
    disable_transition_checks(vg.sim, true)
    s = agentstate_flexible(vg.sim, vg.g2v[idx])
    disable_transition_checks(vg.sim, false)
    s
end
