using Makie: Observables
using GraphMakie, Makie, Colors

import Graphs, Graphs.SimpleGraphs, NetworkLayout

export plot, create_plot, update_plot!

Base.@kwdef mutable struct VahanaPlot
    sim::Simulation
    original::Simulation
    vg::VahanaGraph
    # a Vahanaplot encapsulate a GraphMakie plot
    # figure, axis, plot are all returned by the graphplot function
    figure
    axis
    plot
    # the next field are AgentIDs or Edges that where clicked 
    # or that are actually hovered
    clicked_agent = Vector{AgentID}()
    clicked_edge = Vector{Any}()
    hovered_agent::Union{AgentID, Nothing} = nothing
    hovered_edge = nothing
    # all the agenttypes where the position should be jittered
    pos_jitter = Dict{DataType, Float64}()
    # the offset of an agent is keeped constant, and stored in this dict
    jitter_val = Dict{AgentID, NTuple{2, Float64}}()
    # the function to call for updating the attributes of the graph
    update_fn = nothing
    step_fn = nothing
    reset_fn = nothing
    # click_fn = nothing
    # hover_fn = nothing
    # add a lineplot for the following globals vector
    globals = Dict{Symbol, Observable}()
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
    disable_transition_checks(true)
    changed_attrs = Set{Symbol}()
    for T in vp.vg.agenttypes
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
    for a in changed_attrs 
        vp.plot[a][] = vp.plot[a][]
    end
    disable_transition_checks(false)
    nothing
end

function _set_edge_plot_properties!(f, vp)
    disable_transition_checks(true)
    changed_attrs = Set{Symbol}()
    for i in 1:Graphs.ne(vp.vg)
        tid = vp.vg.edgetypeidx[i]
        T = vp.sim.typeinfos.edges_types[tid]
        vedge = vp.vg.edges[i]
        from = vp.vg.g2v[vedge.src]
        to = vp.vg.g2v[vedge.dst]
        edges = edges_to(vp.sim, to, T)
        if isnothing(edges) # this can happen after a state change of sim
            continue
        end
        es = has_trait(vp.sim, T, :Stateless) ? T() : (filter(edges) do edge
                                                           edge.from == from
                                                       end |> only).state
        new = f(es, from, to, vp)
        attrs = _set_plot_attrs!(vp, i, new, 0, T)
        foreach(a -> push!(changed_attrs, a), keys(new))
    end
    for a in changed_attrs
        vp.plot[a][] = vp.plot[a][]
    end
    disable_transition_checks(false)
    nothing
end

import Base.display

display(vp::VahanaPlot) = Base.display(vp.figure)

obsrange(x) = 1:length(x)

function _draw_globals(sim, axglobals, obglobals)
    plots = Dict{Symbol, Lines}()
    for (name, obs) in obglobals
            
        plots[name] = lines!(axglobals, lift(obsrange, obs), obs, label = String(name))
    end
    axislegend(axglobals)
end

function create_plot(sim::Simulation;
              agenttypes::Vector{DataType} = sim.typeinfos.nodes_types,
              edgetypes::Vector{DataType} = sim.typeinfos.edges_types,
              update_fn = nothing,
              step_fn = nothing,
              reset_fn = nothing,
              pos_jitter = nothing,
              globals::Vector{Symbol} = Vector{Symbol}())
    vg = vahanagraph(sim; agenttypes = agenttypes, edgetypes = edgetypes)

    f, ax, p = plot(vg)

    obglobals = map(sym -> sym => Observable(getglobal(sim, sym)), globals)
    
    vp = VahanaPlot(sim = copy_simulation(sim),
                    original = sim,
                    vg = vg,
                    figure = f,
                    axis = ax,
                    plot = p,
                    update_fn = update_fn,
                    step_fn = step_fn,
                    reset_fn = reset_fn,
                    globals = obglobals)

    if !isnothing(pos_jitter)
        vp.pos_jitter = Dict(pos_jitter)
    end

    function _node_click_action(idx, _, _)
        vp.clicked_agent = vg.g2v[idx]
        update_plot!(vp)
    end
    register_interaction!(vp.axis,
                          :nodeclick, NodeClickHandler(_node_click_action))

    # if length(obglobals) > 0
    #     _draw_globals(sim, Axis(f[1, 2]), obglobals)
    # end
    
    if step_fn !== nothing
        f[2, 1] = buttongrid = GridLayout(tellwidth = false)

        buttongrid[1, 1] = resetbutton = Button(f, label="Reset")
        on(resetbutton.clicks) do _
            @info "reset"
            finish_simulation!(vp.sim)
            vp.sim = copy_simulation(vp.original)
            vp.vg = vahanagraph(vp.sim; agenttypes = agenttypes, edgetypes = edgetypes)
            #            _draw_globals(vp.sim, Axis(f[1, 2]), globals)
            update_plot!(vp)
        end
        
        buttongrid[1, 2] = stepbutton = Button(f, label="Step")
        on(stepbutton.clicks) do _
            step_fn(vp.sim)
            vp.vg = vahanagraph(vp.sim; agenttypes = agenttypes, edgetypes = edgetypes)
            #            _draw_globals(vp.sim, Axis(f[1, 2]), globals)
            update_plot!(vp)
        end
    end

    
    #    end

    update_plot!(vp)
end

function update_plot!(vp::VahanaPlot)
    if ! isnothing(vp.update_fn)
        _set_agents_plot_attrs!(vp.update_fn, vp)
        _set_edge_plot_properties!(vp.update_fn, vp)
    end

    autolimits!(vp.axis)
    
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
                  node_color = [ agentcolors[type_nr(vg.g2v[i]) + 1]
                                 for i in rv ],
                  node_size = [ 10 for _ in rv ],
                  node_marker = [ :circle for _ in rv ],
                  #                  node_attr = [ Dict{Symbol, Observable}() for _ in rv ],
                  nlabels = [ "" for _ in rv ],
                  nlabels_align = [ (:left, :bottom) for _ in rv ],
                  nlabels_color = [ :black for _ in rv ],
                  nlabels_distance = [ 0.0 for _ in rv ],
                  nlabels_offset = [ Makie.Point(0, 0) for _ in rv ],
                  nlabels_textsize = [ 14 for _ in rv ],
                  edge_color = [ edgecolors[vg.edgetypeidx[i] + 1]
                                 for i in re ],
                  edge_width = [ 1.5 for _ in re ],
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
                  arrow_size = [ 12 for _ in re ]
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
    disable_transition_checks(true)
    s = agentstate_flexible(vg.sim, vg.g2v[idx])
    disable_transition_checks(false)
    s
end
