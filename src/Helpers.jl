export checked

"""
    checked(f, g, itr; kwargs...)

Calls g(f, itr; kwargs...), but only if itr != nothing.

As all the Vahana functions that access the edges of a specific agent
can return nothing in the case, that there exist no incoming edge for this agent,
it's often necessery to check this case. 

Example:

Instead of writing 

```@example
nids = neighborids(sim, id, Contact)
if nids != nothing
    foreach(nids) do nid
      add_edge!(sim, id, nid, Inform()
    end
end
```

you can use the checked function to write

```@example
checked(foreach, neighborids(sim, id, Contact)) do nid
    add_edge!(sim, id, nid, Inform())
end
```
"""
function checked(f, g, itr; kwargs...)
    if !isnothing(itr)
        g(f, itr; kwargs...)
    end
end


######################################## internal

# this is for the reduce operations, and tries to determine
# the default value for ranks without any agent/edge
function val4empty(op; kwargs...)
    MT = get(kwargs, :datatype, Int)
    
    # for MPI.reduce we must ensure that each rank has a value
    emptyval = get(kwargs, :init) do
        if op == +
            zero(MT)
        elseif op == *
            one(MT)
        elseif op == max
            -typemax(MT)
        elseif op == min 
            typemax(MT)
        elseif op == &
            true
        elseif op == |
            false
        else
            nothing
        end
    end

    @assert emptyval !== nothing """ 
            Can not derive the init value for the operator. You must add this
            information via the `init` keyword.
        """
    emptyval
end

#################### field symbols
# create symbol for the different fields of an agent/edgetype

# TODO: adjust edgefieldfactory to macros
writefield(T) = Symbol(T, "_write")

readfield(T) = Symbol(T, "_read")

# nextidfield(T) = Symbol(T, "_nextid")

macro nextid(T)
    field = Symbol(T, "_nextid")
    :( sim.$(field) ) |> esc
end 
nextid(sim, T) = getproperty(sim, Symbol(T, "_nextid"))

macro reuse(T)
    field = Symbol(T, "_reuse")
    :( sim.$(field) ) |> esc
end
reuse(sim, T) = getproperty(sim, Symbol(T, "_reuse"))

macro died(T)
    field = Symbol(T, "_died")
    :( sim.$(field) ) |> esc
end
died(sim, T) = getproperty(sim, Symbol(T, "_died"))

macro readstate(T)
    field = Symbol(T, "_read")
    :( sim.$(field).state ) |> esc
end
readstate(sim, T) = getproperty(sim, Symbol(T, "_read")).state

macro writestate(T)
    field = Symbol(T, "_write")
    :( sim.$(field).state ) |> esc
end
writestate(sim, T) = getproperty(sim, Symbol(T, "_write")).state

macro readreuseable(T)
    field = Symbol(T, "_read")
    :( sim.$(field).reuseable ) |> esc
end
readreuseable(sim, T) = getproperty(sim, Symbol(T, "_read")).reuseable

macro writereuseable(T)
    field = Symbol(T, "_write")
    :( sim.$(field).reuseable ) |> esc
end
writereuseable(sim, T) = getproperty(sim, Symbol(T, "_write")).reuseable


macro write(T)
    field = Symbol(T, "_write")
    :( sim.$(field) ) |> esc
end
write(sim, T) =getproperty(sim, Symbol(T, "_write"))


macro read(T)
    field = Symbol(T, "_read")
    :( sim.$(field) ) |> esc
end
read(sim, T) =getproperty(sim, Symbol(T, "_read"))

macro storage(T)
    field = Symbol(T, "_storage")
    :( sim.$(field) ) |> esc
end
storage(sim, T) =getproperty(sim, Symbol(T, "_storage"))


# we use this tests are for the distributed version, in this case
# the tests should be only run on the rank that the id is currently
# living
macro onrankof(aid, ex)
    quote
        if Vahana.process_nr($(esc(aid))) == mpi.rank
            $(esc(ex))
        end
    end
end

macro rankonly(rank, ex)
    quote
        if $rank == mpi.rank
            $(esc(ex))
        end
    end
end

macro rootonly(ex)
    quote
        if 0 == mpi.rank
            $(esc(ex))
        end
    end
end

