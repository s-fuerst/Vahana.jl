export checked, @rootonly, @roottime

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
    if (op == &) || (op == |)
        MT = get(kwargs, :datatype, Bool)
    else
        MT = get(kwargs, :datatype, Int)
    end
    
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
            @assert typemax(MT) isa Int || typemax(MT) isa Bool """
            The & operator is only supported for integer and boolean types
            """
            typemax(MT)
        elseif op == |
            @assert typemax(MT) isa Int || typemax(MT) isa Bool """
            The & operator is only supported for integer and boolean types
            """
            typemax(MT) isa Int ? 0 : false
        else
            nothing
        end
    end

    @assert emptyval !== nothing """\n 
            Can not derive the init value for the operator. You must add this
            information via the `init` keyword.
        """
    emptyval
end

##################### add_number_to_file
# when sim.overwrite is false, we automatically create filenames with
# appended numbers (for the h5 files and also the logs)

function add_number_to_file(filename, i)
    if i < 999999
        filename * '-' * lpad(string(i), 6, "0")
    else
        filename * '-' * string(i)
    end
end

function add_number_to_file(filename)
    i = 0
    while true
        i += 1
        numbered = add_number_to_file(filename, i)
        if ! isfile(numbered * ".h5") && ! isfile(numbered * "_0.h5") &&
            ! isfile(numbered * "_0.log")
            return numbered
        end
    end
end

#################### field symbols
# create symbol for the different fields of an agent/edgetype

# TODO: adjust edgefieldfactory to macros
writefield(T) = Symbol(T).write

readfield(T) = Symbol(T).read

# nextidfield(T) = Symbol(T, "_nextid")

macro nextid(T)
    field = Symbol(T)
    :( sim.$(field).nextid ) |> esc
end 
nextid(sim, T) = getproperty(sim, Symbol(T)).nextid

macro agent(T)
    field = Symbol(T)
    :( sim.$(field) ) |> esc
end

macro edge(T)
    field = Symbol(T)
    :( sim.$(field) ) |> esc
end

macro windows(T)
    field = Symbol(T)
    :( sim.$(field).mpiwindows ) |> esc
end
windows(sim, T) = getproperty(sim, Symbol(T)).mpiwindows

macro readdied(T)
    field = Symbol(T)
    :( sim.$(field).read.died ) |> esc
end
readdied(sim, T) = getproperty(sim, Symbol(T)).read.died

macro writedied(T)
    field = Symbol(T)
    :( sim.$(field).write.died ) |> esc
end
writedied(sim, T) = getproperty(sim, Symbol(T)).write.died

macro readstate(T)
    field = Symbol(T)
    :( sim.$(field).read.state ) |> esc
end
readstate(sim, T) = getproperty(sim, Symbol(T)).read.state

macro writestate(T)
    field = Symbol(T)
    :( sim.$(field).write.state ) |> esc
end
writestate(sim, T) = getproperty(sim, Symbol(T)).write.state

macro readreuseable(T)
    field = Symbol(T)
    :( sim.$(field).read.reuseable ) |> esc
end
readreuseable(sim, T) = getproperty(sim, Symbol(T)).read.reuseable

macro writereuseable(T)
    field = Symbol(T)
    :( sim.$(field).write.reuseable ) |> esc
end
writereuseable(sim, T) = getproperty(sim, Symbol(T)).write.reuseable

macro agentwrite(T)
    field = Symbol(T)
    :( sim.$(field).write ) |> esc
end

macro agentread(T)
    field = Symbol(T)
    :( sim.$(field).read ) |> esc
end

macro edgewrite(T)
    field = Symbol(T)
    :( sim.$(field).write ) |> esc
end
edgewrite(sim, T) = getproperty(sim, Symbol(T)).write

macro edgeread(T)
    field = Symbol(T)
    :( sim.$(field).read ) |> esc
end
edgeread(sim, T) = getproperty(sim, Symbol(T)).read

macro storage(T)
    field = Symbol(T)
    :( sim.$(field).storage ) |> esc
end
storage(sim, T) = getproperty(sim, Symbol(T)).storage


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

macro roottime(ex)
    quote
        if 0 == mpi.rank
            @time $(esc(ex))
        else
            $(esc(ex))
        end
    end
end

edge_attrs(sim, T::DataType) = sim.typeinfos.edges_attr[T]

disable_transition_checks = (disable::Bool) -> config.check_readable = ! disable
