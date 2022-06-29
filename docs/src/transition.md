```@meta
CurrentModule = Vahana
```

As mentioned in the `construct_model` documentation, the model struct
is created dynamically incl. a dynamically name for this struct. 

```@docs
__MODEL__
```


```@docs
apply_transition!
apply_transition
```


!!! tip 

	The agent types must be immutable, but in most cases the agent
	returned by a transition function will have a different state than the
	agent specified as a parameter. If only one field is changed, we still
	need to copy all other fields. The Setfield.jl package can be very
	useful in this case.


Inside a transition function the following functions are used to access the state of the simulation:

```@docs
param
getglobal

edges_to
num_neighbors
has_neighbor

agentstate
agentstate_flexible

neighborstates
neighborstates_flexible

neighborids
edgestates

raster_nodeid
```

Already described...

```@docs
checked
```
