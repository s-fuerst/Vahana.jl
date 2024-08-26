```@meta
CurrentModule = Vahana
```

# Modify Globals 

You can add a new or additional value to the `globals` struct with:

```@docs
set_global!
push_global!
modify_global!
Vahana.mapreduce
```

!!! warning

	The state of the globals struct (the `globals` argument of the 
	[`create_simulation`](@ref) function can not be changed 
	inside of a transition function, as a transition function is calculated on 
	a per agent basis with many evaluations in parallel, and changes to the
	global layer must be a single, synchronized operation.

