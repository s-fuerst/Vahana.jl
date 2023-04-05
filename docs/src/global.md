```@meta
CurrentModule = Vahana
```

# Global Layer

The state of the global layer can not be changed inside of a
transition function, as a transition function is calculated on a per
agent basis with many evaluations in parallel, and changes to the
global layer must be a single, synchronized operation.

To calculate a global value in such a synchronized operation, you can
use one of the following functions:

```@docs
Vahana.mapreduce
calc_raster
calc_rasterstate
```

You can add a new or additional value to the `globals` struct with:

```@docs
set_global!
push_global!
```
