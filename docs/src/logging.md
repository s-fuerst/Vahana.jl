```@meta
CurrentModule = Vahana
```

# Logging

Vahana utilize Julia's Standard Logging Library to optionally create
log files with e.g. the duration of the transition functions. For each
process a seperate file is created.

```@docs
create_logger!
with_logger
log_overview
```
