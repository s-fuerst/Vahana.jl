writefield(T::Symbol) = Symbol(T, "_write")

readfield(T::Symbol) = Symbol(T, "_read")

nextidfield(T::Symbol) = Symbol(T, "_nextid")

function init_type!(sim, ::Val{T}) where T end
