writefield(T::Symbol) = Symbol(T, "_write")

readfield(T::Symbol) = Symbol(T, "_read")

nextidfield(T::Symbol) = Symbol(T, "_nextid")

function init_type!(sim, _)  end

function prepare_write!(sim, _) end
