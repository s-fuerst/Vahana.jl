using Logging, Dates

import Logging: with_logger, shouldlog, min_enabled_level, catch_exceptions, handle_message

export with_logger

mutable struct VahanaLogger <: Logging.AbstractLogger
    stream::IO
    min_level::LogLevel
    starttime::Float64
end

Logging.shouldlog(logger::VahanaLogger, level, _module, group, id) = true

Logging.min_enabled_level(logger::VahanaLogger) = logger.min_level

Logging.catch_exceptions(logger::VahanaLogger) = false

struct Log
    file::Union{IOStream, Nothing}
    logger::Union{VahanaLogger, Nothing}
    debug::Bool
end

function Logging.handle_message(logger::VahanaLogger, level::LogLevel, message, _module,
                         group, id, filepath, line; kwargs...)
    @nospecialize
    buf = IOBuffer()
    iob = IOContext(buf, logger.stream)
    msglines = eachsplit(chomp(convert(String, string(message))::String), '\n')
    msg1, rest = Iterators.peel(msglines)
    println(iob, "Sec: ", time() - logger.starttime)
    println(iob, "  ", msg1)
    for msg in rest
        println(iob, msg)
    end
    for (key, val) in kwargs
        println(iob, "    ", key, " = ", val)
    end
    write(logger.stream, take!(buf))
    nothing
end

function createLogger(name, logging, debug)
    logfile = logging ? open(name * "_" * string(mpi.rank) * ".log"; write = true) :
        nothing
    logger = logging ?
        VahanaLogger(logfile, debug ? Debug : Info, time()) :
        nothing

    Log(logfile, logger, debug)
end

"""
TODO: DOC
"""
function with_logger(f, sim::Simulation)
    if sim.logging.logger !== nothing
        with_logger(f, sim.logging.logger)
        if sim.logging.debug
            flush(sim.logging.logfile)
        end
    end
end

_log_info(sim, txt) = with_logger(() -> @info(txt), sim)

