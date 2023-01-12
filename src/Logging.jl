using Logging, Dates

using Base: debug_color
import Logging: with_logger, shouldlog, min_enabled_level, catch_exceptions, handle_message

export with_logger

struct VahanaLogger <: Logging.AbstractLogger
    stream::IO
    min_level::LogLevel
    debug::Bool
    starttime::Float64
    begintimes::Dict{String, Float64}
    # store <Begin> kwargs to access them at <End> in the ! debug case
    kwargs::Dict{String, Dict}
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
    now = time()
    buf = IOBuffer()
    iob = IOContext(buf, logger.stream)
    msglines = eachsplit(chomp(convert(String, string(message))::String), '\n')
    msg1, rest = Iterators.peel(msglines)
    # check for <Begin> or <End> tags
    msg1split = split(msg1, ">")
        
    if msg1split[1] == "<End"
        if logger.debug
            println(iob, "Time (sec): ", now - logger.starttime)
        else
            kwargs = logger.kwargs[msg1split[2]]
            println(iob, "Start (sec): ",
                    logger.begintimes[msg1split[2]] - logger.starttime)
        end
        println(iob, "  ", logger.debug ? msg1 : msg1split[2],
                " |#| Duration (ms): ",
                (now - logger.begintimes[msg1split[2]]) * 1000)
    elseif msg1split[1] != "<Begin" || logger.debug
        println(iob, "Time (sec): ", now - logger.starttime)
        println(iob, "  ", msg1)
    end
    for msg in rest
        println(iob, msg)
    end
    if msg1split[1] != "<Begin" || logger.debug
        for (key, val) in kwargs
            println(iob, "    ", key, " = ", val)
        end
    end
    write(logger.stream, take!(buf))
    if logger.debug
        flush(logger.stream)
    end
    if msg1split[1] == "<Begin"
        logger.begintimes[msg1split[2]] = time()
        logger.kwargs[msg1split[2]] = kwargs
    end
    nothing
end

function createLogger(name, logging, debug)
    logfile = logging ? open(name * "_" * string(mpi.rank) * ".log"; write = true) :
        nothing
    logger = logging ?
        VahanaLogger(logfile,
                     debug ? Debug : Info,
                     debug,
                     time(),
                     Dict{String, Float64}(),
                     Dict{String, Dict}()) :
        nothing

    Log(logfile, logger, debug)
end

"""
TODO: DOC
"""
function with_logger(f::Function, sim::Simulation)
    if sim.logging.logger !== nothing
        with_logger(f, sim.logging.logger)
    end
end

_log_info(sim, text) = with_logger(() -> @info(text), sim)

_log_debug(sim, text) = with_logger(() -> @debug(text), sim)

function _log_time(f, sim, text, debug = false)
    lf = debug ? _log_debug : _log_info
    lf(sim, "<Begin> " * text)
    r = f()
    lf(sim, "<End> " * text)
    r
end