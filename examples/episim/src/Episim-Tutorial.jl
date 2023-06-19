using Revise

using Vahana

using DelimitedFiles

import Base.+

include("FakeRandomGen.jl")

detect_stateless(true)

@enum EventType Arrival=1 Depature=0

@enum HealthState S E I R

######################################## Agents

struct Person 
    health::HealthState
    health_changed::Int32 
    quarantine_start::Int32 
end

function Person(id::Int64)
    if RandomGen.with_prob(0.0001, id)
        Person(I, 0, -1)
    else
        Person(S, -1, -1)
    end
end

function not_in_quarantine(p::Person)
    p.quarantine_start <= 0
end

function start_quarantine(p::Person, day::Int32)
    if not_in_quarantine(p)
        Person(p.health, p.health_changed, day)
    else
        p
    end
end

struct Location end

######################################## Edges

struct MovementEvent  # from person to location
    type::EventType
    time::Float64
end

struct Contact  # from person to person
    day::Int32
end

struct EndOfDay  # from person to location
    time::Int64
end

struct Infection end # from location to person

struct Inform end # from person to person

######################################## Params

Base.@kwdef struct Params
    θ::Float64
    # the probability, that a contact is traced (added to the Contact network)
    tracing::Float64 
end

######################################## Global

struct Report 
    numS::Int64
    numE::Int64
    numI::Int64
    numR::Int64
    numQ::Int64
end

Report() = Report(0, 0, 0, 0, 0)

Report(p::Person) = Report(p.health == S,
                           p.health == E,
                           p.health == I,
                           p.health == R,
                           !not_in_quarantine(p))

+(a::Report, b::Report) = Report(a.numS + b.numS,
                                 a.numE + b.numE,
                                 a.numI + b.numI,
                                 a.numR + b.numR,
                                 a.numQ + b.numQ)


Base.@kwdef mutable struct Globals
    reports::Vector{Report}
    day::Int32
end

######################################## Transition

function handle_leave(persons, edge, sim, lid)
    pid = edge.from
    if !haskey(persons, pid)
        return
    end
    ptime = persons[pid]
    delete!(persons, pid)

    day = get_global(sim, :day)
    minute::Int64 = floor(day * 24 * 60 + edge.state.time)

    p = agentstate(sim, edge.from, Person)

    if p.health == S
        Σ = 0
        for (oid, otime) in persons
            o = agentstate(sim, oid, Person)
            if o.health == I && not_in_quarantine(o) 
                Σ += min(minute - ptime, minute - otime)
            end
        end
        prob = 1.0 - exp(param(sim, :θ) * Σ)
        if rand() < prob
            add_edge!(sim, lid, pid, Infection())
        end
    elseif p.health == I && not_in_quarantine(p)
        for (oid, otime) in persons
            o = agentstate(sim, oid, Person)
            if o.health == S 
                time = floor(min(minute - ptime, minute - otime))
                prob = 1.0 - exp(param(sim, :θ) * time)
                if rand() < prob
                    add_edge!(sim, lid, oid, Infection())
                end
            end
        end
    end

    # For simplicity I ignore optimization checks like health != R
    if not_in_quarantine(p) 
        for oid in keys(persons)
            o = agentstate(sim, oid, Person)
            if pid != oid &&
                not_in_quarantine(o) &&
                rand() < param(sim, :tracing)

                add_edge!(sim, pid, oid, Contact(day))
                add_edge!(sim, oid, pid, Contact(day))
            end
        end
    end
end 

function contact_model(::Val{Person}, id::AgentID, sim)
    day = get_global(sim, :day)
    checked(foreach, edges(sim, id, Contact)) do edge
        if edge.state.day > day - 2
            add_edge!(sim, id, edge)
        end
    end
end

function contact_model(::Val{Location}, id::AgentID, sim)
    # persons at location, entered at which time
    persons = Dict{AgentID, Int64}()

    checked(foreach, edges(sim, id, EndOfDay)) do edge
        persons[edge.from] = edge.state.time
    end

    event_edges = edges(sim, id, MovementEvent)
    for edge in event_edges
        event = edge.state
        if event.type == Arrival
            persons[edge.from] =
                floor(get_global(sim, :day) * 24 * 60 + event.time)
        else
            handle_leave(persons, edge, sim, id)
        end
    end

    for (pid, ptime) in persons
        add_edge!(sim, pid, id, EndOfDay(ptime))
    end
end    

function change_health(p::Person, newState::HealthState, sim)
    Person(newState, get_global(sim, :day), p.quarantine_start)
end

# function check_infection(p::Person, id, sim)
# end

function disease_progression(p::Person, id, sim)
    ## was check_infection
    if has_edge(sim, id, Infection)
        return change_health(p, E, sim)
    end

    day = get_global(sim, :day)

    ## original diseaseprogression
    if p.health == E && p.health_changed == day - 3
        if rand() < 0.5
            p = start_quarantine(p, day)
            checked(foreach, neighborids(sim, id, Contact)) do nid
                add_edge!(sim, id, nid, Inform())
            end
        end
        change_health(p, I, sim)
    elseif p.health == I && p.health_changed == day - 10
        change_health(p, R, sim)
    else
        p
    end
end

function quarantine(p::Person, id, sim)
    day = get_global(sim, :day)

    if has_edge(sim, id, Inform) && p.quarantine_start < 0
        return Person(p.health, p.health_changed, day)
    elseif p.quarantine_start + 14 == day
        return Person(p.health, p.health_changed, -1)
    end

    p
end

function init(sim, filename)
    d = readdlm(filename, ' ', Int64)

    max_person_id = maximum(d[:,3])  + 1
    max_location_id = maximum(d[:,4]) + 1

    persons = fill(AgentID(0), max_person_id)
    locations = fill(AgentID(0), max_location_id)

    for i in 1:size(d,1)
        type::EventType = d[i,1] == 0 ? Depature : Arrival
        time::Float64 = d[i,2]
        person_id = d[i,3] + 1
        location_id = d[i,4] + 1

        if persons[person_id] == 0
            persons[person_id] = add_agent!(sim, Person(person_id))
        end
        if locations[location_id] == 0
            locations[location_id] = add_agent!(sim, Location())
        end
        add_edge!(sim,
                  persons[person_id],
                  locations[location_id],
                  MovementEvent(type, time))
    end

    finish_init!(sim)

    push_global!(sim, :reports, mapreduce(sim, Report, +, Person; init = Report()))

    sim
end

#add_globalstate!(sim, mapreduce(sim, Person, p -> Report(p), +)

function runstep!(sim)
    set_global!(sim, :day, Int32(getglobal(sim, :day) + 1))

    apply!(sim, contact_model, [ Person, Location ],
                      [ Contact, EndOfDay, MovementEvent ],
                      [ Contact, EndOfDay, Infection ])

    
    apply!(sim, disease_progression, [ Person ],
                      [ Person, Infection, Contact ],
                      [ Person, Inform ])

    apply!(sim, quarantine, [ Person ],
                      [ Person, Inform ],
                      [ Person ])

    report = mapreduce(sim, Report, +, Person; init = Report())
    push_global!(sim, :reports, report)
    println(report)
    sim
end

function runsimulation(sim)
    init(sim, "events-mini.txt")

    for _ in 1:20
        @time runstep!(sim)
    end
    sim
end

const sim = ModelTypes() |>
        register_agenttype!(Person) |>
        register_agenttype!(Location) |>
        register_edgetype!(MovementEvent) |>
        register_edgetype!(Contact) |>
        register_edgetype!(EndOfDay) |>
        register_edgetype!(Infection) |>
        register_edgetype!(Inform) |>
        create_model("Episim-Nutshell") |>
        create_simulation(Params(θ=-0.001, tracing=0.25),
                       Globals(reports = Vector(), day = 0))


#@report_opt runsimulation()
#@profilehtml runsimulation()
runsimulation(sim)
