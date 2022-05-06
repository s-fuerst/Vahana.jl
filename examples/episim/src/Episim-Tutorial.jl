using Vahana

using DelimitedFiles

import Base.+

    include("FakeRandomGen.jl")

@enum EventType Arrival=1 Depature=0

@enum HealthState S E I R

######################################## Agents

struct Person <: Agent
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

struct Location <: Agent end

######################################## Edges

struct MovementEvent <: EdgeState # from person to location
    type::EventType
    time::Float64
end

struct Contact <: EdgeState # from person to person
    day::Int32
end

struct EndOfDay <: EdgeState # from person to location
    time::Int64
end

struct Infection <: EdgeState end # from location to person

struct Inform <: EdgeState end # from person to person

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

    day = getglobal(sim, :day)
    minute::Int64 = floor(day * 24 * 60 + edge.state.time)

    p = agentstate(sim, edge.from)

    if p.health == S
        Σ = 0
        for (oid, otime) in persons
            o = agentstate(sim, oid)
            if o.health == I && not_in_quarantine(o) 
                Σ += min(minute - ptime, minute - otime)
            end
        end
        prob = 1.0 - exp(param(sim, :θ) * Σ)
        if rand() < prob
            add_edge!(sim, lid, pid, Infection)
        end
    elseif p.health == I && not_in_quarantine(p)
        for (oid, otime) in persons
            o = agentstate(sim, oid)
            if o.health == S 
                time = floor(min(minute - ptime, minute - otime))
                prob = 1.0 - exp(param(sim, :θ) * time)
                if rand() < prob
                    add_edge!(sim, lid, oid, Infection)
                end
            end
        end
    end

    # For simplicity I ignore optimization checks like health != R
    if not_in_quarantine(p) 
        for oid in keys(persons)
            o = agentstate(sim, oid)
            if pid != oid &&
                not_in_quarantine(o) &&
                rand() < param(sim, :tracing)

                add_edge!(sim, pid, oid, Contact(day))
                add_edge!(sim, oid, pid, Contact(day))
            end
        end
    end
end 

function contact_model(p::Person, id::AgentID, sim::Simulation)
    day = getglobal(sim, :day)
    for c in edges_to(sim, id, Contact)
        if c.state.day > day - 2
            add_edge!(sim, id, c)
        end
    end
    p
end

function contact_model(l::Location, id::AgentID, sim::Simulation)
    # persons at location, entered at which time
    persons = Dict{AgentID, Int64}()

    for edge in edges_to(sim, id, EndOfDay)
        persons[edge.from] = edge.state.time
    end

    event_edges = edges_to(sim, id, MovementEvent)
    for edge in event_edges
        event = edge.state
        if event.type == Arrival
            persons[edge.from] =
                floor(getglobal(sim, :day) * 24 * 60 + event.time)
        else
            handle_leave(persons, edge, sim, id)
        end
    end

    for (pid, ptime) in persons
        add_edge!(sim, pid, id, EndOfDay(ptime))
    end

    l
end    

function change_health(p::Person, newState::HealthState, sim)
    Person(newState, getglobal(sim, :day), p.quarantine_start)
end

# function check_infection(p::Person, id, sim)
# end

function disease_progression(p::Person, id, sim)
    ## was check_infection
    if length(edges_to(sim, id, Infection)) > 0
        return change_health(p, E, sim)
    end

    day = getglobal(sim, :day)

    ## original diseaseprogression
    if p.health == E && p.health_changed == day - 3
        if rand() < 0.5
            p = start_quarantine(p, day)
            for c in edges_to(sim, id, Contact)
                add_edge!(sim, id, c.from, Inform)
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
    day = getglobal(sim, :day)

    if length(edges_to(sim, id, Inform)) > 0 && p.quarantine_start < 0
        return Person(p.health, p.health_changed, day)
    elseif p.quarantine_start + 14 == day
        return Person(p.health, p.health_changed, -1)
    end

    p
end

function init(filename, params)
    sim = Simulation("Episim-Nutshell",
                     params,
                     Globals(reports = Vector(),
                             day = 0))

    add_agenttype!(sim, Person)
    add_agenttype!(sim, Location)
    add_edgetype!(sim, MovementEvent)
    add_edgetype!(sim, Contact)
    add_edgetype!(sim, EndOfDay)
    add_edgetype!(sim, Infection)
    add_edgetype!(sim, Inform)

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

    pushglobal!(sim, :reports, aggregate(sim, Person, Report, +))

    sim
end

#add_globalstate!(sim, aggregate(sim, Person, p -> Report(p), +)

function runstep!(sim)
    setglobal!(sim, :day, Int32(getglobal(sim, :day) + 1))

    apply_transition!(sim, contact_model, [ Person, Location ],
                      [ EndOfDay, MovementEvent ],
                      [ EndOfDay, Contact, Infection ])

    
    apply_transition!(sim, disease_progression, [ Person ],
                      [ Infection, Contact ],
                      [ Inform ])

    apply_transition!(sim, quarantine, [ Person ],
                      [ Inform ],
                      [])

    report = aggregate(sim, Person, Report, +)
    println(report)
    pushglobal!(sim, :reports, report)
    sim
end

function init(params)
    init("events-mini.txt", params)
end

function runsimulation()
    sim = init(Params(θ=-0.001, tracing=0.25))

    for _ in 1:20
        @time runstep!(sim)
    end
    sim
end

#@report_opt runsimulation()
#@profilehtml runsimulation()
runsimulation()
