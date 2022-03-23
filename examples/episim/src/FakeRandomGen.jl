module RandomGen

@fastmath function with_prob_old(prob::Float64, determination::Int64)::Bool 
    if prob > 0
        m = 1.0 / prob
        return determination % m < 1
    end
    return false
end

@fastmath function with_prob(prob::Float64, determination::Int64)::Bool 
    if prob > 0
        m::Float64 = 1.0 / prob
        d::Int64 = floor(determination / m)
        return (determination - d * m) < 1
    end
    return false
end

@fastmath function with_prob(prob::Float64, determination::UInt64)::Bool 
    if prob > 0
        m::Float64 = 1.0 / prob
        d::Int64 = floor(determination / m)
        return (determination - d * m) < 1
    end
    return false
end


with_prob(prob::Float64, determination::Float64) = 
    with_prob(prob, Int64(determination))


@fastmath function with_prob_rem(prob::Float64, determination::Int64)::Bool 
    if prob > 0
        return rem(determination, 1.0 / prob) < 1
    end
    return false
end


end


function test_prob(numProbs)
    sum = 0
    for i in 1:numProbs
        if RandomGen.with_prob(0.125, i)
            sum = sum + 1
        end
    end
    sum
end

function test_prob_old(numProbs)
    sum = 0
    for i in 1:numProbs
        if RandomGen.with_prob_old(0.125, i)
            sum = sum + 1
        end
    end
    sum
end

function test_prob_rem(numProbs)
    sum = 0
    for i in 1:numProbs
        if RandomGen.with_prob_rem(0.125, i)
            sum = sum + 1
        end
    end
    sum
end

