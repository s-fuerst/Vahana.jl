const NUM_BUFFERS = 2 

function prepare_buffered_write!(coll) 
    coll.write = coll.write + 1
    if coll.write > NUM_BUFFERS
        coll.write = 1
    end
end

function finish_buffered_write!(coll) 
    coll.read = coll.write
end

