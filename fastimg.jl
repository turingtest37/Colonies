
gather options
initialize array 1




function numberfy(t::Tuple{Int8,Bool})
    body
end


function drawsquare(c::AbstractArray)
    c[-1,-1]=(1,false)
    c[-1,0]=(1,false)
    c[-1,1]=(1,false)
    c[-1,2]=(1,false)
    c[0,-1]=(1,false)
    c[0,2]=(1,false)
    c[1,-1]=(1,false)
    c[1,2]=(1,false)
    c[2,-1]=(1,false)
    c[2,0]=(1,false)
    c[2,1]=(1,false)
    c[2,2]=(1,false)
end
