import Base.convert
import DataFrames

function convert(::Type{Array{UInt8}},z::Complex{Int64})
    # println("convert $z")
    rza = cat(1,Base.convert(Array{UInt8}, bits(real(z))), Base.convert(Array{UInt8}, bits(imag(z))))
    # println("rza has length $(size(rza))")
    rza
end

function convert(::Type{Array{UInt8}}, t::Tuple{Complex{Int64},Bool})
    # println("convert t")
    ta = cat(1, convert(Array{UInt8},t[1]), convert(UInt8,t[2]))
    # println("ta has length $(size(ta))")
    ta
end

function convert(::Type{Array{UInt8}}, c::Array{Tuple{Complex{Int64},Bool},2})
    # println("convert c")
    r = convert.(Array{UInt8},c)
    # println("r has length $(size(r))")
    # println("r=$r")
    result = r[1,1]
    for j=2:size(r,1), i=1:size(r,2)
        append!(result,r[i,j])
        # cat(1,result,r[i,j])
    end
    result
end
# a = convert(Array{UInt8}, (-3+6im, false))
# println(a)
# b = convert(Array{UInt8}, (3-6im, true))
# println(b)

function testforsnobee(t::UInt32, on::UInt32)
    # println("t x on = $(t*on)")
    # ratio = (t * on) / 256
    # println("float scaled on time=$ratio")
    ton::UInt32 = t * on
    # println("t*on=$ton before division")
    ton & 0x3FF < 512 ? ton >>= 10 : (ton >>=10; ton += 1)
    # ton >>= 8
    ton = ton > 0 ? ton : 1
    # println("t*on>>8=$ton after division")
    # println("integer scaled on time=$ton")
    off::UInt32 = t-ton
    # println("off=$off")
    off = off > 0 ? off : 1
    ton,off
    # convert(UInt16,ton), convert(UInt16,off)
end


function exactanswer(t::UInt32, on::UInt32)
    ton = convert(Int64,t) * convert(Int64,on) / 1024
    toff = t - ton
    ton, toff
end

function test()
    # df = DataFrame(period="period",on="on")
    # @push df
    for traw in (0:50:1023), onraw in (0:50:1023)
        t::UInt32 = convert(UInt32, traw)
        # println("t=$t, type $(typeof(t))")
        on::UInt32 = convert(UInt32, onraw)
        # println("on=$on, type=$(typeof(on))")
        # t = t > 0 ? t : 1
        # on = on > 0 ? on : 1
        ton, toff = testforsnobee(t,on)
        tonexact, toffexact = exactanswer(t,on)

        onerr = 100*(tonexact-ton)/tonexact
        # s = convert(Int64,ton+off)
        # d = convert(Int64,t-s)
        # err = 100*abs(d)/t
        println("traw=$traw onraw=$onraw on(scaled)=$ton off(scaled)=$toff onerr=$(@sprintf "%.1f" onerr)%")
    end
end
test()
