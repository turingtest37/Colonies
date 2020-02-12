using Plots
using Random
using Images

include("Colonies.jl")
using .Colonies
import Colonies.reduce


function explore(maskrange, maskdim, shuffle, limit = 100)
    # neighborhood = [0 0 1; 1 0 1; 1 0 0]

    vmm = Int[]
    vme = Int[]
    masks = AbstractArray[]
    for msk in Colonies.generate_masks(1:4, maskdim)
        push!(masks, msk.m)
    end
    Random.shuffle!(masks)
    # masks = collect(Iterators.take(, limit))
    neighborhoods = [rand(0:1,maskdim,maskdim) for i=1:limit]

    @debug "mask, neighb lengths = $(length(masks)), $(length(neighborhoods))"
    lm = length(masks)
    ln = length(neighborhoods)
    if lm < ln
        masks = repeat(masks, div(ln, lm))
    end

    for p in zip(neighborhoods, masks)
        n = p[1]
        m = p[2]

        @debug "n, m = " n m
        push!(vmm, round(Int, Colonies.reducedwithmm(n, m), Base.RoundDown))
        push!(vme, round(Int, Colonies.reducedwithem(n, m), Base.RoundDown))
    end

    # histogram(hcat(vmm, vme), label=["MM" "ME"], normalize=:pdf)
    histogram(hcat(vme, vmm), label=["ME" "MM"], normalize=:pdf)
end


# explore(1:4, 3, true)
function splice(src1...)
    a = Array{RGB}[]

    for f in src1
        img = open(f)
        a = vcat(a, img)
    end

end

Colonies.reduce(neighborhood, mask) = mod(mask * neighborhood, 9)
