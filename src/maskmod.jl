module Masks

include("masks.jl")

import Base: iterate, show, length, eltype

export Mask

export generate_masks, the12p7weightedmask, maskfromstring

export test_masks


end
