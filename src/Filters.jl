module Filters

include("filtersnew.jl")

import Base.iterate, Base.show, Base.println, Base.eltype, Base.length

export StateFilter, generate_state_filters

export of, zf, swf, saf

end
