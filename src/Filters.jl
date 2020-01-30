module Filters

include("filtersnew.jl")

import Base.iterate, Base.show, Base.println, Base.eltype

export StateFilter, generate_state_filters

export of, zf, swf, saf 

end
