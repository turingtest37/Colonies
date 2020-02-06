module Filters

import Base.iterate, Base.show, Base.println, Base.eltype, Base.length

include("filtersnew.jl")

export StateFilter
export generate_state_filters, testfilters, filterfromstring
export of, zf, swf, saf

end
