using Cairo

# const my_destroy_c = cconvert(destroy_string, Cint,

function destroy_string(s::AbstractString)
    return ccall(:memset, Ptr{Void}, (Cstring, Cint), '\0', sizeof(s))::Ref{Cstring}
end

type DataKey
    unused::Int32
end

const my_data_key = DataKey(1)
const my_destroy_c_ref = cfunction(destroy_string, Ref{Cchar}, (Ref{Cchar},))

function set_user_data(surface::CairoSurface, key::AbstractString, value::AbstractString)
    ccall((:cairo_surface_set_user_data,Cairo._jl_libcairo),Ptr,(Ptr{Void},Ref{DataKey},Cstring,Ptr{Void}),surface.ptr,my_data_key,value,my_destroy_c_ref)
end


crgb = CairoRGBSurface(100, 100)
cctx = CairoContext(crgb)
set_source_rgb(cctx,0.8,0.8,0.8)
rectangle(cctx,0,0,100, 100)
fill(cctx)
set_user_data(crgb,"crab","gocrazy")
write_to_png(crgb,"noway.png")
println("Done!")
