module Colonies

using Images
using DataFrames
using CSV
using Query
using Random
using UUIDs
using Colors
using Reexport
using Dates
using VideoIO
using LinearAlgebra

import Base: *

include("Filters.jl")
@reexport using .Filters
include("Masks.jl")
@reexport using .Masks

export ColonySeed
export TiledLayout, StackedLayout, VideoLayout
export redraw, generatemany, zipperzapper, reducedwithem, reducedwithmm
export scanandredraw, info, seedwith
export blank, square, random, randedge
export reduce

# for testing only
export extractid

const MAX_FILTER_COUNT = 20
@enum ColonySeed blank=1 square=2 random=3 randedge=4
seedwith(x::ColonySeed) = ColonySeed(Int(x))

const DEFAULT_REDUCE_FUNC = Meta.parse("reducewrank(neighborhood, mask)")

abstract type CWLayout end

mutable struct CommonwealthContext
    xsize::Int
    ysize::Int
    colx::Int
    coly::Int
    mask::Union{Mask, Nothing}
    filter::Union{StateFilter, Nothing}
    seed::Union{ColonySeed, Nothing}
    layout::Union{CWLayout, Nothing}
    reducef::Function
    # CommonwealthContext(nbx::Int, nby::Int, colonyx::Int, colonyy::Int, seed::ColonySeed=blank) = new(nbx,nby,colonyx,colonyy,nothing,nothing,seed)
end

mutable struct ColonyResult
    context::CommonwealthContext
    colonies::Union{Array{Array{Int}}, Nothing}
    repeats::Union{Bool, Nothing}
    repeatidx::Union{Vector{Int}, Nothing}
    filename::Union{AbstractString, Nothing}
    animatedgif::Union{Bool, Nothing}
    img::Any
end

include("persistence.jl")


function initialize_colony(T::Type{<:Number}, xsize::Int, ysize::Int, seed::ColonySeed)

    # Ensure that xsize and ysize are positive ints > 1 before continuing
    @assert xsize > 1 "xsize must be 2 or greater."
    @assert ysize > 1 "ysize must be 2 or greater."

    c = zeros(T, ysize, xsize, 2)

    if seed == blank
        c = ones(T, ysize, xsize, 2)
    elseif seed == random
        c = rand(convert(T,0):convert(T,1), ysize, xsize, 2)
    elseif seed == randedge
        c = rand(convert(T,0):convert(T,1), ysize, xsize, 2)
        c[2:end-2,2:end-2,:] .= 1
    else
        initsquare!(T, c, xsize, ysize)
    end
    return c
end

function initsquare!(T::Type{<:Number}, c::AbstractArray, xsize::Int, ysize::Int)
    y = round(T, ysize/2, RoundUp)
    x = round(T, xsize/2, RoundUp)
    y = round(T, ysize/2, RoundUp)
    x = round(T, xsize/2, RoundUp)

    # Square
    if !isodd(ysize) && !isodd(xsize)
        # Even dimensions
        c[y-1,x-1,1]=convert(T, 1)
        c[y-1,x,1]  =convert(T, 1)
        c[y-1,x+1,1]=convert(T, 1)
        c[y-1,x+2,1]=convert(T, 1)
        c[y,x-1,1]  =convert(T, 1)
        c[y,x+2,1]  =convert(T, 1)
        c[y+1,x-1,1]=convert(T, 1)
        c[y+1,x+2,1]=convert(T, 1)
        c[y+2,x-1,1]=convert(T, 1)
        c[y+2,x,1]  =convert(T, 1)
        c[y+2,x+1,1]=convert(T, 1)
        c[y+2,x+2,1]=convert(T, 1)

    else
        # One or both axes has odd length
        c[y-1,x-1,1]=convert(T, 1)
        c[y-1,x,1]  =convert(T, 1)
        c[y-1,x+1,1]=convert(T, 1)
        c[y,x-1,1]  =convert(T, 1)
        c[y,x+1,1]  =convert(T, 1)
        c[y+1,x-1,1]=convert(T, 1)
        c[y+1,x,1]  =convert(T, 1)
        c[y+1,x+1,1]=convert(T, 1)
    end
    return c
end

function get_rgb(val, switched::Bool)
    if repeater
        switched ? (val/2.5,val/2.5,val/2.5) : ((val+3)/5, (val+3)/5, (val+3)/5)
    else
        switched ? (val/1.11,val/1.11,val/1.11) : ((val+1)/5, (val+1)/5, (val+1)/5)
    end
end

function drawrepeatline!(img::Array, startpt::Tuple{Int,Int}, endpt::Tuple{Int,Int}; color=(1,0,0))
    setindex!(img, RGB(color), startpt[1]:endpt[1], startpt[2]:endpt[2])
end

function drawline!(img::AbstractArray{RGB{Float32}}, startpt::Tuple{Int,Int}, endpt::Tuple{Int,Int}, color = RGB{Float32}(0.,0.,0.))
    img[first(startpt):first(endpt), last(startpt):last(endpt)] .= color
    # setindex!(img, color, startpt[1]:endpt[1], startpt[2]:endpt[2])
end

function drawrepeatbox!(stack::AbstractArray, result::ColonyResult, color = RGB{Float32}(0.,1.,0.))
    # colonies = result.colonies
    repeats = result.repeats
    idxs = result.repeatidx

    if (repeats)
        for (i,s) in enumerate(stack)
            if i in idxs
                @debug "s, before, from index $(i):" s
                s[:,1:2] .= color
                s[:,end-1:end] .= color
                s[1:2,:] .= color
                s[end-1:end,:] .= color
                @debug "s, after" s
            end
        end
    end
    return stack
end

""" Reduces the mask and neighborhood to one float result
"""
reducedwithem(neighborhood::AbstractArray, mask::AbstractArray) = sum(mask) / max(sum(mask .* neighborhood),1.0)

# What a difference a dot makes!
reducedwithmm(neighborhood::AbstractArray, mask::AbstractArray) = sum(mask) / max(sum(mask * neighborhood), 1.0)

"""
Returns the rank of the matrix multiplication of mask x neighborhood.
"""
reducewrank(neighborhood::AbstractArray, mask::AbstractArray) = LinearAlgebra.rank(mask * neighborhood)

# f(neighborhood, mask) = calcreducedval(neighborhood, mask)
reduce(neighborhood::AbstractArray, mask::Mask, filter::StateFilter) = reducedwrank(neighborhood, mask)


# calculatenewcolony(colony::Array{T}, context::CommonwealthContext) where {T<:Number} = calculatenewcolony(f, colony, context)

function calculatenewcolony(colony::Array{T}, context::CommonwealthContext) where {T<:Number}

    mask = context.mask
    # the numeric matrix
    maskmat = mask.m
    filter = context.filter
    ctype = eltype(colony)
    coly, colx = size(colony)
    f = context.reducef

    maskdepth = div(size(maskmat, 1), 2)

    # create a blank target colony
    new_colony = initialize_colony(ctype, colx, coly, blank)

    mini = minj = 1 + maskdepth
    maxi = size(colony, 2) - maskdepth
    maxj = size(colony, 1) - maskdepth
    for i in axes(colony, 2)
        ilims = i < mini || i > maxi
        ilims && continue
        for j in axes(colony, 1)
            jlims = j < minj || j > maxj
            jlims && continue

            neighborhood = view(colony, j-maskdepth:j+maskdepth, i-maskdepth:i+maskdepth, 1)
            @debug "neighborhood = $(neighborhood)"
            @debug "mask = $(mask)"
            @debug "reducef :" reducef

            reduced_val = f(maskmat, neighborhood, filter)

            # @debug "reduced_val = $(reduced_val)"
            if !(typeof(reduced_val) <: Integer)
                reduced_val = round(ctype, reduced_val, Base.RoundDown)
            end
            # @debug "Reduced val is now = $(reduced_val)"

            if haskey(filter.d, reduced_val)
                newv, oldv = filter.d[reduced_val](colony[j,i,1])
            else
                newv, oldv = saf(colony[j,i,1])
            end
            # Store two generations of cells in the image
            new_colony[j,i,2] = oldv
            new_colony[j,i,1] = newv
        end
    end
    return new_colony
end

function calculatecommonwealth(context::CommonwealthContext)
    xsize = context.xsize
    ysize = context.ysize
    colx = context.colx
    coly = context.coly
    mask = context.mask
    filter = context.filter
    seed = context.seed
    colonies = Array{Int}[]

    colony = initialize_colony(Int, colx, coly, seed)

    # load the first colony into the history buffer
    push!(colonies, colony)

    repeater = false
    # Number of colonies in a commonwealth
    cwcnt = xsize * ysize
    # println("xsize=$xsize, ysize=$ysize, offsets=$(size(offsets))")
    idxs = Int[]
    for i in 2:cwcnt
        new_colony = calculatenewcolony(colony, context)
        # repeater = in(new_colony, colonies)
        # this next line is quite expensive; can it be speeded up?
        idx = first(indexin([new_colony], colonies))
        if idx != nothing
            idx = first(idx)
            push!(idxs, idx)
            @debug "colony repeats at index $(idx)!"
            repeater = true
        end
        push!(colonies, new_colony)
        colony = new_colony
    end #offsetsend

    # make the repeater indices unique
    unique!(idxs)

    return ColonyResult(context, colonies, repeater, idxs, nothing, nothing, nothing)
end

"""
Stack each NxN float array vertically (dim=3) and convert to grayscale.
"""
function layoutimage(lo::StackedLayout, imgstack::Vector, context::CommonwealthContext)
    return cat(imgstack[1:end]...,dims=3)
end

layoutimage(lo::VideoLayout, imgstack::Vector, context::CommonwealthContext) = return imgstack

"""
Tile the images in the stack using parameters of the context and convert to grayscale.
"""
function layoutimage(lo::TiledLayout, imgstack::Vector, context::CommonwealthContext)
    @debug "tiling stack of size $(size(imgstack)), eltype $(eltype(imgstack)), to depth $(context.ysize)"
    return hvcat(context.ysize, imgstack[1:end]...)
end

colorimage(imgstack::Vector, result::ColonyResult) = colorimage(result.context.layout, imgstack, result)

function colorimage(layout::Union{TiledLayout,StackedLayout}, imgstack::Vector, result::ColonyResult)
    resultstack = Array{RGB{Float32}}[]
    for s in imgstack
        @debug "before coloring img in stack is " s
        push!(resultstack, RGB{Float32}.(Gray{Float32}.(s)))
    end
    drawrepeatbox!(resultstack, result)
    return resultstack
end

function colorimage(layout::VideoLayout, imgstack::Vector, result::ColonyResult)
    resultstack = Array{Gray{N0f8}}[]
    for s in imgstack
        @debug "before coloring, img in stack is " s
        push!(resultstack, Gray{N0f8}.(s))
    end
    return resultstack
end

"""

"""
function buildimage(context::CommonwealthContext)

    # All the heavy lifting takes place in calculatecommonwealth
    colonyres = calculatecommonwealth(context)
    # And here are the results...
    colonies = colonyres.colonies
    repeats = colonyres.repeats
    repeatidx = colonyres.repeatidx

    @debug "colonies has size $(size(colonies)) and $(repeats ? "repeats with index $repeatidx." : "does not repeat.")"
    # colonies is a stack (i.e. array) of image arrays which can be layed out
    # as a nxm tiled image or as an "animated gif" with nxm layers.

    # First, convert each layer into a matrix of floats whose values {0, 1/3, 2/3, 1}
    # represent the four states of a cell. This will determine the cell's color.
    stack = map(c -> (c[:,:,1] .<< 1 .+ c[:,:,2]) .* 1/3, colonies)

    @debug "stack has size $(size(stack)), eltype $(eltype(stack))"

    stack = colorimage(stack, colonyres)

    @debug "after coloring, first image has size $(size(stack[1])), eltype $(eltype(stack[1]))"

    layout = context.layout
    if isnothing(layout)
        layout = TiledLayout("png")
    end

    colonyres.img = layoutimage(layout, stack, context)
    @debug "layed out image has size $(size(colonyres.img))"
    # stack each NxN float array vertically (dim=3) and convert to grayscale

    return colonyres
end


"""
    redraw(filename::AbstractString)

    Look up the given filename in the colonies database (colony4j.csv)
    and attempt to re

"""
function redraw(file_or_id::AbstractString, destdir::AbstractString, cwx::Int = -1, cwy::Int = -1, colx::Int = -1, coly::Int = -1;
    mask::Union{Mask, Nothing} = nothing,
    filter::Union{StateFilter, Nothing} = nothing,
    seed::Union{ColonySeed, Nothing} = nothing,
    reducef::Function = (m,n,f)->rank(m * n),
    layout::CWLayout = TiledLayout("png"),
    maskrange::UnitRange{Int} = 1:4,
    maskdim::Int = 3,
    shuffle::Bool = true,
    dbfile::Union{IO, AbstractString} = db_filename()
    )

    id = extractid(file_or_id)

    # only look up the DB record if we have to
    if isnothing(mask) || isnothing(filter) || isnothing(seed)
        @info "Searching for record with id='$id'..."
    # search for filename in archive DataFrame
    # open CSV archive file as a DataFrame
        df = CSV.read(dbfile; types=DB_ELTYPES, header=DB_HEADERS, datarow=2, delim=',')
        record = archiverowfromid(df,id)
        if first(size(record)) > 0
            @info "Found record. Regenerating..."
            show(record, allcols=true)
            cwx = cwx < 0 ? first(record[!, columnindex(df, :cwx)]) : cwx
            cwy = cwy < 0 ? first(record[!, columnindex(df, :cwy)]) : cwy
            colx = colx < 0 ? first(record[!, columnindex(df, :colx)]) : colx
            coly = coly < 0 ? first(record[!, columnindex(df, :coly)]) : coly
            s = isnothing(seed) ? first(record[!, columnindex(df, :seed)]) : seed
            m = isnothing(mask) ? first(record[!, columnindex(df, :mask)]) : mask
            f = isnothing(filter) ? first(record[!, columnindex(df, :filter)]) : filter
        else
            println("Failed to find record for $(id)")
            return nothing
        end
        if s isa AbstractString
            seed = eval(Meta.parse(s))
            @debug "Recovered seed=$seed"
        end

        if m isa AbstractString
            mask = maskfromstring(m)
            @debug "Recovered mask=$mask"
        end

        if f isa AbstractString
            filter::StateFilter = filterfromstring(f)
            @debug "Recovered filter=$filter"
        end
    else
        @info "Regenerating with given mask, filter and seed..."
    end

    if isnothing(layout)
        layout = layoutforfile(filename)
    end
    @debug "layout = $(layout)"

    if !isdir(destdir)
        mkdir(destdir)
    end

    # call calculatecommonwealth
    context = CommonwealthContext(cwx, cwy, colx, coly, mask, filter, seed, layout, reducef)
    colonyres = buildimage(context)
    img = colonyres.img
    f = saveimage(colonyres, destdir)
    println("Saved image to $(f)")

end

function layoutforfile(filename::AbstractString)
    ext = last(Base.Filesystem.splitext(fn))
    if lowercase(ext) == ".gif"
        return StackedLayout()
    else
        return TiledLayout(ext)
    end
end

""" scanandredraw


"""
function scanandredraw(srcdir::AbstractString, destdir::AbstractString, cwx::Int, cwy::Int, colx::Int, coly::Int)
    sd = srcdir
    dd = destdir
    if !isdir(dd)
        @info "Creating new directory for redraws:" dd
        mkpath(dd)
    end

    # get list of files in provided directory
    ls = readdir(sd)
    for filename in ls
        startswith(filename,'.') ? continue : nothing
        redraw(filename, destdir, cwx, cwy, colx, coly)
    end
end

"""
Returns an iterator of (StateFilter, Mask) pairs. It does this by collecting
all StateFilters (~573120) and all Masks (3024), repeating
the mask vector as many times as needed in order to provide a result of
one pair of (StateFilter, Mask) from the zip function iterator.
"""
function zipperzapper(r::UnitRange{Int}, dim::Int, shufflefilters::Bool)
    m = collect(generate_masks(r, dim))
    filters = StateFilter[]
    for f in generate_state_filters(shufflefilters)
        push!(filters, f)
    end
    mx = repeat(m, div(length(filters), length(m)))
    zip(mx, filters)
end



function generatemany(cwx::Int, cwy::Int, colony_x::Int, colony_y::Int, extremerandom::Bool;
    limit = -1,
    mask::Union{Mask,Nothing} = nothing,
    filter::Union{StateFilter,Nothing} = nothing,
    seed::Union{ColonySeed,Nothing} = nothing,
    reducef::Function = (m,n,f)->rank(m * n),
    layout::CWLayout = TiledLayout("png"),
    maskrange::UnitRange{Int} = 1:4,
    maskdim::Int = 3,
    shuffle::Bool = true,
    destdir::Union{AbstractString,Nothing} = nothing
    )

    if !isnothing(destdir) && !isdir(destdir)
        mkpath(destdir)
    end

    cnt = 0
    if extremerandom
        # Generate all masks and all filters and iterate through pairs of (mask,filter)
        for p in zipperzapper(maskrange, maskdim, shuffle)
            mm = isnothing(mask) ? p[1] : mask
            ff = isnothing(filter) ? p[2] : filter
            ss = isnothing(seed) ? ColonySeed(rand(UnitRange(1,4))) : seed
            @debug "mask=$(mm), filter=$(ff), seed=$(ss), reducef=$(reducef)"
            context = CommonwealthContext(cwx, cwy, colony_x, colony_y, mm, ff, ss, layout, reducef)
            colonyres = buildimage(context)
            imgf = saveimage(colonyres)
            println(cnt,"-",imgf,(colonyres.repeats ? " *" : ""))
            cnt += 1
            if limit > 0 && cnt > limit
                return
            end
            # Keep this after the test to only print 1-(n-1).
            # println(cnt)
        end
    else
        for mgen in generate_masks(maskrange, maskdim)
            # use the mask provided in the function argument if there is one
            mm = isnothing(mask) ? mgen : mask
            fc = 0
            for fgen in generate_state_filters(shuffle)
                # use the filter provided in the function argument if there is one
                ff = isnothing(filter) ? fgen : filter
                if fc > MAX_FILTER_COUNT
                    @info "Moving to the next mask..."
                    break
                end
                fc += 1
                for i = 1:4
                    if !isnothing(seed) && i > 1
                        # we have already tried this seed, so move on
                        break
                    end
                    ss = isnothing(seed) ? ColonySeed(i) : seed
                    @debug "mask=$(mm), filter=$(ff), seed=$(ss)"
                    context = CommonwealthContext(cwx, cwy, colony_x, colony_y, mm, ff, ss, layout, reducef)
                    colonyres = buildimage(context)
                    imgf = saveimage(colonyres)
                    println(cnt,"-",imgf,(colonyres.repeats ? " *" : ""))
                    cnt += 1
                    if limit > 0 && cnt > limit
                        return
                    end
                end
            end #next filter
        end# next mask
    end
end

function regeneratebest(srcdir::AbstractString="img/best", destdir = "img/giant")

    xsize=25
    ysize=16
    colxsize = 100
    coly = 100
    scalef = 2
    mask = Union{Mask, Nothing}()
    filter = Union{Mask, Nothing}()
    seed = Union{Mask, Nothing}()
    layout = TiledLayout("png")
    context = CommonwealthContext(xsize,ysize,colxsize,coly,mask,filter,seed,layout)

    scanandredraw(srcdir, destdir, context)
end

end #module
