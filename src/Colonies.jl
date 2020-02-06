module Colonies

using Images
using DataFrames
using CSV
using Query
using Random
using UUIDs
using Colors

include("Filters.jl")
using .Filters
include("Masks.jl")
using .Masks

const MAX_FILTER_COUNT = 20
@enum ColonySeed blank=1 square=2 random=3

export Mask, StateFilter, ColonySeed, TiledLayout, StackedLayout
export redraw, generatemany, zipperzapper, reducedwithem, reducedwithmm, scanandredraw
export generate_masks, generate_state_filter

# include("filtersnew.jl")
# include("masks.jl")


abstract type CWLayout end

mutable struct CommonwealthContext
    xsize::Int
    ysize::Int
    colonyxsize::Int
    colonyysize::Int
    mask::Union{Mask, Nothing}
    filter::Union{StateFilter, Nothing}
    seed::Union{ColonySeed, Nothing}
    layout::Union{CWLayout, Nothing}
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
    if seed==blank
        c = ones(T, ysize, xsize, 2)
    elseif seed==random
        c = rand(convert(T,0):convert(T,1), ysize, xsize, 2)
    end

    if seed != blank && seed != random && !isodd(ysize) && !isodd(xsize)
        # Even dimensions
        y = round(T, ysize/2, RoundUp)
        x = round(T, xsize/2, RoundUp)

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


    elseif seed != blank && seed != random && (isodd(ysize) || isodd(xsize))
        # Odd dimensions
        y = round(Int, ysize/2, Base.RoundUp)
        x = round(Int, xsize/2, Base.RoundUp)

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

# f(neighborhood, mask) = calcreducedval(neighborhood, mask)
f(neighborhood, mask) = reducedwithem(neighborhood, mask)

# calculatenewcolony(colony::Array{T}, context::CommonwealthContext) where {T<:Number} = calculatenewcolony(f, colony, context)

function calculatenewcolony(colony::Array{T}, context::CommonwealthContext) where {T<:Number}

    mask = context.mask
    # the numeric matrix
    maskmat = mask.m
    filter = context.filter
    ctype = eltype(colony)
    colonyysize, colonyxsize = size(colony)

    maskdepth = div(size(maskmat, 1), 2)

    # create a blank target colony
    new_colony = initialize_colony(ctype, colonyxsize, colonyysize, blank)

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
            # @debug "neighborhood = $(neighborhood)"
            reduced_val = f(neighborhood, maskmat)
            # @debug "Reduced val = $(reduced_val)"
            # @debug "reduced_val = $(reduced_val)"
            reduced_val = round(ctype, reduced_val, Base.RoundDown)
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
    colonyxsize = context.colonyxsize
    colonyysize = context.colonyysize
    mask = context.mask
    filter = context.filter
    seed = context.seed

    colony = initialize_colony(Int, colonyxsize, colonyysize, seed)
    colonies = Array{Int}[]
    # load the first colony into the history buffer
    push!(colonies, colony)

    repeater = false
    # Number of colonies in a commonwealth
    cwcnt = xsize*ysize
    # println("xsize=$xsize, ysize=$ysize, offsets=$(size(offsets))")
    idxs = Int[]
    for i in 2:cwcnt
        new_colony = calculatenewcolony(colony, context)
        # repeater = in(new_colony, colonies)
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

"""
Tile the images in the stack using parameters of the context and convert to grayscale.
"""
function layoutimage(lo::TiledLayout, imgstack::Vector, context::CommonwealthContext)
    @debug "tiling stack of size $(size(imgstack)), eltype $(eltype(imgstack)), to depth $(context.ysize)"
    return hvcat(context.ysize, imgstack[1:end]...)
end

function colorimage!(imgstack::Vector, result::ColonyResult)
    resultstack = Array{RGB{Float32}}[]
    for s in imgstack
        @debug "before coloring img in stack is " s
        push!(resultstack, RGB{Float32}.(Gray{Float32}.(s)))
    end
    drawrepeatbox!(resultstack, result)
    return resultstack
end

"""

"""
function buildimage(context::CommonwealthContext)
    # All the heavy lifting takes place in calculatecommonwealth
    colonyres = calculatecommonwealth(context)

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

    stack = colorimage!(stack, colonyres)

    @debug "after coloring, stack has size $(size(stack)), eltype $(eltype(stack))"

    layout = context.layout
    if isnothing(layout)
        layout = TiledLayout("png")
    end

    img = layoutimage(layout, stack, context)
    @debug "layed out image has size $(size(img))"
    # stack each NxN float array vertically (dim=3) and convert to grayscale

    # Add the image to the result object
    colonyres.img = img
    return colonyres
end


function extractid(file_or_id::AbstractString)
    t = splitext(file_or_id)
    if t[2] == ""
        # assume id
        id = t[2]
    else
        id = idfromcolonyfilepath(file_or_id)
    end
    return id
end

"""
    redraw(filename::AbstractString)

    Look up the given filename in the colonies database (colony4j.csv)
    and attempt to re

"""
function redraw(file_or_id::AbstractString, destdir::AbstractString, cwx::Int, cwy::Int, colonyx::Int, colonyy::Int;
    mask::Union{Mask,Nothing} = nothing,
    filter::Union{StateFilter,Nothing} = nothing,
    seed::Union{ColonySeed,Nothing} = nothing,
    layout::CWLayout = TiledLayout("png"),
    maskrange::UnitRange{Int} = 1:4,
    maskdim::Int = 3,
    shuffle::Bool = true,
    )

    id = extractid(file_or_id)

    # only look up the DB record if we have to
    if isnothing(mask) || isnothing(filter) || isnothing(seed)
        @info "Searching for record with id='$id'..."
    # search for filename in archive DataFrame
    # open CSV archive file as a DataFrame
        df = CSV.read(db_filename(); types=DB_ELTYPES, header=DB_HEADERS, datarow=2, delim=',')
        record = archiverowfromid(df,id)
        if first(size(record)) > 0
            @info "Found record. Regenerating..." record
            s = isnothing(seed) ? first(record[!, columnindex(df, :seed)]) : seed
            m = isnothing(mask) ? first(record[!, columnindex(df, :mask)]) : mask
            f = isnothing(filter) ? first(record[!, columnindex(df, :filter)]) : filter
        else
            println("Failed to find record for $(id)")
            return nothing
        end

        seed::ColonySeed = eval(Meta.parse(s))
        @debug "Recovered seed=$seed"

        mask::Mask = maskfromstring(m)
        @debug "Recovered mask=$mask"

        filter::StateFilter = filterfromstring(f)
        @debug "Recovered filter=$filter"
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
    context = CommonwealthContext(cwx, cwy, colonyx, colonyy, mask, filter, seed, layout)
    colonyres = buildimage(context)
    img = colonyres.img
    f = save_image_info(colonyres, destdir)
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
function scanandredraw(srcdir::AbstractString, destdir::AbstractString, cwx::Int, cwy::Int, colonyx::Int, colonyy::Int)
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
        redraw(filename, destdir, cwx, cwy, colonyx, colonyy)
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
    layout::CWLayout = TiledLayout("png"),
    maskrange::UnitRange{Int} = 1:4,
    maskdim::Int = 3,
    shuffle::Bool = true,
    destdir::Union{AbstractString,Nothing} = nothing
    )

    if isnothing(destdir)
        regfiledir = create_save_dir("regular")
        repeatfiledir = create_save_dir("repeat")
    else
        if !isdir(destdir)
            mkpath(destdir)
        end
        regfiledir = repeatfiledir = destdir
    end

    cnt = 0
    if extremerandom
        # Generate all masks and all filters and iterate through pairs of (mask,filter)
        for p in zipperzapper(maskrange, maskdim, shuffle)
            mm = isnothing(mask) ? p[1] : mask
            ff = isnothing(filter) ? p[2] : filter
            ss = isnothing(seed) ? ColonySeed(rand(UnitRange(1,3))) : seed
            @debug "mask=$(mm), filter=$(ff), seed=$(ss)"
            context = CommonwealthContext(cwx, cwy, colony_x, colony_y, mm, ff, ss, layout)
            colonyres = buildimage(context)
            save_image_info(colonyres, (colonyres.repeats ? repeatfiledir : regfiledir))
            cnt += 1
            println(cnt)
            if limit > 0 && cnt >= limit
                return cnt
            end
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
                for i = 1:3
                    if !isnothing(seed) && i > 1
                        # we have already tried this seed, so move one
                        break
                    end
                    ss = isnothing(seed) ? ColonySeed(i) : seed
                    @debug "mask=$(mm), filter=$(ff), seed=$(ss)"
                    context = CommonwealthContext(cwx, cwy, colony_x, colony_y, mm, ff, ss, layout)
                    colonyres = buildimage(context)
                    f = save_image_info(colonyres, (colonyres.repeats ? repeatfiledir : regfiledir))
                    println("$f")
                    cnt += 1
                    println(cnt)
                    if limit > 0 && cnt >= limit
                        return cnt
                    end
                end
            end #next filter
        end# next mask
    end
    println("Generated $(cnt) files.")
end

function regeneratebest(srcdir::AbstractString="img/best", destdir = "img/giant")

    xsize=25
    ysize=16
    colonyxsize = 100
    colonyysize = 100
    scalef = 2
    mask = Union{Mask, Nothing}()
    filter = Union{Mask, Nothing}()
    seed = Union{Mask, Nothing}()
    layout = TiledLayout("png")
    context = CommonwealthContext(xsize,ysize,colonyxsize,colonyysize,mask,filter,seed,layout)

    scanandredraw(srcdir, destdir, context)
end

function main(args::Array{String})

    if length(args) < 9
        println("Usage: julia coloniesX.jl <mask> <filter> <xsize> <ysize> <colony_x> <colony_y> <scale_factor> <seed:blank|square|random> <draw_repeat_line:true|false>")
        exit(1)
    end
    println("Calculating with arguments $args")

    mask = maskfromstring(args[1])
    filter = filterfromstring(args[2])
    # println("ARGS[2]=$(args[2])")
    # println("Filter=$filter")
    xsize::Int = eval(parse(args[3]))
    ysize::Int = eval(parse(args[4]))
    colonyxsize::Int = eval(parse(args[5]))
    colonyysize::Int = eval(parse(args[6]))
    scalef::Int = eval(parse(args[7]))
    seed::ColonySeed = eval(parse(args[8]))
    drawrepeatline::Bool = eval(parse(args[9]))

    img = create_drawing_context(xsize, ysize, colonyxsize, colonyysize, scalef)

    offsets = normal_offsets(xsize, ysize, colonyxsize, colonyysize)

    layout = TiledLayout("png")

    context = CommonwealthContext(xsize, ysize, colonyxsize, colonyysize, mask, filter, seed, layout)

    filedir = create_save_dir(filter)
    # println("offsets are $offsets")
    # offsets = get_random_offsets((Int)(round(0.8*COMMONWEALTH_X*COMMONWEALTH_Y)))
    colonyres = buildimage(context)
    save_image_info(colonyres.img, filedir, context, colonyres.repeats)
    @info "Done."
end


# CLIENT CODE STARTS HERE
# main(Base.ARGS)
# generatemany(commonwealth_x,commonwealth_y,colony_x,colony_y,scale_factor)
# generatemany(5,5,50,50)
# regeneratebest()
end #module
