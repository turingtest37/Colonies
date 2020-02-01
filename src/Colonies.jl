using Images
using DataFrames
using CSV
using Query
using Random
using UUIDs
using Colors

# include("Filters.jl")
# using .Filters
# include("maskmod.jl")
# using .Masks

const MAX_FILTER_COUNT = 20
@enum ColonySeed blank=1 square=2 random=3

include("filtersnew.jl")
include("masks.jl")
include("persistence.jl")

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
    repeatidx::Union{Int, Nothing}
    filename::Union{AbstractString, Nothing}
    animatedgif::Union{Bool, Nothing}
    img::Any
end


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

drawrepeatline!(img::Array, startpt::Tuple{Int,Int}, endpt::Tuple{Int,Int}; color=(1,0,0)) = setindex!(img, RGB(color), startpt[1]:endpt[1], startpt[2]:endpt[2])

""" Return the result of applying an algorithm to the neighborhood and the mask.
"""
function calcreducedval(neighborhood, mask)
    sum(mask) / max(sum(mask .* neighborhood),1.0)
end

f(neighborhood, mask) = calcreducedval(neighborhood, mask)

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
    idx = nothing
    for i in 2:cwcnt
        new_colony = calculatenewcolony(colony, context)
        # repeater = in(new_colony, colonies)
        idx = first(indexin([new_colony], colonies))
        if idx != nothing
            @debug "idx=" idx
            repeater = true
            idx = first(idx)
            @info "Colony repeats at index $(idx)!"
        end
        push!(colonies, new_colony)
        colony = new_colony
    end #offsetsend

    return ColonyResult(context, colonies, repeater, idx, nothing, nothing, nothing)
end

"""
    redraw(filename::AbstractString)

    Look up the given filename in the colonies database (colony4j.csv)
    and attempt to re

"""
function redraw(filename::AbstractString, dd::AbstractString, cwx, cwy, colonyx, colonyy; layout = TiledLayout("png"))
    @debug "filename=$filename"
    id = idfromcolonyfilepath(filename)
    @info "Searching for record with id='$id'"
    # search for filename in archive DataFrame
    # open CSV archive file as a DataFrame
    df = CSV.read(db_filename(); types=[String, String, String, String, String, String], header=DB_HEADERS, datarow=2, delim=',')
    record = archiverowfromid(df,id)
    # println(record)
    # if record found, get mask and filter
    if first(size(record)) > 0
        @debug "Recovered record" record
        # convert mask and filter to objects
        s = first(record[!, 3])
        m = first(record[!, 4])
        f = first(record[!, 5])

        seed::ColonySeed = eval(Meta.parse(s))
        @debug "Recovered seed=$seed"

        mask::Mask = maskfromstring(m)
        @debug "Recovered mask=$mask"

        filter::StateFilter = filterfromstring(f)
        @debug "Recovered filter=$filter"

        if isnothing(layout)
            layout = layoutforfile(filename)
        end
        @debug "layout = $(layout)"

        if !isdir(dd)
            dd = create_save_dir(dd, "", true)
        end

        # call calculatecommonwealth
        context = CommonwealthContext(cwx, cwy, colonyx, colonyy, mask, filter, seed, layout)
        colonyres = buildimage(context)
        save_image_info(colonyres.img,dd,context,colonyres.repeats)
    else
        @warn "Failed to find record for $(filename)"
    end
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

function zipperzapper(r::UnitRange{Int}, dim::Int, shufflefilters::Bool)
    m = collect(generate_masks(r, dim))
    filters = StateFilter[]
    for f in generate_state_filters(shufflefilters)
        push!(filters, f)
    end
    mx = repeat(m, div(length(filters), length(m)))
    zip(mx, filters)
end


"""
Stack each NxN float array vertically (dim=3) and convert to grayscale.
"""
function layoutimage(lo::StackedLayout, imgstack::Array, context::CommonwealthContext)
    return Gray.(cat(imgstack[1:end]...,dims=3))
end

"""
Tile the images in the stack using parameters of the context and convert to grayscale.
"""
function layoutimage(lo::TiledLayout, imgstack::Array, context::CommonwealthContext)
    return Gray.(hvcat(context.ysize, imgstack[1:end]...))
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

function generatemany(cwx, cwy, colony_x, colony_y, extremerandom::Bool; limit = -1, layout = TiledLayout("png"))
    regfiledir = create_save_dir("regular")
    repeatfiledir = create_save_dir("repeat")
    cnt = 0
    if extremerandom
        for p in zipperzapper(1:4, 3, true)
            mask, filter = p[1], p[2]
            @debug "mask=$(mask), filter=$(filter)"
            seed = ColonySeed(rand(1:3))
            @debug "Seeding new CW with $(seed)"
            context = CommonwealthContext(cwx, cwy, colony_x, colony_y, mask, filter, seed, layout)
            colonyres = buildimage(context)
            save_image_info(colonyres.img, (colonyres.repeats ? repeatfiledir : regfiledir), context, colonyres.repeats)
            cnt += 1
            if limit > 0 && cnt >= limit
                return cnt
            end
        end
    else
        generatemany(cwx,cwy,colony_x,colony_y,limit,layout)
    end
end

function generatemany(cwx, cwy, colony_x, colony_y; limit = -1, layout = TiledLayout("png"))
    # Generate new directories for each run
    regfiledir = create_save_dir("regular")
    repeatfiledir = create_save_dir("repeat")
    # mask = the12p7weightedmask()
    fc = 0
    cnt = 0
    for mask in generate_masks(1:4, 3)
        fc = 0
        for filter in generate_state_filters(true)
            if fc > MAX_FILTER_COUNT
                @info "Moving to the next mask..."
                break
            end
            fc += 1
            seed = ColonySeed(rand(1:3))
            @debug "Seeding new CW with $(seed)"
            context = CommonwealthContext(cwx, cwy, colony_x, colony_y, mask, filter, seed, layout)
            img, repeater = buildimage(context)
            save_image_info(img, (repeater ? repeatfiledir : regfiledir), seed, mask, filter, repeater)
            cnt += 1
            if limit > 0 && cnt >= limit
                return cnt
            end
        end #next filter
    end# next mask
    return cnt
end

function regeneratebest()
    srcdir = "img/best"
    destdir = "img/giant"
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

    filedir = create_save_dir("$filter")
    # println("offsets are $offsets")
    # offsets = get_random_offsets((Int)(round(0.8*COMMONWEALTH_X*COMMONWEALTH_Y)))
    colonyres = buildimage(context)
    save_image_info(colonyres.img, filedir, context, colonyres.repeats)
    @info "Done."
end

function encode(s::AbstractString)
    m = rand(Mask, collect(generate_masks(1:4, 3)))
    f = rand(StateFilter, collect(generate_state_filters(false)))
    encode(s, m, f)
end

function encode(s::AbstractString, mask::Mask, filter::StateFilter)


    context = CommonwealthContext()

    # a 7 x n matrix where n = nb of characters in string s
    A = hcat((digits(UInt8(c), base=2) for c in s)...)
    # think about this later!!!

end


# MAIN CODE STARTS HERE
# main(Base.ARGS)
# generatemany(commonwealth_x,commonwealth_y,colony_x,colony_y,scale_factor)
# generatemany(5,5,50,50)
# regeneratebest()
