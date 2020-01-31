using Images
using DataFrames
using CSV
using Query
using Random
using UUIDs
# using OffsetArrays
using Colors

include("filtersnew.jl")
include("masks.jl")
# include("Filters.jl")
# using .Filters
# include("maskmod.jl")
# using .Masks

const MAX_FILTER_COUNT = 20
@enum ColonySeed blank=1 square=2 random=3

include("persistence.jl")

mutable struct CommonwealthContext
    xsize::Int
    ysize::Int
    colonyxsize::Int
    colonyysize::Int
    mask::Union{Mask, Nothing}
    filter::Union{StateFilter, Nothing}
    seed::Union{ColonySeed, Nothing}

    # CommonwealthContext(nbx::Int, nby::Int, colonyx::Int, colonyy::Int, seed::ColonySeed=blank) = new(nbx,nby,colonyx,colonyy,nothing,nothing,seed)
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

""" Return the result of applying an algorithm to neighborhood and mask
to fix the return value as the given type of number. Default = Int."""
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

    # create a blank target colony
    new_colony = initialize_colony(ctype, colonyxsize, colonyysize, blank)

    mini = minj = 2
    maxi = size(colony, 2)-1
    maxj = size(colony, 1)-1
    for i in axes(colony, 2)
        ilims = i < mini || i > maxi
        ilims && continue
        for j in axes(colony, 1)
            jlims = j < minj || j > maxj
            jlims && continue

            neighborhood = view(colony, j-1:j+1, i-1:i+1, 1)
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
    for i in 2:cwcnt
        new_colony = calculatenewcolony(colony, context)
        repeater = in(new_colony, colonies)
        push!(colonies, new_colony)
        colony = new_colony
    end #offsetsend

    return colonies, repeater
end

function redraw(filename::AbstractString)
    body
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

    # open CSV archive file as a DataFrame
    df = CSV.read(db_filename(); types=[String, String, String, String, String, String], header=DB_HEADERS, datarow=2, delim=',')

    # get list of files in provided directory
    ls = readdir(sd)
    for filename in ls
        @debug "filename=$filename"
        startswith(filename,'.') ? continue : nothing
        id = idfromcolonyfilepath(filename)
        @info "Searching for record with id='$id'"
        # search for filename in archive DataFrame
        record = archiverowfromid(df,id)
        # println(record)
        # if record found, get mask and filter
        if length(record) > 0
            # println("'$record'")

            # convert mask and filter to objects
            s = first(record[!, 1])
            m = first(record[!, 2])
            f = first(record[!, 3])

            seed::ColonySeed = eval(Meta.parse(s))
            @debug "Recovered seed=$seed"

            mask::Mask = maskfromstring(m)
            @debug "Recovered mask=$mask"

            filter::StateFilter = filterfromstring(get(f[1]))
            @debug "Recovered filter=$filter"

            # call calculatecommonwealth
            context = CommonwealthContext(cwx, cwy, colonyx, colonyy, mask, filter, seed)
            img, repeater = makecwimage(context)
            save_image_info(img,dd,context,repeater)
        else
            @warn "Failed to find record for $(filename)"
        end
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

function makecwimage(context::CommonwealthContext)
    colonies, repeater = calculatecommonwealth(context)
    stack = map(c -> (c[:,:,1] .<< 1 .+ c[:,:,2]) .* 1/3, colonies)

    # stack each NxN float array vertically (dim=3) and convert to grayscale
    img = Gray.(cat(stack[1:end]...,dims=3))
    return img, repeater
end

function generatemany(cwx, cwy, colony_x, colony_y, extremerandom::Bool; limit = -1)
    seed = ColonySeed(rand(1:3))
    @debug "Seeding new CW with $(seed)"
    regfiledir = create_save_dir("regular")
    repeatfiledir = create_save_dir("repeat")
    cnt = 0
    if extremerandom
        for p in zipperzapper(1:4, 3, true)
            mask, filter = p[1], p[2]
            context = CommonwealthContext(cwx, cwy, colony_x, colony_y, mask, filter, seed)
            img, repeater = makecwimage(context)
            save_image_info(img, (repeater ? repeatfiledir : regfiledir), seed, mask, filter, repeater)
            cnt += 1
            if limit > 0 && cnt >= limit
                return cnt
            end
        end
    else
        generatemany(cwx,cwy,colony_x,colony_y,limit)
    end
end

function generatemany(cwx, cwy, colony_x, colony_y; limit = -1)
    seed = ColonySeed(rand(1:3))
    @debug "Seeding new CW with $(seed)"
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
            context = CommonwealthContext(cwx, cwy, colony_x, colony_y, mask, filter, seed)
            img, repeater = makecwimage(context)
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
    context = CommonwealthContext(xsize,ysize,colonyxsize,colonyysize,mask,filter,seed)

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

    context = CommonwealthContext(xsize, ysize, colonyxsize, colonyysize, mask, filter, seed)

    filedir = create_save_dir("$filter")
    # println("offsets are $offsets")
    # offsets = get_random_offsets((Int)(round(0.8*COMMONWEALTH_X*COMMONWEALTH_Y)))
    repeater::Bool = calculatecommonwealth(context)
    save_image_info(crgb, filedir, seed, mask, filter, repeater)
    println("Done.")
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
