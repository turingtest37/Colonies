using Images
using Combinatorics
using StaticArrays
using DataFrames
using CSV
using Query
using Random
using UUIDs
# using OffsetArrays
using Colors

include("Filters.jl")
using .Filters
include("maskmod.jl")
using .Masks

const MAX_FILTER_COUNT = 20
const ID_REGEX = r"^\w+\-([\w\-]*)$"
const DB_FILE_NAME = "colony4j.csv"
const DB_HEADERS = String["id","file","seed","mask","filter","repeater"]

@enum ColonySeed blank=1 square=2 random=3

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

function create_save_dir(dirtype::AbstractString)
    name = joinpath("img",dirtype,string(rand(1:1000000)))
    if !isdir(name)
        mkpath(name)
    end
    name
end

function db_filename()
    return DB_FILE_NAME
end

function archive_image_db(dbf::AbstractString, colonyid, colonypath::AbstractString, seed::ColonySeed, mask::Mask, filter::StateFilter, repeater::Bool)
    df = DataFrame(id=string(colonyid), name=colonypath, seed="$seed", mask="$mask", filter="$filter", repeater="$repeater")
    CSV.write(dbf, df; delim=',', header=DB_HEADERS, append=true)
end

make_colony_id() = UUIDs.uuid1()

make_filename(id, filedir) = joinpath(filedir,"colony4j-$id.png")

make_filename(filedir) = make_filename(filedir,rand(1:1000000))


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
    # c = reshape([(va[j,i], sa[j,i]) for i in 1:xsize for j in 1:ysize], (ysize,xsize))

    y = round(T, ysize/2, RoundUp) + 1
    x = round(T, xsize/2, RoundUp) + 1
    if seed != blank && seed != random && !isodd(ysize) && !isodd(xsize)
        # println(c[Int(xsize/2-1), Int(ysize/2-1)])
        # y,x
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
        y = round(Int, ysize/2, Base.RoundUp) + 1
        x = round(Int, xsize/2, Base.RoundUp) + 1

        c[y-1,x-1,1]=convert(T, 1)
        c[y-1,x,1]  =convert(T, 1)
        c[y-1,x+1,1]=convert(T, 1)
        c[y,x-1,1]  =convert(T, 1)
        c[y,x+1,1]  =convert(T, 1)
        c[y+1,x-1,1]=convert(T, 1)
        c[y+1,x,1]  =convert(T, 1)
        c[y+1,x+1,1]=convert(T, 1)
    end
    c
end

function get_rgb(val, switched::Bool)
    # val*0.09 + (switched ? 0.2 : 0),val*0.09 + (switched ? 0 : 0.2),val*0.1
    # val/2,val/5,(switched ? 0.2 : val/3)

    if repeater
        # switched=black (0) or light gray (1)=0.76 ; else same=med gray(0)=0.4 or light gray(1)=0.6
        # switched ? (val/1.3,val/1.3,val/1.3) : ((val+2)/5, (val+2)/5, (val+2)/5)
        # switched=black (0) or med gray (1)=0.4 ; else same=light gray(0)=0.6 or very light gray(1)=0.8
        switched ? (val/2.5,val/2.5,val/2.5) : ((val+3)/5, (val+3)/5, (val+3)/5)
    else
        # switched=black (0) or very light gray (1)=0.90 ; else same=dark gray(0)=0.2 or med gray(1)=0.4
        switched ? (val/1.11,val/1.11,val/1.11) : ((val+1)/5, (val+1)/5, (val+1)/5)
    end
end

draw_colony!(colony::Array{Tuple{Int,Bool},2}) = draw_colony!(colony, false, false)

drawrepeatline!(img::Array, startpt::Tuple{Int,Int}, endpt::Tuple{Int,Int}; color=(1,0,0)) = setindex!(img, RGB(color), startpt[1]:endpt[1], startpt[2]:endpt[2])

function draw_colony(colony::Array{Tuple{Int,Bool},2}, repeater::Bool, drawrepeat::Bool)
    # println("Drawing colony $(size(colony))")
    for co_y in axes(colony,1)
      for co_x in axes(colony,2)
          val, switched = colony[co_y,co_x]
          r,g,b = get_rgb(val,switched,repeater)
          set_source_rgb(cctx,r,g,b)
          rectangle(cctx, co_x-1, co_y-1, 1, 1)
          fill(cctx)
      end
    end
    drawcolonyoutline(cctx, (1,1), (size(colony,2),size(colony,1)))
    if repeater && drawrepeat
        drawrepeatline(cctx,(0,0),(1,size(colony,1)))
    end
end

calcreducedval(neighborhood::Array, mask::Array) = calcreducedval(Int, neighborhood, mask)

""" Return the result of applying an algorithm to neighborhood and mask
to fix the return value as the given type of number. Default = Int."""
function calcreducedval(t::Type{<: Number}, neighborhood, mask)

    # Element-multiply the colony and the mask; sum must >= 1.0
    weighted_val = max(sum(mask .* neighborhood),1.0)
    # @debug "Weighted_val=$weighted_val"
    masksum = sum(mask)
    # @debug "masksum=$masksum"

    masksum/weighted_val
    # result will be in the range 0-(n * m)^2, where n,m are the dims of the mask
end

f()

"""
Returns a view of the mask for each cell in the colony array.
"""
function maskviews(colony::Array, mask)

    maskviews = Array{SubArray}(undef, size(colony))

    mask_depth_x = div(size(mask,2),2) + 1
    mask_depth_y = div(size(mask,2),1) + 1

    colonyysize, colonyxsize = size(colony)

    for i in eachindex(colony)
        mask_slice_y = UnitRange(mask_depth_y + minimum(slice_offset_y), mask_depth_y + maximum(slice_offset_y))
        mask_slice_x = UnitRange(mask_depth_x + minimum(slice_offset_x), mask_depth_x + maximum(slice_offset_x))
        maskviews[i] = view(mask, mask_slice_y, mask_slice_x)
    end
    return maskviews
end

function offsetviews(colony::Array, mask)

    offsets = Array{SubArray}(undef, size(colony))

    mask_depth_x = div(size(mask,2),2) + 1
    mask_depth_y = div(size(mask,2),1) + 1

    colonyysize, colonyxsize = size(colony)

    # rowlen = size(A,1)
    # collen = size(A,2)
    #
    # for i in eachindex(A)
    #     # for a 3x3 mask calculate indices for (i-1:i+1, i-rowlen:i+rowlen)
    #     # for a 5x5 mask, would be i-2;
    #     w = div(masksize, 2)
    #     r1 = i - w
    #     r2 = i + w
    #     c1 = i - w*rowlen
    #     c2 = i + w*rowlen
    #     rowrange = UnitRange(r1, r2)
    #     colrange = UnitRange(c1, c2)
    #     if !checkbounds(Bool, A, rowrange) || !checkbounds(Bool, A, colrange)
    #         # We are too close to the edge! Pull back! Pull back!
    #         @info "One or both of rows = $rowrange or cols = $colrange was out of bounds of A size $(size(A))"
    #         continue
    #     end
        # offsets[i] = view(A, rowrange, colrange)
    # end

    for co_x in axes(colony,2)
        for co_y in axes(colony,1)
            #   println("x=$co_x,y=$co_y")
            slice_offset_y = UnitRange(-min(co_y-1, mask_depth_y - 1), min(colonyysize - co_y, mask_depth_y - 1))
            #   println("slice_offset_y=$slice_offset_y type is $(typeof(slice_offset_y))")
            # mask_slice_y = UnitRange(mask_depth_y + minimum(slice_offset_y), mask_depth_y + maximum(slice_offset_y))
            #   println("mask_slice_y=$mask_slice_y type is $(typeof(mask_slice_y))")
            colony_slice_y = co_y + minimum(slice_offset_y):co_y+maximum(slice_offset_y)
            #   println("colony_slice_y=$colony_slice_y type is $(typeof(colony_slice_y))")

            slice_offset_x::UnitRange{Int} = -min(co_x-1,mask_depth_x-1):min(colonyxsize-co_x,mask_depth_x-1)
            #   println("slice_offset_x=$slice_offset_x type is $(typeof(slice_offset_x))")
            # mask_slice_x::UnitRange{Int} = mask_depth_x+minimum(slice_offset_x):mask_depth_x+maximum(slice_offset_x)
            #   println("mask_slice_x=$mask_slice_x type is $(typeof(mask_slice_x))")
            colony_slice_x::UnitRange{Int} = co_x+minimum(slice_offset_x):co_x+maximum(slice_offset_x)
            #   println("colony_slice_x=$colony_slice_x type is $(typeof(colony_slice_x))")

            # mask_slice = view(mask, mask_slice_y, mask_slice_x)
            #   println("mask_slice=$mask_slice")

            offsets[co_y,co_x] = view(colony, colony_slice_y, colony_slice_x)

        end
    end
    return offsets
end

function calculatenewcolony(colony::Array{T,3}, context::CommonwealthContext) where {T<:Number}
    mask = context.mask
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

            # @debug "neigh=$(neighborhood)"
            # @debug "maskmat=$(maskmat)"
            reduced_val = calcreducedval(ctype, neighborhood, maskmat)
            # @debug "reduced_val = $(reduced_val)"
            round(ctype, reduced_val, Base.RoundDown)

            if haskey(filter.d, reduced_val)
                newv, oldv = filter.d[reduced_val](colony[j,i,1])
            else
                newv, oldv = Filters.saf(colony[j,i,1])
            end
            new_colony[j,i,1] = newv
            new_colony[j,i,2] = oldv
        end
    end
    return new_colony
end

function save_image_info(img::Array, filedir, seed, mask, filter, repeater::Bool)
    colony_id = make_colony_id()
    colonyFileName = make_filename(colony_id, filedir)
    # img = Gray.(cw[:,:,1])
    @debug "size(img) = $(size(img))"
    save(colonyFileName, img)
    archive_image_db(db_filename(),colony_id,colonyFileName,seed,mask,filter,repeater)
    @info "Wrote file $colonyFileName successfully."
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

function idfromcolonyfilepath(filepath::AbstractString)
    basename = splitext(Base.Filesystem.basename(filepath))
    # println("basename=$basename")
    re = match(ID_REGEX,basename[1])
    # println("re=$re")
    return re[1]
    # join(split(basename[1],'-')[2:end])
end

function archiverowfromid(df::DataFrame, id::AbstractString)
    @from i in df begin
    @where get(i.id)==id
    @select {i.seed, i.mask, i.filter}
    @collect DataFrame
    end
end

function scanandredraw(srcdir::AbstractString, destdir::AbstractString, context::CommonwealthContext)
    sd = srcdir
    # println("sd=$sd")
    dd = destdir
    if !isdir(dd)
        mkpath(dd)
    end

    # open archive file using CSV
    # df = DataFrame()
    # df = CSV.read(db_filename(), DataFrame; delim='\t',header=DB_HEADERS,datarow=1)
    # df = CSV.read(db_filename(), header=["id","file","mask","filter","repeater"], delim='\t')
    df = CSV.read(db_filename(); types=[String, String, String, String, String, String], header=DB_HEADERS, datarow=2, delim=',')
    # get DataFrame for archive contents

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
            img = create_drawing_context(context.xsize,context.ysize,context.colonyxsize,context.colonyysize,context.scalef)
            context.img = img

            # convert mask and filter to objects
            s  = record[1:end][1]
            m = record[1:end][2]
            f = record[1:end][3]
            # println(typeof(s))
            # println(s[1])
            # println(get(s[1]))
            # println("mask =$(get(m[1]))")
            seed::ColonySeed = eval(parse(get(s[1])))
            println("Got seed $seed")
            context.seed = seed

            mask::Mask = maskfromstring(get(m[1]))
            println("mask=$mask")
            context.mask = mask

            filter::StateFilter = filterfromstring(get(f[1]))
            println("filter=$filter")
            context.filter = filter

            # call calculatecommonwealth
            repeater::Bool = calculatecommonwealth(context)
            save_image_info(img,dd,seed,mask,filter,repeater)
        else
            println("Failed to find record for $filename, id='$id'")
        end
    end
end

function generatemany(commonwealth_x,commonwealth_y,colony_x,colony_y)
    fc = 0
    mc = 0
    # Generate new directories for each run
    regfiledir = create_save_dir("regular")
    repeatfiledir = create_save_dir("repeat")
    # mask = the12p7weightedmask()
    for mask in generate_masks(1:4, 3)
        fc = 0

        for filter in generate_state_filters(true)
            if fc > MAX_FILTER_COUNT
                @info "Moving to the next mask..."
                break
            end
            fc+=1

            # Pick a seed at random
            seed = ColonySeed(rand(1:3))
            @debug "Seeding new CW with $(seed)"
            context = CommonwealthContext(commonwealth_x, commonwealth_y, colony_x, colony_y, mask, filter, seed)

            colonies, repeater = calculatecommonwealth(context)
            # iary = reshape(colonies, commonwealth_y, commonwealth_x)
            # @debug "iary has size $(size(iary))"
            img = Gray.(float32.(hvcat(commonwealth_x, colonies[1:end]...)))
            @debug "size(img) = $(size(img))"

            # drawcolonyoutline(img, (1,1), (size(colony,2), size(colony,1)))
            # if repeater && drawrepeat
            #     drawrepeatline(img, (0,0), (1,size(colony,1)))
            # end

            save_image_info(img, (repeater ? repeatfiledir : regfiledir), seed, mask, filter, repeater)
        end #next filter
    end# next mask
    @info "Done."
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
# MAIN CODE STARTS HERE
# main(Base.ARGS)
# generatemany(commonwealth_x,commonwealth_y,colony_x,colony_y,scale_factor)
generatemany(5,5,50,50)
# regeneratebest()
