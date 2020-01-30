using Cairo
using Combinatorics
using StaticArrays
using DataFrames
using CSV
using Query
using Random
using UUIDs

include("masks.jl")
include("filters.jl")

const COLONY_X = 31
const COLONY_Y = 31
const COMMONWEALTH_X = 8
const COMMONWEALTH_Y = 8
const SCALE_FACTOR = 3

# const MAX_MASK_COUNT = 20
const MAX_FILTER_COUNT = 20

const ID_REGEX = r"^\w+\-([\w\-]*)$"

const DB_FILE_NAME = "colony4j.csv"
const DB_HEADERS = String["id","file","seed","mask","filter","repeater"]

@enum ColonySeed blank=1 square=2 random=3

struct RepeatException <: Exception
    mask::Mask
    filter::StateFilter
end

struct NextPleaseException <: Exception
end

mutable struct CommonwealthContext
    xsize::Int
    ysize::Int
    colonyxsize::Int
    colonyysize::Int
    scalef::Int
    offsets::Array{Tuple{Int,Int}}
    mask::Union{Mask, Nothing}
    filter::Union{StateFilter, Nothing}
    drawrepeat::Bool
    seed::Union{ColonySeed, Nothing}
    cctx::Union{CairoContext, Nothing}
    stats::Array{Int,1}
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

spiral_offsets(xsize::Int, ysize::Int) = spiral_offsets(xsize,ysize,1,1)

function spiral_offsets(xsize::Int, ysize::Int, xoffset::Int, yoffset::Int)
    result=[]

    # start near the middle and spiral around!
    push!(result, ((xsize-1)/2) * xoffset, ((ysize-1)/2) * yoffset)

    c=[i for i=1:max(xsize,ysize)]
    xsign=true
    ysign=false
    for i in c
        # println("i=$i")
        # println("xsign=$xsign")
        # println("ysign=$ysign")
        # for each i we do a set for x and a set for y
    # set for x
       for w=1:i
           xsign ? push!(result,(COLONY_X,0)) : push!(result,(-COLONY_X,0))
       end
       xsign=!xsign
    # repeat for y
        for w=1:i
           if ysign
               push!(result,(0,COLONY_Y))
           else
               push!(result,(0,-COLONY_Y))
           end
       end
       ysign=!ysign
    end
    result
end

# xsize is number of colonies along horizontal axis
# xoffset is width in pixels of one colony
function normal_offsets(xsize::Int, ysize::Int, xoffset::Int, yoffset::Int)
    result=Tuple{Int,Int}[]
    for j=1:ysize-1
        for i=1:xsize-1
            push!(result,(xoffset,0))
        end
        push!(result,(-(xsize-1)*xoffset,yoffset))
    end
    for i=1:xsize-1
        push!(result,(xoffset,0))
    end
    # this last one is to ensure xsize*ysize elements in the array`
    push!(result,(0,0))
    # println("offsets are $result for xsize=$xsize, ysize=$ysize")
    result
end

function random_offsets(xsize::Int,ysize::Int,colonyxsize::Int,colonyysize::Int;nb::Int=xoffset/10)
    xo::Array{Int,1} = rand(0:xsize-1,nb)
    yo::Array{Int,1} = rand(0:ysize-1,nb)
    [(first(t) * colonyxsize, last(t) * colonyysize) for t in zip(xo,yo)]
end

# Returns a drawing context that has been initialized for the Commonwealth
function create_drawing_context(xsize::Int, ysize::Int, colonyxsize::Int, colonyysize::Int, scalef::Int)

    # Set up PNG painting surface
    crgb = CairoRGBSurface(xsize * colonyxsize * scalef, ysize * colonyysize * scalef)
    cctx = CairoContext(crgb)
    Cairo.scale(cctx,scalef,scalef)
    Cairo.save(cctx)
    set_source_rgb(cctx,1,1,1)
    rectangle(cctx,0,0,xsize * colonyxsize * scalef, colonyysize * colonyysize * scalef)
    fill(cctx)
    restore(cctx)
    cctx, crgb
end

function initialize_colony(xsize::Int, ysize::Int, seed::ColonySeed)
    sa = falses(ysize,xsize)
    if seed==blank
        va = ones(ysize,xsize)
    elseif seed==random
        va = rand(0:1,(ysize,xsize))
    else
        va = zeros(ysize,xsize)
    end

    c = reshape([(va[j,i], sa[j,i]) for i in 1:xsize for j in 1:ysize], (ysize,xsize))

    y = Integer(round(ysize/2))
    x = Integer(round(xsize/2))
    if seed != blank && seed != random && !isodd(ysize) && !isodd(xsize)
        # println(c[Int(xsize/2-1), Int(ysize/2-1)])
        # y,x
        c[y-1,x-1]=(1,false)
        c[y-1,x]=(1,false)
        c[y-1,x+1]=(1,false)
        c[y-1,x+2]=(1,false)
        c[y,x-1]=(1,false)
        c[y,x+2]=(1,false)
        c[y+1,x-1]=(1,false)
        c[y+1,x+2]=(1,false)
        c[y+2,x-1]=(1,false)
        c[y+2,x]=(1,false)
        c[y+2,x+1]=(1,false)
        c[y+2,x+2]=(1,false)


    elseif seed != blank && seed != random && (isodd(ysize) || isodd(xsize))
        y = Integer(round(ysize/2,Base.RoundUp))
        x = Integer(round(xsize/2,Base.RoundUp))

        c[y-1,x-1]=(1,false)
        c[y-1,x]=(1,false)
        c[y-1,x+1]=(1,false)
        c[y,x-1]=(1,false)
        c[y,x+1]=(1,false)
        c[y+1,x-1]=(1,false)
        c[y+1,x]=(1,false)
        c[y+1,x+1]=(1,false)
    end
    c
end

function get_rgb(val::Int, switched::Bool, repeater::Bool)
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

draw_colony(cctx::CairoContext, colony::Array{Tuple{Int,Bool},2}) = draw_colony(cctx, colony, false, false)

function drawrepeatline(cctx::CairoContext, startpt::Tuple{Int,Int}, endpt::Tuple{Int,Int}; color=(1,0,0))
    Cairo.save(cctx)
    set_source_rgb(cctx,color[1],color[2],color[3])
    rectangle(cctx,startpt[1],startpt[2],endpt[1],endpt[2])
    fill(cctx)
    Cairo.restore(cctx)
end

function drawcolonyoutline(cctx::CairoContext, tlp::Tuple{Int,Int}, brp::Tuple{Int,Int}; color=(0.1,0.9,0.1,0.3))
    Cairo.save(cctx)
    Cairo.set_line_width(cctx, 1.0);
    Cairo.set_source_rgba(cctx,color[1],color[2],color[3],color[4])
    Cairo.move_to(cctx,tlp[1],tlp[2])
    Cairo.line_to(cctx,brp[1],tlp[2])
    Cairo.line_to(cctx,brp[1],brp[2])
    Cairo.line_to(cctx,tlp[1],brp[2])
    Cairo.line_to(cctx,tlp[1],tlp[2])
    Cairo.close_path(cctx)
    Cairo.stroke(cctx)
    Cairo.restore(cctx)
end

function drawdistbars(context::CommonwealthContext)
    cctx = get(context.cctx)
    xlen = context.colonyxsize
    ylen = context.colonyysize
    stats = context.stats
    maxbarlen = Integer(round(ylen/3,RoundDown))-2
    # divide x space into 11 bars and 12 spaces
    xunit = Integer(round(xlen/23,RoundDown))
    # println("xunit=$xunit")
    statsum = sum(stats)
    # println("statsum=$statsum")

    Cairo.save(cctx)

    # draw light gray background rectangle
    Cairo.set_source_rgba(cctx,0.9,0.9,0.9,1.0)
    Cairo.rectangle(cctx,2,Integer(round(2*ylen/3,RoundUp)),xlen-4,maxbarlen+2)
    Cairo.fill(cctx)
    Cairo.set_source_rgba(cctx,1.0,0.0,0.0,1.0) #red
    Cairo.set_line_width(cctx, xunit);
    # base line
    ybase = ylen
    xbase = 2*xunit
    Cairo.move_to(cctx,xbase,ybase)
    for i in stats
        # println("i=$i")
        barlen = Integer(round(maxbarlen*i/statsum,RoundUp))
        # println("calc barlen=$barlen")
        # println("line will go to x=$xbase, y=$(ybase-barlen)")
        Cairo.line_to(cctx,xbase,ybase-barlen)
        xbase += 2*xunit
        Cairo.move_to(cctx,xbase,ybase)
        # println("after increment, cursor at $xbase,$ybase")
    end
    Cairo.close_path(cctx)
    Cairo.stroke(cctx)
    Cairo.restore(cctx)
end

function addtostats(context::CommonwealthContext, val::Int)
    stats = context.stats
    v = min(val, 10)
    stats[v+1] += 1
end

function draw_colony(cctx::CairoContext, colony::Array{Tuple{Int,Bool},2}, repeater::Bool, drawrepeat::Bool)
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

function calcreducedval(neighborhood::Array{Int,2}, rawmask::Mask, mask_slice::SubArray)
    mask = rawmask.m

    weighted_val::Int = sum(mask_slice .* neighborhood)
    # println("weighted_val=$weighted_val")
    # reduced_val::Int = round(sum(mask) / (max(weighted_val,1)),)
    # wsum::Int = sum(mask)
    wsum::Int = sum(mask_slice)
    # println("wsum=$wsum")
    reduced_val = Integer(round(wsum/max(weighted_val,1), Base.RoundDown))
end

function calculatenewcolony(colony::Array{Tuple{Int,Bool},2}, context::CommonwealthContext)
    rawmask = context.mask
    mask = rawmask.m

    filter = context.filter
    # println("mask=$mask")
    # mask::StaticArray = mask.m
    mask_depth_x = Integer(((size(mask,2)-1)/2)+1)
    # println("mask_depth_x=$mask_depth_x")
    mask_depth_y = Integer(((size(mask,1)-1)/2)+1)

    colonyysize, colonyxsize = size(colony)

    # create a blank target colony
    new_colony = initialize_colony(colonyxsize, colonyysize, blank)

    for co_y in axes(colony,1)
        for co_x in axes(colony,2)
            #   println("x=$co_x,y=$co_y")
            slice_offset_y = UnitRange(-min(co_y-1, mask_depth_y - 1), min(colonyysize - co_y, mask_depth_y - 1))
            #   println("slice_offset_y=$slice_offset_y type is $(typeof(slice_offset_y))")
            mask_slice_y = UnitRange(mask_depth_y + minimum(slice_offset_y), mask_depth_y + maximum(slice_offset_y))
            #   println("mask_slice_y=$mask_slice_y type is $(typeof(mask_slice_y))")
            colony_slice_y = co_y + minimum(slice_offset_y):co_y+maximum(slice_offset_y)
            #   println("colony_slice_y=$colony_slice_y type is $(typeof(colony_slice_y))")

            slice_offset_x::UnitRange{Int} = -min(co_x-1,mask_depth_x-1):min(colonyxsize-co_x,mask_depth_x-1)
            #   println("slice_offset_x=$slice_offset_x type is $(typeof(slice_offset_x))")
            mask_slice_x::UnitRange{Int} = mask_depth_x+minimum(slice_offset_x):mask_depth_x+maximum(slice_offset_x)
            #   println("mask_slice_x=$mask_slice_x type is $(typeof(mask_slice_x))")
            colony_slice_x::UnitRange{Int} = co_x+minimum(slice_offset_x):co_x+maximum(slice_offset_x)
            #   println("colony_slice_x=$colony_slice_x type is $(typeof(colony_slice_x))")

            mask_slice = view(mask, mask_slice_y, mask_slice_x)
            #   println("mask_slice=$mask_slice")

            neighborhood = [i[1] for i in view(colony, colony_slice_y, colony_slice_x)]
            # println("neighborhood=$neighborhood")
            reduced_val = calcreducedval(neighborhood,rawmask,mask_slice)

            addtostats(context,reduced_val)

            # reduced_val::Int = weighted_val % 10
            # println("reduced_val=$reduced_val")

            # Calculate the new cell value based on surrounding cells
            # weighting of the value of each neighboring cell; logical &&; some algorithm to transform cells;
            # use matrix math or embedded loops to calculate new center cell value
            if haskey(filter.d, reduced_val)
                new_val, switched = filter.d[reduced_val](colony[co_y,co_x][1])
            else
                new_val, switched = saf(colony[co_y,co_x][1])
            end
            new_colony[co_y,co_x] = (new_val,switched)
        end
    end
    return new_colony
end

function save_image_info(crgb::CairoSurface, filedir, seed, mask, filter, repeater::Bool)
    colony_id = make_colony_id()
    colonyFileName = make_filename(colony_id, filedir)
    write_to_png(crgb,colonyFileName)
    archive_image_db(db_filename(),colony_id,colonyFileName,seed,mask,filter,repeater)
    println("Wrote file $colonyFileName successfully.")
end

function ismember(colonies::Array, colony::Array{Tuple{Int,Bool},2})
    for i in eachindex(colonies)
        # println(colonies[i])
        isassigned(colonies, i) && colonies[i]==colony ? (return true) : nothing
    end
    return false
end

function calculatecommonwealth(context::CommonwealthContext)
    xsize = context.xsize
    ysize = context.ysize
    colonyxsize = context.colonyxsize
    colonyysize = context.colonyysize
    offsets = context.offsets
    mask = context.mask
    filter = context.filter
    cctx = context.cctx
    drawrepeat = context.drawrepeat
    seed = context.seed

    # println("Calculating commonwealth from $context")

    prev_colonies=Array{Array{Tuple{Int,Bool},2}}(undef,5)
    colony = initialize_colony(colonyxsize, colonyysize, seed)
    prev_colonies[5]=colony
    # @profile draw_colony(colony, cctx)
    draw_colony(cctx, colony)
    repeater = false
    # load the first colony into the history buffer, at the end
    # println("xsize=$xsize, ysize=$ysize, offsets=$(size(offsets))")
    for i in 0:(xsize*ysize)-1
        # if i > offset_len
        #     break
        # end
        new_colony = calculatenewcolony(colony, context)
        offset = offsets[i+1]
        Cairo.translate(cctx,first(offset),last(offset))
        repeater = ismember(prev_colonies, new_colony)
        draw_colony(cctx, new_colony, repeater, drawrepeat)
        # drawdistbars(context)
        repeater && drawrepeat ? drawrepeat=false : nothing
        prev_colonies[i%5+1]=new_colony
        colony = new_colony
    end #offsetsend

    return repeater
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
        println("filename=$filename")
        startswith(filename,'.') ? continue : nothing
        id = idfromcolonyfilepath(filename)
        println("searching for record with id='$id'")
        # search for filename in archive DataFrame
        record = archiverowfromid(df,id)
        # println(record)
        # if record found, get mask and filter
        if length(record) > 0
            # println("'$record'")
            cctx,crgb = create_drawing_context(context.xsize,context.ysize,context.colonyxsize,context.colonyysize,context.scalef)
            context.cctx = cctx

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
            save_image_info(crgb,dd,seed,mask,filter,repeater)
        else
            println("Failed to find record for $filename, id='$id'")
        end
    end
end

function createcontext(nbx::Int, nby::Int, colonyx::Int, colonyy::Int, scalef::Int, drawrepeat::Bool=false, seed::ColonySeed=blank)
    offsets = normal_offsets(nbx,nby,colonyx,colonyy)
    return CommonwealthContext(nbx,nby,colonyx,colonyy,scalef,offsets,nothing,nothing,drawrepeat,seed,nothing,zeros(11))
end

function generatemany()
    fc = 0
    mc = 0
    # Generate new directories for each run
    regfiledir = create_save_dir("regular")
    repeatfiledir = create_save_dir("repeat")
    # println("offsets are $offsets")
    # offsets = get_random_offsets((Int)(round(0.8*COMMONWEALTH_X*COMMONWEALTH_Y)))
    # offset_len = length(offsets)

    # mask = the12p7weightedmask()
    for mask in generate_masks()
        fc = 0

        for filter in generate_state_filters(true)
            if fc > MAX_FILTER_COUNT
                println("Moving to the next mask...")
                break
            end
            fc+=1

            cctx,crgb = create_drawing_context(COMMONWEALTH_X,COMMONWEALTH_Y,COLONY_X,COLONY_Y,SCALE_FACTOR)
            # println("Current filter=$filter")
            seed = ColonySeed(rand(1:3))
            context=createcontext(COMMONWEALTH_X,COMMONWEALTH_Y,COLONY_X,COLONY_Y,SCALE_FACTOR,true,seed)
# println("context = $context")
            context.cctx = cctx
            context.filter = filter
            context.mask = mask
            repeater::Bool = calculatecommonwealth(context)
            save_image_info(crgb, (repeater ? repeatfiledir : regfiledir), seed, mask, filter, repeater)
        end #next filter
    end# next mask
    println("Done.")
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
    drawrepline = true
    cctx,crgb = create_drawing_context(xsize, ysize, colonyxsize, colonyysize, scalef)
    context = createcontext(xsize,ysize,colonyxsize,colonyysize,scalef,drawrepline)
    context.cctx=cctx

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

    cctx,crgb = create_drawing_context(xsize, ysize, colonyxsize, colonyysize, scalef)
    offsets = normal_offsets(xsize,ysize,colonyxsize,colonyysize)

    context = CommonwealthContext(xsize,ysize,colonyxsize,colonyysize,scalef,offsets,mask,filter,drawrepeatline,seed,cctx)

    filedir = create_save_dir("$filter")
    # println("offsets are $offsets")
    # offsets = get_random_offsets((Int)(round(0.8*COMMONWEALTH_X*COMMONWEALTH_Y)))
    repeater::Bool = calculatecommonwealth(context)
    save_image_info(crgb, filedir, seed, mask, filter, repeater)
    println("Done.")
end
# MAIN CODE STARTS HERE
# main(Base.ARGS)
generatemany()
# regeneratebest()
