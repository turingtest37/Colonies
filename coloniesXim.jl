using Cairo
using Combinatorics
using StaticArrays
using DataFrames
using CSV
using Query
using FNVHash

include("masks.jl")
include("filtersIm.jl")
include("convert.jl")

const COLONY_X = 25
const COLONY_Y = 25
const COMMONWEALTH_X = 10
const COMMONWEALTH_Y = 10
const SCALE_FACTOR = 3

# const MAX_MASK_COUNT = 20
const MAX_FILTER_COUNT = 20

const ID_REGEX = r"^\w+\-([\w\-]*)$"

const DB_FILE_NAME = "colony4j.csv"
const DB_HEADERS = String["id","file","seed","mask","filter","repeater","repeatidx"]

@enum ColonySeed blank=1 square=2 random=3

immutable RepeatException <: Exception
    mask::Mask
    filter::StateFilter
end

type NextPleaseException <: Exception
end

mutable struct CommonwealthContext
    xsize::Int64
    ysize::Int64
    colonyxsize::Int64
    colonyysize::Int64
    scalef::Int64
    offsets::Array{Tuple{Int64,Int64}}
    mask::Nullable{Mask}
    filter::Nullable{StateFilter}
    drawrepeat::Bool
    seed::Nullable{ColonySeed}
    cctx::Nullable{CairoContext}
    stats::Array{Int64,1}
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

function archive_image_db(dbf::AbstractString, colonyid::Base.Random.UUID, colonypath::AbstractString, seed::ColonySeed, mask::Mask, filter::StateFilter, repeater::Bool, repeatidx::Int64)
    df = DataFrame(id=string(colonyid), name=colonypath, seed="$seed", mask="$mask", filter="$filter", repeater="$repeater",repeatidx="$repeatidx")
    CSV.write(dbf, df; delim=',', header=true, colnames=DB_HEADERS, append=true)
end

function make_colony_id()
    Base.Random.uuid1()
end

function make_filename(id, filedir)
    joinpath(filedir,"colony4j-$id.png")
end

function make_filename(filedir)
    make_filename(filedir,rand(1:1000000))
end

function spiral_offsets(xsize::Int64,ysize::Int64)
    spiral_offsets(xsize,ysize,1,1)
end

function spiral_offsets(xsize::Int64,ysize::Int64,xoffset::Int64,yoffset::Int64)
    result=[]

    # start near the middle and spiral around!
    push!(result,((xsize-1)/2)*xoffset,((ysize-1)/2)*yoffset)

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
function normal_offsets(xsize::Int64, ysize::Int64, xoffset::Int64, yoffset::Int64)
    result=Tuple{Int64,Int64}[]
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

function random_offsets(xsize::Int64,ysize::Int64,colonyxsize::Int64,colonyysize::Int64;nb::Int64=xoffset/10)
    xo::Array{Int64,1} = rand(0:xsize-1,nb)
    yo::Array{Int64,1} = rand(0:ysize-1,nb)
    [(first(t)*colonyxsize,last(t)*colonyysize) for t in zip(xo,yo)]
end

# Returns a drawing context that has been initialized for the Commonwealth
function create_drawing_context(xsize::Int64, ysize::Int64, colonyxsize::Int64, colonyysize::Int64, scalef::Int64)

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

function initialize_colony(xsize::Integer, ysize::Integer, seed::ColonySeed)
    sa = falses(ysize,xsize)
    if seed==blank
        va = ones(ysize,xsize)
    elseif seed==random
        va = [complex(rand(-1:1),rand(-1:1)) for j=1:ysize, i=1:xsize]
    else
        va = zeros(ysize,xsize)
    end

    c::Array{Tuple{Complex{Int64},Bool}} = reshape([(va[j,i], sa[j,i]) for i in 1:xsize for j in 1:ysize], (ysize,xsize))

    y::Int = Integer(round(ysize/2))
    x::Int = Integer(round(xsize/2))
    if seed == square && !isodd(ysize) && !isodd(xsize)
        # println(c[Int64(xsize/2-1), Int64(ysize/2-1)])
        # y,x
        c[y-1,x-1]=(complex(-1,-1),false)
        c[y-1,x]=(complex(0,-1),false)
        c[y-1,x+1]=(complex(1,-1),false)
        c[y-1,x+2]=(complex(2,-1),false)
        c[y,x-1]=(complex(-1,0),false)
        c[y,x+2]=(complex(2,0),false)
        c[y+1,x-1]=(complex(-1,1),false)
        c[y+1,x+2]=(complex(2,1),false)
        c[y+2,x-1]=(complex(-1,2),false)
        c[y+2,x]=(complex(0,2),false)
        c[y+2,x+1]=(complex(1,2),false)
        c[y+2,x+2]=(complex(2,2),false)


    elseif seed == square && (isodd(ysize) || isodd(xsize))
        y = Integer(round(ysize/2,Base.RoundUp))
        x = Integer(round(xsize/2,Base.RoundUp))

        c[y-1,x-1]=(complex(-1,-1),false)
        c[y-1,x]=(complex(0,-1),false)
        c[y-1,x+1]=(complex(1,-1),false)
        c[y,x-1]=(complex(-1,0),false)
        c[y,x+1]=(complex(1,0),false)
        c[y+1,x-1]=(complex(-1,1),false)
        c[y+1,x]=(complex(0,1),false)
        c[y+1,x+1]=(complex(1,0),false)
    end
    c
end

function get_rgb(val::Complex{Int64}, switched::Bool, repeatidx::Int64)
    # val*0.09 + (switched ? 0.2 : 0),val*0.09 + (switched ? 0 : 0.2),val*0.1
    # val/2,val/5,(switched ? 0.2 : val/3)
    aa = angle(val)
    a = (aa >= 0 ? aa/2pi : (aa+2pi)/2pi)

    if repeatidx > 0
        # switched=black (0) or light gray (1)=0.76 ; else same=med gray(0)=0.4 or light gray(1)=0.6
        # switched ? (val/1.3,val/1.3,val/1.3) : ((val+2)/5, (val+2)/5, (val+2)/5)
        # switched=black (0) or med gray (1)=0.4 ; else same=light gray(0)=0.6 or very light gray(1)=0.8
        switched ? (a,a,a) : (a*.90,a*.90,a*.90)
    else
        # switched=black (0) or very light gray (1)=0.90 ; else same=dark gray(0)=0.2 or med gray(1)=0.4
        switched ? (a*.80,a*.80,a*.80) : (a*.70,a*.70,a*.70)
    end
end

function drawrepeatline(cctx::CairoContext, startpt::Tuple{Int64,Int64}, endpt::Tuple{Int64,Int64}; color=(1,0,0))
    Cairo.save(cctx)
    set_source_rgb(cctx,color[1],color[2],color[3])
    rectangle(cctx,startpt[1],startpt[2],endpt[1],endpt[2])
    fill(cctx)
    Cairo.restore(cctx)
end

function drawrepeatidx(cctx::CairoContext, repeatidx::Int64, startpt::Tuple{Int64,Int64}; color=(1,0,0))
    Cairo.save(cctx)
    Cairo.set_source_rgb(cctx,color[1],color[2],color[3])
    Cairo.move_to(cctx,startpt[1],startpt[2])
    Cairo.set_font_size(cctx,10)
    Cairo.show_text(cctx,"$repeatidx")
    Cairo.restore(cctx)
end

function drawcolonyoutline(cctx::CairoContext, tlp::Tuple{Int64,Int64}, brp::Tuple{Int64,Int64}; color=(0.1,0.9,0.1,0.3))
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

function addtostats(context::CommonwealthContext, val::Int64)
    stats = context.stats
    v = val < 10 ? val : 10
    stats[v+1] += 1
end

function draw_colony(colony::Array{Tuple{Complex{Int64},Bool},2}, cctx::CairoContext)
    draw_colony(colony, 0, false, cctx)
end

function draw_colony(colony::Array{Tuple{Complex{Int64},Bool},2}, repeatidx::Int64, drawrepeat::Bool, cctx::CairoContext)
    # println("Drawing colony $(size(colony))")
    co_x=0
    co_y=0
    for co_y in indices(colony,1)
      for co_x in indices(colony,2)
          val::Complex{Int64}, switched::Bool = colony[co_y,co_x]
          r::Float64,g::Float64,b::Float64 = get_rgb(val,switched,repeatidx);
          set_source_rgb(cctx,r,g,b)
          rectangle(cctx,co_x-1,co_y-1,1,1)
          fill(cctx)
      end
    end
    drawcolonyoutline(cctx,(1,1),(size(colony,2),size(colony,1)))
    if repeatidx>0 && drawrepeat
        drawrepeatline(cctx,(0,0),(1,size(colony,1)))
        drawrepeatidx(cctx,repeatidx,(5,15))
    end
end

function pickclosestpt(v::Complex{Int64})
    aa = angle(v)
    a = (aa >= 0 ? aa : aa + 2pi)
    a <= pi/8 || a > 15pi/8 ? complex(1,0) : a <= 3pi/8 ? complex(1,1) : a <= 5pi/8 ? complex(0,1) : a <= 7pi/8 ? complex(-1,1) : a <= 9pi/8 ? complex(-1,0) : a <= 11pi/8 ? complex(-1,-1) : a <= 13pi/8 ? complex(0,-1) : complex(1,-1)
end

function calcreducedval(neighborhood::Array{Complex{Int64},2}, rawmask::Mask, mask_slice::SubArray)
    # mask = rawmask.m
    weightedsum = sum(mask_slice .* neighborhood)/sum(mask_slice)
    weighted_val::Complex{Int64} = complex(round(real(weightedsum), RoundUp), round(imag(weightedsum), RoundUp))
    # weighted_val::Complex{Int64} = sum(mask_slice.*neighborhood)
    # println("weighted_val=$weighted_val")
    # reduced_val::Int64 = round(sum(mask) / (max(weighted_val,1)),)
    # wsum::Int64 = sum(mask)
    # wsum::Int64 = sum(mask_slice)
# println("wsum=$wsum")
    reduced_val::Complex{Int64} = pickclosestpt(weighted_val)

end

function calculatenewcolony(colony::Array{Tuple{Complex{Int64},Bool},2}, context::CommonwealthContext)
    rawmask::Mask = get(context.mask)
    mask = rawmask.m

    filter::StateFilter = get(context.filter)
    # println("mask=$mask")
    # mask::StaticArray = mask.m
    mask_depth_x::Int64 = round(size(mask,2)/2,RoundUp)
    # println("mask_depth_x=$mask_depth_x")
    mask_depth_y::Int64 = round(size(mask,1)/2,RoundUp)

    (colonyysize::Int64, colonyxsize::Int64) = size(colony)

    # create a blank target colony
    new_colony::Array{Tuple{Complex{Int64},Bool},2} = initialize_colony(colonyxsize, colonyysize, blank)

    for co_y::Int64 in indices(colony,1)
        for co_x::Int64 in indices(colony,2)
            #   println("x=$co_x,y=$co_y")
            slice_offset_y::UnitRange{Int64} = -min(co_y-1,mask_depth_y-1):min(colonyysize-co_y,mask_depth_y-1)
            #   println("slice_offset_y=$slice_offset_y type is $(typeof(slice_offset_y))")
            mask_slice_y::UnitRange{Int64} = mask_depth_y+minimum(slice_offset_y):mask_depth_y+maximum(slice_offset_y)
            #   println("mask_slice_y=$mask_slice_y type is $(typeof(mask_slice_y))")
            colony_slice_y::UnitRange{Int64} = co_y+minimum(slice_offset_y):co_y+maximum(slice_offset_y)
            #   println("colony_slice_y=$colony_slice_y type is $(typeof(colony_slice_y))")

            slice_offset_x::UnitRange{Int64} = -min(co_x-1,mask_depth_x-1):min(colonyxsize-co_x,mask_depth_x-1)
            #   println("slice_offset_x=$slice_offset_x type is $(typeof(slice_offset_x))")
            mask_slice_x::UnitRange{Int64} = mask_depth_x+minimum(slice_offset_x):mask_depth_x+maximum(slice_offset_x)
            #   println("mask_slice_x=$mask_slice_x type is $(typeof(mask_slice_x))")
            colony_slice_x::UnitRange{Int64} = co_x+minimum(slice_offset_x):co_x+maximum(slice_offset_x)
            #   println("colony_slice_x=$colony_slice_x type is $(typeof(colony_slice_x))")

            mask_slice = view(mask, mask_slice_y, mask_slice_x)
            #   println("mask_slice=$mask_slice")

            neighborhood::Array{Complex{Int64},2} = [i[1] for i in view(colony, colony_slice_y, colony_slice_x)]
            # println("neighborhood=$neighborhood")
            reduced_val::Complex{Int64} = calcreducedval(neighborhood,rawmask,mask_slice)

            # addtostats(context,reduced_val)

            # reduced_val::Int64 = weighted_val % 10
            # println("reduced_val=$reduced_val")

            # Calculate the new cell value based on surrounding cells
            # weighting of the value of each neighboring cell; logical &&; some algorithm to transform cells;
            # use matrix math or embedded loops to calculate new center cell value
            if haskey(filter.d,reduced_val)
                new_val,switched = filter.d[reduced_val](colony[co_y,co_x][1])
            else
                new_val,switched = saf(colony[co_y,co_x][1])
            end
            new_colony[co_y,co_x] = (new_val,switched)
        end
    end
    new_colony
end

function save_image_info(crgb::CairoSurface, filedir, seed, mask, filter, repeater::Bool, repeatidx::Int64)
    colony_id = make_colony_id()
    colonyFileName = make_filename(colony_id, filedir)
    write_to_png(crgb,colonyFileName)
    archive_image_db(db_filename(),colony_id,colonyFileName,seed,mask,filter,repeater,repeatidx)
    println("Wrote file $colonyFileName successfully.")
end

function ismember(colonies::Array, colony::Array{Tuple{Complex{Int64},Bool},2}, currentidx::Int64)::Int64
    dist = 0
    for i in eachindex(colonies)
        if isassigned(colonies, i) && colonies[i]==colony
            if currentidx > i
                dist = currentidx-i
            else
                dist = 10+currentidx-i
            end
            # println("match with i=$i, currentidx=$currentidx")
            break
        end
    end
    return dist
end

function addtoprevious(dict::Dict{Array{Tuple{Complex{Int64},Bool},2},Int64},colony::Array{Tuple{Complex{Int64},Bool},2},index::Int64)
    # a::Array{UInt8} = convert(Array{UInt8},colony)
    # println("a has length $(size(a))")
    # hash = fnv32(a)

    v = get!(dict,colony,index)
    # println("Got entry $v for $hash and index=$index")
    index-v
end

function calculatecommonwealth(context::CommonwealthContext)::Int64
    xsize::Int64 = context.xsize
    ysize::Int64 = context.ysize
    colonyxsize::Int64 = context.colonyxsize
    colonyysize::Int64 = context.colonyysize
    offsets::Array{Tuple{Int64,Int64}} = context.offsets
    mask::Mask = get(context.mask)
    filter::StateFilter = get(context.filter)
    cctx::CairoContext = get(context.cctx)
    drawrepeat::Bool = context.drawrepeat
    seed::ColonySeed = get(context.seed)

    # println("Calculating commonwealth from $context")

    # this is a dictionary (hash) of previously calculated
    prev_colonies = Dict{Array{Tuple{Complex{Int64},Bool},2},Int64}()

    colony = initialize_colony(colonyxsize, colonyysize, seed)

    addtoprevious(prev_colonies,colony,1)
    # @profile draw_colony(colony, cctx)
    draw_colony(colony, cctx)
    repeatidx = 0
    # load the first colony into the history buffer, at the end
    for i in 2:(xsize*ysize)+1
        # if i > offset_len
        #     break
        # end
        new_colony = calculatenewcolony(colony,context)
        offset = offsets[i-1]
        Cairo.translate(cctx,first(offset),last(offset))
        if drawrepeat
            # repeat index = i if the colony is new; otherwise repeatidx < i
            repeatidx = addtoprevious(prev_colonies,new_colony,i)
        end
        draw_colony(new_colony, repeatidx, drawrepeat, cctx)
        # drawdistbars(context)
        # Detect if colony is a repeat by comparing it to current i index
        repeatidx > 0 && drawrepeat ? drawrepeat=false : nothing
        colony = new_colony
    end #offsetsend
    repeatidx
end

function idfromcolonyfilepath(filepath::AbstractString)
    basename = splitext(Base.Filesystem.basename(filepath))
    # println("basename=$basename")
    re = match(ID_REGEX,basename[1])
    # println("re=$re")
    re[1]
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
            repeatidx::Int64 = calculatecommonwealth(context)
            save_image_info(crgb,dd,seed,mask,filter,repeatidx>0,repeatidx)
        else
            println("Failed to find record for $filename, id='$id'")
        end
    end
end

function createcontext(nbx::Int64, nby::Int64, colonyx::Int64, colonyy::Int64, scalef::Int64, drawrepeat::Bool=false, seed::ColonySeed=blank)::CommonwealthContext

    offsets = normal_offsets(nbx,nby,colonyx,colonyy)
    CommonwealthContext(nbx,nby,colonyx,colonyy,scalef,offsets,Nullable{Mask}(),Nullable{StateFilter}(),drawrepeat,seed,Nullable{CairoContext}(),zeros(11))
end

function generatemany()
    fc = 0
    mc = 0
    # Generate new directories for each run
    regfiledir = create_save_dir("regular")
    repeatfiledir = create_save_dir("repeat")
    # println("offsets are $offsets")
    # offsets = get_random_offsets((Int64)(round(0.8*COMMONWEALTH_X*COMMONWEALTH_Y)))
    # offset_len = length(offsets)

    # mask = the12p7weightedmask()
    for mask::Mask in generate_masks()
        fc = 0

        for filter in generate_state_filters(true)
            if fc > MAX_FILTER_COUNT
                println("Moving to the next mask...")
                break
            end
            fc+=1

            cctx,crgb = create_drawing_context(COMMONWEALTH_X,COMMONWEALTH_Y,COLONY_X,COLONY_Y,SCALE_FACTOR)
            # println("Current filter=$filter")
            # seed::ColonySeed = rand(1:3)
            seed::ColonySeed = rand(1:2)
            println("Generating image from seed $seed...")
            context=createcontext(COMMONWEALTH_X,COMMONWEALTH_Y,COLONY_X,COLONY_Y,SCALE_FACTOR,true,seed)
# println("context = $context")
            context.cctx = cctx
            context.filter = filter
            context.mask = mask
            repeatidx::Int64 = calculatecommonwealth(context)
            println(repeatidx > 0 ? "Repeat." : "Regular.")
            save_image_info(crgb, (repeatidx>0 ? repeatfiledir : regfiledir), seed, mask, filter, repeatidx>0, repeatidx)
        end #next filter
    end# next mask
    println("Done.")
end

function regeneratebest()
    srcdir = "img/best"
    destdir = "img/giant"
    xsize=25
    ysize=16
    colonyxsize = 101
    colonyysize = 101
    scalef = 2
    mask = Nullable{Mask}()
    filter = Nullable{StateFilter}()
    seed = Nullable{ColonySeed}()
    drawrepline = true
    cctx,crgb = create_drawing_context(xsize, ysize, colonyxsize, colonyysize, scalef)
    context = createcontext(xsize,ysize,colonyxsize,colonyysize,scalef,drawrepline)
    context.cctx=cctx

    scanandredraw(srcdir::AbstractString, destdir::AbstractString, context::CommonwealthContext)
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
    xsize::Int64 = eval(parse(args[3]))
    ysize::Int64 = eval(parse(args[4]))
    colonyxsize::Int64 = eval(parse(args[5]))
    colonyysize::Int64 = eval(parse(args[6]))
    scalef::Int64 = eval(parse(args[7]))
    seed::ColonySeed = eval(parse(args[8]))
    drawrepeatline::Bool = eval(parse(args[9]))

    cctx,crgb = create_drawing_context(xsize, ysize, colonyxsize, colonyysize, scalef)
    offsets = normal_offsets(xsize,ysize,colonyxsize,colonyysize)

    context = CommonwealthContext(xsize,ysize,colonyxsize,colonyysize,scalef,offsets,mask,filter,drawrepeatline,seed,cctx)

    filedir = create_save_dir("$filter")
    # println("offsets are $offsets")
    # offsets = get_random_offsets((Int64)(round(0.8*COMMONWEALTH_X*COMMONWEALTH_Y)))
    repeatidx::Int64 = calculatecommonwealth(context)
    save_image_info(crgb, filedir, seed, mask, filter, repeatidx>0, repeatidx)
    println("Done.")
end
# MAIN CODE STARTS HERE
# main(Base.ARGS)
generatemany()
# regeneratebest()
