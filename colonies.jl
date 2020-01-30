using Graphics
using Cairo
# using Tk
using Combinatorics
# using Filesystem

# win = Tk.Toplevel("Test", 500, 500)
# c = Canvas(win)
# pack(c, expand=true, fill="both")
# ctx = getgc(c)
# set_coords(ctx, 100, 100, 400, 400, 0, 100, 100, 0)

COLONY_X = 100
COLONY_Y = 100
COMMONWEALTH_X = 10
COMMONWEALTH_Y = 1



# Returns a drawing context that has been initialized for the
# Commonwealth
function create_drawing_context(s::CairoSurface, x_pixels::Int64, y_pixels::Int64)
  # Set up PNG painting surface
  cctx = CairoContext(s)
  save(cctx)
  set_source_rgb(cctx,0.8,0.8,0.8)
  rectangle(cctx,0,0,x_pixels,y_pixels)
  fill(cctx)
  restore(cctx)
  save(cctx)
  cctx
end



ROT90=[0 -1;1 0]
ROT90_2=ROT90^2
ROT90_3=ROT90^3
MIRRV=[1 0;0 -1]
MIRRH=[-1 0;0 1]
MIRRXY=[-0.5 0.5; 0.5 -0.5]
MIRRX_Y=[-0.5 -0.5;-0.5 -0.5]

# always returns 1
of = function un(x) return 1; end
# always returns 0
zf = function nyet(x) return 0; end
# switches 1 to 0 or 0 to 1
swf = function changer(x) return 1-x; end
# returns the same value as given
saf = function meme(x) return x; end


function generate_state_filters(neighbors)
  patterns = []
  for i=1:size(neighbors)[1]
    for z in combinations(neighbors,i)
      b = setdiff(neighbors,z)
      for j=1:size(b)[1]
        for o in combinations(b,j)
          c = setdiff(b,o)
          for k=1:size(c)[1]
            for sa = combinations(c,k)
              sw = setdiff(c,sa)

              od = Dict{Int8,Function}(x=>of for x in o)
              zd = Dict{Int8,Function}(x=>zf for x in z)
              sad = Dict{Int8,Function}(x=>saf for x in sa)
              swd = Dict{Int8,Function}(x=>swf for x in sw)
              push!(patterns,merge(od,zd,sad,swd))
              return patterns
            end
          end
        end
      end
    end
  end
  patterns
end

# Pairs are all the possible permutations of coordinate offsets
# from the current, center pixel. e.g. "-1,0" means "current x - 1, current y"
# Returns an array of all possible permutations -2 to +2
# function generate_coord_pairs()
#   pairs = []
#   for p in permutations([-2 -1 0 1 2],2)
#     # println("p=$p")
#     push!(pairs,p)
#   end
#   push!(pairs,[-2,-2])
#   push!(pairs,[-1,-1])
#   push!(pairs,[0,0])
#   push!(pairs,[1,1])
#   push!(pairs,[2,2])
#   pairs
# end

function create_save_dir(i::Int)
  if !isdir("colonies4j-$i")
    mkdir("colonies4j-$i")
  end
end

# Returns false if the mask has already been used, either as-is, rotated or reflected
function ensure_unique_mask(maskDict, mask)
  if haskey(maskDict,mask) return false
  elseif haskey(maskDict,ROT90*mask) return false
  elseif haskey(maskDict,ROT90_2*mask) return false
  elseif haskey(maskDict,ROT90_3*mask) return false
  elseif haskey(maskDict,MIRRV*mask) return false
  elseif haskey(maskDict,MIRRH*mask) return false
  elseif haskey(maskDict,MIRRV*MIRRH*mask) return false
  end
  true
end



#Returns an iterator of weighted masks for use in evaluating the current Commonwealth
function generate_masks()
  masks = []
  pairs = generate_coord_pairs()
  s=size(pairs,1)
  println("Pairs has size $s")
  # for i=4:size(pairs,1)
  for i=4:5
    println("Creating sets of $i pairs...")
    create_save_dir(i)

    # this takes too long to calculate
    # consider using an iterator and generating a random number that must meet a threshold if the current iterator entry is to be accepted.
    for coordset in permutations(pairs,i)
      # println("With i=$i, coordset=$coordset");
      # inter = [j for j in coordset]
      # println("Inter = $inter")
      a = hcat([j for j in coordset]...) # create a nx2 matrix from the set of points in coordset
      # println("Base mask is $a")
      push!(masks,hcat(a,(MIRRV*MIRRH)*a)) # concatenate the mask with its horiz. and vertical reflection
      # println("Complete mask is $mask")
    end
  end
  masks
end

# Returns a 2-D array of weight values, one for each member of a neighborhood
function generate_weights()
  [1 2 2 2 2 2 1;
  1 2 4 4 4 2 1;
  1 2 4 8 4 2 1;
  1 2 4 16 4 2 1;
  1 2 4 8 4 2 1;
  1 2 4 4 4 2 1;
  1 2 2 2 2 2 1]
end

# MAIN CODE STARTS HERE

# initialize colony with random 0-1 (could also set to 0 except for seed values)
colony = rand([0,1],COLONY_X,COLONY_Y)
# println("Starting with colony $colony")
new_colony = Array{Int8,2}(COLONY_X,COLONY_Y)

weights = generate_weights()
weight_depth_x = (Int8)((size(weights)[2]-1)/2)+1
# println("weight_depth_x=$weight_depth_x")
weight_depth_y = (Int8)((size(weights)[1]-1)/2)+1
wsum = sum(weights)
# println("wsum=$wsum")

neighbor_sums = Int8[0,1,2,3,4,5,6,7,8,9]
filter_ary = generate_state_filters(neighbor_sums)
# println("Found $(size(keys(filter_ary[1]))) filters.")
# println("Got $filter_ary")


# just fake it for the first one
filter = filter_ary[1]
# println("filter is type $(typeof(filter))")
# Outer loop of the Commonwealth
# for cw = 1:totalCommonwealths

#
# drawing context
crgb = CairoRGBSurface(COMMONWEALTH_X * COLONY_X, COMMONWEALTH_Y * COLONY_Y)
println("crgb is $crgb")
cctx = create_drawing_context(crgb, COMMONWEALTH_X * COLONY_X, COMMONWEALTH_Y * COLONY_Y)

for i in 1:COMMONWEALTH_X
  for j in 1:COMMONWEALTH_Y
    # Each colony
      for co_y in indices(colony)[1]
        for co_x in indices(colony)[2]
          # println("$co_x,$co_y")
          weight_slice = weights[
          max(weight_depth_y-(co_y-1),1):min(weight_depth_y+(COLONY_Y-co_y),size(weights)[1]),
          max(weight_depth_x-(co_x-1),1):min(weight_depth_x+(COLONY_X-co_x),size(weights)[2])
          ]

          # println("weight_slice=$weight_slice")
          # create a slice the size of the weight mask
          # start_x = max(1,co_x-(weight_depth_x-1))
          # println("start_x=$start_x")
          # end_x = min(COLONY_X,co_x+(weight_depth_x-1))
          # println("end_x=$end_x")
          # start_y = max(1,co_y-(weight_depth_y-1))
          # end_y = min(COLONY_Y,co_y+(weight_depth_y-1))

          neighborhood = colony[
          max(weight_depth_y-(co_y-1),1):min(weight_depth_y+(COLONY_Y-co_y),size(weights)[1]),
          max(weight_depth_x-(co_x-1),1):min(weight_depth_x+(COLONY_X-co_x),size(weights)[2])
          ]

      #    start_x:end_x,start_y:end_y]

          # println("neighborhood=$neighborhood")

    # NW corner : if start_x < weight_center_x then weight_start_x=

          weighted_val = sum(weight_slice.*neighborhood)
          # println("weighted_val=$weighted_val")
          reduced_val = Int8(round(wsum / (abs(weighted_val)+1))) % 9
          # println("reduced_val=$reduced_val")

    # Calculate the new cell value based on surrounding cells
    # weighting of the value of each neighboring cell; logical &&; some algorithm to transform cells;
    # use matrix math or embedded loops to calculate new center cell value
          new_val = filter[reduced_val](colony[co_y,co_x])
          new_colony[co_y,co_x] = new_val
          set_source_rgb(cctx,new_val==0?0.1:0.9, new_val==0?0.1:0.9, new_val==0?0.1:0.9)
          rectangle(cctx,i*co_x,j*co_y,1,1)
          fill(cctx)
        end
      end
      # println("New colony is $new_colony")
      colony = new_colony
      new_colony = Array{Int8,2}(COLONY_X,COLONY_Y)
  end
end
save(cctx)
write_to_png(crgb,"1st_colony4j.png")
println("Wrote file successfully.")



# save new_colony as an image

  # do delta check against previous colony; break if delta==0
  # maybe generate n random cell pairs, compare current and previous on those pairs only. n is calculated to represent a low (< 1 percent??) of chance matches.
# end
# m = generate_masks()
# sz = size(m,1)
# println("Generated $sz masks")
