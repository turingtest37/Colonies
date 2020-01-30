using Combinatorics
using StaticArrays

const ROT90=@SMatrix [0 -1;1 0]
const ROT90_2=ROT90^2
const ROT90_3=ROT90^3
const MIRRV=@SMatrix [1 0;0 -1]
const MIRRH=@SMatrix [-1 0;0 1]
const MIRRXY=@SMatrix [-0.5 0.5; 0.5 -0.5]
const MIRRX_Y=@SMatrix [-0.5 -0.5;-0.5 -0.5]
const MASK_SIZE = (5,5)

MATCHING_PAIRS = [[i,i] for i=-2:2]

# Pairs are all the possible permutations of coordinate offsets
# from the current, center pixel. e.g. "-1,0" means "current x - 1, current y"
# This function returns an array of all possible permutations <-2,-2> to <2,2> as a nx2 matrix
function generate_coord_pairs()
    vcat(shuffle(RandomDevice(),collect(permutations([-2 -1 0 1 2],2))), MATCHING_PAIRS)    # MVector{0,SVector{2,Int64}}()
end


function savemask(maskDict::Dict, mask::Array{Int64,2})
    maskDict[mask]=true
    maskDict[ROT90*mask]=true
    maskDict[ROT90_2*mask]=true
    maskDict[ROT90_3*mask]=true
    maskDict[MIRRV*mask]=true
    maskDict[MIRRH*mask]=true
    maskDict[MIRRV*MIRRH*mask]=true
end
# Returns false if the mask has already been used, either as-is, rotated or reflected
function ensure_unique_mask(maskDict::Dict, mask::Array{Int64,2})
  !haskey(maskDict,mask)
end

immutable MaskIter{T,D}
    i::T
    d::D
end

immutable Mask{T}
    m::T
end

Base.show(io::IO,mask::Mask)=print(io,mask.m)

function Base.start(iter::MaskIter)
    Base.start(iter.i)
end

function Base.done(iter::MaskIter, state::Any)
    Base.done(iter.i, state)
end

function completemask(pairs::Array)
    #convert the current permutation of 4 points to a StaticVector for speed
    v = SVector{4}(pairs)
    # convert to a 6 x 2 array
    a = hcat(v...) # create a nx2 matrix from the set of points in coordset
    # println("a=$a")
    # mirror the points across horiz and vertical axes to create the full mask
    b=hcat(a,MIRRH*a)
    # println("b=$b")
    c=hcat(b,MIRRV*b)
    # println("c=$c")
    # b=hcat(a,(MIRRV*MIRRH)*a)
    c
end

function Base.next(iter::MaskIter, state::Any)
    # obtain the next permutation of points (coordinates)
    (pairs,s) = Base.next(iter.i,state)
    m = completemask(pairs)
    while !ensure_unique_mask(iter.d,m)
        if !Base.done(iter,s)
            (pairs,s) = Base.next(iter.i,s)
        else
            break
        end
        m = completemask(pairs)
    end
    savemask(iter.d,m)

    middle::Int = round(first(MASK_SIZE)/2,RoundUp)
    # start with a blank slate
    z = zeros(Complex{Int8},first(MASK_SIZE),last(MASK_SIZE))
    for i=1:size(m,2)
        # the next line assigns a complex number to each spot in the zero matrix designated by the coordinates
        # the real portion is coordinate x, imaginary is y
        z[middle+m[1,i],middle+m[2,i]]=complex(m[1,i], m[2,i])
    end
    weights=generate_weights(0:1)
    # println("Weights = $weights")

    (Mask(SMatrix{5,5}(weights .* z)),s)
end

function maskfromstring(s::AbstractString)
    a::AbstractArray = eval(parse(s))
    if size(a,1) == 5 && size(a,2) == 5
        return Mask(SMatrix{5,5}(a))
    elseif size(a,1) == 7 && size(a,2) == 7
        return Mask(SMatrix{7,7}(a))
    end
    nothing
end

Base.length(iter::MaskIter) = Base.length(iter.i)
Base.eltype(::Type{MaskIter}) = Mask

# Returns an iterator of weighted masks for use in evaluating the current Commonwealth
function generate_masks()
    pairs = generate_coord_pairs()
    MaskIter(permutations(pairs,4),Dict{Array{Int64,2},Bool}())
end

# Returns a 2-D array of weight values, one for each member of a neighborhood
function generate_weights(r::Range)
    r = rand(r,9)
    a=r[1]
    b=r[2]
    c=r[3]
    d=r[4]
    e=r[5]
    f=r[6]
    g=r[7]
    h=r[8]
    i=1
    @SMatrix [a b c b a;
    d e f e d;
    g h i h g;
    d e f e d;
    a b c b a]
  # @SMatrix [1 1 1 1 1;
            # 1 1 2 1 1;
            # 1 2 5 2 1;
            # 1 1 2 1 1;
            # 1 1 1 1 1]
end

function the12p7weightedmask()::Mask
    Mask(SMatrix{7,7}([1 2 2 2 2 2 1;1 2 4 4 4 2 1;1 2 4 8 4 2 1;1 2 4 16 4 2 1;1 2 4 8 4 2 1;1 2 4 8 4 2 1;1 2 2 2 2 2 1]))
end

function test_masks()
    i=0
    for m in generate_masks()
        println("m=$m")
        i+=1
        i>10 ? break : nothing
    end
    println("Counted $i unique masks.")
end
# test_masks()
