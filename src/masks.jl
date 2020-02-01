using Combinatorics
using StaticArrays
using Random

const ROT90=@SMatrix [0 -1;1 0]
const ROT90_2=ROT90^2
const ROT90_3=ROT90^3
const MIRRV=@SMatrix [1 0;0 -1]
const MIRRH=@SMatrix [-1 0;0 1]
const MIRRXY=@SMatrix [-0.5 0.5; 0.5 -0.5]
const MIRRX_Y=@SMatrix [-0.5 -0.5;-0.5 -0.5]

rng = Random.RandomDevice()

mutable struct MaskIter{T,TT}
    i::T
    d::TT
    wr::UnitRange
    dim::Int
end

struct Mask{T}
    m::T
end

# Pairs are all the possible permutations of coordinate offsets
# from the current, center pixel. e.g. "-1,0" means "current x-1, current y"
# Returns an array of all possible permutations -2 to +2
function generate_coord_pairs(dim::Int = 5)
    n = div(dim, 2)
    matching_pairs = [Int[i,i] for i = -n:n]
    non_matching_pairs = collect(permutations(-n:n, 2))
    pairs = vcat(matching_pairs, non_matching_pairs)
    shuffle(rng, pairs)
end


function savemask!(maskDict::Dict, mask::Array{Int,2})
    maskDict[mask]=true
    maskDict[ROT90*mask]=true
    maskDict[ROT90_2*mask]=true
    maskDict[ROT90_3*mask]=true
    maskDict[MIRRV*mask]=true
    maskDict[MIRRH*mask]=true
    maskDict[MIRRV*MIRRH*mask]=true
end

# Returns false if the mask has already been used, either as-is, rotated or reflected
ensure_unique_mask(maskDict::Dict, mask::Array{Int,2}) = !haskey(maskDict,mask)

Base.show(io::IO, mask::Mask) = print(io, mask.m)

function Base.iterate(iter::MaskIter)
    # obtain the next permutation of points (coordinates)
    t = Base.iterate(iter.i)
    t != nothing || return nothing
    pairs, s = t[1], t[2]
    m = completemask(pairs)
    while !ensure_unique_mask(iter.d, m)
        t = Base.iterate(iter.i, s)
        t != nothing || return nothing
        pairs, s = t[1], t[2]
        m = completemask(pairs)
    end
    savemask!(iter.d, m)
    return buildweightedmask(m, iter.wr, iter.dim), s
end

function completemask(pairs)
    @debug "pairs=$(pairs)"# println("pairs = $pairs")
    #convert the current permutation of 4 points to a StaticVector for speed
    v = SVector{4}(pairs)

    # convert to a 6 x 2 array
    # create a nx2 matrix from the set of points in coordset
    a = hcat(v...)
    @debug "a=$(a)"

    # mirror the points across horiz and vertical axes to create the full mask
    b=hcat(a, MIRRH * a)
    @debug "b=$(b)"

    c=hcat(b, MIRRV * b)
    @debug "c=$(c)"

    #unique columns only
    return unique(c, dims=2)
end

function Base.iterate(iter::MaskIter, s::Any)
    # obtain the next permutation of points (coordinates)
    unique = false
    m = nothing
    while !unique
        t = Base.iterate(iter.i, s)
        t != nothing || return nothing
        pairs, s = t[1], t[2]
        m = completemask(pairs)
        unique = ensure_unique_mask(iter.d, m)
    end
    savemask!(iter.d, m)
    return buildweightedmask(m, iter.wr, iter.dim), s
end

function buildweightedmask(m::Array, weightrange::UnitRange, dim::Int=5)
    isodd(dim) || raise(ArgumentError("dim must be odd. (default=5)"))

    @debug("Array m = $(m)")
    middle = div(dim,2) + 1
    # start with a blank slate
    z = zeros(Int8, dim, dim)
    for i=1:size(m,2)
        # the next line assigns a 1 to each spot in the zero matrix designated by the coordinates
        z[middle+m[1,i], middle+m[2,i]] = 1
    end
    weights = generate_weights(weightrange, dim)
    # println("Weights = $weights")

    return Mask(SMatrix{eval(dim),eval(dim)}(weights .* z))
end

""" Creates a square mask from the given string that can be parsed into
a square matrix.
"""
function maskfromstring(s::AbstractString)
    a::AbstractArray = eval(Meta.parse(s))
    n = size(a,1)
    # assuming a square matrix
    return Mask(SMatrix{eval(n),eval(n)}(a))
end

Base.length(iter::MaskIter) = Base.length(iter.i)
Base.eltype(::Type{MaskIter}) = Mask

"""Returns an iterator of square, weighted masks.
"""
function generate_masks(weightrange::UnitRange, dim::Int = 5)
    @info "Generating masks..."
    pairs = generate_coord_pairs(dim)
    return MaskIter(permutations(pairs,4), Dict{Array{Int,2},Bool}(), weightrange, dim)
end

# Returns a 2-D array of weight values, one for each member of a neighborhood
function generate_weights(weightrange::UnitRange{Int}, dim::Int)

    rweights = rand(rng, weightrange, dim * dim)
    @debug "rweights=$(rweights)"

    weightsets = Dict{Int,Tuple}([
    3 => ([Symbol(a) for a in 'a':'d'],
            Meta.parse("[a b a;
                        c d c
                        a b a]")),
    5 => ([Symbol(a) for a in 'a':'h'],
            Meta.parse("[
                        a b c b a;
                        d e f e d;
                        g h i h g;
                        d e f e d;
                        a b c b a]")),
    7 => ([Symbol(a) for a in 'a':'p'],
    Meta.parse("[
                a b c d c b a;
                e f g h g f e;
                i j k l k j i;
                m n o p o n m;
                i j k l k j i;
                e f g h g f e;
                a b c d c b a]"))])
    #build and populate the matrix
    @debug "dict = $(weightsets)"

    mset = weightsets[dim]
    @debug "mset=$(mset)"
    vars = mset[1]
    @debug "vars=$(vars)"
    matrix = mset[2]
    @debug "matrix=$(matrix) of type $typeof(matrix)"
    for (i, x) in enumerate(vars)
        @debug "i=$i, x=$x"
        @eval $x = $rweights[$i]
    end
    A = @eval $matrix
    return SMatrix{dim, dim}(A)
end

function the12p7weightedmask()::Mask
    Mask(SMatrix{7,7}([1 2 2 2 2 2 1;1 2 4 4 4 2 1;1 2 4 8 4 2 1;1 2 4 16 4 2 1;1 2 4 8 4 2 1;1 2 4 8 4 2 1;1 2 2 2 2 2 1]))
end

function test_masks()
    i=10
    [m for m in generate_masks(1:9, 3)]
    t=@elapsed begin
        for m in generate_masks(1:5, 3)
            @show m
            i -= 1
            i < 0 && break
        end
    end
    println("elapsed = $t s.")
end
# test_masks()
