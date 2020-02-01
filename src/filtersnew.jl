using Random
using Combinatorics

# always returns 1
const of = function un(x::Int) return (1,x); end
# always returns 0
const zf = function nyet(x::Int) return (0,x); end
# switches 1 to 0 or 0 to 1
const swf = function changer(x::Int) return ((1-x),x); end
# returns the same value as given
const saf = function meme(x::Int) return (x,x); end

mutable struct FilterState
    s1::Any
    s2::Any
    s3::Any
    s4::Any
    coeff_state::Any
    bi_state::Any
    neighbors::Array{Int}
    buckets::Union{Array,SubArray}
    coeffs::Union{Array,SubArray}
end


mutable struct FilterIterator
    i1::Any
    i2::Any
    i3::Any
    i4::Any
    coeff_i::Any
    bucket_i::Any
    state::FilterState
end

# It will be a miracle if this works.
# function Base.length(iter::FilterIterator)
#
#     res = []
#     t = Base.iterate(iter)
#     while t != nothing
#         v,s = t[1],t[2]
#         push!(res, v)
#         t = Base.iterate(iter, s)
#     end
#     Base.length(res)
#
#     # # 500000
#     # ilist = Any[]
#     # for f in (iter.i1, iter.i2, iter.i3, iter.i4, iter.coeff_i, iter.bucket_i)
#     #     !isnothing(f) && push!(ilist, f)
#     # end
#     # @debug "ilist" ilist
#     #
#     # foldr(*, (Base.length(f) for f in ilist), init=1)
# end

struct StateFilter
    d::Dict{Int64,Function}
end

function bucketcoeffs()
     return [7 1 1 1;
            6 2 1 1;
            5 3 1 1;
            5 2 2 1;
            4 4 1 1;
            4 3 2 1;
            4 2 2 2;
            3 3 3 1;
            3 2 3 2]
end

function Base.println(sf::StateFilter)
    d::Dict{Int64,Function} = sf.d
    for k in keys(d)
        println("$k = $(d[k])")
    end
end

function swap_k_v(d::Dict)
    result = Dict{Function,Array{Int,1}}()
    for k::Int in keys(d)
      v=get(result,d[k]) do
          Array{Int,1}()
      end
      # println("k=$k v=$v")
      result[d[k]]=push!(v,k)
    end
    result
end

function Base.show(io::IO, sf::StateFilter)
    r = swap_k_v(sf.d)
    print(io::IO, "$(get(r,zf,""))=>0;$(get(r,of,""))=>1;$(get(r,saf,""))=>same;$(get(r,swf,""))=>switch")
end

Base.eltype(::Type{FilterIterator}) = StateFilter

function Base.iterate(iter::FilterIterator)
    t = Base.iterate(iter.bucket_i)
    if t != nothing
        b, iter.state.bi_state = t[1], t[2]
        @debug "Initial value, state for bucket iterator=$b, $(iter.state.bi_state)"
        d = buildresult(b)
        return (StateFilter(d), iter.state)
    else
        @warn "Bucket iterator is broken!"
        return nothing
    end
end

function buildresult(buckets)
    od = Dict{Int,Function}(x => of for x::Int in buckets[1])
    zd = Dict{Int,Function}(x => zf for x::Int in buckets[2])
    sad = Dict{Int,Function}(x => saf for x::Int in buckets[3])
    swd = Dict{Int,Function}(x => swf for x::Int in buckets[4])
    return merge(od,zd,sad,swd)
end

function Base.iterate(iter::FilterIterator, state::FilterState)

    @debug "Iterate"
    t = Base.iterate(iter.bucket_i, state.bi_state)
    if t != nothing
        b, state.bi_state = t[1], t[2]
        @debug "new bucket = $(b)"
        @debug "new state.bi_state = $(state.bi_state)"
    else
        # Need to replenish the buckets
        @debug "No more permutations. Replenishing buckets and recreating the permutator..."
        b = refreshbuckets(iter)
        if b != nothing
            @debug "Got new buckets $(b) from refreshbuckets()"
            iter.bucket_i = permutations(b, 4)
            return Base.iterate(iter)
        else
            @debug "refreshbuckets() returned nothing! Bailing out!"
            return nothing
        end
    end
    d = buildresult(b)
    return (StateFilter(d), state)
end

function reiterate(fi::FilterIterator, indx::Int)
    @debug "Rebuilding iterator $(indx)"
    state = fi.state
    neighbors = state.neighbors
    buckets = state.buckets
    coeffs = state.coeffs

    if (indx > 1)
        A = setdiff(sort(neighbors), sort(vcat([buckets[i] for i in 1:indx-1]...)))
    else
        A = neighbors
    end
    n = coeffs[indx]
    if length(A) > n
        itr = combinations(A, n)
        setfield!(fi, Symbol(eval("i$indx")), itr)
        t = Base.iterate(itr)
        if t != nothing
            buckets[indx] = t[1]
            s = Symbol(eval("s$indx"))
            setfield!(state, s, t[2])
        else
            raise("empty iterator $itr")
        end
    else
        buckets[indx] = A
    end
    @debug "Set new value $(buckets[indx]) for bucket $indx"
    return fi
end

function refreshbuckets(fi::FilterIterator)

    coeff_iterator = fi.coeff_i

    state = fi.state
    @debug "FilterState = $(state)"

    coeff_state = state.coeff_state
    # always the same neighbors
    neighbors = state.neighbors
    # current buckets
    buckets = state.buckets
    # current coefficients
    coeffs = state.coeffs

    # First, try iterating 2 and rebuilding 3, 4
    t = Base.iterate(fi.i2, fi.state.s2)
    if t != nothing
        buckets[2], fi.state.s2 = t[1], t[2]
        @debug "New value $(buckets[2]) for bucket 2"
        try
            fi = reiterate(reiterate(fi, 3), 4)
        catch e
            @warn e
        end
    else
    # Iterator 2 is empty. Increment iterator 1 and rebuild 2,3,4
        t = Base.iterate(fi.i1, state.s1)
        if t != nothing
            buckets[1], state.s1 = t[1], t[2]
            @debug "New value $(buckets[1]) for bucket 1"
            try
                fi = reiterate(reiterate(reiterate(fi, 2), 3), 4)
            catch e
                @warn e
            end
        else
            # Iterator 1 is empty. Choose new coefficients and rebuild 1,2,3,4
            t = Base.iterate(fi.coeff_i, state.coeff_state)
            if t != nothing
                state.coeffs = t[1]
                state.coeff_state = t[2]
                # REBUILD 1
                @debug "Rebuilding the whole shebang with coeffs $(state.coeffs)"
                try
                    fi = reiterate(reiterate(reiterate(reiterate(fi,1), 2), 3), 4)
                catch e
                    @warn "Reiterate threw an exception: $(e)"
                end
            else
                @debug "We are out of coefficients! Time to go home."
                return nothing
            end
        end
    end
    return buckets
end

# returns a new StateFilter built by parsing the passed in String representation
function filterfromstring(s::AbstractString)
    d=Dict{Int64,Function}()
    drev = Dict{String,Function}("0"=>nyet, "1"=>un, "switch"=>changer, "same"=>meme)
    for s in split(s,';')
        a = split(s,"=>")
        if isa(eval(Meta.parse(a[1])),Array)
            for i in eval(Meta.parse(a[1]))
                d[Integer(i)]=drev[a[2]]
            end
        end
    end
    StateFilter(d)
end


function generate_state_filters(isshuffled::Bool = false)

    @info "Generating state filters..."

    buckets = [Int[], Int[], Int[], Int[]]
    s1, s2, s3, s4 = nothing, nothing, nothing, nothing
    fi1,fi2,fi3,fi4 = nothing, nothing, nothing, nothing
    bucket_i = nothing
    bi_state = nothing

    neighbors = [i for i = 0:9]
    if isshuffled
        Random.shuffle!(Random.RandomDevice(), neighbors)
    end
    @debug "Neighbors begins its life as $(neighbors)"

    c = bucketcoeffs()
    coeff_i = eachrow(c)
    # now initialize each bucket iterator with the first set of coefficients
    t = Base.iterate(coeff_i)
     # should never be nothing at this point
    coeffs, coeff_state = t[1], t[2]
    @debug "The first set of coefficients = $(coeffs)"
    filterstate = FilterState(s1,s2,s3,s4,coeff_state,bi_state,neighbors,buckets,coeffs)
    fi = FilterIterator(fi1,fi2,fi3,fi4,coeff_i,bucket_i,filterstate)

    for i in 1:4
        fi = reiterate(fi,i)
    end

    fi.bucket_i = permutations(filterstate.buckets, 4)
    filterstate.bi_state = nothing
    return fi
end

function testfilters()
    for f in generate_state_filters(true)
    end
    i=0
    t = @elapsed begin
        for f in generate_state_filters(true)
        # @info "StateFilter: $f"
            i+=1
        end
    end
    @info "Created $i unique filters in $t s."
end
