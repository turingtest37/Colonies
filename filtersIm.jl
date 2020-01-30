using Combinatorics


# switch to conj
# switch to -conj
# zero
# same
# always returns 1+im
# const zf = function nyet(z::Complex{Int64}) r=rand(-1:1);i=rand(-1:1);zn=complex(r,i);return (zn, z!=zn); end
const zf = function nyet(z::Complex{Int64}) r=0;i=1;zn=complex(r,i);return (zn, z!=zn); end
# switches real part of z
const swr = function changerr(z::Complex{Int64}) return (-conj(z), true); end
# switches 1+im to 1-im
const swi = function changeri(z::Complex{Int64}) return (conj(z), true); end
# returns the same value as given
const saf = function meme(z::Complex{Int64}) return (z, false); end

mutable struct FilterState{T,TT,TTT,TTTT,TTTTT,TTTTTT}
    s1::T
    s2::TT
    s3::TTT
    s4::TTTT
    s5::TTTTT
    s6::TTTTTT
    neighbors::Array{Complex{Int64},1}
    z::Array{Complex{Int64},1}
    b::Array{Complex{Int64},1}
    o::Array{Complex{Int64},1}
    c::Array{Complex{Int64},1}
    j::Int64
    k::Int64
    sa::Array{Complex{Int64},1}
    sw::Array{Complex{Int64},1}
    is_shuffled::Bool
end


mutable struct FilterIterator{T,TT,TTT,TTTT,TTTTT,TTTTTT}
    fi1::T
    fi2::TT
    fi3::TTT
    fi4::TTTT
    fi5::TTTTT
    fi6::TTTTTT
    state::FilterState
end

struct StateFilter
    d::Dict{Complex{Int64},Function}
end

# function Base.println(sf::StateFilter)
#     d::Dict{Int64,Function} = sf.d
#     for k in keys(d)
#         println("$k = $(d[k])")
#     end
# end

function swap_k_v(d::Dict)
    result = Dict{Function,Array{Complex,1}}()
    for k::Complex in keys(d)
      v=get(result,d[k]) do
          Array{Complex,1}()
      end
      # println("k=$k v=$v")
      result[d[k]]=push!(v,k)
    end
    result
end

function Base.show(io::IO, sf::StateFilter)
    r = swap_k_v(sf.d)
    print(io::IO, "$(get(r,zf,nothing))=>0;$(get(r,swr,nothing))=>switchr;$(get(r,saf,nothing))=>same;$(get(r,swi,nothing))=>switchi")
end

Base.eltype(::Type{FilterIterator}) = StateFilter

function Base.done(iter::FilterIterator, state::FilterState)
    Base.done(iter.fi1, state.s1) &&
    Base.done(iter.fi2, state.s2) &&
    Base.done(iter.fi3, state.s3) &&
    Base.done(iter.fi4, state.s4) &&
    Base.done(iter.fi5, state.s5) &&
    Base.done(iter.fi6, state.s6)
end

# this is the last one called
function Base.next(iter::FilterIterator, state::FilterState)

    o = state.o
    z = state.z
    sa = state.sa
    sw = state.sw
    b = state.b
    c = state.c
    j = state.j
    neighbors = state.neighbors
    is_shuffled = state.is_shuffled

    # now go in reverse order of creation
    if !Base.done(iter.fi6, state.s6)
        (state.sa, state.s6) = Base.next(iter.fi6, state.s6)
        state.sw = setdiff(c, state.sa)
    elseif !Base.done(iter.fi5, state.s5)
        # need to increment fi5 and recreate fi6
        (state.k, state.s5) = Base.next(iter.fi5, state.s5)
        iter.fi6 = combinations(c,state.k)
        state.s6 = Base.start(iter.fi6)
    elseif !Base.done(iter.fi4, state.s4)
        (state.o, state.s4) = Base.next(iter.fi4, state.s4)
        state.c = setdiff(b,(is_shuffled ? shuffle(state.o) : state.o))
        iter.fi5 = indices(c,1)
        state.s5 = Base.start(iter.fi5)
    elseif !Base.done(iter.fi3, state.s3)
        (state.j, state.s3) = Base.next(iter.fi3,state.s3)
        iter.fi4 = combinations(b,state.j)
        state.s4 = Base.start(iter.fi4)
    elseif !Base.done(iter.fi2,state.s2)
        (state.z, state.s2) = Base.next(iter.fi2,state.s2)
        state.b = setdiff(neighbors,(is_shuffled ? shuffle(state.z) : state.z))
        iter.fi3 = indices(state.b,1)
        state.s3 = Base.start(iter.fi3)
    elseif !Base.done(iter.fi1, state.s1)
        (i1,state.s1) = Base.next(iter.fi1,state.s1)
        iter.fi2 = combinations(neighbors,i1)
        state.s2 = Base.start(iter.fi2)
    end

# use the previous iterator values to create the return value
    swrd = Dict{Complex{Int64},Function}(x=>swr for x::Complex{Int64} in o)
    zfd = Dict{Complex{Int64},Function}(x=>zf for x::Complex{Int64} in z)
    safd = Dict{Complex{Int64},Function}(x=>saf for x::Complex{Int64} in sa)
    swid = Dict{Complex{Int64},Function}(x=>swi for x::Complex{Int64} in sw)

    (StateFilter(merge(swrd,zfd,safd,swid)), state)
end

# returns a new StateFilter built by parsing the passed in String representation
function filterfromstring(s::AbstractString)
    d=Dict{Complex{Int64},Function}()
    drev = Dict{String,Function}("0"=>nyet, "switchi"=>changeri, "switchr"=>changerr, "same"=>meme)
    for s in split(s,';')
        a = split(s,"=>")
        # println("a is $a")
        if isa(eval(parse(a[1])),Array)
            # println("eval a = $(eval(a))")
            for i in eval(parse(a[1]))
                println("i is $i type=$(typeof(i))")
                d[i]=drev[a[2]]
            end
        end
    end
    StateFilter(d)
end

# do nothing much here
function Base.start(iter::FilterIterator)
    iter.state
end

function generate_state_filters(isshuffled::Bool=false)
    o::Array{Complex{Int64},1} = []
    z::Array{Complex{Int64},1} = []
    sa::Array{Complex{Int64},1} = []
    sw::Array{Complex{Int64},1} = []
    b::Array{Complex{Int64},1} = []
    c::Array{Complex{Int64},1} = []
    j::Int64=0
    k::Int64=0

    # println("Generating state filters...")
    n = reshape([complex(i,j) for i=-1:1, j=-1:1],9)
    neighbors = isshuffled ? shuffle!(RandomDevice(),n) : n
    # println("neighbors is type $(typeof(neighbors))")
    # IMPORTANT: Have to use length-2 in randperm because if 10 comes up in the iterator, the combinations() call below will produce a set of 10 elements which, when passed into the setdiff() call will produce a zero-length array. The zero-length array causes the whole thing to barf.
    fi1 = isshuffled ? randperm(length(neighbors)-2) : indices(neighbors,1)
    # println("fi1 is type $(typeof(fi1))")
    s1 = Base.start(fi1)
    if !Base.done(fi1,s1)
        (i,s1) = Base.next(fi1,s1)
        # println("First i is $i")
        fi2 = combinations(neighbors,i)
        # println("fi2 is type $(typeof(fi2))")
        s2 = Base.start(fi2)
        # println("s2 is type $(typeof(s2))")
        if !Base.done(fi2,s2)
            (z,s2) = Base.next(fi2,s2)
            # println("First z is $z, type=$(typeof(z))")
            b = setdiff(neighbors,z)
            # println("First b is $b")
            fi3 = isshuffled ? randperm(max(length(b)-1,1)) : indices(b,1)
            # println("fi3 is $fi3 type $(typeof(fi3))")
            s3 = Base.start(fi3)
            if !Base.done(fi3,s3)
                (j,s3) = Base.next(fi3,s3)
                # println("First j is $j")
                fi4 = combinations(b,j)
                # println("fi4 is $fi4 type $(typeof(fi4))")
                s4 = Base.start(fi4)
                if !Base.done(fi4,s4)
                    (o,s4) = Base.next(fi4,s4)
                    # println("First o is $o")
                    c = setdiff(b,o)
                    # println("First c is $c")
                    fi5 = indices(c,1)
                    # println("fi5 is $fi5 type $(typeof(fi5))")
                    s5 = Base.start(fi5)
                    if !Base.done(fi5,s5)
                        (k,s5) = Base.next(fi5,s5)
                        # println("First k is $k")
                        fi6 = combinations(c,k)
                        # println("fi6 is $fi6 type $(typeof(fi6))")
                        s6 = Base.start(fi6)
                        if !Base.done(fi6,s6)
                            (sa,s6) = Base.next(fi6,s6)
                            # println("First sa is $sa")
                            sw = setdiff(c,sa)
                            # println("First sw is $sw")
                            return FilterIterator(fi1,fi2,fi3,fi4,fi5,fi6,FilterState(s1,s2,s3,s4,s5,s6,neighbors,z,b,o,c,j,k,sa,sw,isshuffled))
                        end
                    end
                end
            end
        end
    end
    throw("Failed to create a StateFilter.")
end

function testfilters(runs)
    # for j in 1:runs
        i=0
        for f in generate_state_filters(true)
            if i>10
                break
            end
            println(f)
            i+=1
        end
        println("Created $i unique filters.")
    # end
end
testfilters(20)
println("Yoohoo!")
