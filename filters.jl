using Random
using Combinatorics

# always returns 1
const of = function un(x::Int) return (1,x==0); end
# always returns 0
const zf = function nyet(x::Int) return (0,x==1); end
# switches 1 to 0 or 0 to 1
const swf = function changer(x::Int) return ((1-x),true); end
# returns the same value as given
const saf = function meme(x::Int) return (x,false); end

mutable struct FilterState{T,TT,TTT,TTTT,TTTTT,TTTTTT}
    s1::T
    s2::TT
    s3::TTT
    s4::TTTT
    s5::TTTTT
    s6::TTTTTT
    neighbors::Array{Int,1}
    z::Array{Int,1}
    b::Array{Int,1}
    o::Array{Int,1}
    c::Array{Int,1}
    j::Int
    k::Int
    sa::Array{Int,1}
    sw::Array{Int,1}
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
    d::Dict{Int64,Function}
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

function Base.iterate(iter::FilterIterator, state::FilterState)

    o = state.o
    z = state.z
    sa = state.sa
    sw = state.sw
    b = state.b
    c = state.c
    j = state.j
    k = state.k
    neighbors = state.neighbors
    is_shuffled = state.is_shuffled
    @debug "Entering iterate"
    @debug "o = $(o), z=$(z), sa=$(sa), sw=$(sw), b=$(b), c=$(c), j=$(j), k=$(k), neighbors=$(neighbors), ishuffled=$(is_shuffled)"
    @debug "iter.fi6 = $(iter.fi6)"
    @debug "state.s6 = $(state.s6)"
    # now go in reverse order of creation
    t = Base.iterate(iter.fi6, state.s6)
    if t != nothing
        @debug "t = $t"
        state.sa, state.s6 = t[1], t[2]
        @debug "state.sa = $(state.sa)"
        @debug "state.s6 = $(state.s6)"
        state.sw = setdiff(c, state.sa)
        @debug "Just set state.sw to $(state.sw)"

    else
        t = Base.iterate(iter.fi5, state.s5)
        if t != nothing
            @debug "t = $t"
        # need to increment fi5 and recreate fi6
            state.k, state.s5 = t[1], t[2]
            @debug "state.k = $(state.k)"
            @debug "state.s5 = $(state.s5)"
            iter.fi6 = combinations(c, state.k)
            t = Base.iterate(iter.fi6)
            if t != nothing
                @debug "t = $t"
                state.sa, state.s6 = t[1], t[2]
            end
        else
            t = Base.iterate(iter.fi4, state.s4)
            if t != nothing
                @debug "t = $t"
                state.o, state.s4 = t[1], t[2]
                state.c = setdiff(b,(is_shuffled ? shuffle(state.o) : state.o))
                # println("state.c = $(state.c); c = $c")
                iter.fi5 = axes(c, 1)
                t = Base.iterate(iter.fi5)
                if t != nothing
                    @debug "t = $t"
                    state.k, state.s5 = t[1], t[2]
                end
            else
                t = Base.iterate(iter.fi3, state.s3)
                if t != nothing
                    @debug "t = $t"
                    state.j, state.s3 = t[1], t[2]
                    iter.fi4 = combinations(b, state.j)
                    t = Base.iterate(iter.fi4)
                    if t != nothing
                        @debug "t = $t"
                        state.o, state.s4 = t[1], t[2]
                    end
                    # println("state.o = $(state.o); o = $o")
                else
                    t = Base.iterate(iter.fi2, state.s2)
                    if t != nothing
                        @debug "t = $t"
                        state.z, state.s2 = t[1], t[2]
                        state.b = setdiff(neighbors, (is_shuffled ? shuffle(state.z) : state.z))
                        iter.fi3 = axes(state.b, 1)
                        t = Base.iterate(iter.fi3)
                        if t!= nothing
                            @debug "t = $t"
                            state.j, state.s3 = t[1], t[2]
                        end
                    else
                        t = Base.iterate(iter.fi1, state.s1)
                        if t != nothing
                            i1, state.s1 = t[1], t[2]
                            iter.fi2 = combinations(neighbors, i1)
                            t = Base.iterate(iter.fi2)
                            if t != nothing
                                @debug "t = $t"
                                state.z, state.s2 = t[1], t[2]
                            end
                        else
                            return nothing
                        end
                    end
                end
            end
        end
    end

# use the previous iterator values to create the return value
    od = Dict{Int,Function}(x=>of for x::Int in o)
    zd = Dict{Int,Function}(x=>zf for x::Int in z)
    sad = Dict{Int,Function}(x=>saf for x::Int in sa)
    swd = Dict{Int,Function}(x=>swf for x::Int in sw)

    return (StateFilter(merge(od,zd,sad,swd)), state)
end

# returns a new StateFilter built by parsing the passed in String representation
function filterfromstring(s::AbstractString)
    d=Dict{Int64,Function}()
    drev = Dict{String,Function}("0"=>nyet, "1"=>un, "switch"=>changer, "same"=>meme)
    for s in split(s,';')
        a = split(s,"=>")
        # println("a is $a")
        if isa(eval(parse(a[1])),Array)
            # println("eval a = $(eval(a))")
            for i in eval(parse(a[1]))
                # println("i is $i type=$(typeof(i))")
                d[Integer(i)]=drev[a[2]]
            end
        end
    end
    StateFilter(d)
end

Base.iterate(iter::FilterIterator) = Base.iterate(iter, iter.state)

function generate_state_filters(isshuffled = false)
    o = Int[]
    z = Int[]
    sa = Int[]
    sw = Int[]
    b = Int[]
    c = Int[]
    j = 0
    k = 0

    @info "Generating state filters..."
    neighbors = [i for i = 0:9]
    if isshuffled
        Random.shuffle!(Random.RandomDevice(), neighbors)
    end

    # IMPORTANT: We have to use length-2 in randperm because if 10 comes up
    # in the iterator, the combinations() call below will produce a set of 10 elements
    # which, when passed into the setdiff() call will produce a zero-length array.
    # The zero-length array causes the whole thing to barf.
    fi1 = isshuffled ? randperm(Random.RandomDevice(), length(neighbors)-2) : axes(neighbors,1)
    @debug "fi1 is type $(typeof(fi1))"
    @debug "fi1 = $(fi1)"
    t = Base.iterate(fi1)
    if t != nothing
        i, s1 = t[1], t[2]
        @debug "i = $i"
        @debug "s1 = $s1"
        fi2 = combinations(neighbors, i)
        # println("fi2 is type $(typeof(fi2))")
        t = Base.iterate(fi2)
        # println("s1 is type $(typeof(s1))")
        if t != nothing
            z, s2 = t[1], t[2]
            # println("First z is $z, type=$(typeof(z))")
            b = setdiff(neighbors, z)
            # println("First b is $b")
            fi3 = isshuffled ? randperm(Random.RandomDevice(), max(length(b)-1,1)) : axes(b,1)
            # println("fi3 is $fi3 type $(typeof(fi3))")
            t = Base.iterate(fi3)
            if t != nothing
                j, s3 = t[1], t[2]
                # println("First j is $j")
                fi4 = combinations(b, j)
                # println("fi4 is $fi4 type $(typeof(fi4))")
                t = Base.iterate(fi4)
                if t != nothing
                    o, s4  = t[1], t[2]
                    # println("First o is $o")
                    c = setdiff(b, o)
                    # println("First c is $c")
                    fi5 = axes(c,1)
                    # println("fi5 is $fi5 type $(typeof(fi5))")
                    t = Base.iterate(fi5)
                    if t != nothing
                        k, s5 = t[1], t[2]
                        # println("First k is $k")
                        fi6 = combinations(c,k)
                        # println("fi6 is $fi6 type $(typeof(fi6))")
                        t = Base.iterate(fi6)
                        if t != nothing
                            sa, s6 = t[1], t[2]
                            println("First sa is $sa")
                            sw = setdiff(c,sa)
                            println("First sw is $sw")
                            return FilterIterator(fi1,fi2,fi3,fi4,fi5,fi6,
                            FilterState(s1,s2,s3,s4,s5,s6,neighbors,z,b,o,c,j,k,sa,sw,isshuffled))
                        end
                    end
                end
            end
        end
        throw("Failed to create a StateFilter.")
    end
end
# buildfs() = FilterState(s1,s2,s3,s4,s5,s6,neighbors,z,b,o,c,j,k,sa,sw,isshuffled)

function testfilters(runs)
    for j in 1:runs
        i=0
        for f in generate_state_filters(true)
            i+=1
        end
        println("Created $i unique filters.")
    end
end




testfilters(10)
println("Yoohoo!")
