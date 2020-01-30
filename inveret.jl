dd = Dict("a"=>1,"b"=>2,"c"=>3,"d"=>2,"e"=>1)
ee=Dict{Int64,Array{String}}()

for k in keys(dd)
  v=get!(ee,dd[k]) do
      Array{String}(1)
  end
  println("k=$k v=$v")
  ee[dd[k]]=push!(v,k)
end
