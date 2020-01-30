using Images
using Flux
using Flux: throttle
using FileIO
using DataFrames
using Statistics
using Serialization

function normalize(filenm::AbstractString)
    println("normalizing $filenm")
    img = FileIO.load(filenm)
    imgg = Float64.(Gray.(img))
    sigma = std(imgg)
    imgg = imfilter(imgg, Kernel.gaussian(sigma))
    img = imresize(imgg,100,100)
    return img
end

# set up training and test sets
# dataframe with:
#     dir path
#     image name as UUID
#     width, height, color/gray
#     size
#     keep=0|1
# load image
# scale it down to 100x100
# flux it!

basedir = splitdir(@__DIR__)[1]
tstd = joinpath(basedir,"ml","tst")
trd = joinpath(basedir,"ml","tr")
# mkdir(joinpath(basedir,"tr"))
# mkdir(joinpath(basedir,"tst"))

trl = Int8[]
trimgs = []

imgfile = joinpath(@__DIR__, "imgs.bin")
labelfile = joinpath(@__DIR__, "labels.bin")

function process_img_dir()
    for (root, dir, files) in walkdir(trd)
        # println("looking at $dir")

        for d in dir
            fcnt = 0
            fls = readdir(joinpath(trd,d))
            for f in fls
                splitext(f)[2] == ".png" || continue
                push!(trimgs, normalize(joinpath(trd,d,f)))
                fcnt += 1
            end
            if d == "y"
                push!(trl, ones(Int8,fcnt))
            else
                push!(trl, zeros(Int8,fcnt))
            end
        end
    end
    Serialization.serialize(imgfile, trimgs)
    Serialization.serialize(labelfile, trl)
end

if isfile(imgfile)
    trimgs = Serialization.deserialize(imgfile)
    trl = Serialization.deserialize(labelfile)
else
    process_img_dir()
end


println("trimgs has size $(size(trimgs))")
println("trl has size $(size(trl))")

m = Chain(
Conv((3,3), 3 => 64, relu, pad=(1, 1), stride=(1, 1)),
BatchNorm(64),
Conv((3,3), 64 => 64, relu, pad=(1, 1), stride=(1, 1)),
BatchNorm(64),
softmax
)

loss(x, y) = crossentropy(m(x),y)

accuracy(x, y) = mean(m(x) .== y)

evalcb = () -> @show(loss(trimgs, trl))

opt = ADAM()

Flux.train!(loss, params(m), zip(trimgs, trl), opt, cb = throttle(evalcb, 10))

atr = accuracy(trimgs, trl)
println("training accuracy is $atr")

# trimgs = []
push!(a, normalize("/Users/doug/dev/automaton/colonies12p7-1row/colonies12p7-150dpi-rainbow--1-1366939139754.png"))
# push!(a, normalize("/Users/doug/dev/automaton/colonies15/colonies15-158302-1366076820082.png"))
# trl=[1,0]
