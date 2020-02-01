const DB_FILE_NAME = "colony4j.csv"
const DB_HEADERS = String["id","file","seed","mask","filter","repeater"]
const ID_REGEX = r"^\w+\-([\w\-]*)$"

abstract type CWLayout end

struct TiledLayout <: CWLayout
    ext::AbstractString
end

struct StackedLayout <: CWLayout
    ext::AbstractString
    StackedLayout() = new("gif")
end

make_colony_id() = UUIDs.uuid1()

make_filename(id, filedir, ext="png") = joinpath(filedir,string("colony4j-$id.", ext))

make_filename(filedir) = make_filename(filedir,rand(1:1000000))

function db_filename()
    return DB_FILE_NAME
end

function create_save_dir(dirname::AbstractString, dirtype::AbstractString, rndname::Bool = true)
    name = joinpath(dirname, dirtype, (rndname ? string(rand(1:1000000)) : ""))
    if !isdir(name)
        mkpath(name)
    end
    return name
end

create_save_dir(dirtype::AbstractString) = create_save_dir("img", dirtype, true)

function archive_image_db(dbf::AbstractString, colonyid, colonypath::AbstractString, seed::ColonySeed, mask::Mask, filter::StateFilter, repeater::Bool)
    df = DataFrame(id=string(colonyid), name=colonypath, seed="$seed", mask="$mask", filter="$filter", repeater="$repeater")
    io = open(dbf, "a")
    CSV.write(io, df; delim=',', header=DB_HEADERS, append=true)
    close(io)
end



function save_image_info(img::Array{Gray{Float64}}, filedir::AbstractString, context, repeater::Bool)
    seed = context.seed
    mask = context.mask
    filter = context.filter
    layout = context.layout
    colony_id = make_colony_id()
    colonyFileName = make_filename(colony_id, filedir, layout.ext)
    @debug "Attempting to save image to $(colonyFileName)"
    save(colonyFileName, img)
    archive_image_db(db_filename(),colony_id,colonyFileName,seed,mask,filter,repeater)
    @info "Saved image file $colonyFileName"
end


function idfromcolonyfilepath(filepath::AbstractString)
    basename = splitext(Base.Filesystem.basename(filepath))
    re = match(ID_REGEX,basename[1])
    return re[1]
end

function archiverowfromid(df::DataFrame, id::AbstractString)
    @from i in df begin
    @where i.id==id
    @select {i.id, i.file, i.seed, i.mask, i.filter, i.repeater}
    @collect DataFrame
    end
end

function info(filename::AbstractString)
    df = CSV.read(db_filename(); types=[String, String, String, String, String, String], header=DB_HEADERS, datarow=2, delim=',')
    id = idfromcolonyfilepath(filename)
    @info "Searching for record with id='$id'"
    # search for filename in archive DataFrame
    r = archiverowfromid(df,id)
    show(r, allcols=true)
end
