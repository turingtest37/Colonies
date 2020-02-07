using Dates

const DB_FILE_NAME = "colony4j2.csv"
const DB_HEADERS = String["id","file","cwx","cwy","colx","coly","seed","mask","filter","repeater","repeatidx","layout"]
const DB_ELTYPES = [String, String, Int, Int, Int, Int, String, String, String, Bool, String, String]
const ID_REGEX = r"^\w+\-([\w\-]*)$"
const HHMMSS_FORMAT = dateformat"HHMMSS"

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

function create_save_dir(dirname::AbstractString, dirtype::AbstractString, mksubdir::Bool = true)
    name = joinpath(dirname, dirtype, (mksubdir ? Dates.format(now(), HHMMSS_FORMAT) : ""))
    if !isdir(name)
        mkpath(name)
    end
    return name
end

create_save_dir(dirname::AbstractString, rndname::Bool) = create_save_dir(dirname, "", rndname)
create_save_dir(dirtype::AbstractString) = create_save_dir("img", dirtype, true)

function archive_image_db(dbf::AbstractString; kwargs...)
    # (k,v)
    # S = :([Symbol(s)=", $(s)" for s in DB_HEADERS])
    @debug "Saving image with args" kwargs
    df = DataFrame(kwargs...)
    @debug "Here she is..." df
    io = nothing
    if isfile(dbf)
        io = open(dbf, "a")
        CSV.write(io, df; delim=',', append=true)
    else
        @debug "Creating new archive file " dbf
        io = open(dbf, "w")
        CSV.write(io, df; delim=',', writeheader=true)
    end

    close(io)
    dbf
end

function save_image_info(result::ColonyResult, filedir::AbstractString)
    context = result.context
    img = result.img
    seed = context.seed
    mask = context.mask
    filter = context.filter
    layout = context.layout
    id = make_colony_id()
    file = make_filename(id, filedir, layout.ext)
    @debug "Attempting to save image to $(file)"
    save(file, img)

    archive_image_db(db_filename();
    :id => id,
    :file => file,
    :cwx => context.xsize,
    :cwy => context.ysize,
    :colx => context.colx,
    :coly => context.coly,
    :seed => context.seed,
    :mask => context.mask,
    :filter => context.filter,
    :repeater => result.repeats,
    :repeatidx => (isnothing(result.repeatidx) ? string(Int[]) : string(result.repeatidx)),
    :layout => context.layout
    )

    # archive_image_db(db_filename(),colony_id,colonyFileName,seed,mask,filter,repeater)
    @debug "Saved image file $file"
    return file
end


function idfromcolonyfilepath(filepath::AbstractString)
    basename = splitext(Base.Filesystem.basename(filepath))
    re = match(ID_REGEX,basename[1])
    return re[1]
end

function archiverowfromid(df::DataFrame, id::AbstractString)
    @from i in df begin
    @where i.id==id
    @select i
    @collect DataFrame
    end
end

function info(filename::AbstractString)
    # df = CSV.read(db_filename(); types=[String for i=1:length(DB_HEADERS)], header=DB_HEADERS, datarow=2, delim=',')
    df = CSV.read(db_filename(); types=DB_ELTYPES, header=DB_HEADERS, datarow=2, delim=',')
    id = idfromcolonyfilepath(filename)
    @info "Searching for record with id='$id'"
    # search for filename in archive DataFrame
    r = archiverowfromid(df,id)
    if size(df, 1) < 1
        println("No record found for file $(filename)")
    else
        show(r, allcols=true)
    end
end
