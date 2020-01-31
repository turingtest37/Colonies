const DB_FILE_NAME = "colony4j.csv"
const DB_HEADERS = String["id","file","seed","mask","filter","repeater"]
const ID_REGEX = r"^\w+\-([\w\-]*)$"


make_colony_id() = UUIDs.uuid1()

make_filename(id, filedir, ext="png") = joinpath(filedir,string("colony4j-$id.", ext))

make_filename(filedir) = make_filename(filedir,rand(1:1000000))

function db_filename()
    return DB_FILE_NAME
end

function create_save_dir(dirtype::AbstractString)
    name = joinpath("img",dirtype,string(rand(1:1000000)))
    if !isdir(name)
        mkpath(name)
    end
    name
end

function archive_image_db(dbf::AbstractString, colonyid, colonypath::AbstractString, seed::ColonySeed, mask::Mask, filter::StateFilter, repeater::Bool)
    df = DataFrame(id=string(colonyid), name=colonypath, seed="$seed", mask="$mask", filter="$filter", repeater="$repeater")
    CSV.write(dbf, df; delim=',', header=DB_HEADERS, append=true)
end

function save_image_info(img::Array, filedir, seed::ColonySeed, mask::Mask, filter::StateFilter, repeater::Bool)
    colony_id = make_colony_id()
    colonyFileName = make_filename(colony_id, filedir, "gif")
    save(colonyFileName, img)
    archive_image_db(db_filename(),colony_id,colonyFileName,seed,mask,filter,repeater)
    @info "Wrote file $colonyFileName successfully."
end


function idfromcolonyfilepath(filepath::AbstractString)
    basename = splitext(Base.Filesystem.basename(filepath))
    # println("basename=$basename")
    re = match(ID_REGEX,basename[1])
    # println("re=$re")
    return re[1]
    # join(split(basename[1],'-')[2:end])
end

function archiverowfromid(df::DataFrame, id::AbstractString)
    @from i in df begin
    @where i.id==id
    @select {i.seed, i.mask, i.filter}
    @collect DataFrame
    end
end

function info(filename::AbstractString)
    df = CSV.read(db_filename(); types=[String, String, String, String, String, String], header=DB_HEADERS, datarow=2, delim=',')
    id = idfromcolonyfilepath(filename)
    @info "Searching for record with id='$id'"
    # search for filename in archive DataFrame
    return archiverowfromid(df,id)
end
