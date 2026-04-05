using Pkg

const COMPAT_ENV_DIR = normpath(joinpath(@__DIR__, "..", ".julia", "fertility_jls_compat"))
Pkg.activate(COMPAT_ENV_DIR)
Pkg.instantiate()

using Serialization
using AbstractPPL
using Turing
using MCMCChains
using DataFrames
using CSV

const DATA_DIR = normpath(joinpath(@__DIR__, "..", "data", "fertility"))

function chain_to_dataframe(chain)
    return DataFrame(chain)
end

function convert_jls_to_csv(input_path::AbstractString)
    chain = deserialize(input_path)
    df = chain_to_dataframe(chain)
    output_path = replace(input_path, r"\.jls$" => ".csv")
    CSV.write(output_path, df)
    println("Wrote $(basename(output_path))")
end

function main()
    input_files = sort(filter(name -> endswith(name, ".jls"), readdir(DATA_DIR)))

    if isempty(input_files)
        error("No .jls files found in $(DATA_DIR)")
    end

    for file_name in input_files
        convert_jls_to_csv(joinpath(DATA_DIR, file_name))
    end
end

main()
