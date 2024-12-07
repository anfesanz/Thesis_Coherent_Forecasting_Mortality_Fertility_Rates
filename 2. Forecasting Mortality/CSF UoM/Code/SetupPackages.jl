using Pkg

function setup_packages(pkgs::Vector{String})
    for pkg in pkgs
        if !(pkg in keys(Pkg.installed()))
            println("Installing package: $pkg")
            Pkg.add(pkg)
        else
            println("Updating package: $pkg")
            Pkg.update(pkg)
        end
    end
end

packages_to_setup = ["Turing", "Random", "Distributions", "DataFrames", 
                     "LinearAlgebra", "StatsFuns", "MCMCChains", "Serialization", "Distributed", "SymPy", "Roots"]

Pkg.activate(".")  # Activate the current environment
Pkg.resolve()      # Resolve any dependency issues
setup_packages(packages_to_setup)
