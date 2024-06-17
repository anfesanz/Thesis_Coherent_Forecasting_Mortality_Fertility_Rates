# Path to the data file
Data_Created = "Data_Created/"
Output = "Output/"

#Install packages if not already added
using Pkg
function ensure_packages_installed(pkgs::Vector{String})
    installed = Pkg.dependencies()
    for pkg_name in pkgs
        if !(pkg_name in keys(installed))
            println("Package $pkg_name is not installed. Installing...")
            Pkg.add(pkg_name)
        else
            println("Package $pkg_name is already installed.")
        end
    end
end
required_packages = ["Turing", "Random", "Distributions", "DataFrames", 
                     "LinearAlgebra", "StatsFuns", "MCMCChains", "Serialization", "Distributed"]
ensure_packages_installed(required_packages)

# Do distributed runs
using Distributed
addprocs(16)  # Adjust the number of workers

@everywhere begin
    using Turing, Random, Distributions, DataFrames, LinearAlgebra, StatsFuns, MCMCChains, Serialization
   
    # Load the data
    d = deserialize("$(Data_Created)d.jls")
    loge = deserialize("$(Data_Created)loge.jls")
    
    @model function model4(d, loge, h)
        #Model 3 Coherent (betas and kapas) Model each gender
        # Verify that all matrices have the same dimensions
        if any([size(d[i]) != size(loge[i]) for i in 1:8]) || any([size(d[1]) != size(d[i]) for i in 2:8])
            error("The sizes of the matrices are different")
        else
            n_ages, n_periods = size(d[1])
            n_for = h
            n_groups = length(d)
            # Prior for a 
            a1 ~ MvNormal(zeros(n_ages), 10)
            a2 ~ MvNormal(zeros(n_ages), 10)
            a3 ~ MvNormal(zeros(n_ages), 10)
            a4 ~ MvNormal(zeros(n_ages), 10)
            a5 ~ MvNormal(zeros(n_ages), 10)
            a6 ~ MvNormal(zeros(n_ages), 10)
            a7 ~ MvNormal(zeros(n_ages), 10)
            a8 ~ MvNormal(zeros(n_ages), 10)
            # Priors for b
            b ~ Dirichlet(ones(n_ages))
            # Priors and random walk for k
            sigma ~ Exponential(0.1)

            #Drift RW
            c ~ Normal(0, sqrt(10))

            #Init k vectors
            k = Vector(undef, n_periods)

            #Prior for k
            k[1] ~ Normal(c, sigma)

            #Generate Random Walk
            for i in 2:n_periods
                k[i] ~ Normal(c + k[i-1], sigma)
            end
            #Normalizing k (restriction sum k is zero)
            sum_k = sum(k)
            for i in 1:n_periods
                k[i] = k[i] - sum_k / n_periods
            end
            #Priors to Log mortality
            Sigma1 ~ Exponential(1)
            Sigma2 ~ Exponential(1)
            Sigma3 ~ Exponential(1)
            Sigma4 ~ Exponential(1)
            Sigma5 ~ Exponential(1)
            Sigma6 ~ Exponential(1)
            Sigma7 ~ Exponential(1)
            Sigma8 ~ Exponential(1)
            #Init Matrix to dead count
            d_est1 = Matrix(undef, n_ages, n_periods)
            d_est2 = Matrix(undef, n_ages, n_periods)
            d_est3 = Matrix(undef, n_ages, n_periods)
            d_est4 = Matrix(undef, n_ages, n_periods)
            d_est5 = Matrix(undef, n_ages, n_periods)
            d_est6 = Matrix(undef, n_ages, n_periods)
            d_est7 = Matrix(undef, n_ages, n_periods)
            d_est8 = Matrix(undef, n_ages, n_periods)
            #Init matrix to log mortality (Quantiles generated)
            logmu1 = Matrix(undef, n_ages, n_periods)
            logmu2 = Matrix(undef, n_ages, n_periods)
            logmu3 = Matrix(undef, n_ages, n_periods)
            logmu4 = Matrix(undef, n_ages, n_periods)
            logmu5 = Matrix(undef, n_ages, n_periods)
            logmu6 = Matrix(undef, n_ages, n_periods)
            logmu7 = Matrix(undef, n_ages, n_periods)
            logmu8 = Matrix(undef, n_ages, n_periods)

            # Likelihood for d1, d2, d3 and d4
            ###################################
            for x in 1:n_ages
                for t in 1:n_periods
                    logmu1[x, t] ~ Normal(a1[x] + b[x] * k[t], Sigma1)
                    d_est1[x, t] = exp(logmu1[x, t] + loge[1][x, t])
                    d[1][x, t] ~ Poisson(d_est1[x, t])

                    logmu2[x, t] ~ Normal(a2[x] + b[x] * k[t], Sigma2)
                    d_est2[x, t] = exp(logmu2[x, t] + loge[2][x, t])
                    d[2][x,t] ~ Poisson(d_est2[x, t])

                    logmu3[x, t] ~ Normal(a3[x] + b[x] * k[t], Sigma3)
                    d_est3[x, t] = exp(logmu3[x, t] + loge[3][x, t])
                    d[3][x, t] ~ Poisson(d_est3[x, t])

                    logmu4[x, t] ~ Normal(a4[x] + b[x] * k[t], Sigma4)
                    d_est4[x, t] = exp(logmu4[x, t] + loge[4][x, t])
                    d[4][x, t] ~ Poisson(d_est4[x, t])

                    logmu5[x, t] ~ Normal(a5[x] + b[x] * m[t], Sigma5)
                    d_est5[x, t] = exp(logmu5[x, t] + loge[5][x, t])
                    d[5][x, t] ~ Poisson(d_est5[x, t])

                    logmu6[x, t] ~ Normal(a6[x] + b[x] * k[t], Sigma6)
                    d_est6[x, t] = exp(logmu6[x, t] + loge[6][x, t])
                    d[6][x,t] ~ Poisson(d_est6[x, t])

                    logmu7[x, t] ~ Normal(a7[x] + b[x] * k[t], Sigma7)
                    d_est7[x, t] = exp(logmu7[x, t] + loge[7][x, t])
                    d[7][x, t] ~ Poisson(d_est7[x, t])

                    logmu8[x, t] ~ Normal(a8[x] + b[x] * k[t], Sigma8)
                    d_est8[x, t] = exp(logmu8[x, t] + loge[8][x, t])
                    d[8][x, t] ~ Poisson(d_est8[x, t])
                end
            end
            #Generated quantities (Forecast and validation)
            ###############################################
            #Forecasting kappa (kf =kappa forecast general) 
            kf = Vector(undef, n_for)
            kf[1] ~ Normal(c+ k[n_periods], sigma)        
            # Generate kf values
            for t in 2:n_for
                kf[t] ~ Normal(c1 + kf[t - 1], sigma)
            end
        end
    end 
end
# Specification Model4(betas,kappas and gender coherent)
modelesp4 = model4(d, loge, 15)

function perform_sampling_distributed(model, iterations)
    # Parallel sampling
    chains = pmap(_ -> sample(model, NUTS(0.65), iterations), 1:nprocs())
    return reduce(chainscat, chains)
end

# Call once for pre-compilation
@time perform_sampling_distributed(modelesp4, 10)

# Perform sampling
# Here, the @time macro is used for a simple timing of the function execution
@time chain_model4 = perform_sampling_distributed(modelesp4, 1000)

# Export the results
serialize("$(Output)chain_model4.jls", chain_model4)