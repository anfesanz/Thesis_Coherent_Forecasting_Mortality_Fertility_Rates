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
    
    @model function model2(d, loge, h)
        #Model 2 Coherent (betas) Model each gender
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
            bf ~ Dirichlet(ones(n_ages))
            bm ~ Dirichlet(ones(n_ages))
            # Priors and random walk for k
            sigma1 ~ Exponential(0.1)
            sigma2 ~ Exponential(0.1)
            sigma3 ~ Exponential(0.1)
            sigma4 ~ Exponential(0.1)
            sigma5 ~ Exponential(0.1)
            sigma6 ~ Exponential(0.1)
            sigma7 ~ Exponential(0.1)
            sigma8 ~ Exponential(0.1)
            #Drift RW
            c1 ~ Normal(0, sqrt(10))
            c2 ~ Normal(0, sqrt(10))
            c3 ~ Normal(0, sqrt(10))
            c4 ~ Normal(0, sqrt(10))
            c5 ~ Normal(0, sqrt(10))
            c6 ~ Normal(0, sqrt(10))
            c7 ~ Normal(0, sqrt(10))
            c8 ~ Normal(0, sqrt(10))
            #Init k vectors
            k1 = Vector(undef, n_periods)
            k2 = Vector(undef, n_periods)
            k3 = Vector(undef, n_periods)
            k4 = Vector(undef, n_periods)
            k5 = Vector(undef, n_periods)
            k6 = Vector(undef, n_periods)
            k7 = Vector(undef, n_periods)
            k8 = Vector(undef, n_periods)
            #Prior for k
            k1[1] ~ Normal(c1, sigma1)
            k2[1] ~ Normal(c2, sigma2)
            k3[1] ~ Normal(c3, sigma3)
            k4[1] ~ Normal(c4, sigma4)
            k5[1] ~ Normal(c5, sigma5)
            k6[1] ~ Normal(c6, sigma6)
            k7[1] ~ Normal(c7, sigma7)
            k8[1] ~ Normal(c8, sigma8)
            #Generate Random Walk
            for i in 2:n_periods
                k1[i] ~ Normal(c1 + k1[i-1], sigma1)
                k2[i] ~ Normal(c2 + k2[i-1], sigma2)
                k3[i] ~ Normal(c3 + k3[i-1], sigma3)
                k4[i] ~ Normal(c4 + k4[i-1], sigma4)
                k5[i] ~ Normal(c5 + k5[i-1], sigma5)
                k6[i] ~ Normal(c6 + k6[i-1], sigma6)
                k7[i] ~ Normal(c7 + k7[i-1], sigma7)
                k8[i] ~ Normal(c8 + k8[i-1], sigma8)
            end
            #Normalizing k (restriction sum k is zero)
            sum_k1 = sum(k1)
            sum_k2 = sum(k2)
            sum_k3 = sum(k3)
            sum_k4 = sum(k4)
            sum_k5 = sum(k5)
            sum_k6 = sum(k6)
            sum_k7 = sum(k7)
            sum_k8 = sum(k8)
            for i in 1:n_periods
                k1[i] = k1[i] - sum_k1 / n_periods
                k2[i] = k2[i] - sum_k2 / n_periods
                k3[i] = k3[i] - sum_k3 / n_periods
                k4[i] = k4[i] - sum_k4 / n_periods
                k5[i] = k5[i] - sum_k5 / n_periods
                k6[i] = k6[i] - sum_k6 / n_periods
                k7[i] = k7[i] - sum_k7 / n_periods
                k8[i] = k8[i] - sum_k8 / n_periods
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
                    logmu1[x, t] ~ Normal(a1[x] + bf[x] * k1[t], Sigma1)
                    d_est1[x, t] = exp(logmu1[x, t] + loge[1][x, t])
                    d[1][x, t] ~ Poisson(d_est1[x, t])

                    logmu2[x, t] ~ Normal(a2[x] + bf[x] * k2[t], Sigma2)
                    d_est2[x, t] = exp(logmu2[x, t] + loge[2][x, t])
                    d[2][x,t] ~ Poisson(d_est2[x, t])

                    logmu3[x, t] ~ Normal(a3[x] + bf[x] * k3[t], Sigma3)
                    d_est3[x, t] = exp(logmu3[x, t] + loge[3][x, t])
                    d[3][x, t] ~ Poisson(d_est3[x, t])

                    logmu4[x, t] ~ Normal(a4[x] + bf[x] * k4[t], Sigma4)
                    d_est4[x, t] = exp(logmu4[x, t] + loge[4][x, t])
                    d[4][x, t] ~ Poisson(d_est4[x, t])

                    logmu5[x, t] ~ Normal(a5[x] + bm[x] * k5[t], Sigma5)
                    d_est5[x, t] = exp(logmu5[x, t] + loge[5][x, t])
                    d[5][x, t] ~ Poisson(d_est5[x, t])

                    logmu6[x, t] ~ Normal(a6[x] + bm[x] * k6[t], Sigma6)
                    d_est6[x, t] = exp(logmu6[x, t] + loge[6][x, t])
                    d[6][x,t] ~ Poisson(d_est6[x, t])

                    logmu7[x, t] ~ Normal(a7[x] + bm[x] * k7[t], Sigma7)
                    d_est7[x, t] = exp(logmu7[x, t] + loge[7][x, t])
                    d[7][x, t] ~ Poisson(d_est7[x, t])

                    logmu8[x, t] ~ Normal(a8[x] + bm[x] * k8[t], Sigma8)
                    d_est8[x, t] = exp(logmu8[x, t] + loge[8][x, t])
                    d[8][x, t] ~ Poisson(d_est8[x, t])
                end
            end
            #Generated quantities (Forecast and validation)
            ###############################################
            #Forecasting kappa
            kf1 = Vector(undef, n_for)
            kf2 = Vector(undef, n_for)
            kf3 = Vector(undef, n_for)
            kf4 = Vector(undef, n_for)
            kf5 = Vector(undef, n_for)
            kf6 = Vector(undef, n_for)
            kf7 = Vector(undef, n_for)
            kf8 = Vector(undef, n_for)

            kf1[1] ~ Normal(c1+ k1[n_periods], sigma1)
            kf2[1] ~ Normal(c2+ k2[n_periods], sigma2)
            kf3[1] ~ Normal(c3+ k3[n_periods], sigma3)
            kf4[1] ~ Normal(c4+ k4[n_periods], sigma4)
            kf5[1] ~ Normal(c5+ k5[n_periods], sigma5)
            kf6[1] ~ Normal(c6+ k6[n_periods], sigma6)
            kf7[1] ~ Normal(c7+ k7[n_periods], sigma7)
            kf8[1] ~ Normal(c8+ k8[n_periods], sigma8)

            # Generate kf values
            for t in 2:n_for
                kf1[t] ~ Normal(c1 + kf1[t - 1], sigma1)
                kf2[t] ~ Normal(c2 + kf2[t - 1], sigma2)
                kf3[t] ~ Normal(c3 + kf3[t - 1], sigma3)
                kf4[t] ~ Normal(c4 + kf4[t - 1], sigma4)
                kf5[t] ~ Normal(c5 + kf5[t - 1], sigma5)
                kf6[t] ~ Normal(c6 + kf6[t - 1], sigma6)
                kf7[t] ~ Normal(c7 + kf7[t - 1], sigma7)
                kf8[t] ~ Normal(c8 + kf8[t - 1], sigma8)
            end
        end
    end 
end
# Specification Model2(beta coherent)
modelesp2 = model2(d, loge, 15)

function perform_sampling_distributed(model, iterations)
    # Parallel sampling
    chains = pmap(_ -> sample(model, NUTS(0.65), iterations), 1:nprocs())
    return reduce(chainscat, chains)
end

# Call once for pre-compilation
@time perform_sampling_distributed(modelesp2, 10)

# Perform sampling
# Here, the @time macro is used for a simple timing of the function execution
@time chain_model2 = perform_sampling_distributed(modelesp2, 1000)

# Export the results
serialize("$(Output)chain_model2.jls", chain_model2)