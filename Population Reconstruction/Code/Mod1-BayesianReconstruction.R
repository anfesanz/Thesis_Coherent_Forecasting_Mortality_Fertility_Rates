#Bayesian hierachical model for reconstruction population

#The fist input needed is the baseline population. 
#That in our case could be 2018 (or the combination between 2018 and 2005) for Colombia
#This population need to be expresed as percentage


#R and JAGS 
#R packages: rjags, dclone and snow


#The rjags package provides an interface from R to the JAGS library for Bayesian data analysis.
#JAGS uses Markov Chain Monte Carlo (MCMC) to generate a sequence of dependent samples 
#from the posterior distribution of the parameters.


#dclone: Data Cloning and MCMC Tools for Maximum Likelihood Methods
#Low level functions for implementing maximum likelihood estimating procedures
#for complex models using data cloning and Bayesian Markov chain Monte Carlo methods 
#as described in Solymos 2010 (R Journal 2(2):29–37).
#Sequential and parallel MCMC support for 'JAGS', 'WinBUGS', 'OpenBUGS', and 'Stan'.

#snow: Simple Network of Workstations
#Support for simple parallel computing in R.


library(rjags)
library(dclone)
library(snow)
library(lemon)
library(tidyverse)
library(devtools)
library(wcde)
library(gganimate)


#Bayesian reconstruction
#Level 1: Modelling census counts (Input 1980, 1990, 2000, 2010)
#output any age

# Load necessary packages
library(rstan)
library(ggplot2)

# Generate simulated data
set.seed(123)
n_groups <- 2
n_obs <- 200
theta <- rnorm(n_groups, 2, 1)

y <- rnbinom(10, mu = 100, size = 30)

y <- rnbinom(n_groups * n_obs, mu = exp(theta), size = 1)
y <- matrix(y, ncol = 200)
y <- as.data.frame(y)
# Create data list for Stan
data_list <- list(
  y = y,
  n_groups = n_groups,
  n_obs = n_obs
)

# Define the Stan model
model <- "
data {
  int<lower=0> n_groups; // number of groups
  int<lower=0> n_obs; // number of observations per group
  int<lower=0> y[n_groups, n_obs]; // observations
}
parameters {
  real mu[n_groups]; // group-level means
  real<lower=0> phi; // dispersion parameter
}
model {
  for (i in 1:n_groups) {
    mu[i] ~ normal(0, 1); // prior for group-level means
    for (j in 1:n_obs) {
      y[i, j] ~ neg_binomial_2(exp(mu[i]), phi); // likelihood
    }
  }
  phi ~ gamma(1, 1); // prior for dispersion parameter
}
"

# Compile the model
stan_model <- stan_model(model_code = model)

# Fit the model
fit <- sampling(stan_model, data = data_list, chains = 4, iter = 1000, warmup = 500, seed = 123)

# Print summary of the posterior distribution
print(fit)

# Plot posterior distribution of group-level means
ggplot(data.frame(fit$extract("mu")), aes(x = mu)) +
  geom_density(fill = "blue", alpha = 0.3) +
  xlab("Group-level mean") +
  ylab("Density") +
  ggtitle("Posterior distribution of group-level means")




