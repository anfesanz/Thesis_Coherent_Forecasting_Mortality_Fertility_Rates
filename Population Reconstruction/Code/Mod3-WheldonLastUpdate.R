#############################################################################
rm(list=ls(all=TRUE))
#Working directory
getwd()
setwd("/Users/felipesanchez/Library/CloudStorage/Dropbox-Personal/UoM/UoM from MA/Mortality by Education/")
#https://github.com/markalava/Bayesian-Reconstruction/blob/master/workshops/ALAP_2018/popReconstructWrappers/thailand_vignette/thailand_example.Rmd

## Install the package
library("devtools")
## Not run
## devtools::install_github(repo = "markalava/Bayesian-Reconstruction/workshops/ALAP_2018/popReconstructWrappers/package")
library(popReconstructWrappers)
## Create a directory for results
#dir.create("poprecontructionF")
setwd("poprecontructionF")

#Complete data into independent series
oldls <- c(ls(), "oldls")
data("Thailand_Example")
setdiff(ls(), oldls)


#force(asFertTHAI.mat) ASFR 1960, 1965, ..., 1995
#force(asSurvTHAI.mat) Male and Females survival proportions 1960, 1965, ..., 1995
#force(asMigTHAI.mat) Male and Female migration 1960, 1965, ..., 1995 (all are zero)
#force(baselineTHAI.mat) Male and Female Population 1960
#force(censusTHAI.mat) Male and Female Census Counts 1970, 1980, 1990, 2000
#force(srbTHAI.mat) sbr 1960, 1965, ..., 1995
#force(thaiFemale.sf) strange number growing .5 from 1960 until 1995.8 


## Fertility Rates and SRBs
asFertTHAI.mat[1:12, 1:5]

## Survival and Migration Proportions, Population Counts 
lapply(asSurvTHAI.mat, "[", i = 1:3, j = 1:5)

## Running the Reconstruction
#1. Set the seed:
set.seed(1)

#2. Set the number of iterations. This will typically be in the 100,000s 
n.iter <- 100
burn.in <- 10

#3. Convert elicited relative errors to hyperparamaters:
invGam.params <- make.hyper.params(absDev = list(fert = 0.1, surv = 0.1, mig = 0.2
                                                 ,pop = 0.1, srb = 0.1)
                                   ,prob = list(fert = 0.9, surv = 0.9, mig = 0.9, pop = 0.9, srb = 0.9)
                                   ,alpha = list(fert = 0.5, surv = 0.5, mig = 0.5, pop = 0.5, srb = 0.5)
                                   ,s.star = unlist(asSurvTHAI.mat)
)
invGam.params[1:2] 


#4. Set the function arguments
estModArgs <- 
  list(## Algorithm parameters
    n.iter = n.iter, burn.in = burn.in
    ## Inputs:
    
    ## Inverse gamma parameters
    ,al.f = invGam.params$al.f
    ,be.f = invGam.params$be.f
    ,al.s = invGam.params$al.s
    ,be.s = invGam.params$be.s
    ,al.g = invGam.params$al.g
    ,be.g = invGam.params$be.g
    ,al.n = invGam.params$al.n
    ,be.n = invGam.params$be.n
    ,al.srb = invGam.params$al.srb
    ,be.srb = invGam.params$be.srb
    ## Initial estimates 
    ,mean.f = asFertTHAI.mat
    ,mean.s = asSurvTHAI.mat
    ,mean.g = asMigTHAI.mat
    ,mean.b = baselineTHAI.mat
    ,mean.srb = srbTHAI.mat
    ,pop.data = censusTHAI.mat
    ## Start values
    ,start.f = asFertTHAI.mat
    ,start.s = asSurvTHAI.mat
    ,start.g = asMigTHAI.mat
    ,start.b = baselineTHAI.mat
    ,start.srb = srbTHAI.mat
    ,start.sigmasq.f = 5
    ,start.sigmasq.s = 5
    ,start.sigmasq.g = 5
    ,start.sigmasq.n = 5
    ,start.sigmasq.srb = 5
    ## Periods and age group size
    ,proj.periods = ncol(asFertTHAI.mat)
    ,age.size = 5
    ## Proposal variances and other
    ,prop.varcovar = thai.propvar
    ,verb = TRUE, progress.step = 1E3
  ) 

#6. Run, then save the reconstruction:
ThaiMcmc <- do.call(pop.est.sampler, args = estModArgs)
save(ThaiMcmc, file = "thai_mcmc.RData")

## Tuning 
#The MCMC algorithm needs to be tuned to ensure the chains have mixed well. This is indicated by Metropolis acceptance ratios. MCMC acceptance ratios between about 0.2 and 0.5 are acceptable. These can be plotted for each input parameter.
## Not run (see below)
## plot.acceptance.props(ThaiMcmc)
## Not run
try(
  conditional.variances(ThaiMcmc)
)

knitr::include_graphics("extras/img/Thai_20130612srb10_accProps.png")


## Post-process
#The output from the sampler, `ThaiMcmc`, contains the posterior distribution for the input parameters, namely fertiltiy rates, survival proportions, etc. To get posterior samples for other parameters, such as mortality rates, TFR, they can be transformed using `post.process.recon()`.
ThaiCounts <- post.process.recon(ThaiMcmc
                                 ,sep.factors = list(female = thaiFemale.sf
                                                     ,male = thaiMale.sf)
                                 ,name.pref = "Thai."
                                 ,name.suf = ""
)

# Generate Prior Distribution Samples
#If you want to create plots that compare the prior and posterior distributions, run `sample.from.prior()`. Note: this can take a long time. These also need post-processing to get mortality rates, TFR, etc. Use `post.process.recon` again as below.
#This method used as the joint distribution of the parameters under the prior is complicated by the need to ensure that the population remains positive; without this constraint, very extreme migration could imply negative populations. The distributions for rates, such as mortality rates, are also complicated because they depend not only on mortality parameters but also the size of the  population at-risk at each age group and time period. Drawing a large, random sample saves us from having to work all this out by hand.
ThaiPrior <-
  sample.from.prior(n.iter = 10, #Change to at least 1e3
                    al.f = invGam.params$al.f,
                    be.f = invGam.params$be.f,
                    al.s = invGam.params$al.s,
                    be.s = invGam.params$be.s,
                    al.g = invGam.params$al.g,
                    be.g = invGam.params$be.g,
                    al.n = invGam.params$al.n,
                    be.n = invGam.params$be.n,
                    al.srb = invGam.params$al.srb,
                    be.srb = invGam.params$be.srb,
                    mean.f = asFertTHAI.mat,
                    mean.s = asSurvTHAI.mat,
                    mean.g = asMigTHAI.mat,
                    mean.b = baselineTHAI.mat,
                    mean.srb = srbTHAI.mat,
                    age.size = 5,
                    name.pref = "Thai.",
                    name.suf = "")

ThaiPriorCounts <- post.process.recon(ThaiPrior,
                                      sep.factors = list(female = thaiFemale.sf
                                                         ,male = thaiMale.sf),
                                      name.pref = "Thai.",
                                      name.suf = "Prior")

# Summarizing Results
qrecon <- get.quantiles.recon(param = c("e0", "tfr", "total.mig.count","cohort.nq0", 
                                "period.nq0", "IMR", "mort.rate", "mig.rate", 
                                "birth.count", "death.count", "surv.prop", "srb", "baseline.count"),
                              results.recon = ThaiMcmc,
                              results.post.process.recon = ThaiCounts,
                              results.prior = ThaiPrior,
                              results.post.process.prior = ThaiPriorCounts) 

head(qrecon)
## Look at a particular parameter:
head(subset(qrecon, param == "mort.rate"))



library(ggplot2)

gp <-
  ggplot(subset(qrecon, param == "tfr"), aes(x = year, y = param.50pctl)) +
  geom_line(aes(col = legend), size = 1.1) + geom_point(aes(col = legend)) +
  geom_ribbon(aes(ymin = param.2.5pctl, ymax = param.97.5pctl, fill = legend), alpha = 0.2)

knitr::include_graphics("extras/img/Thai_tfr_priorpost_q95_20130612srb10.png")
knitr::include_graphics("extras/img/Thai_leb_priorpost_q95_female_20130612srb10.png")
knitr::include_graphics("extras/img/Thai_leb_priorpost_q95_male_20130612srb10.png")

