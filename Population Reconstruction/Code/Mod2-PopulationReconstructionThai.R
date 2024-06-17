

#https://github.com/ihmeuw-demographics/demCore/tree/fd9fd6873ced9e1761ec24c497b6fb9d26b829bd

remotes::install_github("ihmeuw-demographics/hierarchyUtils")
remotes::install_github("ihmeuw-demographics/demCore")

library(demCore)


# the settings for this run of ccmpp
thailand_settings <- list(
  years = seq(1960, 1995, 5),
  sexes = c("female", "male"),
  ages = seq(0, 80, 5),
  ages_mortality = seq(0, 85, 5),
  ages_asfr = seq(15, 45, 5)
)

a=demCore::thailand_initial_estimates

thailand_population <- ccmpp(
  inputs = demCore::thailand_initial_estimates,
  settings = thailand_settings
)
str(thailand_population)

