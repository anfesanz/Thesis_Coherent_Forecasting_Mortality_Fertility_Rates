# Complete_CRPS_Workflow.R
# Demonstrates full pipeline from Julia-like output to CRPS evaluation
# This Script responds the reviews request to do an assessment of forecast distributions.
# See Gneiting and Raftery (2004); Gneiting and Raftery (2008).  

#This are the posterior draws of the models (in Julia)
#chain_model1f1.jls
#chain_model1f2.jls
#chain_model1f3.jls
#chain_model1f4.jls
#chain_model1m1.jls
#chain_model1m2.jls
#chain_model1m3.jls
#chain_model1m4.jls
#chain_model2f.jls
#chain_model2m.jls
#chain_model3f.jls
#chain_model3m.jls
#chain_model4.jls


library(scoringRules)
library(ggplot2)
library(reshape2)

#Change directory to where the .jls files are located /Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/data/mortality/
setwd("/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/data/")

#Import the csv files- Mortality
m_model1f1 <- read.csv("mortality/chain_model1f1.csv")
m_model1f2 <- read.csv("mortality/chain_model1f2.csv")
m_model1f3 <- read.csv("mortality/chain_model1f3.csv")
m_model1f4 <- read.csv("mortality/chain_model1f4.csv")
m_model1m1 <- read.csv("mortality/chain_model1m1.csv")
m_model1m2 <- read.csv("mortality/chain_model1m2.csv")
m_model1m3 <- read.csv("mortality/chain_model1m3.csv")
m_model1m4 <- read.csv("mortality/chain_model1m4.csv")
m_model2f <- read.csv("mortality/chain_model2f.csv")
m_model2m <- read.csv("mortality/chain_model2m.csv")
m_model3f <- read.csv("mortality/chain_model3f.csv")
m_model3m <- read.csv("mortality/chain_model3m.csv")
m_model4 <- read.csv("mortality/chain_model4.csv")

#Import the csv files- Fertility
f_model1f1 <- read.csv("fertility/chain_model1f1.csv")
f_model1f2 <- read.csv("fertility/chain_model1f2.csv")
f_model1f3 <- read.csv("fertility/chain_model1f3.csv")
f_model1f4 <- read.csv("fertility/chain_model1f4.csv")
f_model2f <- read.csv("fertility/chain_model2f.csv")
f_model3f <- read.csv("fertility/chain_model3f.csv")

#names of the columns in the csv files
colnames(m_model1f1)
colnames(m_model1f2)
colnames(m_model1f3)
colnames(m_model1f4)
colnames(m_model1m1)
colnames(m_model1m2)
colnames(m_model1m3)
colnames(m_model1m4)
colnames(m_model2f)
colnames(m_model2m)
colnames(m_model3f)
colnames(m_model3m)
colnames(m_model4)

colnames(f_model1f1)
colnames(f_model1f2)
colnames(f_model1f3)
colnames(f_model1f4)
colnames(f_model2f)
colnames(f_model3f)


#Mortality. read csv /Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/data/mortality/exf1.csv
mxf1 <- log(read.csv("mortality/mxf1.csv")/(read.csv("mortality/exf1.csv")))  
mxf2 <- log(read.csv("mortality/mxf2.csv")/(read.csv("mortality/exf2.csv")))
mxf3 <- log(read.csv("mortality/mxf3.csv")/(read.csv("mortality/exf3.csv")))
mxf4 <- log(read.csv("mortality/mxf4.csv")/(read.csv("mortality/exf4.csv")))
mxm1 <- log(read.csv("mortality/mxm1.csv")/(read.csv("mortality/exm1.csv")))
mxm2 <- log(read.csv("mortality/mxm2.csv")/(read.csv("mortality/exm2.csv")))
mxm3 <- log(read.csv("mortality/mxm3.csv")/(read.csv("mortality/exm3.csv")))
mxm4 <- log(read.csv("mortality/mxm4.csv")/(read.csv("mortality/exm4.csv")))

#Fertility. read csv /Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/data/fertility/exf1.csv
fxf1 <- log(read.csv("fertility/fx1.csv")/(read.csv("fertility/exfe1.csv")))
fxf2 <- log(read.csv("fertility/fx2.csv")/(read.csv("fertility/exfe2.csv")))
fxf3 <- log(read.csv("fertility/fx3.csv")/(read.csv("fertility/exfe3.csv")))
fxf4 <- log(read.csv("fertility/fx4.csv")/(read.csv("fertility/exfe4.csv")))


mean(m_model1f1$logmu.1.1.)

# CRPS for m_model1f1 against the observed 11 x 21 matrix mxf1
# Each logmu[i,j] column contains posterior draws for one cell of mxf1.
build_logmu_draw_matrix <- function(chain_df, n_age = 11, n_year = 21, n_draws = NULL) {
  if (is.null(n_draws)) {
    n_draws <- nrow(chain_df)
  }

  draw_matrix <- matrix(NA_real_, nrow = n_age * n_year, ncol = n_draws)
  row_idx <- 1

  for (i in seq_len(n_age)) {
    for (j in seq_len(n_year)) {
      col_candidates <- c(
        sprintf("logmu[%d,%d]", i, j),
        sprintf("logmu.%d.%d.", i, j)
      )
      col_name <- col_candidates[col_candidates %in% names(chain_df)][1]

      if (is.na(col_name)) {
        stop(sprintf("Column for logmu[%d,%d] was not found in the chain.", i, j))
      }

      draw_matrix[row_idx, ] <- chain_df[seq_len(n_draws), col_name]
      row_idx <- row_idx + 1
    }
  }

  draw_matrix
}

build_named_draw_matrix <- function(chain_df, base_name, n_age = 11, n_year = 21, n_draws = NULL) {
  if (is.null(n_draws)) {
    n_draws <- nrow(chain_df)
  }

  draw_matrix <- matrix(NA_real_, nrow = n_age * n_year, ncol = n_draws)
  row_idx <- 1

  for (i in seq_len(n_age)) {
    for (j in seq_len(n_year)) {
      col_candidates <- c(
        sprintf("%s[%d,%d]", base_name, i, j),
        sprintf("%s.%d.%d.", base_name, i, j)
      )
      col_name <- col_candidates[col_candidates %in% names(chain_df)][1]

      if (is.na(col_name)) {
        stop(sprintf("Column for %s[%d,%d] was not found in the chain.", base_name, i, j))
      }

      draw_matrix[row_idx, ] <- chain_df[seq_len(n_draws), col_name]
      row_idx <- row_idx + 1
    }
  }

  draw_matrix
}


actuals_mxf1 <- as.vector(as.matrix(mxf1))
forecast_draws_f1_mod1 <- build_logmu_draw_matrix(m_model1f1, n_age = 11, n_year = 21)
crps_f1_mod1 <- crps_sample(y = actuals_mxf1, dat = forecast_draws_f1_mod1)
forecast_draws_f1_mod2 <- build_named_draw_matrix(m_model2f, base_name = "logmu1", n_age = 11, n_year = 21)
crps_f1_mod2 <- crps_sample(y = actuals_mxf1, dat = forecast_draws_f1_mod2)
forecast_draws_f1_mod3 <- build_named_draw_matrix(m_model3f, base_name = "logmu1", n_age = 11, n_year = 21)
crps_f1_mod3 <- crps_sample(y = actuals_mxf1, dat = forecast_draws_f1_mod3)
forecast_draws_f1_mod4 <- build_named_draw_matrix(m_model4, base_name = "logmu1", n_age = 11, n_year = 21)
crps_f1_mod4 <- crps_sample(y = actuals_mxf1, dat = forecast_draws_f1_mod4)

crps_mxf1 <- data.frame(
  Model = c("f1_mod1", "f1_mod2", "f1_mod3", "f1_mod4"),
  Mean_CRPS = c(
    mean(crps_f1_mod1),
    mean(crps_f1_mod2),
    mean(crps_f1_mod3),
    mean(crps_f1_mod4)
  ),
  Median_CRPS = c(
    median(crps_f1_mod1),
    median(crps_f1_mod2),
    median(crps_f1_mod3),
    median(crps_f1_mod4)
  ),
  SD_CRPS = c(
    sd(crps_f1_mod1),
    sd(crps_f1_mod2),
    sd(crps_f1_mod3),
    sd(crps_f1_mod4)
  )
)

print("=== CRPS for mxf1 across mortality models ===")
print(crps_mxf1)

actuals_mxf2 <- as.vector(as.matrix(mxf2))
forecast_draws_f2_mod1 <- build_logmu_draw_matrix(m_model1f2, n_age = 11, n_year = 21)
crps_f2_mod1 <- crps_sample(y = actuals_mxf2, dat = forecast_draws_f2_mod1)
forecast_draws_f2_mod2 <- build_named_draw_matrix(m_model2f, base_name = "logmu2", n_age = 11, n_year = 21)
crps_f2_mod2 <- crps_sample(y = actuals_mxf2, dat = forecast_draws_f2_mod2)
forecast_draws_f2_mod3 <- build_named_draw_matrix(m_model3f, base_name = "logmu2", n_age = 11, n_year = 21)
crps_f2_mod3 <- crps_sample(y = actuals_mxf2, dat = forecast_draws_f2_mod3)
forecast_draws_f2_mod4 <- build_named_draw_matrix(m_model4, base_name = "logmu2", n_age = 11, n_year = 21)
crps_f2_mod4 <- crps_sample(y = actuals_mxf2, dat = forecast_draws_f2_mod4) 

crps_mxf2 <- data.frame(
  Model = c("f2_mod1", "f2_mod2", "f2_mod3", "f2_mod4"),
  Mean_CRPS = c(
    mean(crps_f2_mod1),
    mean(crps_f2_mod2),
    mean(crps_f2_mod3),
    mean(crps_f2_mod4)
  ),
  Median_CRPS = c(
    median(crps_f2_mod1),
    median(crps_f2_mod2),
    median(crps_f2_mod3),
    median(crps_f2_mod4)
  ),
  SD_CRPS = c(
    sd(crps_f2_mod1),
    sd(crps_f2_mod2),
    sd(crps_f2_mod3),
    sd(crps_f2_mod4)
  )
)

print("=== CRPS for mxf2 across mortality models ===")
print(crps_mxf2)


actuals_mxf3 <- as.vector(as.matrix(mxf3))
forecast_draws_f3_mod1 <- build_logmu_draw_matrix(m_model1f3, n_age = 11, n_year = 21)
crps_f3_mod1 <- crps_sample(y = actuals_mxf3, dat = forecast_draws_f3_mod1)
forecast_draws_f3_mod2 <- build_named_draw_matrix(m_model2f, base_name = "logmu3", n_age = 11, n_year = 21)
crps_f3_mod2 <- crps_sample(y = actuals_mxf3, dat = forecast_draws_f3_mod2)
forecast_draws_f3_mod3 <- build_named_draw_matrix(m_model3f, base_name = "logmu3", n_age = 11, n_year = 21)
crps_f3_mod3 <- crps_sample(y = actuals_mxf3, dat = forecast_draws_f3_mod3)
forecast_draws_f3_mod4 <- build_named_draw_matrix(m_model4, base_name = "logmu3", n_age = 11, n_year = 21)
crps_f3_mod4 <- crps_sample(y = actuals_mxf3, dat = forecast_draws_f3_mod4)

crps_mxf3 <- data.frame(
  Model = c("f3_mod1", "f3_mod2", "f3_mod3", "f3_mod4"),
  Mean_CRPS = c(
    mean(crps_f3_mod1),
    mean(crps_f3_mod2),
    mean(crps_f3_mod3),
    mean(crps_f3_mod4)
  ),
  Median_CRPS = c(
    median(crps_f3_mod1),
    median(crps_f3_mod2),
    median(crps_f3_mod3),
    median(crps_f3_mod4)
  ),
  SD_CRPS = c(
    sd(crps_f3_mod1),
    sd(crps_f3_mod2),
    sd(crps_f3_mod3),
    sd(crps_f3_mod4)
  )
)

print("=== CRPS for mxf3 across mortality models ===")
print(crps_mxf3)


actuals_mxf4 <- as.vector(as.matrix(mxf4))
forecast_draws_f4_mod1 <- build_logmu_draw_matrix(m_model1f4, n_age = 11, n_year = 21)
crps_f4_mod1 <- crps_sample(y = actuals_mxf4, dat = forecast_draws_f4_mod1)
forecast_draws_f4_mod2 <- build_named_draw_matrix(m_model2f, base_name = "logmu4", n_age = 11, n_year = 21)
crps_f4_mod2 <- crps_sample(y = actuals_mxf4, dat = forecast_draws_f4_mod2)
forecast_draws_f4_mod3 <- build_named_draw_matrix(m_model3f, base_name = "logmu4", n_age = 11, n_year = 21)
crps_f4_mod3 <- crps_sample(y = actuals_mxf4, dat = forecast_draws_f4_mod3)
forecast_draws_f4_mod4 <- build_named_draw_matrix(m_model4, base_name = "logmu4", n_age = 11, n_year = 21)
crps_f4_mod4 <- crps_sample(y = actuals_mxf4, dat = forecast_draws_f4_mod4)

crps_mxf4 <- data.frame(
  Model = c("f4_mod1", "f4_mod2", "f4_mod3", "f4_mod4"),
  Mean_CRPS = c(
    mean(crps_f4_mod1),
    mean(crps_f4_mod2),
    mean(crps_f4_mod3),
    mean(crps_f4_mod4)
  ),
  Median_CRPS = c(
    median(crps_f4_mod1),
    median(crps_f4_mod2),
    median(crps_f4_mod3),
    median(crps_f4_mod4)
  ),
  SD_CRPS = c(
    sd(crps_f4_mod1),
    sd(crps_f4_mod2),
    sd(crps_f4_mod3),
    sd(crps_f4_mod4)
  )
)

print("=== CRPS for mxf4 across mortality models ===")
print(crps_mxf4)


actuals_mxm1 <- as.vector(as.matrix(mxm1))
forecast_draws_m1_mod1 <- build_logmu_draw_matrix(m_model1m1, n_age = 11, n_year = 21)
crps_m1_mod1 <- crps_sample(y = actuals_mxm1, dat = forecast_draws_m1_mod1)
forecast_draws_m1_mod2 <- build_named_draw_matrix(m_model2m, base_name = "logmu1", n_age = 11, n_year = 21)
crps_m1_mod2 <- crps_sample(y = actuals_mxm1, dat = forecast_draws_m1_mod2)
forecast_draws_m1_mod3 <- build_named_draw_matrix(m_model3m, base_name = "logmu1", n_age = 11, n_year = 21)
crps_m1_mod3 <- crps_sample(y = actuals_mxm1, dat = forecast_draws_m1_mod3)
forecast_draws_m1_mod4 <- build_named_draw_matrix(m_model4, base_name = "logmu5", n_age = 11, n_year = 21)
crps_m1_mod4 <- crps_sample(y = actuals_mxm1, dat = forecast_draws_m1_mod4)

crps_mxm1 <- data.frame(
  Model = c("m1_mod1", "m1_mod2", "m1_mod3", "m1_mod4"),
  Mean_CRPS = c(
    mean(crps_m1_mod1),
    mean(crps_m1_mod2),
    mean(crps_m1_mod3),
    mean(crps_m1_mod4)
  ),
  Median_CRPS = c(
    median(crps_m1_mod1),
    median(crps_m1_mod2),
    median(crps_m1_mod3),
    median(crps_m1_mod4)
  ),
  SD_CRPS = c(
    sd(crps_m1_mod1),
    sd(crps_m1_mod2),
    sd(crps_m1_mod3),
    sd(crps_m1_mod4)
  )
)

print("=== CRPS for mxm1 across mortality models ===")
print(crps_mxm1)


actuals_mxm2 <- as.vector(as.matrix(mxm2))
forecast_draws_m2_mod1 <- build_logmu_draw_matrix(m_model1m2, n_age = 11, n_year = 21)
crps_m2_mod1 <- crps_sample(y = actuals_mxm2, dat = forecast_draws_m2_mod1)
forecast_draws_m2_mod2 <- build_named_draw_matrix(m_model2m, base_name = "logmu2", n_age = 11, n_year = 21)
crps_m2_mod2 <- crps_sample(y = actuals_mxm2, dat = forecast_draws_m2_mod2)
forecast_draws_m2_mod3 <- build_named_draw_matrix(m_model3m, base_name = "logmu2", n_age = 11, n_year = 21)
crps_m2_mod3 <- crps_sample(y = actuals_mxm2, dat = forecast_draws_m2_mod3)
forecast_draws_m2_mod4 <- build_named_draw_matrix(m_model4, base_name = "logmu6", n_age = 11, n_year = 21)
crps_m2_mod4 <- crps_sample(y = actuals_mxm2, dat = forecast_draws_m2_mod4)

crps_mxm2 <- data.frame(
  Model = c("m2_mod1", "m2_mod2", "m2_mod3", "m2_mod4"),
  Mean_CRPS = c(
    mean(crps_m2_mod1),
    mean(crps_m2_mod2),
    mean(crps_m2_mod3),
    mean(crps_m2_mod4)
  ),
  Median_CRPS = c(
    median(crps_m2_mod1),
    median(crps_m2_mod2),
    median(crps_m2_mod3),
    median(crps_m2_mod4)
  ),
  SD_CRPS = c(
    sd(crps_m2_mod1),
    sd(crps_m2_mod2),
    sd(crps_m2_mod3),
    sd(crps_m2_mod4)
  )
)

print("=== CRPS for mxm2 across mortality models ===")
print(crps_mxm2)

actuals_mxm3 <- as.vector(as.matrix(mxm3))
forecast_draws_m3_mod1 <- build_logmu_draw_matrix(m_model1m3, n_age = 11, n_year = 21)
crps_m3_mod1 <- crps_sample(y = actuals_mxm3, dat = forecast_draws_m3_mod1)
forecast_draws_m3_mod2 <- build_named_draw_matrix(m_model2m, base_name = "logmu3", n_age = 11, n_year = 21)
crps_m3_mod2 <- crps_sample(y = actuals_mxm3, dat = forecast_draws_m3_mod2)
forecast_draws_m3_mod3 <- build_named_draw_matrix(m_model3m, base_name = "logmu3", n_age = 11, n_year = 21)
crps_m3_mod3 <- crps_sample(y = actuals_mxm3, dat = forecast_draws_m3_mod3)
forecast_draws_m3_mod4 <- build_named_draw_matrix(m_model4, base_name = "logmu7", n_age = 11, n_year = 21)
crps_m3_mod4 <- crps_sample(y = actuals_mxm3, dat = forecast_draws_m3_mod4)

crps_mxm3 <- data.frame(
  Model = c("m3_mod1", "m3_mod2", "m3_mod3", "m3_mod4"),
  Mean_CRPS = c(
    mean(crps_m3_mod1),
    mean(crps_m3_mod2),
    mean(crps_m3_mod3),
    mean(crps_m3_mod4)
  ),
  Median_CRPS = c(
    median(crps_m3_mod1),
    median(crps_m3_mod2),
    median(crps_m3_mod3),
    median(crps_m3_mod4)
  ),
  SD_CRPS = c(
    sd(crps_m3_mod1),
    sd(crps_m3_mod2),
    sd(crps_m3_mod3),
    sd(crps_m3_mod4)
  )
)

print("=== CRPS for mxm3 across mortality models ===")
print(crps_mxm3)

actuals_mxm4 <- as.vector(as.matrix(mxm4))
forecast_draws_m4_mod1 <- build_logmu_draw_matrix(m_model1m4, n_age = 11, n_year = 21)
crps_m4_mod1 <- crps_sample(y = actuals_mxm4, dat = forecast_draws_m4_mod1)
forecast_draws_m4_mod2 <- build_named_draw_matrix(m_model2m, base_name = "logmu4", n_age = 11, n_year = 21)
crps_m4_mod2 <- crps_sample(y = actuals_mxm4, dat = forecast_draws_m4_mod2)
forecast_draws_m4_mod3 <- build_named_draw_matrix(m_model3m, base_name = "logmu4", n_age = 11, n_year = 21)
crps_m4_mod3 <- crps_sample(y = actuals_mxm4, dat = forecast_draws_m4_mod3)
forecast_draws_m4_mod4 <- build_named_draw_matrix(m_model4, base_name = "logmu8", n_age = 11, n_year = 21)
crps_m4_mod4 <- crps_sample(y = actuals_mxm4, dat = forecast_draws_m4_mod4)

crps_mxm4 <- data.frame(
  Model = c("m4_mod1", "m4_mod2", "m4_mod3", "m4_mod4"),
  Mean_CRPS = c(
    mean(crps_m4_mod1),
    mean(crps_m4_mod2),
    mean(crps_m4_mod3),
    mean(crps_m4_mod4)
  ),
  Median_CRPS = c(
    median(crps_m4_mod1),
    median(crps_m4_mod2),
    median(crps_m4_mod3),
    median(crps_m4_mod4)
  ),
  SD_CRPS = c(
    sd(crps_m4_mod1),
    sd(crps_m4_mod2),
    sd(crps_m4_mod3),
    sd(crps_m4_mod4)
   )
)

print("=== CRPS for mxm4 across mortality models ===")
print(crps_mxm4)

# Print the CRPS results across the educational groups for the 4 mortality models
print(crps_mxf1)
print(crps_mxf2)
print(crps_mxf3)
print(crps_mxf4)
print(crps_mxm1)
print(crps_mxm2)
print(crps_mxm3)
print(crps_mxm4)

model_labels <- c("Model A", "Model B", "Model C", "Model D")

female_mean_crps <- cbind(
  crps_mxf1$Mean_CRPS,
  crps_mxf2$Mean_CRPS,
  crps_mxf3$Mean_CRPS,
  crps_mxf4$Mean_CRPS
)

male_mean_crps <- cbind(
  crps_mxm1$Mean_CRPS,
  crps_mxm2$Mean_CRPS,
  crps_mxm3$Mean_CRPS,
  crps_mxm4$Mean_CRPS
)

summary_mean_crps <- data.frame(
  Model = model_labels,
  CRPS_Females = rowMeans(female_mean_crps),
  CRPS_Males = rowMeans(male_mean_crps)
)

print("=== Mean CRPS by Model and Sex ===")
print(summary_mean_crps)

table_to_latex <- c(
  "\\begin{table}[ht]",
  "\\centering",
  "\\begin{tabular}{lrr}",
  "\\hline",
  "Model & CRPS Females & CRPS Males \\\\",
  "\\hline",
  apply(summary_mean_crps, 1, function(row) {
    sprintf("%s & %.6f & %.6f \\\\",
            row[["Model"]],
            as.numeric(row[["CRPS_Females"]]),
            as.numeric(row[["CRPS_Males"]]))
  }),
  "\\hline",
  "\\end{tabular}",
  "\\caption{Mean CRPS by model and sex}",
  "\\label{tab:mean_crps_by_sex}",
  "\\end{table}"
)

writeLines(table_to_latex, "summary_mean_crps.tex")




#Fertility
##########
actuals_fxf1 <- as.vector(as.matrix(fxf1))
forecast_fertility_draws_f1_mod1 <- build_logmu_draw_matrix(f_model1f1, n_age = 6, n_year = 21)
crps_fertility_f1_mod1 <- crps_sample(y = actuals_fxf1, dat = forecast_fertility_draws_f1_mod1)
forecast_fertility_draws_f1_mod2 <- build_named_draw_matrix(f_model2f, base_name = "logmu1", n_age = 6, n_year = 21)
crps_fertility_f1_mod2 <- crps_sample(y = actuals_fxf1, dat = forecast_fertility_draws_f1_mod2)
forecast_fertility_draws_f1_mod3 <- build_named_draw_matrix(f_model3f, base_name = "logmu1", n_age = 6, n_year = 21)
crps_fertility_f1_mod3 <- crps_sample(y = actuals_fxf1, dat = forecast_fertility_draws_f1_mod3)

crps_fxf1 <- data.frame(
  Model = c("f1_mod1", "f1_mod2", "f1_mod3"),
  Mean_CRPS = c(
    mean(crps_fertility_f1_mod1),
    mean(crps_fertility_f1_mod2),
    mean(crps_fertility_f1_mod3)
  ),
  Median_CRPS = c(
    median(crps_fertility_f1_mod1),
    median(crps_fertility_f1_mod2),
    median(crps_fertility_f1_mod3)
  ),
  SD_CRPS = c(
    sd(crps_fertility_f1_mod1),
    sd(crps_fertility_f1_mod2),
    sd(crps_fertility_f1_mod3)
  )
)
print("=== CRPS for fxf1 across fertility models ===")
print(crps_fxf1)

actuals_fxf2 <- as.vector(as.matrix(fxf2))
forecast_fertility_draws_f2_mod1 <- build_logmu_draw_matrix(f_model1f2, n_age = 6, n_year = 21)
crps_fertility_f2_mod1 <- crps_sample(y = actuals_fxf2, dat = forecast_fertility_draws_f2_mod1)
forecast_fertility_draws_f2_mod2 <- build_named_draw_matrix(f_model2f, base_name = "logmu2", n_age = 6, n_year = 21)
crps_fertility_f2_mod2 <- crps_sample(y = actuals_fxf2, dat = forecast_fertility_draws_f2_mod2)
forecast_fertility_draws_f2_mod3 <- build_named_draw_matrix(f_model3f, base_name = "logmu2", n_age = 6, n_year = 21)
crps_fertility_f2_mod3 <- crps_sample(y = actuals_fxf2, dat = forecast_fertility_draws_f2_mod3)

crps_fxf2 <- data.frame(
  Model = c("f2_mod1", "f2_mod2", "f2_mod3"),
  Mean_CRPS = c(
    mean(crps_fertility_f2_mod1),
    mean(crps_fertility_f2_mod2),
    mean(crps_fertility_f2_mod3)
  ),
  Median_CRPS = c(
    median(crps_fertility_f2_mod1),
    median(crps_fertility_f2_mod2),
    median(crps_fertility_f2_mod3)
  ),
  SD_CRPS = c(
    sd(crps_fertility_f2_mod1),
    sd(crps_fertility_f2_mod2),
    sd(crps_fertility_f2_mod3)
  )
)
print("=== CRPS for fxf2 across fertility models ===")
print(crps_fxf2)

actuals_fxf3 <- as.vector(as.matrix(fxf3))
forecast_fertility_draws_f3_mod1 <- build_logmu_draw_matrix(f_model1f3, n_age = 6, n_year = 21)
crps_fertility_f3_mod1 <- crps_sample(y = actuals_fxf3, dat = forecast_fertility_draws_f3_mod1)
forecast_fertility_draws_f3_mod2 <- build_named_draw_matrix(f_model2f, base_name = "logmu3", n_age = 6, n_year = 21)
crps_fertility_f3_mod2 <- crps_sample(y = actuals_fxf3, dat = forecast_fertility_draws_f3_mod2)
forecast_fertility_draws_f3_mod3 <- build_named_draw_matrix(f_model3f, base_name = "logmu3", n_age = 6, n_year = 21)
crps_fertility_f3_mod3 <- crps_sample(y = actuals_fxf3, dat = forecast_fertility_draws_f3_mod3)

crps_fxf3 <- data.frame(
  Model = c("f3_mod1", "f3_mod2", "f3_mod3"),
  Mean_CRPS = c(
    mean(crps_fertility_f3_mod1),
    mean(crps_fertility_f3_mod2),
    mean(crps_fertility_f3_mod3)
  ),
  Median_CRPS = c(
    median(crps_fertility_f3_mod1),
    median(crps_fertility_f3_mod2),
    median(crps_fertility_f3_mod3)
  ),
  SD_CRPS = c(
    sd(crps_fertility_f3_mod1),
    sd(crps_fertility_f3_mod2),
    sd(crps_fertility_f3_mod3)
  )
)
print("=== CRPS for fxf3 across fertility models ===")
print(crps_fxf3)

actuals_fxf4 <- as.vector(as.matrix(fxf4))
forecast_fertility_draws_f4_mod1 <- build_logmu_draw_matrix(f_model1f4, n_age = 6, n_year = 21)
crps_fertility_f4_mod1 <- crps_sample(y = actuals_fxf4, dat = forecast_fertility_draws_f4_mod1)
forecast_fertility_draws_f4_mod2 <- build_named_draw_matrix(f_model2f, base_name = "logmu4", n_age = 6, n_year = 21)
crps_fertility_f4_mod2 <- crps_sample(y = actuals_fxf4, dat = forecast_fertility_draws_f4_mod2)
forecast_fertility_draws_f4_mod3 <- build_named_draw_matrix(f_model3f, base_name = "logmu4", n_age = 6, n_year = 21)
crps_fertility_f4_mod3 <- crps_sample(y = actuals_fxf4, dat = forecast_fertility_draws_f4_mod3)

crps_fxf4 <- data.frame(
  Model = c("f4_mod1", "f4_mod2", "f4_mod3"),
  Mean_CRPS = c(
    mean(crps_fertility_f4_mod1),
    mean(crps_fertility_f4_mod2),
    mean(crps_fertility_f4_mod3)
  ),
  Median_CRPS = c(
    median(crps_fertility_f4_mod1),
    median(crps_fertility_f4_mod2),
    median(crps_fertility_f4_mod3)
  ),
  SD_CRPS = c(
    sd(crps_fertility_f4_mod1),
    sd(crps_fertility_f4_mod2),
    sd(crps_fertility_f4_mod3)
  )
)
print("=== CRPS for fxf4 across fertility models ===")
print(crps_fxf4)



model_labels <- c("Model A", "Model B", "Model C")

mean_crps <- cbind(
  crps_fxf1$Mean_CRPS,
  crps_fxf2$Mean_CRPS,
  crps_fxf3$Mean_CRPS,
  crps_fxf4$Mean_CRPS
)

summary_mean_crps_fertility <- data.frame(
  Model = model_labels,
  CRPS_Females = rowMeans(mean_crps)
)

print("=== Mean CRPS by Model and Sex ===")
print(summary_mean_crps_fertility)

table_to_latex_fertility <- function(summary_df) {
  table_rows <- vapply(
    seq_len(nrow(summary_df)),
    function(i) {
      sprintf(
        "%s & %.6f \\\\",
        summary_df$Model[i],
        summary_df$CRPS_Females[i]
      )
    },
    character(1)
  )

  c(
    "\\begin{table}[ht]",
    "\\centering",
    "\\begin{tabular}{lr}",
    "\\hline",
    "Model & CRPS \\\\",
    "\\hline",
    table_rows,
    "\\hline",
    "\\end{tabular}",
    "\\caption{Mean CRPS by model}",
    "\\label{tab:mean_crps_fertility}",
    "\\end{table}"
  )
}

writeLines(
  table_to_latex_fertility(summary_mean_crps_fertility),
  "summary_mean_crps_fertility.tex"
)

