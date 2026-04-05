# Complete_CRPS_Workflow.R

library(scoringRules)
library(ggplot2)
library(reshape2)



# Manual CRPS calculation for a single forecast to understand it
manual_crps_demo <- function(forecast_draws, actual, n_points = 100) {
  # Convert samples to empirical CDF
  forecast_sorted <- sort(forecast_draws)
  forecast_ecdf <- ecdf(forecast_sorted)
  
  # Create grid of evaluation points
  z_grid <- seq(min(forecast_sorted) - 1, max(forecast_sorted) + 1, 
                length.out = n_points)
  
  # Calculate CRPS = ∫ (F(z) - 1{y ≤ z})² dz
  crps_values <- numeric(length(z_grid))
  for(i in seq_along(z_grid)) {
    F_z <- forecast_ecdf(z_grid[i])
    indicator <- ifelse(actual <= z_grid[i], 1, 0)
    crps_values[i] <- (F_z - indicator)^2
  }
  
  # Numerical integration using trapezoidal rule
  crps <- sum(diff(z_grid) * (crps_values[-1] + crps_values[-length(crps_values)]) / 2)
  
  # Also return the components for visualization
  return(list(
    crps = crps,
    z_grid = z_grid,
    squared_error = crps_values
  ))
}



# ---- 1. SIMULATE JULIA/TURING OUTPUT ----
# In practice, you'd load from CSV: 
# forecasts <- as.matrix(read.csv("forecast_draws.csv"))
# actuals <- read.csv("actuals.csv")[,1]

set.seed(123)
n_forecasts <- 20
n_mcmc_draws <- 1000

# True DGP: Y_t ~ Gamma(shape=5, rate=0.5)
true_shape <- 5
true_rate <- 0.5

# Generate actuals
actuals <- rgamma(n_forecasts, shape = true_shape, rate = true_rate)

# Model 1: Correct specification (Gamma)
forecasts_correct <- matrix(rgamma(n_forecasts * n_mcmc_draws, 
                                   shape = true_shape, rate = true_rate),
                            nrow = n_mcmc_draws, ncol = n_forecasts)

# Model 2: Misspecified (Normal, ignoring positivity)
forecasts_misspecified <- matrix(rnorm(n_forecasts * n_mcmc_draws,
                                       mean = true_shape/true_rate,  # true mean
                                       sd = sqrt(true_shape/true_rate^2)),  # true sd
                                 nrow = n_mcmc_draws, ncol = n_forecasts)

# Model 3: Biased and overconfident
forecasts_bad <- matrix(rnorm(n_forecasts * n_mcmc_draws,
                              mean = (true_shape/true_rate) * 1.5,  # 50% high bias
                              sd = sqrt(true_shape/true_rate^2) * 0.5),  # too narrow
                        nrow = n_mcmc_draws, ncol = n_forecasts)

# ---- 2. COMPUTE CRPS ----
crps_correct <- crps_sample(y = actuals, dat = t(forecasts_correct))
crps_misspecified <- crps_sample(y = actuals, dat = t(forecasts_misspecified))
crps_bad <- crps_sample(y = actuals, dat = t(forecasts_bad))

# ---- 3. RESULTS AND STATISTICAL TEST ----
results_summary <- data.frame(
  Model = c("Correct (Gamma)", "Misspecified (Normal)", "Biased/Overconfident"),
  Mean_CRPS = c(mean(crps_correct), mean(crps_misspecified), mean(crps_bad)),
  Median_CRPS = c(median(crps_correct), median(crps_misspecified), median(crps_bad)),
  SE_CRPS = c(sd(crps_correct)/sqrt(n_forecasts), 
              sd(crps_misspecified)/sqrt(n_forecasts),
              sd(crps_bad)/sqrt(n_forecasts))
)

print("=== CRPS Results ===")
print(results_summary)

# ---- 4. DIEBOLD-MARIANO TEST FOR SIGNIFICANCE ----
# Test if Model 1 significantly outperforms Model 2
dm_test <- function(score1, score2) {
  d <- score1 - score2  # positive means model2 better (lower CRPS)
  n <- length(d)
  dm_stat <- mean(d) / (sd(d) / sqrt(n))
  p_value <- 2 * pt(-abs(dm_stat), df = n-1)
  return(list(statistic = dm_stat, p_value = p_value))
}

dm_1_vs_2 <- dm_test(crps_correct, crps_misspecified)
dm_1_vs_3 <- dm_test(crps_correct, crps_bad)

cat("\n=== Diebold-Mariano Tests ===\n")
cat(sprintf("Correct vs Misspecified: DM = %.3f, p = %.4f\n", 
            dm_1_vs_2$statistic, dm_1_vs_2$p_value))
cat(sprintf("Correct vs Biased: DM = %.3f, p = %.4f\n", 
            dm_1_vs_3$statistic, dm_1_vs_3$p_value))

# ---- 5. VISUALIZATION ----
# Time series of CRPS
crps_ts <- data.frame(
  Time = rep(1:n_forecasts, 3),
  CRPS = c(crps_correct, crps_misspecified, crps_bad),
  Model = rep(c("Correct", "Misspecified", "Biased"), each = n_forecasts)
)

p4 <- ggplot(crps_ts, aes(x = Time, y = CRPS, color = Model)) +
  geom_line(size = 0.8) +
  geom_point(size = 1.5) +
  labs(title = "CRPS Over Time by Model",
       subtitle = "Lower values indicate better probabilistic forecasts",
       y = "CRPS") +
  theme_minimal() +
  scale_color_manual(values = c("Correct" = "#2E8B57", 
                                "Misspecified" = "#DAA520", 
                                "Biased" = "#CD5C5C"))

print(p4)

# Boxplot comparison
p5 <- ggplot(crps_ts, aes(x = Model, y = CRPS, fill = Model)) +
  geom_boxplot() +
  labs(title = "CRPS Distribution by Model",
       y = "CRPS") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip()

print(p5)

# ---- 6. EXPORT RESULTS FOR REPORTING ----
write.csv(results_summary, "crps_summary.csv", row.names = FALSE)
saveRDS(list(crps_correct = crps_correct, 
             crps_misspecified = crps_misspecified,
             crps_bad = crps_bad,
             actuals = actuals), 
        "crps_results.rds")
