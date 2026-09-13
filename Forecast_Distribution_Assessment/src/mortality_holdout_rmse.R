#!/usr/bin/env Rscript

################################################################################
# Created by          : Felipe Sanchez
# Contact             : anfesanz@gmail.com
# Date                : 2026-09-13
# Language            : R
# Script Name / Ref.  : mortality_holdout_rmse.R
# Description         : Calculates holdout RMSE, MSE, mean error, and CRPS for
#                       labelled 2014-2018 mortality forecasts from Models A-D.
# Inputs              : MORTALITY_FORECAST_DIR (13 forecast_log_rates_draws.csv
#                       files exported by the rerun HPC mortality models).
# Outputs             : CSV metrics and forecasts, LaTeX RMSE and CRPS tables,
#                       and scale-validation diagnostics in outputs/
#                       revised_evaluation_tables/.
################################################################################

forecast_root <- Sys.getenv("MORTALITY_FORECAST_DIR", unset = "")
if (!nzchar(forecast_root) || !dir.exists(forecast_root)) {
  stop("Set MORTALITY_FORECAST_DIR to the HPC output/evaluation_local_pieces directory.", call. = FALSE)
}

output_root <- file.path(getwd(), "outputs", "revised_evaluation_tables")
csv_dir <- file.path(output_root, "csv")
latex_dir <- file.path(output_root, "latex")
diagnostics_dir <- file.path(output_root, "diagnostics")
for (path in c(csv_dir, latex_dir, diagnostics_dir)) dir.create(path, recursive = TRUE, showWarnings = FALSE)

model_ids <- c("mortality_a", "mortality_b", "mortality_c", "mortality_d")
model_labels <- c(mortality_a = "Model A", mortality_b = "Model B", mortality_c = "Model C", mortality_d = "Model D")
holdout_years <- 2014:2018

assert_true <- function(condition, message) if (!isTRUE(condition)) stop(message, call. = FALSE)

files <- unlist(lapply(model_ids, function(model) {
  Sys.glob(file.path(forecast_root, model, "*", "forecast_log_rates_draws.csv"))
}), use.names = FALSE)
assert_true(length(files) == 13L, sprintf("Expected 13 combined model-piece files, found %d.", length(files)))

draws <- do.call(rbind, lapply(files, read.csv, check.names = FALSE))
required <- c("model", "iteration", "chain", "group", "age_index", "forecast_year", "forecast_log_rate", "observed_log_rate")
assert_true(all(required %in% names(draws)), "A forecast CSV is missing required labelled columns.")
assert_true(identical(sort(unique(draws$model)), model_ids), "The model IDs do not match Models A-D.")
assert_true(identical(sort(unique(draws$forecast_year)), holdout_years), "Forecast years are not exactly 2014-2018.")
assert_true(identical(sort(unique(draws$age_index)), 1:11), "Forecast files do not include all 11 mortality ages.")

draw_key <- interaction(draws$model, draws$group, draws$age_index, draws$forecast_year, draws$chain, draws$iteration, drop = TRUE)
assert_true(!anyDuplicated(draw_key), "Duplicate forecast draw keys were found.")
cell_key <- interaction(draws$model, draws$group, draws$age_index, draws$forecast_year, drop = TRUE)
draw_counts <- table(cell_key)
assert_true(length(unique(draw_counts)) == 1L && unname(draw_counts[1]) >= 100L,
            "Forecast cells must have an equal number of posterior draws (at least 100).")

obs_key <- interaction(draws$group, draws$age_index, draws$forecast_year, drop = TRUE)
assert_true(all(vapply(split(draws$observed_log_rate, obs_key), function(x) diff(range(x)) < 1e-12, logical(1))),
            "Observed holdout rates differ across model exports.")

means <- aggregate(forecast_log_rate ~ model + group + age_index + forecast_year + observed_log_rate, draws, mean)
names(means)[names(means) == "forecast_log_rate"] <- "predicted_log_rate"
means$squared_error <- (means$predicted_log_rate - means$observed_log_rate)^2
assert_true(nrow(means) == 4L * 8L * 11L * 5L, "Expected 1,760 model/group/age/year forecast cells.")

# This is the existing thesis calculation: sqrt(mean((observed - predicted)^2)).
# The only changed input is the labelled posterior forecast mean, rather than
# columns 17:21 from a full-data fitted logmu array.
legacy_group_metrics <- do.call(rbind, lapply(split(means, interaction(means$model, means$group, drop = TRUE)), function(x) {
  data.frame(
    model = model_labels[[x$model[1]]],
    group = x$group[1],
    n_observations = nrow(x),
    rmse = sqrt(mean((x$observed_log_rate - x$predicted_log_rate)^2)),
    mse = mean((x$observed_log_rate - x$predicted_log_rate)^2),
    mean_error = mean(x$observed_log_rate - x$predicted_log_rate),
    stringsAsFactors = FALSE
  )
}))

assert_true(requireNamespace("scoringRules", quietly = TRUE),
            "Package 'scoringRules' is required to calculate CRPS.")
crps_cells <- do.call(rbind, lapply(model_ids, function(model) {
  model_draws <- draws[draws$model == model, ]
  cell_id <- interaction(model_draws$group, model_draws$age_index, model_draws$forecast_year, drop = TRUE)
  draw_id <- interaction(model_draws$chain, model_draws$iteration, drop = TRUE)
  cells <- model_draws[!duplicated(cell_id), c("group", "age_index", "forecast_year", "observed_log_rate")]
  cell_position <- match(cell_id, unique(cell_id))
  draw_position <- match(draw_id, unique(draw_id))
  forecast_matrix <- matrix(NA_real_, nrow = nrow(cells), ncol = length(unique(draw_id)))
  forecast_matrix[cbind(cell_position, draw_position)] <- model_draws$forecast_log_rate
  assert_true(!anyNA(forecast_matrix), sprintf("%s has incomplete posterior forecast draws.", model_labels[[model]]))
  data.frame(
    model = model_labels[[model]],
    cells,
    crps = scoringRules::crps_sample(y = cells$observed_log_rate, dat = forecast_matrix),
    stringsAsFactors = FALSE
  )
}))

summarise_crps <- function(data, suffix) {
  result <- do.call(rbind, lapply(unname(model_labels), function(model) {
    x <- data[data$model == model, ]
    data.frame(model = model, n_observations = nrow(x), value = mean(x$crps))
  }))
  names(result)[names(result) == "value"] <- suffix
  result
}
pooled_crps <- summarise_crps(crps_cells, "mean_crps_2014_2018")
final_year_crps <- summarise_crps(crps_cells[crps_cells$forecast_year == 2018L, ], "mean_crps_2018_only")
crps <- merge(pooled_crps, final_year_crps[, c("model", "mean_crps_2018_only")], by = "model", sort = FALSE)
crps <- crps[match(unname(model_labels), crps$model), ]
crps_by_group <- aggregate(crps ~ model + group, crps_cells, mean)

scale_audit <- do.call(rbind, lapply(model_ids, function(model) {
  x <- means[means$model == model, ]
  data.frame(model = model_labels[[model]], prediction_min = min(x$predicted_log_rate), prediction_max = max(x$predicted_log_rate),
             observed_min = min(x$observed_log_rate), observed_max = max(x$observed_log_rate))
}))
assert_true(all(abs(rowMeans(scale_audit[, c("prediction_min", "prediction_max")]) - rowMeans(scale_audit[, c("observed_min", "observed_max")])) < 1.5),
            "At least one model is not on the observed log-rate scale.")

score <- function(data, suffix) {
  result <- do.call(rbind, lapply(model_ids, function(model) {
    x <- data[data$model == model, ]
    data.frame(model = model_labels[[model]], n_observations = nrow(x), value = sqrt(mean(x$squared_error)))
  }))
  names(result)[names(result) == "value"] <- suffix
  result
}

pooled <- score(means, "rmse_2014_2018")
final_year <- score(means[means$forecast_year == 2018L, ], "rmse_2018_only")
rmse <- merge(pooled, final_year[, c("model", "rmse_2018_only")], by = "model", sort = FALSE)
rmse <- rmse[match(unname(model_labels), rmse$model), ]

write.csv(means, file.path(csv_dir, "mortality_holdout_posterior_mean_forecasts.csv"), row.names = FALSE)
write.csv(rmse, file.path(csv_dir, "revised_table_6_3_mortality_holdout_rmse.csv"), row.names = FALSE)
write.csv(legacy_group_metrics, file.path(csv_dir, "mortality_holdout_rmse_mse_me_by_group.csv"), row.names = FALSE)
write.csv(crps, file.path(csv_dir, "mortality_holdout_mean_crps.csv"), row.names = FALSE)
write.csv(crps_by_group, file.path(csv_dir, "mortality_holdout_mean_crps_by_group.csv"), row.names = FALSE)
write.csv(scale_audit, file.path(diagnostics_dir, "mortality_holdout_forecast_scale_audit.csv"), row.names = FALSE)

latex_rows <- sprintf("%s & %.4f & %.4f \\\\", rmse$model, rmse$rmse_2014_2018, rmse$rmse_2018_only)
writeLines(c(
  "% Generated by src/mortality_holdout_rmse.R from labelled HPC posterior forecast draws.",
  "\\begin{table}[ht]", "\\centering",
  "\\caption{Out-of-sample mortality forecast evaluation: pooled root mean square error (RMSE) of log mortality rates over the holdout period 2014--2018 and in the final forecast year, using models estimated with observations from 1998 to 2013.}",
  "\\label{tab:outofsample_mortality_rmse}", "\\begin{tabular}{lrr}", "\\hline",
  "Model & RMSE, 2014--2018 & RMSE, 2018 only \\\\", "\\hline", latex_rows,
  "\\hline", "\\end{tabular}", "\\end{table}"
), file.path(latex_dir, "revised_table_6_3_mortality_holdout_rmse.tex"))

crps_latex_rows <- sprintf("%s & %.4f & %.4f \\\\", crps$model, crps$mean_crps_2014_2018, crps$mean_crps_2018_only)
writeLines(c(
  "% Generated by src/mortality_holdout_rmse.R from labelled HPC posterior forecast draws.",
  "\\begin{table}[ht]", "\\centering",
  "\\caption{Out-of-sample mortality forecast evaluation: mean continuous ranked probability score (CRPS) of log mortality rates over the holdout period 2014--2018 and in the final forecast year, using models estimated with observations from 1998 to 2013. Lower scores indicate better calibrated and sharper probabilistic forecasts.}",
  "\\label{tab:outofsample_mortality_crps}", "\\begin{tabular}{lrr}", "\\hline",
  "Model & Mean CRPS, 2014--2018 & Mean CRPS, 2018 only \\\\", "\\hline", crps_latex_rows,
  "\\hline", "\\end{tabular}", "\\end{table}"
), file.path(latex_dir, "mortality_holdout_crps.tex"))

writeLines(c(
  "# Mortality Holdout Forecast Validation", "",
  "- Training period: 1998-2013; holdout: 2014-2018.",
  sprintf("- Combined model-piece files: %d; posterior draws per forecast cell: %d.", length(files), unname(draw_counts[1])),
  "- Forecast log rates are the HPC exporter formula `alpha + beta * kf`.",
  "- Observed holdout values match across every model key.",
  "- All model forecasts passed the common log-rate scale check; no post-hoc scaling offset was applied.", "",
  "| Model | RMSE, 2014-2018 | RMSE, 2018 only |", "| --- | ---: | ---: |",
  sprintf("| %s | %.4f | %.4f |", rmse$model, rmse$rmse_2014_2018, rmse$rmse_2018_only)
), file.path(diagnostics_dir, "mortality_holdout_forecast_validation.md"))

print(rmse, row.names = FALSE)
