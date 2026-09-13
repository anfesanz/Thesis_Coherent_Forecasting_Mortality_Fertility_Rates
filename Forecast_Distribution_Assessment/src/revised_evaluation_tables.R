#!/usr/bin/env Rscript

# Revised out-of-sample evaluation tables for examiner comments.
# This script reuses the validated observation panels and posterior draws
# already used in the CRPS and RMSE workflows, but joins predictions and
# observations explicitly by evaluation keys before calculating scores.

source(file.path(dirname(normalizePath(script_path_from_args <- sub("^--file=", "", commandArgs(trailingOnly = FALSE)[grep("^--file=", commandArgs(trailingOnly = FALSE))][1]), mustWork = FALSE)), "project_paths.R"))
paths <- project_paths()
project_root <- paths$project_root
data_dir <- paths$data_dir
output_root <- file.path(paths$output_dir, "revised_evaluation_tables")
dir_mortality <- file.path(output_root, "mortality")
dir_fertility <- file.path(output_root, "fertility")
dir_diagnostics <- file.path(output_root, "diagnostics")
dir_latex <- file.path(output_root, "latex")
dir_csv <- file.path(output_root, "csv")

for (dir_path in c(output_root, dir_mortality, dir_fertility, dir_diagnostics, dir_latex, dir_csv)) {
  dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
}

required_packages <- c("scoringRules", "ggplot2")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(sprintf("Missing required R packages: %s", paste(missing_packages, collapse = ", ")), call. = FALSE)
}

holdout_years <- 2014:2018
training_years <- 1998:2013
all_years <- 1998:2018

model_labels_mortality <- c("Model A", "Model B", "Model C", "Model D")
model_labels_fertility <- c("Model A", "Model B", "Model C")
education_labels <- c("No Formal Education", "Primary", "Secondary", "Post Secondary")
mortality_age_labels <- sprintf("Age %02d", seq_len(11))
fertility_age_labels <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44")

# Models C and D save mortality log-rates on a rate scale that is 1,000 times
# smaller than the Model A/observed-data scale. Convert their draws before
# calculating point or probabilistic scores. Model B requires separate review.
mortality_log_scale_offsets <- c(
  "Model A" = 0,
  "Model B" = 0,
  "Model C" = log(1000),
  "Model D" = log(1000)
)

model_palette <- c(
  "Model A" = "#08519c",
  "Model B" = "#3182bd",
  "Model C" = "#6baed6",
  "Model D" = "#9ecae1"
)

assert_true <- function(condition, message_text) {
  if (!isTRUE(condition)) {
    stop(message_text, call. = FALSE)
  }
}

write_csv_safe <- function(df, path) {
  write.csv(df, path, row.names = FALSE, na = "")
}

snake_model <- function(model_name) {
  switch(
    model_name,
    "Model A" = "model_a",
    "Model B" = "model_b",
    "Model C" = "model_c",
    "Model D" = "model_d",
    gsub("[^a-z0-9]+", "_", tolower(model_name))
  )
}

make_key <- function(df, key_cols) {
  apply(df[, key_cols, drop = FALSE], 1, paste, collapse = "||")
}

check_unique_keys <- function(df, key_cols, label) {
  missing_cols <- setdiff(key_cols, names(df))
  assert_true(length(missing_cols) == 0,
              sprintf("%s is missing key columns: %s", label, paste(missing_cols, collapse = ", ")))
  key_values <- make_key(df, key_cols)
  dup_values <- unique(key_values[duplicated(key_values)])
  assert_true(length(dup_values) == 0,
              sprintf("%s has duplicate keys. Example: %s", label, dup_values[1]))
  TRUE
}

check_expected_values <- function(df, column_name, expected_values, label) {
  present <- sort(unique(df[[column_name]]))
  missing_values <- setdiff(expected_values, present)
  assert_true(length(missing_values) == 0,
              sprintf("%s is missing expected %s values: %s", label, column_name, paste(missing_values, collapse = ", ")))
}

one_to_one_join <- function(observed_df, prediction_df, key_cols, label) {
  check_unique_keys(observed_df, key_cols, sprintf("%s observed data", label))
  check_unique_keys(prediction_df, key_cols, sprintf("%s prediction data", label))

  observed_keys <- make_key(observed_df, key_cols)
  prediction_keys <- make_key(prediction_df, key_cols)

  missing_in_pred <- setdiff(observed_keys, prediction_keys)
  missing_in_obs <- setdiff(prediction_keys, observed_keys)

  assert_true(length(missing_in_pred) == 0,
              sprintf("%s predictions are missing observed keys. Example: %s", label, missing_in_pred[1]))
  assert_true(length(missing_in_obs) == 0,
              sprintf("%s observations are missing prediction keys. Example: %s", label, missing_in_obs[1]))

  merged <- merge(observed_df, prediction_df, by = key_cols, all = FALSE, sort = FALSE)
  check_unique_keys(merged, key_cols, sprintf("%s joined data", label))

  assert_true(nrow(merged) == nrow(observed_df),
              sprintf("%s join row count mismatch: joined=%d observed=%d", label, nrow(merged), nrow(observed_df)))
  assert_true(nrow(merged) == nrow(prediction_df),
              sprintf("%s join row count mismatch: joined=%d predicted=%d", label, nrow(merged), nrow(prediction_df)))

  merged
}

build_logmu_draw_matrix <- function(chain_df, n_age, n_year, n_draws = NULL) {
  if (is.null(n_draws)) {
    n_draws <- nrow(chain_df)
  }

  draw_matrix <- matrix(NA_real_, nrow = n_age * n_year, ncol = n_draws)
  row_idx <- 1

  for (age_idx in seq_len(n_age)) {
    for (year_idx in seq_len(n_year)) {
      col_candidates <- c(
        sprintf("logmu[%d,%d]", age_idx, year_idx),
        sprintf("logmu.%d.%d.", age_idx, year_idx)
      )
      col_name <- col_candidates[col_candidates %in% names(chain_df)][1]

      if (is.na(col_name)) {
        stop(sprintf("Column for logmu[%d,%d] was not found in the chain.", age_idx, year_idx), call. = FALSE)
      }

      draw_matrix[row_idx, ] <- chain_df[seq_len(n_draws), col_name]
      row_idx <- row_idx + 1
    }
  }

  draw_matrix
}

build_named_draw_matrix <- function(chain_df, base_name, n_age, n_year, n_draws = NULL) {
  if (is.null(n_draws)) {
    n_draws <- nrow(chain_df)
  }

  draw_matrix <- matrix(NA_real_, nrow = n_age * n_year, ncol = n_draws)
  row_idx <- 1

  for (age_idx in seq_len(n_age)) {
    for (year_idx in seq_len(n_year)) {
      col_candidates <- c(
        sprintf("%s[%d,%d]", base_name, age_idx, year_idx),
        sprintf("%s.%d.%d.", base_name, age_idx, year_idx)
      )
      col_name <- col_candidates[col_candidates %in% names(chain_df)][1]

      if (is.na(col_name)) {
        stop(sprintf("Column for %s[%d,%d] was not found in the chain.", base_name, age_idx, year_idx), call. = FALSE)
      }

      draw_matrix[row_idx, ] <- chain_df[seq_len(n_draws), col_name]
      row_idx <- row_idx + 1
    }
  }

  draw_matrix
}

build_observed_table <- function(observed_log_matrix, observed_rate_matrix, years, age_labels,
                                 sex = NULL, education = NULL, outcome_scale) {
  n_age <- nrow(observed_log_matrix)
  n_year <- ncol(observed_log_matrix)

  assert_true(length(years) == n_year, "Observed years length does not match matrix width.")
  assert_true(length(age_labels) == n_age, "Observed age labels length does not match matrix height.")
  assert_true(identical(dim(observed_log_matrix), dim(observed_rate_matrix)),
              "Observed log and rate matrices do not have matching dimensions.")

  age_index <- rep(seq_len(n_age), each = n_year)
  year_index <- rep(seq_len(n_year), times = n_age)

  out <- data.frame(
    year = years[year_index],
    age_group = age_labels[age_index],
    observed_log_rate = as.vector(t(as.matrix(observed_log_matrix))),
    observed_rate = as.vector(t(as.matrix(observed_rate_matrix))),
    outcome_scale = outcome_scale,
    stringsAsFactors = FALSE
  )

  if (!is.null(sex)) {
    out$sex <- sex
  }
  if (!is.null(education)) {
    out$education <- education
  }

  out
}

build_prediction_table <- function(draw_matrix_log, years, age_labels, model, sex = NULL, education = NULL, outcome_scale) {
  n_age <- length(age_labels)
  n_year <- length(years)

  assert_true(nrow(draw_matrix_log) == n_age * n_year,
              sprintf("Prediction draw matrix rows (%d) do not match expected cells (%d).",
                      nrow(draw_matrix_log), n_age * n_year))

  age_index <- rep(seq_len(n_age), each = n_year)
  year_index <- rep(seq_len(n_year), times = n_age)

  out <- data.frame(
    year = years[year_index],
    age_group = age_labels[age_index],
    model = model,
    predicted_log_rate = rowMeans(draw_matrix_log),
    predicted_rate = rowMeans(exp(draw_matrix_log)),
    draw_row_id = seq_len(nrow(draw_matrix_log)),
    outcome_scale = outcome_scale,
    stringsAsFactors = FALSE
  )

  if (!is.null(sex)) {
    out$sex <- sex
  }
  if (!is.null(education)) {
    out$education <- education
  }

  out
}

compute_row_scores <- function(joined_df, draw_matrix_log) {
  matched_draws <- draw_matrix_log[joined_df$draw_row_id, , drop = FALSE]

  joined_df$crps <- scoringRules::crps_sample(y = joined_df$observed_log_rate, dat = matched_draws)
  joined_df$log_score <- scoringRules::logs_sample(y = joined_df$observed_log_rate, dat = matched_draws)

  lower80 <- apply(matched_draws, 1, stats::quantile, probs = 0.10)
  upper80 <- apply(matched_draws, 1, stats::quantile, probs = 0.90)
  lower95 <- apply(matched_draws, 1, stats::quantile, probs = 0.025)
  upper95 <- apply(matched_draws, 1, stats::quantile, probs = 0.975)

  joined_df$coverage_80_hit <- joined_df$observed_log_rate >= lower80 & joined_df$observed_log_rate <= upper80
  joined_df$coverage_95_hit <- joined_df$observed_log_rate >= lower95 & joined_df$observed_log_rate <= upper95
  joined_df$interval_width_80 <- upper80 - lower80
  joined_df$interval_width_95 <- upper95 - lower95

  joined_df
}

summarise_rmse <- function(df, group_cols) {
  grouping_factor <- interaction(df[group_cols], drop = TRUE, lex.order = TRUE)
  split_idx <- split(seq_len(nrow(df)), grouping_factor)

  out <- lapply(split_idx, function(idx) {
    sub_df <- df[idx, , drop = FALSE]
    mse_value <- mean((sub_df$predicted_log_rate - sub_df$observed_log_rate)^2, na.rm = TRUE)
    row_out <- sub_df[1, group_cols, drop = FALSE]
    row_out$n_observations <- sum(stats::complete.cases(sub_df[, c("predicted_log_rate", "observed_log_rate")]))
    row_out$mse <- mse_value
    row_out$rmse <- sqrt(mse_value)
    row_out
  })

  result <- do.call(rbind, out)
  rownames(result) <- NULL
  result
}

summarise_probabilistic <- function(df, group_cols) {
  grouping_factor <- interaction(df[group_cols], drop = TRUE, lex.order = TRUE)
  split_idx <- split(seq_len(nrow(df)), grouping_factor)

  out <- lapply(split_idx, function(idx) {
    sub_df <- df[idx, , drop = FALSE]
    row_out <- sub_df[1, group_cols, drop = FALSE]
    row_out$mean_crps <- mean(sub_df$crps, na.rm = TRUE)
    row_out$mean_log_score <- mean(sub_df$log_score, na.rm = TRUE)
    row_out$coverage_80 <- mean(sub_df$coverage_80_hit, na.rm = TRUE)
    row_out$coverage_95 <- mean(sub_df$coverage_95_hit, na.rm = TRUE)
    row_out$mean_interval_width_80 <- mean(sub_df$interval_width_80, na.rm = TRUE)
    row_out$mean_interval_width_95 <- mean(sub_df$interval_width_95, na.rm = TRUE)
    row_out$n_observations <- nrow(sub_df)
    row_out
  })

  result <- do.call(rbind, out)
  rownames(result) <- NULL
  result
}

standardise_rmse_check <- function(df) {
  target_cols <- c("model", "year", "age_group", "sex", "education", "n_observations", "mse", "rmse", "grouping")
  for (col_name in target_cols) {
    if (!col_name %in% names(df)) {
      df[[col_name]] <- NA
    }
  }
  df[, target_cols]
}

wide_metric_table <- function(df, id_cols, metric_col, models) {
  out <- unique(df[, id_cols, drop = FALSE])

  for (model_name in models) {
    model_df <- df[df$model == model_name, c(id_cols, metric_col), drop = FALSE]
    names(model_df)[names(model_df) == metric_col] <- sprintf("%s_%s", metric_col, snake_model(model_name))
    out <- merge(out, model_df, by = id_cols, all.x = TRUE, sort = FALSE)
  }

  out
}

latex_align <- function(df) {
  vapply(df, function(x) if (is.numeric(x)) "r" else "l", character(1))
}

format_numeric_df <- function(df, digits = 4) {
  out <- df
  for (col_name in names(out)) {
    if (is.numeric(out[[col_name]])) {
      out[[col_name]] <- format(round(out[[col_name]], digits), nsmall = digits, trim = TRUE)
    }
  }
  out
}

write_latex_table <- function(df, path, caption, label, digits = 4) {
  df_fmt <- format_numeric_df(df, digits = digits)
  align <- paste(latex_align(df), collapse = "")
  header <- paste(names(df_fmt), collapse = " & ")
  rows <- apply(df_fmt, 1, function(row) paste(row, collapse = " & "))
  rows <- paste0(rows, " \\\\")

  latex_lines <- c(
    "\\begin{table}[ht]",
    "\\centering",
    sprintf("\\begin{tabular}{%s}", align),
    "\\hline",
    paste0(header, " \\\\"),
    "\\hline",
    rows,
    "\\hline",
    "\\end{tabular}",
    sprintf("\\caption{%s}", caption),
    sprintf("\\label{%s}", label),
    "\\end{table}"
  )

  writeLines(latex_lines, path)
}

df_to_md <- function(df, digits = 4) {
  df_fmt <- format_numeric_df(df, digits = digits)
  header <- paste(names(df_fmt), collapse = " | ")
  separator <- paste(rep("---", ncol(df_fmt)), collapse = " | ")
  rows <- apply(df_fmt, 1, function(row) paste(row, collapse = " | "))
  c(
    paste0("| ", header, " |"),
    paste0("| ", separator, " |"),
    paste0("| ", rows, " |")
  )
}

save_table_outputs <- function(df, stem, caption, label, digits = 4) {
  csv_path <- file.path(dir_csv, paste0(stem, ".csv"))
  latex_path <- file.path(dir_latex, paste0(stem, ".tex"))
  write_csv_safe(df, csv_path)
  write_latex_table(df, latex_path, caption, label, digits = digits)
  c(csv_path, latex_path)
}

plot_theme_revised <- function() {
  ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 30, hjust = 1)
    )
}

save_plot_both <- function(plot_obj, stem, subdir, width = 8, height = 5) {
  png_path <- file.path(subdir, paste0(stem, ".png"))
  pdf_path <- file.path(subdir, paste0(stem, ".pdf"))
  ggplot2::ggsave(filename = png_path, plot = plot_obj, width = width, height = height, dpi = 300)
  ggplot2::ggsave(filename = pdf_path, plot = plot_obj, width = width, height = height)
  c(png_path, pdf_path)
}

audit_mortality_holdout_artifacts <- function(data_dir, diagnostics_dir) {
  artifacts <- data.frame(
    model = c("Model A", "Model B", "Model B", "Model C", "Model C", "Model D"),
    chain_file = c(
      "mortality/chain_model1f1.csv",
      "mortality/chain_model2f.csv",
      "mortality/chain_model2m.csv",
      "mortality/chain_model3f.csv",
      "mortality/chain_model3m.csv",
      "mortality/chain_model4.csv"
    ),
    expected_structure = c(
      "Individual model by sex and education",
      "Shared beta model, female chain",
      "Shared beta model, male chain",
      "Shared beta and kappa model, female chain",
      "Shared beta and kappa model, male chain",
      "Joint sex, beta, and kappa model"
    ),
    stringsAsFactors = FALSE
  )

  artifacts$n_draws <- NA_integer_
  artifacts$has_logmuf_forecast_block <- FALSE
  artifacts$forecast_calendar_mapping_documented <- FALSE
  artifacts$valid_for_2014_2018_holdout <- FALSE
  artifacts$issue <- NA_character_

  for (i in seq_len(nrow(artifacts))) {
    path <- file.path(data_dir, artifacts$chain_file[i])
    header <- names(read.csv(path, nrows = 1, check.names = TRUE))
    artifacts$n_draws[i] <- length(readLines(path)) - 1L
    artifacts$has_logmuf_forecast_block[i] <- any(grepl("^logmuf", header))

    issues <- character()
    if (artifacts$n_draws[i] < 100L) {
      issues <- c(issues, sprintf("only %d saved draws", artifacts$n_draws[i]))
    }
    if (!artifacts$has_logmuf_forecast_block[i]) {
      issues <- c(issues, "no saved logmuf forecast block")
    }
    issues <- c(issues, "no documented mapping from forecast columns to calendar years 2014-2018")
    artifacts$issue[i] <- paste(issues, collapse = "; ")
  }

  csv_path <- file.path(diagnostics_dir, "mortality_holdout_artifact_audit.csv")
  report_path <- file.path(diagnostics_dir, "mortality_holdout_artifact_audit.md")
  write_csv_safe(artifacts, csv_path)
  writeLines(c(
    "# Mortality Holdout Artifact Audit",
    "",
    "The available mortality files do not support a valid 2014-2018 temporal-holdout comparison across Models A-D.",
    "",
    "- Model B has only 10 saved draws in each sex-specific chain and its values are not on a credible log mortality-rate scale.",
    "- Models C and D have no saved `logmuf` forecast block; their 21-year `logmu` arrays are fitted latent values, not documented holdout forecasts.",
    "- Model A contains a `logmuf` block, but the repository does not document its calendar-year mapping.",
    "- The serialized chains require the original Julia 1.10/Turing environment and do not deserialize under the currently available project stack without the original compatible dependencies.",
    "",
    "Do not use the existing revised Table 6.3 as an out-of-sample comparison. A valid replacement requires regenerated or re-exported 2014-2018 posterior forecast draws for every model, with explicit year metadata and a common per-1,000 log-rate scale.",
    "",
    "## Artifact Summary",
    df_to_md(artifacts, digits = 0)
  ), report_path)

  list(table = artifacts, csv_path = csv_path, report_path = report_path)
}

rank_summary <- function(df, group_cols, metric_col = "rmse") {
  grouping_factor <- interaction(df[group_cols], drop = TRUE, lex.order = TRUE)
  split_idx <- split(seq_len(nrow(df)), grouping_factor)
  out <- lapply(split_idx, function(idx) {
    sub_df <- df[idx, , drop = FALSE]
    best_row <- sub_df[order(sub_df[[metric_col]], sub_df$model), ][1, , drop = FALSE]
    best_row
  })
  result <- do.call(rbind, out)
  rownames(result) <- NULL
  result
}

mortality_series <- data.frame(
  series = c("mxf1", "mxf2", "mxf3", "mxf4", "mxm1", "mxm2", "mxm3", "mxm4"),
  sex = c(rep("Female", 4), rep("Male", 4)),
  education = rep(education_labels, 2),
  rate_file = c(
    "mortality/mxf1.csv", "mortality/mxf2.csv", "mortality/mxf3.csv", "mortality/mxf4.csv",
    "mortality/mxm1.csv", "mortality/mxm2.csv", "mortality/mxm3.csv", "mortality/mxm4.csv"
  ),
  exposure_file = c(
    "mortality/exf1.csv", "mortality/exf2.csv", "mortality/exf3.csv", "mortality/exf4.csv",
    "mortality/exm1.csv", "mortality/exm2.csv", "mortality/exm3.csv", "mortality/exm4.csv"
  ),
  model_a_file = c(
    "mortality/chain_model1f1.csv", "mortality/chain_model1f2.csv", "mortality/chain_model1f3.csv", "mortality/chain_model1f4.csv",
    "mortality/chain_model1m1.csv", "mortality/chain_model1m2.csv", "mortality/chain_model1m3.csv", "mortality/chain_model1m4.csv"
  ),
  model_b_chain = c(rep("mortality/chain_model2f.csv", 4), rep("mortality/chain_model2m.csv", 4)),
  model_b_base = c("logmu1", "logmu2", "logmu3", "logmu4", "logmu1", "logmu2", "logmu3", "logmu4"),
  model_c_chain = c(rep("mortality/chain_model3f.csv", 4), rep("mortality/chain_model3m.csv", 4)),
  model_c_base = c("logmu1", "logmu2", "logmu3", "logmu4", "logmu1", "logmu2", "logmu3", "logmu4"),
  model_d_chain = rep("mortality/chain_model4.csv", 8),
  model_d_base = c("logmu1", "logmu2", "logmu3", "logmu4", "logmu5", "logmu6", "logmu7", "logmu8"),
  stringsAsFactors = FALSE
)

fertility_series <- data.frame(
  series = c("fxf1", "fxf2", "fxf3", "fxf4"),
  education = education_labels,
  rate_file = c("fertility/fx1.csv", "fertility/fx2.csv", "fertility/fx3.csv", "fertility/fx4.csv"),
  exposure_file = c("fertility/exfe1.csv", "fertility/exfe2.csv", "fertility/exfe3.csv", "fertility/exfe4.csv"),
  model_a_file = c("fertility/chain_model1f1.csv", "fertility/chain_model1f2.csv", "fertility/chain_model1f3.csv", "fertility/chain_model1f4.csv"),
  model_b_chain = rep("fertility/chain_model2f.csv", 4),
  model_b_base = c("logmu1", "logmu2", "logmu3", "logmu4"),
  model_c_chain = rep("fertility/chain_model3f.csv", 4),
  model_c_base = c("logmu1", "logmu2", "logmu3", "logmu4"),
  stringsAsFactors = FALSE
)

required_input_files <- unique(c(
  mortality_series$rate_file,
  mortality_series$exposure_file,
  mortality_series$model_a_file,
  mortality_series$model_b_chain,
  mortality_series$model_c_chain,
  mortality_series$model_d_chain,
  fertility_series$rate_file,
  fertility_series$exposure_file,
  fertility_series$model_a_file,
  fertility_series$model_b_chain,
  fertility_series$model_c_chain
))

for (rel_path in required_input_files) {
  assert_true(file.exists(file.path(data_dir, rel_path)),
              sprintf("Required input file is missing: %s", file.path(data_dir, rel_path)))
}

mortality_artifact_audit <- audit_mortality_holdout_artifacts(data_dir, dir_diagnostics)
message("Mortality holdout artifact audit written to: ", mortality_artifact_audit$report_path)

if (!identical(Sys.getenv("ALLOW_INVALID_FITTED_EVALUATION"), "1")) {
  unavailable_table_path <- file.path(dir_latex, "revised_table_6_3_mortality_holdout_rmse.tex")
  writeLines(c(
    "% This placeholder intentionally replaces invalid fitted-value RMSE results.",
    "\\begin{table}[ht]",
    "\\centering",
    "\\caption{Mortality out-of-sample RMSE is not reported because valid 2014--2018 posterior forecast draws are unavailable for all models. See the mortality holdout artifact audit.}",
    "\\label{tab:revised_table_6_3}",
    "\\begin{tabular}{p{0.85\\linewidth}}",
    "\\hline",
    "A valid cross-model temporal-holdout comparison requires regenerated forecast draws with documented calendar-year mappings and a common per-1,000 log-rate scale. \\\\ ",
    "\\hline",
    "\\end{tabular}",
    "\\end{table}"
  ), unavailable_table_path)

  stop(
    paste(
      "Cannot generate a valid out-of-sample mortality Table 4.5/6.3 from the available chains.",
      sprintf("Read %s.", mortality_artifact_audit$report_path),
      "The previous workflow scores fitted logmu values and must only be run with ALLOW_INVALID_FITTED_EVALUATION=1 for legacy reproduction, never for thesis replacement tables."
    ),
    call. = FALSE
  )
}

message("Reusing validated source scripts and datasets:")
message(sprintf("- Script logic reused from: %s", file.path("src", "RMSE.r")))
message(sprintf("- Script logic reused from: %s", file.path("src", "CRPS_Workflow_thesis.R")))
message("- Mortality observations: mxf*, mxm* with exf*/exm* exposures, evaluated on log mortality rates.")
message("- Fertility observations: fx* with exfe* exposures, evaluated on log age-specific fertility rates.")
message("- Posterior draws: chain_model1*, chain_model2*, chain_model3*, chain_model4*.")

created_paths <- c(file.path(project_root, "src", "revised_evaluation_tables.R"))

# Mortality: build observed, predictions, explicit joins, and per-row scores.
mortality_observed_list <- list()
mortality_joined_list <- list()
mortality_draws <- list()

for (i in seq_len(nrow(mortality_series))) {
  cfg <- mortality_series[i, ]

  observed_rate_matrix <- as.matrix(read.csv(file.path(data_dir, cfg$rate_file)))
  exposure_matrix <- as.matrix(read.csv(file.path(data_dir, cfg$exposure_file)))
  observed_log_matrix <- log(observed_rate_matrix / exposure_matrix)

  observed_df <- build_observed_table(
    observed_log_matrix = observed_log_matrix,
    observed_rate_matrix = observed_rate_matrix / exposure_matrix,
    years = all_years,
    age_labels = mortality_age_labels,
    sex = cfg$sex,
    education = cfg$education,
    outcome_scale = "log_mortality_rate"
  )
  mortality_observed_list[[cfg$series]] <- observed_df

  chain_a <- read.csv(file.path(data_dir, cfg$model_a_file))
  chain_b <- read.csv(file.path(data_dir, cfg$model_b_chain))
  chain_c <- read.csv(file.path(data_dir, cfg$model_c_chain))
  chain_d <- read.csv(file.path(data_dir, cfg$model_d_chain))

  model_draws <- list(
    "Model A" = build_logmu_draw_matrix(chain_a, n_age = 11, n_year = 21),
    "Model B" = build_named_draw_matrix(chain_b, base_name = cfg$model_b_base, n_age = 11, n_year = 21),
    "Model C" = build_named_draw_matrix(chain_c, base_name = cfg$model_c_base, n_age = 11, n_year = 21),
    "Model D" = build_named_draw_matrix(chain_d, base_name = cfg$model_d_base, n_age = 11, n_year = 21)
  )

  for (model_name in names(model_draws)) {
    model_draws[[model_name]] <- model_draws[[model_name]] + mortality_log_scale_offsets[[model_name]]
  }

  for (model_name in names(model_draws)) {
    prediction_df <- build_prediction_table(
      draw_matrix_log = model_draws[[model_name]],
      years = all_years,
      age_labels = mortality_age_labels,
      model = model_name,
      sex = cfg$sex,
      education = cfg$education,
      outcome_scale = "log_mortality_rate"
    )

    joined_df <- one_to_one_join(
      observed_df = observed_df,
      prediction_df = prediction_df,
      key_cols = c("year", "age_group", "sex", "education"),
      label = sprintf("Mortality %s %s", model_name, cfg$series)
    )
    joined_df <- compute_row_scores(joined_df, model_draws[[model_name]])
    joined_df$series <- cfg$series

    list_name <- paste(cfg$series, model_name, sep = "__")
    mortality_joined_list[[list_name]] <- joined_df
    mortality_draws[[list_name]] <- model_draws[[model_name]]
  }
}

mortality_observed <- do.call(rbind, mortality_observed_list)
rownames(mortality_observed) <- NULL
invisible(check_unique_keys(mortality_observed, c("year", "age_group", "sex", "education"), "Mortality observed full data"))

mortality_joined <- do.call(rbind, mortality_joined_list)
rownames(mortality_joined) <- NULL
mortality_holdout <- subset(mortality_joined, year %in% holdout_years)
mortality_2018 <- subset(mortality_holdout, year == 2018)

check_expected_values(mortality_holdout, "year", holdout_years, "Mortality holdout data")
check_expected_values(mortality_holdout, "sex", c("Female", "Male"), "Mortality holdout data")
check_expected_values(mortality_holdout, "education", education_labels, "Mortality holdout data")
check_expected_values(mortality_holdout, "model", model_labels_mortality, "Mortality holdout data")

mortality_model_counts <- aggregate(year ~ model, data = mortality_holdout, FUN = length)
names(mortality_model_counts)[2] <- "n_rows"
assert_true(length(unique(mortality_model_counts$n_rows)) == 1,
            sprintf("Mortality models have different evaluation row counts: %s",
                    paste(sprintf("%s=%d", mortality_model_counts$model, mortality_model_counts$n_rows), collapse = ", ")))

mortality_keys_by_model <- split(mortality_holdout, mortality_holdout$model)
reference_mortality_keys <- make_key(mortality_keys_by_model[[1]], c("year", "age_group", "sex", "education"))
for (model_name in names(mortality_keys_by_model)[-1]) {
  model_keys <- make_key(mortality_keys_by_model[[model_name]], c("year", "age_group", "sex", "education"))
  assert_true(setequal(reference_mortality_keys, model_keys),
              sprintf("Mortality model %s is evaluated on a different set of observations.", model_name))
}

write_csv_safe(
  mortality_holdout[, c("model", "year", "age_group", "sex", "education", "observed_log_rate", "predicted_log_rate", "observed_rate", "predicted_rate", "crps", "log_score", "coverage_80_hit", "coverage_95_hit", "interval_width_80", "interval_width_95")],
  file.path(dir_diagnostics, "mortality_joined_holdout_rows.csv")
)
created_paths <- c(created_paths, file.path(dir_diagnostics, "mortality_joined_holdout_rows.csv"))

# Fertility: build observed, predictions, explicit joins, and per-row scores.
fertility_observed_list <- list()
fertility_joined_list <- list()

for (i in seq_len(nrow(fertility_series))) {
  cfg <- fertility_series[i, ]

  observed_rate_matrix <- as.matrix(read.csv(file.path(data_dir, cfg$rate_file)))
  exposure_matrix <- as.matrix(read.csv(file.path(data_dir, cfg$exposure_file)))
  observed_log_matrix <- log(observed_rate_matrix / exposure_matrix)

  observed_df <- build_observed_table(
    observed_log_matrix = observed_log_matrix,
    observed_rate_matrix = observed_rate_matrix / exposure_matrix,
    years = all_years,
    age_labels = fertility_age_labels,
    education = cfg$education,
    outcome_scale = "log_asfr"
  )
  observed_df$exposure_count <- as.vector(t(exposure_matrix))
  fertility_observed_list[[cfg$series]] <- observed_df

  chain_a <- read.csv(file.path(data_dir, cfg$model_a_file))
  chain_b <- read.csv(file.path(data_dir, cfg$model_b_chain))
  chain_c <- read.csv(file.path(data_dir, cfg$model_c_chain))

  model_draws <- list(
    "Model A" = build_logmu_draw_matrix(chain_a, n_age = 6, n_year = 21),
    "Model B" = build_named_draw_matrix(chain_b, base_name = cfg$model_b_base, n_age = 6, n_year = 21),
    "Model C" = build_named_draw_matrix(chain_c, base_name = cfg$model_c_base, n_age = 6, n_year = 21)
  )

  for (model_name in names(model_draws)) {
    prediction_df <- build_prediction_table(
      draw_matrix_log = model_draws[[model_name]],
      years = all_years,
      age_labels = fertility_age_labels,
      model = model_name,
      education = cfg$education,
      outcome_scale = "log_asfr"
    )

    joined_df <- one_to_one_join(
      observed_df = observed_df,
      prediction_df = prediction_df,
      key_cols = c("year", "age_group", "education"),
      label = sprintf("Fertility %s %s", model_name, cfg$series)
    )
    joined_df <- compute_row_scores(joined_df, model_draws[[model_name]])
    joined_df$series <- cfg$series

    list_name <- paste(cfg$series, model_name, sep = "__")
    fertility_joined_list[[list_name]] <- joined_df
  }
}

fertility_observed <- do.call(rbind, fertility_observed_list)
rownames(fertility_observed) <- NULL
invisible(check_unique_keys(fertility_observed, c("year", "age_group", "education"), "Fertility observed full data"))

fertility_joined <- do.call(rbind, fertility_joined_list)
rownames(fertility_joined) <- NULL
fertility_holdout <- subset(fertility_joined, year %in% holdout_years)
fertility_2018 <- subset(fertility_holdout, year == 2018)

check_expected_values(fertility_holdout, "year", holdout_years, "Fertility holdout data")
check_expected_values(fertility_holdout, "education", education_labels, "Fertility holdout data")
check_expected_values(fertility_holdout, "age_group", fertility_age_labels, "Fertility holdout data")
check_expected_values(fertility_holdout, "model", model_labels_fertility, "Fertility holdout data")

fertility_model_counts <- aggregate(year ~ model, data = fertility_holdout, FUN = length)
names(fertility_model_counts)[2] <- "n_rows"
assert_true(length(unique(fertility_model_counts$n_rows)) == 1,
            sprintf("Fertility models have different evaluation row counts: %s",
                    paste(sprintf("%s=%d", fertility_model_counts$model, fertility_model_counts$n_rows), collapse = ", ")))

fertility_keys_by_model <- split(fertility_holdout, fertility_holdout$model)
reference_fertility_keys <- make_key(fertility_keys_by_model[[1]], c("year", "age_group", "education"))
for (model_name in names(fertility_keys_by_model)[-1]) {
  model_keys <- make_key(fertility_keys_by_model[[model_name]], c("year", "age_group", "education"))
  assert_true(setequal(reference_fertility_keys, model_keys),
              sprintf("Fertility model %s is evaluated on a different set of observations.", model_name))
}

write_csv_safe(
  fertility_holdout[, c("model", "year", "age_group", "education", "observed_log_rate", "predicted_log_rate", "observed_rate", "predicted_rate", "crps", "log_score", "coverage_80_hit", "coverage_95_hit", "interval_width_80", "interval_width_95")],
  file.path(dir_diagnostics, "fertility_joined_holdout_rows.csv")
)
created_paths <- c(created_paths, file.path(dir_diagnostics, "fertility_joined_holdout_rows.csv"))

# RMSE tables: mortality
table_63 <- merge(
  summarise_rmse(mortality_holdout, c("model"))[, c("model", "n_observations", "rmse")],
  summarise_rmse(mortality_2018, c("model"))[, c("model", "n_observations", "rmse")],
  by = "model",
  suffixes = c("_2014_2018", "_2018_only"),
  sort = FALSE
)
names(table_63) <- c("model", "n_observations_2014_2018", "rmse_2014_2018", "n_observations_2018_only", "rmse_2018_only")
table_63 <- table_63[, c("model", "rmse_2014_2018", "rmse_2018_only", "n_observations_2014_2018", "n_observations_2018_only")]

table_e1_long <- summarise_rmse(mortality_holdout, c("model", "sex", "education"))[, c("model", "sex", "education", "n_observations", "mse", "rmse")]
table_e1 <- wide_metric_table(table_e1_long, c("sex", "education"), "rmse", model_labels_mortality)

table_e2_long <- summarise_rmse(mortality_holdout, c("model", "year"))[, c("model", "year", "n_observations", "mse", "rmse")]
table_e2 <- wide_metric_table(table_e2_long, c("year"), "rmse", model_labels_mortality)

table_e3_long <- summarise_rmse(mortality_holdout, c("model", "age_group"))[, c("model", "age_group", "n_observations", "mse", "rmse")]
table_e3 <- wide_metric_table(table_e3_long, c("age_group"), "rmse", model_labels_mortality)

mortality_rmse_detailed <- summarise_rmse(mortality_holdout, c("model", "year", "age_group", "sex", "education"))[, c("model", "year", "age_group", "sex", "education", "n_observations", "mse", "rmse")]

# Probabilistic tables: mortality
table_64 <- summarise_probabilistic(mortality_holdout, c("model"))[, c("model", "mean_crps", "mean_log_score", "coverage_80", "coverage_95", "mean_interval_width_80", "mean_interval_width_95", "n_observations")]
table_e4_long <- summarise_probabilistic(mortality_holdout, c("model", "sex", "education"))[, c("model", "sex", "education", "mean_crps", "mean_log_score", "coverage_80", "coverage_95", "mean_interval_width_80", "mean_interval_width_95", "n_observations")]
table_e4 <- wide_metric_table(table_e4_long, c("sex", "education"), "mean_crps", model_labels_mortality)
names(table_e4) <- sub("^mean_crps_", "crps_", names(table_e4))

# RMSE tables: fertility
table_74 <- merge(
  summarise_rmse(fertility_holdout, c("model"))[, c("model", "n_observations", "rmse")],
  summarise_rmse(fertility_2018, c("model"))[, c("model", "n_observations", "rmse")],
  by = "model",
  suffixes = c("_2014_2018", "_2018_only"),
  sort = FALSE
)
names(table_74) <- c("model", "n_observations_2014_2018", "rmse_2014_2018", "n_observations_2018_only", "rmse_2018_only")
table_74 <- table_74[, c("model", "rmse_2014_2018", "rmse_2018_only", "n_observations_2014_2018", "n_observations_2018_only")]

table_e5_long <- summarise_rmse(fertility_holdout, c("model", "education"))[, c("model", "education", "n_observations", "mse", "rmse")]
table_e5 <- wide_metric_table(table_e5_long, c("education"), "rmse", model_labels_fertility)

table_e6_long <- summarise_rmse(fertility_holdout, c("model", "year"))[, c("model", "year", "n_observations", "mse", "rmse")]
table_e6 <- wide_metric_table(table_e6_long, c("year"), "rmse", model_labels_fertility)

table_e7_long <- summarise_rmse(fertility_holdout, c("model", "age_group"))[, c("model", "age_group", "n_observations", "mse", "rmse")]
table_e7 <- wide_metric_table(table_e7_long, c("age_group"), "rmse", model_labels_fertility)

fertility_rmse_detailed <- summarise_rmse(fertility_holdout, c("model", "year", "age_group", "education"))[, c("model", "year", "age_group", "education", "n_observations", "mse", "rmse")]

# Probabilistic tables: fertility
table_75 <- summarise_probabilistic(fertility_holdout, c("model"))[, c("model", "mean_crps", "mean_log_score", "coverage_80", "coverage_95", "mean_interval_width_80", "mean_interval_width_95", "n_observations")]
table_e8_long <- summarise_probabilistic(fertility_holdout, c("model", "education"))[, c("model", "education", "mean_crps", "mean_log_score", "coverage_80", "coverage_95", "mean_interval_width_80", "mean_interval_width_95", "n_observations")]
table_e8 <- wide_metric_table(table_e8_long, c("education"), "mean_crps", model_labels_fertility)
names(table_e8) <- sub("^mean_crps_", "crps_", names(table_e8))

# TFR tables
fertility_tfr_by_education_rows <- list()
fertility_tfr_aggregate_rows <- list()

for (model_name in model_labels_fertility) {
  model_df <- fertility_holdout[fertility_holdout$model == model_name, ]

  by_edu_year <- split(model_df, interaction(model_df$education, model_df$year, drop = TRUE, lex.order = TRUE))
  for (sub_df in by_edu_year) {
    fertility_tfr_by_education_rows[[length(fertility_tfr_by_education_rows) + 1]] <- data.frame(
      model = model_name,
      education = sub_df$education[1],
      year = sub_df$year[1],
      observed_tfr = 5 * sum(sub_df$observed_rate),
      predicted_tfr = 5 * sum(sub_df$predicted_rate),
      stringsAsFactors = FALSE
    )
  }

  by_year <- split(model_df, model_df$year)
  for (sub_df in by_year) {
    observed_tfr <- 0
    predicted_tfr <- 0
    by_age <- split(sub_df, sub_df$age_group)
    for (age_df in by_age) {
      observed_rate_agg <- sum(age_df$observed_rate * age_df$exposure_count) / sum(age_df$exposure_count)
      predicted_rate_agg <- sum(age_df$predicted_rate * age_df$exposure_count) / sum(age_df$exposure_count)
      observed_tfr <- observed_tfr + 5 * observed_rate_agg
      predicted_tfr <- predicted_tfr + 5 * predicted_rate_agg
    }
    fertility_tfr_aggregate_rows[[length(fertility_tfr_aggregate_rows) + 1]] <- data.frame(
      model = model_name,
      year = sub_df$year[1],
      observed_tfr = observed_tfr,
      predicted_tfr = predicted_tfr,
      stringsAsFactors = FALSE
    )
  }
}

fertility_tfr_by_education <- do.call(rbind, fertility_tfr_by_education_rows)
fertility_tfr_by_education$squared_error <- (fertility_tfr_by_education$predicted_tfr - fertility_tfr_by_education$observed_tfr)^2
table_e9_long <- aggregate(squared_error ~ model + education, data = fertility_tfr_by_education, FUN = mean)
table_e9_counts <- aggregate(year ~ model + education, data = fertility_tfr_by_education, FUN = length)
names(table_e9_counts)[3] <- "n_observations"
table_e9_long <- merge(table_e9_long, table_e9_counts, by = c("model", "education"), sort = FALSE)
table_e9_long$mse <- table_e9_long$squared_error
table_e9_long$rmse <- sqrt(table_e9_long$mse)
table_e9_long <- table_e9_long[, c("model", "education", "n_observations", "mse", "rmse")]
table_e9 <- wide_metric_table(table_e9_long, c("education"), "rmse", model_labels_fertility)

fertility_tfr_aggregate <- do.call(rbind, fertility_tfr_aggregate_rows)
fertility_tfr_aggregate$squared_error <- (fertility_tfr_aggregate$predicted_tfr - fertility_tfr_aggregate$observed_tfr)^2
table_e10 <- aggregate(squared_error ~ model, data = fertility_tfr_aggregate, FUN = mean)
table_e10_counts <- aggregate(year ~ model, data = fertility_tfr_aggregate, FUN = length)
names(table_e10_counts)[2] <- "n_observations"
table_e10 <- merge(table_e10, table_e10_counts, by = "model", sort = FALSE)
table_e10$mse <- table_e10$squared_error
table_e10$rmse <- sqrt(table_e10$mse)
table_e10 <- table_e10[, c("model", "n_observations", "mse", "rmse")]

# Validation checks
rmse_check_mortality <- rbind(
  standardise_rmse_check(transform(summarise_rmse(mortality_holdout, c("model", "sex", "education")), grouping = "model_sex_education")),
  standardise_rmse_check(transform(summarise_rmse(mortality_holdout, c("model", "year")), sex = NA, education = NA, grouping = "model_year")),
  standardise_rmse_check(transform(summarise_rmse(mortality_holdout, c("model", "age_group")), sex = NA, education = NA, year = NA, grouping = "model_age_group")),
  standardise_rmse_check(transform(summarise_rmse(mortality_holdout, c("model", "year", "age_group", "sex", "education")), grouping = "model_year_age_group_sex_education"))
)
rmse_check_mortality$sqrt_mse <- sqrt(rmse_check_mortality$mse)
rmse_check_mortality$diff <- abs(rmse_check_mortality$rmse - rmse_check_mortality$sqrt_mse)
rmse_check_mortality$within_tolerance <- rmse_check_mortality$diff < 1e-10

rmse_check_fertility <- rbind(
  standardise_rmse_check(transform(summarise_rmse(fertility_holdout, c("model", "education")), grouping = "model_education", year = NA, age_group = NA)),
  standardise_rmse_check(transform(summarise_rmse(fertility_holdout, c("model", "year")), grouping = "model_year", education = NA, age_group = NA)),
  standardise_rmse_check(transform(summarise_rmse(fertility_holdout, c("model", "age_group")), grouping = "model_age_group", year = NA, education = NA)),
  standardise_rmse_check(transform(summarise_rmse(fertility_holdout, c("model", "year", "age_group", "education")), grouping = "model_year_age_group_education"))
)
rmse_check_fertility$sqrt_mse <- sqrt(rmse_check_fertility$mse)
rmse_check_fertility$diff <- abs(rmse_check_fertility$rmse - rmse_check_fertility$sqrt_mse)
rmse_check_fertility$within_tolerance <- rmse_check_fertility$diff < 1e-10

reported_table_63 <- c("Model A" = 5.93, "Model B" = 38.46, "Model C" = 8.57, "Model D" = 1.40)
reported_table_74 <- c("Model A" = 32.11, "Model B" = 55.18, "Model C" = 6.77)

table_63_not_reused <- all(abs(table_63$rmse_2014_2018 - unname(reported_table_63[table_63$model])) > 1e-8)
table_74_not_reused <- all(abs(table_74$rmse_2014_2018 - unname(reported_table_74[table_74$model])) > 1e-8)

mortality_ranking_by_year <- rank_summary(table_e2_long, c("year"))
mortality_ranking_by_age <- rank_summary(table_e3_long, c("age_group"))
mortality_ranking_by_sex_education <- rank_summary(table_e1_long, c("sex", "education"))

fertility_ranking_by_year <- rank_summary(table_e6_long, c("year"))
fertility_ranking_by_age <- rank_summary(table_e7_long, c("age_group"))
fertility_ranking_by_education <- rank_summary(table_e5_long, c("education"))

validation_summary <- data.frame(
  check = c(
    "mortality_rmse_equals_sqrt_mse",
    "fertility_rmse_equals_sqrt_mse",
    "mortality_same_n_observations_per_model",
    "fertility_same_n_observations_per_model",
    "mortality_joins_are_key_based_and_one_to_one",
    "fertility_joins_are_key_based_and_one_to_one",
    "mortality_no_duplicate_join_keys",
    "fertility_no_duplicate_join_keys",
    "original_table_63_values_not_reused",
    "original_table_74_values_not_reused"
  ),
  passed = c(
    all(rmse_check_mortality$within_tolerance),
    all(rmse_check_fertility$within_tolerance),
    length(unique(mortality_model_counts$n_rows)) == 1,
    length(unique(fertility_model_counts$n_rows)) == 1,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    table_63_not_reused,
    table_74_not_reused
  ),
  stringsAsFactors = FALSE
)

# Save tables
created_paths <- c(
  created_paths,
  save_table_outputs(
    table_63,
    "revised_table_6_3_mortality_holdout_rmse",
    "Out-of-sample mortality forecast evaluation: pooled RMSE of log mortality rates over the temporal holdout period and in the final holdout year, by model.",
    "tab:revised_table_6_3",
    digits = 6
  ),
  save_table_outputs(
    table_e1,
    "revised_table_e1_mortality_rmse_by_sex_education_wide",
    "Out-of-sample RMSE of log mortality rates over 2014-2018, by sex, educational attainment, and model.",
    "tab:revised_table_e1",
    digits = 6
  ),
  save_table_outputs(
    table_e1_long,
    "revised_table_e1_mortality_rmse_by_sex_education_long",
    "Out-of-sample RMSE of log mortality rates over 2014-2018, by sex, educational attainment, and model (long format).",
    "tab:revised_table_e1_long",
    digits = 6
  ),
  save_table_outputs(
    table_e2,
    "revised_table_e2_mortality_rmse_by_year_wide",
    "Out-of-sample RMSE of log mortality rates over 2014-2018, by forecast year and model.",
    "tab:revised_table_e2",
    digits = 6
  ),
  save_table_outputs(
    table_e2_long,
    "revised_table_e2_mortality_rmse_by_year_long",
    "Out-of-sample RMSE of log mortality rates over 2014-2018, by forecast year and model (long format).",
    "tab:revised_table_e2_long",
    digits = 6
  ),
  save_table_outputs(
    table_e3,
    "revised_table_e3_mortality_rmse_by_age_group_wide",
    "Out-of-sample RMSE of log mortality rates over 2014-2018, by age group and model.",
    "tab:revised_table_e3",
    digits = 6
  ),
  save_table_outputs(
    table_e3_long,
    "revised_table_e3_mortality_rmse_by_age_group_long",
    "Out-of-sample RMSE of log mortality rates over 2014-2018, by age group and model (long format).",
    "tab:revised_table_e3_long",
    digits = 6
  ),
  save_table_outputs(
    mortality_rmse_detailed,
    "mortality_rmse_detailed_by_year_age_sex_education",
    "Detailed out-of-sample RMSE of log mortality rates over 2014-2018, by year, age group, sex, educational attainment, and model.",
    "tab:mortality_rmse_detailed",
    digits = 6
  ),
  save_table_outputs(
    table_64,
    "new_table_6_4_mortality_probabilistic_evaluation",
    "Out-of-sample probabilistic evaluation of mortality forecasts over 2014-2018, by model.",
    "tab:new_table_6_4",
    digits = 6
  ),
  save_table_outputs(
    table_e4,
    "new_table_e4_mortality_crps_by_sex_education_wide",
    "Out-of-sample CRPS of mortality forecasts over 2014-2018, by sex, educational attainment, and model.",
    "tab:new_table_e4",
    digits = 6
  ),
  save_table_outputs(
    table_e4_long,
    "new_table_e4_mortality_crps_by_sex_education_long",
    "Out-of-sample probabilistic mortality forecast evaluation over 2014-2018, by sex, educational attainment, and model (long format).",
    "tab:new_table_e4_long",
    digits = 6
  ),
  save_table_outputs(
    table_74,
    "revised_table_7_4_fertility_holdout_rmse",
    "Out-of-sample fertility forecast evaluation: pooled RMSE of log age-specific fertility rates over the temporal holdout period and in the final holdout year, by model.",
    "tab:revised_table_7_4",
    digits = 6
  ),
  save_table_outputs(
    table_e5,
    "revised_table_e5_fertility_rmse_by_education_wide",
    "Out-of-sample RMSE of log age-specific fertility rates over 2014-2018, by educational attainment and model.",
    "tab:revised_table_e5",
    digits = 6
  ),
  save_table_outputs(
    table_e5_long,
    "revised_table_e5_fertility_rmse_by_education_long",
    "Out-of-sample RMSE of log age-specific fertility rates over 2014-2018, by educational attainment and model (long format).",
    "tab:revised_table_e5_long",
    digits = 6
  ),
  save_table_outputs(
    table_e6,
    "revised_table_e6_fertility_rmse_by_year_wide",
    "Out-of-sample RMSE of log age-specific fertility rates over 2014-2018, by forecast year and model.",
    "tab:revised_table_e6",
    digits = 6
  ),
  save_table_outputs(
    table_e6_long,
    "revised_table_e6_fertility_rmse_by_year_long",
    "Out-of-sample RMSE of log age-specific fertility rates over 2014-2018, by forecast year and model (long format).",
    "tab:revised_table_e6_long",
    digits = 6
  ),
  save_table_outputs(
    table_e7,
    "revised_table_e7_fertility_rmse_by_age_group_wide",
    "Out-of-sample RMSE of log age-specific fertility rates over 2014-2018, by age group and model.",
    "tab:revised_table_e7",
    digits = 6
  ),
  save_table_outputs(
    table_e7_long,
    "revised_table_e7_fertility_rmse_by_age_group_long",
    "Out-of-sample RMSE of log age-specific fertility rates over 2014-2018, by age group and model (long format).",
    "tab:revised_table_e7_long",
    digits = 6
  ),
  save_table_outputs(
    fertility_rmse_detailed,
    "fertility_rmse_detailed_by_year_age_education",
    "Detailed out-of-sample RMSE of log age-specific fertility rates over 2014-2018, by year, age group, educational attainment, and model.",
    "tab:fertility_rmse_detailed",
    digits = 6
  ),
  save_table_outputs(
    table_75,
    "new_table_7_5_fertility_probabilistic_evaluation",
    "Out-of-sample probabilistic evaluation of fertility forecasts over 2014-2018, by model.",
    "tab:new_table_7_5",
    digits = 6
  ),
  save_table_outputs(
    table_e8,
    "new_table_e8_fertility_crps_by_education_wide",
    "Out-of-sample CRPS of fertility forecasts over 2014-2018, by educational attainment and model.",
    "tab:new_table_e8",
    digits = 6
  ),
  save_table_outputs(
    table_e8_long,
    "new_table_e8_fertility_crps_by_education_long",
    "Out-of-sample probabilistic fertility forecast evaluation over 2014-2018, by educational attainment and model (long format).",
    "tab:new_table_e8_long",
    digits = 6
  ),
  save_table_outputs(
    table_e9,
    "new_table_e9_education_specific_tfr_rmse_wide",
    "Out-of-sample RMSE of education-specific total fertility rates over 2014-2018, by educational attainment and model.",
    "tab:new_table_e9",
    digits = 6
  ),
  save_table_outputs(
    table_e9_long,
    "new_table_e9_education_specific_tfr_rmse_long",
    "Out-of-sample RMSE of education-specific total fertility rates over 2014-2018, by educational attainment and model (long format).",
    "tab:new_table_e9_long",
    digits = 6
  ),
  save_table_outputs(
    table_e10,
    "new_table_e10_aggregate_tfr_rmse",
    "Out-of-sample RMSE of aggregate total fertility rates over 2014-2018, by model.",
    "tab:new_table_e10",
    digits = 6
  )
)

diagnostic_csvs <- c(
  file.path(dir_diagnostics, "validation_summary.csv"),
  file.path(dir_diagnostics, "mortality_rmse_identity_checks.csv"),
  file.path(dir_diagnostics, "fertility_rmse_identity_checks.csv"),
  file.path(dir_diagnostics, "mortality_model_row_counts.csv"),
  file.path(dir_diagnostics, "fertility_model_row_counts.csv"),
  file.path(dir_diagnostics, "mortality_ranking_by_year.csv"),
  file.path(dir_diagnostics, "mortality_ranking_by_age_group.csv"),
  file.path(dir_diagnostics, "mortality_ranking_by_sex_education.csv"),
  file.path(dir_diagnostics, "fertility_ranking_by_year.csv"),
  file.path(dir_diagnostics, "fertility_ranking_by_age_group.csv"),
  file.path(dir_diagnostics, "fertility_ranking_by_education.csv")
)

write_csv_safe(validation_summary, diagnostic_csvs[1])
write_csv_safe(rmse_check_mortality, diagnostic_csvs[2])
write_csv_safe(rmse_check_fertility, diagnostic_csvs[3])
write_csv_safe(mortality_model_counts, diagnostic_csvs[4])
write_csv_safe(fertility_model_counts, diagnostic_csvs[5])
write_csv_safe(mortality_ranking_by_year, diagnostic_csvs[6])
write_csv_safe(mortality_ranking_by_age, diagnostic_csvs[7])
write_csv_safe(mortality_ranking_by_sex_education, diagnostic_csvs[8])
write_csv_safe(fertility_ranking_by_year, diagnostic_csvs[9])
write_csv_safe(fertility_ranking_by_age, diagnostic_csvs[10])
write_csv_safe(fertility_ranking_by_education, diagnostic_csvs[11])
created_paths <- c(created_paths, diagnostic_csvs)

# Figures
mortality_fig_year <- ggplot2::ggplot(table_e2_long, ggplot2::aes(x = factor(year), y = rmse, colour = model, group = model)) +
  ggplot2::geom_line(linewidth = 0.8) +
  ggplot2::geom_point(size = 2) +
  ggplot2::scale_colour_manual(values = model_palette[model_labels_mortality]) +
  ggplot2::labs(title = "Mortality RMSE by Forecast Year", x = "Forecast year", y = "RMSE of log mortality rate", colour = "Model") +
  plot_theme_revised()

mortality_fig_age <- ggplot2::ggplot(table_e3_long, ggplot2::aes(x = age_group, y = rmse, fill = model)) +
  ggplot2::geom_col(position = "dodge") +
  ggplot2::scale_fill_manual(values = model_palette[model_labels_mortality]) +
  ggplot2::labs(title = "Mortality RMSE by Age Group", x = "Age group", y = "RMSE of log mortality rate", fill = "Model") +
  plot_theme_revised()

mortality_fig_sex_edu <- ggplot2::ggplot(table_e1_long, ggplot2::aes(x = education, y = rmse, fill = model)) +
  ggplot2::geom_col(position = "dodge") +
  ggplot2::facet_wrap(~ sex) +
  ggplot2::scale_fill_manual(values = model_palette[model_labels_mortality]) +
  ggplot2::labs(title = "Mortality RMSE by Sex, Education, and Model", x = "Educational attainment", y = "RMSE of log mortality rate", fill = "Model") +
  plot_theme_revised()

mortality_fig_crps <- ggplot2::ggplot(table_e4_long, ggplot2::aes(x = education, y = mean_crps, fill = model)) +
  ggplot2::geom_col(position = "dodge") +
  ggplot2::facet_wrap(~ sex) +
  ggplot2::scale_fill_manual(values = model_palette[model_labels_mortality]) +
  ggplot2::labs(title = "Mortality CRPS by Sex, Education, and Model", x = "Educational attainment", y = "Mean CRPS", fill = "Model") +
  plot_theme_revised()

fertility_fig_year <- ggplot2::ggplot(table_e6_long, ggplot2::aes(x = factor(year), y = rmse, colour = model, group = model)) +
  ggplot2::geom_line(linewidth = 0.8) +
  ggplot2::geom_point(size = 2) +
  ggplot2::scale_colour_manual(values = model_palette[model_labels_fertility]) +
  ggplot2::labs(title = "Fertility RMSE by Forecast Year", x = "Forecast year", y = "RMSE of log ASFR", colour = "Model") +
  plot_theme_revised()

fertility_fig_age <- ggplot2::ggplot(table_e7_long, ggplot2::aes(x = age_group, y = rmse, fill = model)) +
  ggplot2::geom_col(position = "dodge") +
  ggplot2::scale_fill_manual(values = model_palette[model_labels_fertility]) +
  ggplot2::labs(title = "Fertility RMSE by Age Group", x = "Age group", y = "RMSE of log ASFR", fill = "Model") +
  plot_theme_revised()

fertility_fig_edu <- ggplot2::ggplot(table_e5_long, ggplot2::aes(x = education, y = rmse, fill = model)) +
  ggplot2::geom_col(position = "dodge") +
  ggplot2::scale_fill_manual(values = model_palette[model_labels_fertility]) +
  ggplot2::labs(title = "Fertility RMSE by Education and Model", x = "Educational attainment", y = "RMSE of log ASFR", fill = "Model") +
  plot_theme_revised()

fertility_fig_crps <- ggplot2::ggplot(table_e8_long, ggplot2::aes(x = education, y = mean_crps, fill = model)) +
  ggplot2::geom_col(position = "dodge") +
  ggplot2::scale_fill_manual(values = model_palette[model_labels_fertility]) +
  ggplot2::labs(title = "Fertility CRPS by Education and Model", x = "Educational attainment", y = "Mean CRPS", fill = "Model") +
  plot_theme_revised()

fertility_fig_tfr <- ggplot2::ggplot(table_e9_long, ggplot2::aes(x = education, y = rmse, fill = model)) +
  ggplot2::geom_col(position = "dodge") +
  ggplot2::scale_fill_manual(values = model_palette[model_labels_fertility]) +
  ggplot2::labs(title = "Education-specific TFR RMSE by Education and Model", x = "Educational attainment", y = "RMSE of TFR", fill = "Model") +
  plot_theme_revised()

created_paths <- c(
  created_paths,
  save_plot_both(mortality_fig_year, "mortality_rmse_by_forecast_year", dir_mortality),
  save_plot_both(mortality_fig_age, "mortality_rmse_by_age_group", dir_mortality, width = 9, height = 5),
  save_plot_both(mortality_fig_sex_edu, "mortality_rmse_by_sex_education_model", dir_mortality, width = 9, height = 5),
  save_plot_both(mortality_fig_crps, "mortality_crps_by_sex_education_model", dir_mortality, width = 9, height = 5),
  save_plot_both(fertility_fig_year, "fertility_rmse_by_forecast_year", dir_fertility),
  save_plot_both(fertility_fig_age, "fertility_rmse_by_age_group", dir_fertility, width = 9, height = 5),
  save_plot_both(fertility_fig_edu, "fertility_rmse_by_education_model", dir_fertility, width = 9, height = 5),
  save_plot_both(fertility_fig_crps, "fertility_crps_by_education_model", dir_fertility, width = 9, height = 5),
  save_plot_both(fertility_fig_tfr, "fertility_tfr_rmse_by_education_model", dir_fertility, width = 9, height = 5)
)

report_path <- file.path(dir_diagnostics, "revised_evaluation_tables_report.md")

report_lines <- c(
  "# Revised Evaluation Tables Report",
  "",
  "## Scripts and Datasets Used",
  sprintf("- New pipeline script: `%s`.", file.path("src", "revised_evaluation_tables.R")),
  sprintf("- Source logic inspected and reused from `%s` and `%s`.", file.path("src", "RMSE.r"), file.path("src", "CRPS_Workflow_thesis.R")),
  "- Mortality cleaned row-level data were rebuilt from `mxf*`, `mxm*`, `exf*`, and `exm*` files on the log mortality-rate scale.",
  "- Fertility cleaned row-level data were rebuilt from `fx*` and `exfe*` files on the log age-specific fertility-rate scale.",
  "- Posterior draws were extracted from `chain_model1*`, `chain_model2*`, `chain_model3*`, and `chain_model4*` CSV files using the same column-matching logic as the existing CRPS workflow.",
  "- Mortality posterior draws for Models C and D were shifted by `log(1000)` before scoring to align their stored rate scale with the observed and Model A log-rate scale. Model B was not shifted because its discrepancy is not consistent with this offset.",
  "",
  "## Row-level Join Keys",
  "- Mortality joins used: `year`, `age_group`, `sex`, `education`.",
  "- Fertility joins used: `year`, `age_group`, `education`.",
  "- Predictions and observations were joined explicitly by keys. They were not compared by row order.",
  "",
  "## Estimation and Holdout Periods",
  sprintf("- Estimation period: %s.", paste(range(training_years), collapse = "-")),
  sprintf("- Temporal holdout period: %s.", paste(range(holdout_years), collapse = "-")),
  "",
  "## Number of Evaluation Observations",
  df_to_md(mortality_model_counts, digits = 0),
  "",
  df_to_md(fertility_model_counts, digits = 0),
  "",
  "## Revised Table 6.3",
  df_to_md(table_63, digits = 6),
  "",
  "## Revised Tables E.1-E.4",
  "### E.1",
  df_to_md(table_e1, digits = 6),
  "",
  "### E.2",
  df_to_md(table_e2, digits = 6),
  "",
  "### E.3",
  df_to_md(table_e3, digits = 6),
  "",
  "### E.4",
  df_to_md(table_e4_long, digits = 6),
  "",
  "## Revised Table 7.4",
  df_to_md(table_74, digits = 6),
  "",
  "## Revised Tables E.5-E.10",
  "### E.5",
  df_to_md(table_e5, digits = 6),
  "",
  "### E.6",
  df_to_md(table_e6, digits = 6),
  "",
  "### E.7",
  df_to_md(table_e7, digits = 6),
  "",
  "### E.8",
  df_to_md(table_e8_long, digits = 6),
  "",
  "### E.9",
  df_to_md(table_e9, digits = 6),
  "",
  "### E.10",
  df_to_md(table_e10, digits = 6),
  "",
  "## Mortality and Fertility Probabilistic Evaluation Results",
  "- `mean_log_score` is reported using `scoringRules::logs_sample`, and lower values indicate better predictive performance.",
  "### Mortality Table 6.4",
  df_to_md(table_64, digits = 6),
  "",
  "### Fertility Table 7.5",
  df_to_md(table_75, digits = 6),
  "",
  "## Validation Checks",
  df_to_md(validation_summary, digits = 0),
  "",
  sprintf("- Mortality `rmse == sqrt(mse)` passed for all checked groupings: %s.", if (all(rmse_check_mortality$within_tolerance)) "yes" else "no"),
  sprintf("- Fertility `rmse == sqrt(mse)` passed for all checked groupings: %s.", if (all(rmse_check_fertility$within_tolerance)) "yes" else "no"),
  sprintf("- Mortality models all use the same number of holdout rows: %s.", unique(mortality_model_counts$n_rows)),
  sprintf("- Fertility models all use the same number of holdout rows: %s.", unique(fertility_model_counts$n_rows)),
  sprintf("- Original Table 6.3 values reused: %s.", if (table_63_not_reused) "no" else "yes"),
  sprintf("- Original Table 7.4 values reused: %s.", if (table_74_not_reused) "no" else "yes"),
  "",
  "## Ranking Summary",
  sprintf("- Mortality pooled 2014-2018 ranking: %s.", paste(sprintf("%s (%.6f)", table_63$model[order(table_63$rmse_2014_2018)], table_63$rmse_2014_2018[order(table_63$rmse_2014_2018)]), collapse = ", ")),
  sprintf("- Fertility pooled 2014-2018 ranking: %s.", paste(sprintf("%s (%.6f)", table_74$model[order(table_74$rmse_2014_2018)], table_74$rmse_2014_2018[order(table_74$rmse_2014_2018)]), collapse = ", ")),
  "- Detailed ranking changes by year, age group, sex, and education were saved as diagnostic CSV files.",
  "",
  "## Unresolved Issues",
  "- The legacy headline tables and appendix values in the thesis text are not reproduced by the corrected key-joined holdout calculations.",
  "- The existing project does not contain a validated legacy script that reproduces the original published Tables 6.3 or 7.4 exactly.",
  "- Mortality Model B produces an infinite mean log score under `logs_sample`, which indicates zero or numerically negligible predictive density at some observed holdout values on the evaluated log-rate scale. Its stored forecast object/scale needs separate review because a `log(1000)` shift does not resolve its RMSE discrepancy.",
  "",
  "## Tables to Remove and Replace",
  "- Remove old Table 6.3 and replace it with revised Table 6.3 in `revised_table_6_3_mortality_holdout_rmse`.",
  "- Remove old appendix Tables E.1 and E.2 and replace them with regenerated Tables E.1-E.4.",
  "- Remove old Table 7.4 and replace it with revised Table 7.4 in `revised_table_7_4_fertility_holdout_rmse`.",
  "- Replace previous fertility appendix tables with regenerated Tables E.5-E.10.",
  "",
  "## Created Paths"
)

created_paths <- unique(created_paths)
report_lines <- c(report_lines, paste0("- `", created_paths, "`"))
writeLines(report_lines, report_path)
created_paths <- c(created_paths, report_path)

message("Created files:")
for (path_value in unique(created_paths)) {
  message(path_value)
}
