#!/usr/bin/env Rscript

# RMSE validation workflow for the thesis forecasting models.
# This script reuses the same cleaned observations and posterior forecast draws.

source(file.path(dirname(normalizePath(script_path_from_args <- sub("^--file=", "", commandArgs(trailingOnly = FALSE)[grep("^--file=", commandArgs(trailingOnly = FALSE))][1]), mustWork = FALSE)), "project_paths.R"))
paths <- project_paths()
project_root <- paths$project_root
data_dir <- paths$data_dir
output_dir <- file.path(paths$results_dir, "rmse_validation")

if (!requireNamespace("scoringRules", quietly = TRUE)) {
  stop("Package 'scoringRules' is required for the RMSE/CRPS comparison outputs.")
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (!identical(Sys.getenv("ALLOW_INVALID_FITTED_EVALUATION"), "1")) {
  stop(
    paste(
      "This script's mortality RMSE path scores fitted logmu values, not documented 2014-2018 forecast draws.",
      "Run make revised-evaluation to create the artifact audit.",
      "Use make legacy-rmse only to reproduce the invalid historical calculation for audit purposes."
    ),
    call. = FALSE
  )
}

holdout_years <- 2014:2018
all_years <- 1998:2018
holdout_year_idx <- match(holdout_years, all_years)

assert_true <- function(condition, message_text) {
  if (!isTRUE(condition)) {
    stop(message_text, call. = FALSE)
  }
}

write_csv_safe <- function(df, path) {
  write.csv(df, path, row.names = FALSE, na = "")
}

log_validation <- function(label, df, model_col = "model", year_col = NULL, extra_cols = NULL) {
  message(sprintf("[%s] rows=%d cols=%d", label, nrow(df), ncol(df)))

  if (!is.null(year_col) && year_col %in% names(df)) {
    years_present <- sort(unique(df[[year_col]]))
    message(sprintf("[%s] years=%s", label, paste(years_present, collapse = ", ")))
  }

  if (model_col %in% names(df)) {
    message(sprintf("[%s] models=%s", label, paste(sort(unique(df[[model_col]])), collapse = ", ")))
  }

  if (!is.null(extra_cols)) {
    for (col_name in extra_cols) {
      if (col_name %in% names(df)) {
        message(sprintf("[%s] %s=%s", label, col_name, paste(sort(unique(df[[col_name]])), collapse = ", ")))
      }
    }
  }
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
        stop(sprintf("Column for logmu[%d,%d] was not found in the chain.", age_idx, year_idx))
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
        stop(sprintf("Column for %s[%d,%d] was not found in the chain.", base_name, age_idx, year_idx))
      }

      draw_matrix[row_idx, ] <- chain_df[seq_len(n_draws), col_name]
      row_idx <- row_idx + 1
    }
  }

  draw_matrix
}

build_long_panel <- function(observed_log_matrix, observed_rate_matrix, draw_matrix_log, years, age_labels,
                             outcome, sex = NA_character_, education = NA_character_) {
  n_age <- nrow(observed_log_matrix)
  n_year <- ncol(observed_log_matrix)

  assert_true(identical(dim(observed_log_matrix), dim(observed_rate_matrix)),
              sprintf("%s observed log and rate matrices have different dimensions.", outcome))
  assert_true(nrow(draw_matrix_log) == n_age * n_year,
              sprintf("%s draw matrix rows (%d) do not match n_age * n_year (%d).",
                      outcome, nrow(draw_matrix_log), n_age * n_year))
  assert_true(length(years) == n_year,
              sprintf("%s years length (%d) does not match matrix columns (%d).",
                      outcome, length(years), n_year))
  assert_true(length(age_labels) == n_age,
              sprintf("%s age labels length (%d) does not match matrix rows (%d).",
                      outcome, length(age_labels), n_age))

  cell_idx <- seq_len(n_age * n_year)
  age_index <- rep(seq_len(n_age), each = n_year)
  year_index <- rep(seq_len(n_year), times = n_age)

  df <- data.frame(
    outcome = outcome,
    sex = sex,
    education = education,
    age_index = age_index,
    age_group = age_labels[age_index],
    year_index = year_index,
    year = years[year_index],
    cell_index = cell_idx,
    observed_log = as.vector(t(as.matrix(observed_log_matrix))),
    observed_rate = as.vector(t(as.matrix(observed_rate_matrix))),
    predicted_log = rowMeans(draw_matrix_log),
    predicted_rate = rowMeans(exp(draw_matrix_log)),
    stringsAsFactors = FALSE
  )

  df
}

add_crps_column <- function(df, draw_matrix_log, observed_col, output_col) {
  df[[output_col]] <- scoringRules::crps_sample(
    y = df[[observed_col]],
    dat = draw_matrix_log[df$cell_index, , drop = FALSE]
  )
  df
}

add_model_table <- function(df, model_name, model_label) {
  df$model_key <- model_name
  df$model <- model_label
  df
}

summarise_metric <- function(df, group_cols, prediction_col, observed_col, include_crps_col = NULL) {
  required_cols <- c(group_cols, prediction_col, observed_col)
  missing_cols <- setdiff(required_cols, names(df))
  assert_true(length(missing_cols) == 0,
              sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")))

  grouping_factor <- interaction(df[group_cols], drop = TRUE, lex.order = TRUE)
  split_idx <- split(seq_len(nrow(df)), grouping_factor)

  out <- lapply(split_idx, function(idx) {
    sub_df <- df[idx, , drop = FALSE]
    complete_idx <- complete.cases(sub_df[, c(prediction_col, observed_col)])
    sub_complete <- sub_df[complete_idx, , drop = FALSE]

    assert_true(nrow(sub_complete) > 0,
                sprintf("No complete observations available for grouping: %s",
                        paste(sub_df[1, group_cols, drop = TRUE], collapse = " | ")))

    mse_value <- mean((sub_complete[[prediction_col]] - sub_complete[[observed_col]])^2)
    row_out <- sub_complete[1, group_cols, drop = FALSE]
    row_out$n_observations <- nrow(sub_complete)
    row_out$mse <- mse_value
    row_out$rmse <- sqrt(mse_value)

    if (!is.null(include_crps_col) && include_crps_col %in% names(sub_complete)) {
      row_out$mean_crps <- mean(sub_complete[[include_crps_col]], na.rm = TRUE)
    }

    row_out
  })

  result <- do.call(rbind, out)
  rownames(result) <- NULL
  result
}

standardise_summary_table <- function(df, domain, summary_name, outcome, scale, period) {
  target_cols <- c(
    "domain", "summary_name", "outcome", "scale", "period",
    "model", "year", "sex", "education", "age_group",
    "n_observations", "mse", "rmse", "mean_crps"
  )

  df$domain <- domain
  df$summary_name <- summary_name
  df$outcome <- outcome
  df$scale <- scale
  df$period <- period

  for (col_name in target_cols) {
    if (!col_name %in% names(df)) {
      df[[col_name]] <- NA
    }
  }

  df[, target_cols]
}

build_long_comparison <- function(values_matrix, value_name, groups, models) {
  rows <- vector("list", length(groups) * length(models))
  row_idx <- 1
  for (group_name in groups) {
    for (model_name in models) {
      rows[[row_idx]] <- data.frame(
        group = group_name,
        model = model_name,
        value = values_matrix[group_name, model_name],
        stringsAsFactors = FALSE
      )
      row_idx <- row_idx + 1
    }
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  names(out)[names(out) == "value"] <- value_name
  out
}

report_candidate_table <- function(df, value_col, label) {
  rows <- apply(df, 1, function(row) {
    sprintf("| %s | %.6f |", row[["model"]], as.numeric(row[[value_col]]))
  })

  c(
    sprintf("### %s", label),
    "| model | value |",
    "|---|---:|",
    rows,
    ""
  )
}

model_labels_mortality <- c("Model A", "Model B", "Model C", "Model D")
model_labels_fertility <- c("Model A", "Model B", "Model C")
education_labels <- c("No Formal Education", "Primary", "Secondary", "Post Secondary")
mortality_age_labels <- sprintf("Age %02d", seq_len(11))
fertility_age_labels <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44")

# Models C and D save mortality log-rates on a rate scale 1,000 times smaller
# than the observed and Model A scale. Convert before calculating all scores.
mortality_log_scale_offsets <- c(
  "Model A" = 0,
  "Model B" = 0,
  "Model C" = log(1000),
  "Model D" = log(1000)
)

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

for (path_value in unique(c(
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
))) {
  assert_true(file.exists(file.path(data_dir, path_value)),
              sprintf("Required input file is missing: %s", file.path(data_dir, path_value)))
}

mortality_long <- list()

for (i in seq_len(nrow(mortality_series))) {
  cfg <- mortality_series[i, ]

  observed_rate <- as.matrix(read.csv(file.path(data_dir, cfg$rate_file)))
  exposure <- as.matrix(read.csv(file.path(data_dir, cfg$exposure_file)))
  observed_log <- log(observed_rate / exposure)

  assert_true(identical(dim(observed_log), c(11L, 21L)),
              sprintf("%s does not have the expected 11 x 21 shape.", cfg$series))

  chain_a <- read.csv(file.path(data_dir, cfg$model_a_file))
  chain_b <- read.csv(file.path(data_dir, cfg$model_b_chain))
  chain_c <- read.csv(file.path(data_dir, cfg$model_c_chain))
  chain_d <- read.csv(file.path(data_dir, cfg$model_d_chain))

  draw_a <- build_logmu_draw_matrix(chain_a, n_age = 11, n_year = 21)
  draw_b <- build_named_draw_matrix(chain_b, base_name = cfg$model_b_base, n_age = 11, n_year = 21)
  draw_c <- build_named_draw_matrix(chain_c, base_name = cfg$model_c_base, n_age = 11, n_year = 21) + mortality_log_scale_offsets[["Model C"]]
  draw_d <- build_named_draw_matrix(chain_d, base_name = cfg$model_d_base, n_age = 11, n_year = 21) + mortality_log_scale_offsets[["Model D"]]

  panel_base <- build_long_panel(
    observed_log_matrix = observed_log,
    observed_rate_matrix = observed_rate / exposure,
    draw_matrix_log = draw_a,
    years = all_years,
    age_labels = mortality_age_labels,
    outcome = "mortality_log_rate",
    sex = cfg$sex,
    education = cfg$education
  )

  model_panels <- list(
    add_model_table(panel_base, "model_a", model_labels_mortality[1]),
    add_model_table(build_long_panel(observed_log, observed_rate / exposure, draw_b, all_years, mortality_age_labels,
                                     "mortality_log_rate", cfg$sex, cfg$education), "model_b", model_labels_mortality[2]),
    add_model_table(build_long_panel(observed_log, observed_rate / exposure, draw_c, all_years, mortality_age_labels,
                                     "mortality_log_rate", cfg$sex, cfg$education), "model_c", model_labels_mortality[3]),
    add_model_table(build_long_panel(observed_log, observed_rate / exposure, draw_d, all_years, mortality_age_labels,
                                     "mortality_log_rate", cfg$sex, cfg$education), "model_d", model_labels_mortality[4])
  )

  draw_list <- list(draw_a, draw_b, draw_c, draw_d)
  for (j in seq_along(model_panels)) {
    model_panels[[j]] <- add_crps_column(model_panels[[j]], draw_list[[j]], "observed_log", "crps_log")
    model_panels[[j]]$series <- cfg$series
  }

  mortality_long[[cfg$series]] <- do.call(rbind, model_panels)
}

mortality_long <- do.call(rbind, mortality_long)
rownames(mortality_long) <- NULL

assert_true(all(holdout_years %in% mortality_long$year),
            "Mortality holdout years 2014-2018 were not all found in the long data.")
assert_true(all(model_labels_mortality %in% mortality_long$model),
            "Not all mortality models are present in the long data.")
assert_true(all(education_labels %in% mortality_long$education),
            "Not all mortality education groups are present in the long data.")
assert_true(all(c("Female", "Male") %in% mortality_long$sex),
            "Both mortality sexes must be present in the long data.")

log_validation("Mortality long data", mortality_long, year_col = "year", extra_cols = c("sex", "education"))

mortality_holdout <- subset(mortality_long, year %in% holdout_years)
mortality_2018 <- subset(mortality_holdout, year == 2018)

assert_true(nrow(mortality_holdout) == length(model_labels_mortality) * 8L * 11L * 5L,
            sprintf("Unexpected mortality holdout row count: %d", nrow(mortality_holdout)))

table_m1 <- summarise_metric(
  mortality_holdout,
  group_cols = c("model"),
  prediction_col = "predicted_log",
  observed_col = "observed_log",
  include_crps_col = "crps_log"
)
table_m1 <- table_m1[, c("model", "n_observations", "rmse", "mean_crps")]

table_m2 <- summarise_metric(
  mortality_holdout,
  group_cols = c("model", "year"),
  prediction_col = "predicted_log",
  observed_col = "observed_log",
  include_crps_col = "crps_log"
)
table_m2 <- table_m2[, c("model", "year", "n_observations", "rmse", "mean_crps")]

table_m3 <- summarise_metric(
  mortality_holdout,
  group_cols = c("model", "sex", "education"),
  prediction_col = "predicted_log",
  observed_col = "observed_log",
  include_crps_col = "crps_log"
)
table_m3 <- table_m3[, c("model", "sex", "education", "n_observations", "mse", "rmse", "mean_crps")]

table_m4 <- summarise_metric(
  mortality_holdout,
  group_cols = c("model", "age_group"),
  prediction_col = "predicted_log",
  observed_col = "observed_log",
  include_crps_col = "crps_log"
)
table_m4 <- table_m4[, c("model", "age_group", "n_observations", "rmse", "mean_crps")]

table_m5 <- summarise_metric(
  mortality_holdout,
  group_cols = c("model", "year", "sex", "education"),
  prediction_col = "predicted_log",
  observed_col = "observed_log",
  include_crps_col = "crps_log"
)
table_m5 <- table_m5[, c("model", "year", "sex", "education", "n_observations", "rmse", "mean_crps")]

table_m6 <- summarise_metric(
  mortality_2018,
  group_cols = c("model"),
  prediction_col = "predicted_log",
  observed_col = "observed_log",
  include_crps_col = "crps_log"
)
table_m6 <- table_m6[, c("model", "n_observations", "rmse", "mean_crps")]

log_validation("Table M1", table_m1)
log_validation("Table M2", table_m2, year_col = "year")
log_validation("Table M3", table_m3, extra_cols = c("sex", "education"))
log_validation("Table M4", table_m4, extra_cols = c("age_group"))
log_validation("Table M5", table_m5, year_col = "year", extra_cols = c("sex", "education"))
log_validation("Table M6", table_m6)

rmse_mse_check <- transform(
  table_m3,
  sqrt_mse = sqrt(mse),
  diff = abs(rmse - sqrt(mse)),
  within_tolerance = abs(rmse - sqrt(mse)) < 1e-10
)

appendix_groups <- c(
  "No Formal Education, Female",
  "No Formal Education, Male",
  "Primary, Female",
  "Primary, Male",
  "Secondary, Female",
  "Secondary, Male",
  "Post Secondary, Female",
  "Post Secondary, Male"
)

appendix_rmse_matrix <- rbind(
  c(1.00, 0.97, 1.04, 1.02),
  c(0.74, 0.73, 1.00, 0.85),
  c(1.43, 1.44, 1.45, 1.47),
  c(1.10, 1.12, 1.08, 1.13),
  c(1.69, 1.69, 1.74, 1.72),
  c(1.36, 1.38, 1.47, 1.36),
  c(1.78, 1.78, 1.80, 1.82),
  c(1.66, 1.61, 1.62, 1.57)
)
rownames(appendix_rmse_matrix) <- appendix_groups
colnames(appendix_rmse_matrix) <- model_labels_mortality

appendix_mse_matrix <- rbind(
  c(0.99, 0.95, 1.09, 1.03),
  c(0.54, 0.53, 1.00, 0.72),
  c(2.06, 2.06, 2.11, 2.16),
  c(1.22, 1.26, 1.16, 1.27),
  c(2.85, 2.85, 3.01, 2.96),
  c(1.84, 1.90, 2.16, 1.85),
  c(3.16, 3.18, 3.23, 3.30),
  c(2.76, 2.59, 2.63, 2.48)
)
rownames(appendix_mse_matrix) <- appendix_groups
colnames(appendix_mse_matrix) <- model_labels_mortality

appendix_rmse_long <- build_long_comparison(appendix_rmse_matrix, "appendix_rmse", appendix_groups, model_labels_mortality)
appendix_mse_long <- build_long_comparison(appendix_mse_matrix, "appendix_mse", appendix_groups, model_labels_mortality)
appendix_compare <- merge(appendix_rmse_long, appendix_mse_long, by = c("group", "model"))

table_m3_compare <- transform(
  table_m3,
  group = paste(education, sex, sep = ", "),
  rounded_rmse = round(rmse, 2),
  rounded_mse = round(mse, 2)
)

appendix_validation <- merge(
  table_m3_compare[, c("model", "group", "n_observations", "mse", "rmse", "rounded_mse", "rounded_rmse", "mean_crps")],
  appendix_compare,
  by = c("model", "group"),
  all.x = TRUE
)

appendix_validation$rmse_matches_rounded <- appendix_validation$rounded_rmse == appendix_validation$appendix_rmse
appendix_validation$mse_matches_rounded <- appendix_validation$rounded_mse == appendix_validation$appendix_mse
appendix_validation$rmse_minus_appendix <- appendix_validation$rmse - appendix_validation$appendix_rmse
appendix_validation$mse_minus_appendix <- appendix_validation$mse - appendix_validation$appendix_mse

calc_from_group_rmse <- aggregate(rmse ~ model, data = table_m3, FUN = mean)
names(calc_from_group_rmse)[names(calc_from_group_rmse) == "rmse"] <- "mean_of_group_rmses"

training_years <- 1998:2013
mortality_training <- subset(mortality_long, year %in% training_years)

table_63_reported <- data.frame(
  model = model_labels_mortality,
  reported_table_63 = c(5.93, 38.46, 8.57, 1.40),
  stringsAsFactors = FALSE
)

mortality_diagnostic_candidates <- list(
  correct_holdout_log = summarise_metric(mortality_holdout, c("model"), "predicted_log", "observed_log")[, c("model", "rmse")],
  final_year_2018_log = summarise_metric(mortality_2018, c("model"), "predicted_log", "observed_log")[, c("model", "rmse")],
  all_years_log = summarise_metric(mortality_long, c("model"), "predicted_log", "observed_log")[, c("model", "rmse")],
  training_1998_2013_log = summarise_metric(mortality_training, c("model"), "predicted_log", "observed_log")[, c("model", "rmse")],
  holdout_rate = summarise_metric(mortality_holdout, c("model"), "predicted_rate", "observed_rate")[, c("model", "rmse")],
  mean_group_rmse = calc_from_group_rmse,
  holdout_sqrt_sse = aggregate((predicted_log - observed_log)^2 ~ model, data = mortality_holdout, FUN = sum),
  misaligned_holdout = NULL
)

names(mortality_diagnostic_candidates$correct_holdout_log)[2] <- "value"
names(mortality_diagnostic_candidates$final_year_2018_log)[2] <- "value"
names(mortality_diagnostic_candidates$all_years_log)[2] <- "value"
names(mortality_diagnostic_candidates$training_1998_2013_log)[2] <- "value"
names(mortality_diagnostic_candidates$holdout_rate)[2] <- "value"
names(mortality_diagnostic_candidates$mean_group_rmse)[2] <- "value"
names(mortality_diagnostic_candidates$holdout_sqrt_sse)[2] <- "value"
mortality_diagnostic_candidates$holdout_sqrt_sse$value <- sqrt(mortality_diagnostic_candidates$holdout_sqrt_sse$value)
mortality_diagnostic_candidates$holdout_sqrt_sse <- mortality_diagnostic_candidates$holdout_sqrt_sse[, c("model", "value")]

misaligned_mortality <- list()
for (i in seq_len(nrow(mortality_series))) {
  cfg <- mortality_series[i, ]
  observed_rate <- as.matrix(read.csv(file.path(data_dir, cfg$rate_file)))
  exposure <- as.matrix(read.csv(file.path(data_dir, cfg$exposure_file)))
  observed_log <- log(observed_rate / exposure)

  chain_a <- read.csv(file.path(data_dir, cfg$model_a_file))
  chain_b <- read.csv(file.path(data_dir, cfg$model_b_chain))
  chain_c <- read.csv(file.path(data_dir, cfg$model_c_chain))
  chain_d <- read.csv(file.path(data_dir, cfg$model_d_chain))

  draw_list <- list(
    build_logmu_draw_matrix(chain_a, 11, 21),
    build_named_draw_matrix(chain_b, cfg$model_b_base, 11, 21),
    build_named_draw_matrix(chain_c, cfg$model_c_base, 11, 21),
    build_named_draw_matrix(chain_d, cfg$model_d_base, 11, 21)
  )

  observed_wrong <- as.vector(as.matrix(observed_log))
  keep_rows <- rep(holdout_year_idx, times = 11) + rep((seq_len(11) - 1) * 21, each = length(holdout_year_idx))

  for (j in seq_along(draw_list)) {
    predicted_wrong <- rowMeans(draw_list[[j]])[keep_rows]
    observed_wrong_holdout <- observed_wrong[keep_rows]
    misaligned_mortality[[length(misaligned_mortality) + 1]] <- data.frame(
      model = model_labels_mortality[j],
      squared_error = (predicted_wrong - observed_wrong_holdout)^2,
      stringsAsFactors = FALSE
    )
  }
}

misaligned_mortality <- do.call(rbind, misaligned_mortality)
mortality_diagnostic_candidates$misaligned_holdout <- aggregate(squared_error ~ model, data = misaligned_mortality, FUN = mean)
names(mortality_diagnostic_candidates$misaligned_holdout)[2] <- "value"
mortality_diagnostic_candidates$misaligned_holdout$value <- sqrt(mortality_diagnostic_candidates$misaligned_holdout$value)

mortality_diagnostic_summary <- do.call(
  rbind,
  lapply(names(mortality_diagnostic_candidates), function(candidate_name) {
    candidate_df <- mortality_diagnostic_candidates[[candidate_name]]
    merged <- merge(candidate_df, table_63_reported, by = "model", all.x = TRUE)
    merged$candidate <- candidate_name
    merged$abs_diff_from_reported <- abs(merged$value - merged$reported_table_63)
    merged
  })
)

fertility_long <- list()

for (i in seq_len(nrow(fertility_series))) {
  cfg <- fertility_series[i, ]

  observed_rate <- as.matrix(read.csv(file.path(data_dir, cfg$rate_file)))
  exposure <- as.matrix(read.csv(file.path(data_dir, cfg$exposure_file)))
  observed_log <- log(observed_rate / exposure)

  assert_true(identical(dim(observed_log), c(6L, 21L)),
              sprintf("%s does not have the expected 6 x 21 shape.", cfg$series))

  chain_a <- read.csv(file.path(data_dir, cfg$model_a_file))
  chain_b <- read.csv(file.path(data_dir, cfg$model_b_chain))
  chain_c <- read.csv(file.path(data_dir, cfg$model_c_chain))

  draw_a <- build_logmu_draw_matrix(chain_a, n_age = 6, n_year = 21)
  draw_b <- build_named_draw_matrix(chain_b, base_name = cfg$model_b_base, n_age = 6, n_year = 21)
  draw_c <- build_named_draw_matrix(chain_c, base_name = cfg$model_c_base, n_age = 6, n_year = 21)

  model_panels <- list(
    add_model_table(build_long_panel(observed_log, observed_rate / exposure, draw_a, all_years, fertility_age_labels,
                                     "fertility_log_asfr", education = cfg$education), "model_a", model_labels_fertility[1]),
    add_model_table(build_long_panel(observed_log, observed_rate / exposure, draw_b, all_years, fertility_age_labels,
                                     "fertility_log_asfr", education = cfg$education), "model_b", model_labels_fertility[2]),
    add_model_table(build_long_panel(observed_log, observed_rate / exposure, draw_c, all_years, fertility_age_labels,
                                     "fertility_log_asfr", education = cfg$education), "model_c", model_labels_fertility[3])
  )

  draw_list <- list(draw_a, draw_b, draw_c)
  for (j in seq_along(model_panels)) {
    model_panels[[j]] <- add_crps_column(model_panels[[j]], draw_list[[j]], "observed_log", "crps_log")
    model_panels[[j]]$series <- cfg$series
    model_panels[[j]]$exposure_count <- as.vector(t(exposure))
  }

  fertility_long[[cfg$series]] <- do.call(rbind, model_panels)
}

fertility_long <- do.call(rbind, fertility_long)
rownames(fertility_long) <- NULL

assert_true(all(holdout_years %in% fertility_long$year),
            "Fertility holdout years 2014-2018 were not all found in the long data.")
assert_true(all(model_labels_fertility %in% fertility_long$model),
            "Not all fertility models are present in the long data.")
assert_true(all(education_labels %in% fertility_long$education),
            "Not all fertility education groups are present in the long data.")

log_validation("Fertility long data", fertility_long, year_col = "year", extra_cols = c("education", "age_group"))

fertility_holdout <- subset(fertility_long, year %in% holdout_years)
fertility_2018 <- subset(fertility_holdout, year == 2018)

table_f1 <- summarise_metric(
  fertility_holdout,
  group_cols = c("model"),
  prediction_col = "predicted_log",
  observed_col = "observed_log",
  include_crps_col = "crps_log"
)
table_f1 <- table_f1[, c("model", "n_observations", "rmse", "mean_crps")]

table_f2 <- summarise_metric(
  fertility_holdout,
  group_cols = c("model", "year"),
  prediction_col = "predicted_log",
  observed_col = "observed_log",
  include_crps_col = "crps_log"
)
table_f2 <- table_f2[, c("model", "year", "n_observations", "rmse", "mean_crps")]

table_f3 <- summarise_metric(
  fertility_holdout,
  group_cols = c("model", "age_group"),
  prediction_col = "predicted_log",
  observed_col = "observed_log",
  include_crps_col = "crps_log"
)
table_f3 <- table_f3[, c("model", "age_group", "n_observations", "rmse", "mean_crps")]

table_f4 <- summarise_metric(
  fertility_holdout,
  group_cols = c("model", "education"),
  prediction_col = "predicted_log",
  observed_col = "observed_log",
  include_crps_col = "crps_log"
)
table_f4 <- table_f4[, c("model", "education", "n_observations", "rmse", "mean_crps")]

table_f5 <- summarise_metric(
  fertility_holdout,
  group_cols = c("model", "year", "age_group", "education"),
  prediction_col = "predicted_log",
  observed_col = "observed_log",
  include_crps_col = "crps_log"
)
table_f5 <- table_f5[, c("model", "year", "age_group", "education", "n_observations", "rmse", "mean_crps")]

table_f6 <- summarise_metric(
  fertility_2018,
  group_cols = c("model"),
  prediction_col = "predicted_log",
  observed_col = "observed_log",
  include_crps_col = "crps_log"
)
table_f6 <- table_f6[, c("model", "n_observations", "rmse", "mean_crps")]

fertility_asfr_rate_compare <- rbind(
  transform(
    summarise_metric(fertility_holdout, c("model"), "predicted_log", "observed_log", "crps_log")[, c("model", "n_observations", "rmse", "mean_crps")],
    outcome = "ASFR",
    scale = "log"
  ),
  transform(
    summarise_metric(fertility_holdout, c("model"), "predicted_rate", "observed_rate")[, c("model", "n_observations", "rmse")],
    mean_crps = NA_real_,
    outcome = "ASFR",
    scale = "rate"
  )
)

fertility_tfr_rows <- list()
fertility_tfr_aggregate_rows <- list()

for (model_name in model_labels_fertility) {
  model_df <- subset(fertility_holdout, model == model_name)

  by_edu_year <- split(model_df, interaction(model_df$education, model_df$year, drop = TRUE, lex.order = TRUE))
  for (sub_df in by_edu_year) {
    observed_tfr <- 5 * sum(sub_df$observed_rate)
    predicted_tfr <- 5 * sum(sub_df$predicted_rate)
    fertility_tfr_rows[[length(fertility_tfr_rows) + 1]] <- data.frame(
      model = model_name,
      education = sub_df$education[1],
      year = sub_df$year[1],
      observed_tfr = observed_tfr,
      predicted_tfr = predicted_tfr,
      stringsAsFactors = FALSE
    )
  }

  by_year <- split(model_df, model_df$year)
  for (sub_df in by_year) {
    agg_by_age <- split(sub_df, interaction(sub_df$age_group, drop = TRUE))
    observed_tfr <- 0
    predicted_tfr <- 0
    for (age_df in agg_by_age) {
      observed_rate_agg <- sum(age_df$observed_rate * age_df$exposure_count) / sum(age_df$exposure_count)
      predicted_rate_agg <- sum(age_df$predicted_rate * age_df$exposure_count) / sum(age_df$exposure_count)
      observed_tfr <- observed_tfr + 5 * observed_rate_agg
      predicted_tfr <- predicted_tfr + 5 * predicted_rate_agg
    }

    fertility_tfr_aggregate_rows[[length(fertility_tfr_aggregate_rows) + 1]] <- data.frame(
      model = model_name,
      year = unique(sub_df$year),
      observed_tfr = observed_tfr,
      predicted_tfr = predicted_tfr,
      stringsAsFactors = FALSE
    )
  }
}

fertility_tfr_by_education <- do.call(rbind, fertility_tfr_rows)
fertility_tfr_by_education$education_specific_squared_error <- (fertility_tfr_by_education$predicted_tfr - fertility_tfr_by_education$observed_tfr)^2

fertility_tfr_aggregate <- do.call(rbind, fertility_tfr_aggregate_rows)
fertility_tfr_aggregate$aggregate_squared_error <- (fertility_tfr_aggregate$predicted_tfr - fertility_tfr_aggregate$observed_tfr)^2

fertility_tfr_compare <- rbind(
  data.frame(
    model = aggregate(education_specific_squared_error ~ model, data = fertility_tfr_by_education, FUN = mean)$model,
    n_observations = aggregate(education_specific_squared_error ~ model, data = fertility_tfr_by_education, FUN = length)$education_specific_squared_error,
    rmse = sqrt(aggregate(education_specific_squared_error ~ model, data = fertility_tfr_by_education, FUN = mean)$education_specific_squared_error),
    outcome = "Education-specific TFR",
    scale = "rate",
    stringsAsFactors = FALSE
  ),
  data.frame(
    model = aggregate(aggregate_squared_error ~ model, data = fertility_tfr_aggregate, FUN = mean)$model,
    n_observations = aggregate(aggregate_squared_error ~ model, data = fertility_tfr_aggregate, FUN = length)$aggregate_squared_error,
    rmse = sqrt(aggregate(aggregate_squared_error ~ model, data = fertility_tfr_aggregate, FUN = mean)$aggregate_squared_error),
    outcome = "Aggregate TFR",
    scale = "rate",
    stringsAsFactors = FALSE
  )
)

fertility_metric_compare <- fertility_asfr_rate_compare[, c("model", "n_observations", "rmse", "mean_crps", "outcome", "scale")]
fertility_tfr_compare$mean_crps <- NA_real_
fertility_metric_compare <- rbind(
  fertility_metric_compare,
  fertility_tfr_compare[, c("model", "n_observations", "rmse", "mean_crps", "outcome", "scale")]
)

table_74_reported <- data.frame(
  model = model_labels_fertility,
  reported_table_74 = c(32.11, 55.18, 6.77),
  stringsAsFactors = FALSE
)

fertility_diagnostic_candidates <- list(
  correct_holdout_log_asfr = summarise_metric(fertility_holdout, c("model"), "predicted_log", "observed_log")[, c("model", "rmse")],
  final_year_2018_log_asfr = summarise_metric(fertility_2018, c("model"), "predicted_log", "observed_log")[, c("model", "rmse")],
  holdout_rate_asfr = summarise_metric(fertility_holdout, c("model"), "predicted_rate", "observed_rate")[, c("model", "rmse")],
  education_specific_tfr = fertility_tfr_compare[fertility_tfr_compare$outcome == "Education-specific TFR", c("model", "rmse")],
  aggregate_tfr = fertility_tfr_compare[fertility_tfr_compare$outcome == "Aggregate TFR", c("model", "rmse")]
)

fertility_diagnostic_summary <- do.call(
  rbind,
  lapply(names(fertility_diagnostic_candidates), function(candidate_name) {
    candidate_df <- fertility_diagnostic_candidates[[candidate_name]]
    names(candidate_df)[2] <- "value"
    merged <- merge(candidate_df, table_74_reported, by = "model", all.x = TRUE)
    merged$candidate <- candidate_name
    merged$abs_diff_from_reported <- abs(merged$value - merged$reported_table_74)
    merged
  })
)

merged_rmse_crps <- rbind(
  standardise_summary_table(transform(table_m1, year = NA, sex = NA, education = NA, age_group = NA), "mortality", "overall_holdout", "mortality_log_rate", "log", "2014-2018"),
  standardise_summary_table(transform(table_m2, sex = NA, education = NA, age_group = NA), "mortality", "by_year", "mortality_log_rate", "log", "2014-2018"),
  standardise_summary_table(transform(table_m3, year = NA, age_group = NA), "mortality", "by_sex_education", "mortality_log_rate", "log", "2014-2018"),
  standardise_summary_table(transform(table_m4, year = NA, sex = NA, education = NA), "mortality", "by_age_group", "mortality_log_rate", "log", "2014-2018"),
  standardise_summary_table(transform(table_m5, age_group = NA), "mortality", "by_year_sex_education", "mortality_log_rate", "log", "2014-2018"),
  standardise_summary_table(transform(table_m6, year = 2018, sex = NA, education = NA, age_group = NA), "mortality", "overall_2018", "mortality_log_rate", "log", "2018"),
  standardise_summary_table(transform(table_f1, year = NA, sex = NA, education = NA, age_group = NA), "fertility", "overall_holdout", "fertility_log_asfr", "log", "2014-2018"),
  standardise_summary_table(transform(table_f2, sex = NA, education = NA, age_group = NA), "fertility", "by_year", "fertility_log_asfr", "log", "2014-2018"),
  standardise_summary_table(transform(table_f3, year = NA, sex = NA, education = NA), "fertility", "by_age_group", "fertility_log_asfr", "log", "2014-2018"),
  standardise_summary_table(transform(table_f4, year = NA, sex = NA, age_group = NA), "fertility", "by_education", "fertility_log_asfr", "log", "2014-2018"),
  standardise_summary_table(transform(table_f5, sex = NA), "fertility", "by_year_age_group_education", "fertility_log_asfr", "log", "2014-2018"),
  standardise_summary_table(transform(table_f6, year = 2018, sex = NA, education = NA, age_group = NA), "fertility", "overall_2018", "fertility_log_asfr", "log", "2018")
)

write_csv_safe(table_m1, file.path(output_dir, "table_m1_mortality_overall_holdout.csv"))
write_csv_safe(table_m2, file.path(output_dir, "table_m2_mortality_by_year.csv"))
write_csv_safe(table_m3, file.path(output_dir, "table_m3_mortality_by_model_sex_education.csv"))
write_csv_safe(table_m4, file.path(output_dir, "table_m4_mortality_by_age_group.csv"))
write_csv_safe(table_m5, file.path(output_dir, "table_m5_mortality_by_year_sex_education.csv"))
write_csv_safe(table_m6, file.path(output_dir, "table_m6_mortality_2018_only.csv"))
write_csv_safe(rmse_mse_check, file.path(output_dir, "mortality_mse_rmse_identity_check.csv"))
write_csv_safe(appendix_validation, file.path(output_dir, "mortality_appendix_validation.csv"))
write_csv_safe(mortality_diagnostic_summary, file.path(output_dir, "mortality_table_63_diagnostic_candidates.csv"))

write_csv_safe(table_f1, file.path(output_dir, "fertility_overall_holdout_log_asfr.csv"))
write_csv_safe(table_f2, file.path(output_dir, "fertility_by_year_log_asfr.csv"))
write_csv_safe(table_f3, file.path(output_dir, "fertility_by_age_group_log_asfr.csv"))
write_csv_safe(table_f4, file.path(output_dir, "fertility_by_education_log_asfr.csv"))
write_csv_safe(table_f5, file.path(output_dir, "fertility_by_year_age_group_education_log_asfr.csv"))
write_csv_safe(table_f6, file.path(output_dir, "fertility_2018_only_log_asfr.csv"))
write_csv_safe(fertility_metric_compare, file.path(output_dir, "fertility_metric_comparison.csv"))
write_csv_safe(fertility_diagnostic_summary, file.path(output_dir, "fertility_table_74_diagnostic_candidates.csv"))

write_csv_safe(merged_rmse_crps, file.path(output_dir, "rmse_crps_merged_summaries.csv"))

mortality_overall_ranking <- table_m1[order(table_m1$rmse), c("model", "rmse")]
mortality_2018_ranking <- table_m6[order(table_m6$rmse), c("model", "rmse")]
fertility_overall_ranking <- table_f1[order(table_f1$rmse), c("model", "rmse")]
fertility_2018_ranking <- table_f6[order(table_f6$rmse), c("model", "rmse")]

best_mortality_candidate <- mortality_diagnostic_summary[order(mortality_diagnostic_summary$abs_diff_from_reported), ][1, ]
best_fertility_candidate <- fertility_diagnostic_summary[order(fertility_diagnostic_summary$abs_diff_from_reported), ][1, ]

report_lines <- c(
  "# RMSE Validation Report",
  "",
  "## Scope",
  "- Reused the same cleaned observation panels and posterior forecast draws used in the CRPS workflow.",
  "- Evaluated the 2014-2018 holdout period explicitly, with a separate 2018-only recalculation.",
  "- Computed RMSE from row-level squared errors on the appropriate evaluation scale.",
  "",
  "## Mortality",
  sprintf("- Corrected pooled holdout RMSE ranking: %s.", paste(sprintf("%s (%.4f)", mortality_overall_ranking$model, mortality_overall_ranking$rmse), collapse = ", ")),
  sprintf("- Corrected 2018-only RMSE ranking: %s.", paste(sprintf("%s (%.4f)", mortality_2018_ranking$model, mortality_2018_ranking$rmse), collapse = ", ")),
  sprintf("- `rmse == sqrt(mse)` check passed for all mortality model/sex/education groups: %s.", if (all(rmse_mse_check$within_tolerance)) "yes" else "no"),
  sprintf("- Appendix rounded RMSE matches: %d/%d groups.", sum(appendix_validation$rmse_matches_rounded, na.rm = TRUE), nrow(appendix_validation)),
  sprintf("- Appendix rounded MSE matches: %d/%d groups.", sum(appendix_validation$mse_matches_rounded, na.rm = TRUE), nrow(appendix_validation)),
  sprintf("- Closest attempted reproduction of Table 6.3: `%s` (minimum absolute gap %.4f).", best_mortality_candidate$candidate, best_mortality_candidate$abs_diff_from_reported),
  "- The `chain_model1*_eval.csv` files contain 16 forecast cells per age group and therefore align with the training period rather than the 2014-2018 holdout.",
  "",
  "## Fertility",
  sprintf("- Corrected pooled holdout RMSE ranking on log ASFR: %s.", paste(sprintf("%s (%.4f)", fertility_overall_ranking$model, fertility_overall_ranking$rmse), collapse = ", ")),
  sprintf("- Corrected 2018-only RMSE ranking on log ASFR: %s.", paste(sprintf("%s (%.4f)", fertility_2018_ranking$model, fertility_2018_ranking$rmse), collapse = ", ")),
  sprintf("- Closest attempted reproduction of Table 7.4: `%s` (minimum absolute gap %.4f).", best_fertility_candidate$candidate, best_fertility_candidate$abs_diff_from_reported),
  "- Separate RMSE outputs were produced for log ASFR, ASFR, education-specific TFR, and aggregate TFR.",
  "",
  "## Output Files",
  "- Mortality tables M1-M6, appendix validation, and diagnostics are in `results/rmse_validation`.",
  "- Fertility RMSE tables, metric comparisons, and diagnostics are in `results/rmse_validation`.",
  "- Combined RMSE/CRPS summaries are in `results/rmse_validation/rmse_crps_merged_summaries.csv`.",
  "",
  "## Mortality Candidate Diagnostics",
  report_candidate_table(subset(mortality_diagnostic_summary, candidate == "correct_holdout_log")[, c("model", "value")], "value", "Correct holdout pooled RMSE"),
  report_candidate_table(subset(mortality_diagnostic_summary, candidate == "final_year_2018_log")[, c("model", "value")], "value", "Correct 2018-only RMSE"),
  "",
  "## Fertility Candidate Diagnostics",
  report_candidate_table(subset(fertility_diagnostic_summary, candidate == "correct_holdout_log_asfr")[, c("model", "value")], "value", "Correct holdout pooled RMSE on log ASFR"),
  report_candidate_table(subset(fertility_diagnostic_summary, candidate == "final_year_2018_log_asfr")[, c("model", "value")], "value", "Correct 2018-only RMSE on log ASFR")
)

writeLines(unlist(report_lines), file.path(output_dir, "rmse_validation_report.md"))

message("RMSE validation outputs written to: ", output_dir)
