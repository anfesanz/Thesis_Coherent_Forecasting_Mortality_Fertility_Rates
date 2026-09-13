# Shared path configuration for portable R workflows.
# Set FORECAST_ASSESSMENT_DATA_DIR or FORECAST_ASSESSMENT_OUTPUT_DIR to use
# external storage (for example, a OneDrive folder) without editing scripts.

script_path_from_args <- function() {
  file_args <- commandArgs(trailingOnly = FALSE)
  file_args <- file_args[grepl("^--file=", file_args)]

  if (length(file_args) == 0) {
    return(NULL)
  }

  normalizePath(sub("^--file=", "", file_args[1]), mustWork = FALSE)
}

find_project_root <- function(start = script_path_from_args()) {
  if (is.null(start)) {
    start <- getwd()
  }

  current <- normalizePath(if (dir.exists(start)) start else dirname(start), mustWork = FALSE)
  repeat {
    if (file.exists(file.path(current, "Project.toml")) && dir.exists(file.path(current, "src"))) {
      return(current)
    }

    parent <- dirname(current)
    if (identical(parent, current)) {
      stop("Could not locate the project root. Set FORECAST_ASSESSMENT_PROJECT_ROOT.", call. = FALSE)
    }
    current <- parent
  }
}

project_paths <- function() {
  configured_root <- Sys.getenv("FORECAST_ASSESSMENT_PROJECT_ROOT", unset = "")
  project_root <- if (nzchar(configured_root)) {
    normalizePath(configured_root, mustWork = TRUE)
  } else {
    find_project_root()
  }

  configured_data <- Sys.getenv("FORECAST_ASSESSMENT_DATA_DIR", unset = "")
  configured_output <- Sys.getenv("FORECAST_ASSESSMENT_OUTPUT_DIR", unset = "")
  configured_results <- Sys.getenv("FORECAST_ASSESSMENT_RESULTS_DIR", unset = "")
  data_dir <- if (nzchar(configured_data)) configured_data else file.path(project_root, "data")
  output_dir <- if (nzchar(configured_output)) configured_output else file.path(project_root, "outputs")
  results_dir <- if (nzchar(configured_results)) configured_results else file.path(project_root, "results")

  data_dir <- normalizePath(data_dir, mustWork = FALSE)
  output_dir <- normalizePath(output_dir, mustWork = FALSE)
  results_dir <- normalizePath(results_dir, mustWork = FALSE)

  if (!dir.exists(data_dir)) {
    stop(sprintf("Data directory does not exist: %s", data_dir), call. = FALSE)
  }

  list(project_root = project_root, data_dir = data_dir, output_dir = output_dir, results_dir = results_dir)
}
