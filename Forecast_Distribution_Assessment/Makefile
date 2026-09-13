# Reproducible workflows for Forecast Distribution Assessment.
# Run `make help` to list available targets.

R ?= Rscript
JULIA ?= julia

# Machine-specific settings belong in config/local.mk (see the example file).
-include config/local.mk

DATA_DIR ?= $(CURDIR)/data
OUTPUT_DIR ?= $(CURDIR)/outputs
RESULTS_DIR ?= $(CURDIR)/results
export FORECAST_ASSESSMENT_PROJECT_ROOT := $(CURDIR)
export FORECAST_ASSESSMENT_DATA_DIR := $(DATA_DIR)
export FORECAST_ASSESSMENT_OUTPUT_DIR := $(OUTPUT_DIR)
export FORECAST_ASSESSMENT_RESULTS_DIR := $(RESULTS_DIR)

.DEFAULT_GOAL := help

.PHONY: help show-config all install-r-packages rmse legacy-rmse revised-evaluation legacy-fitted-evaluation crps-thesis crps-plots crps-teaching julia-export

help:
	@printf '%s\n' 'Available targets:'
	@printf '%s\n' '  make install-r-packages  Install required R packages.'
	@printf '%s\n' '  make rmse                Stop unsafe fitted-value RMSE and direct to the audit.'
	@printf '%s\n' '  make legacy-rmse         Reproduce invalid historical RMSE for audit only.'
	@printf '%s\n' '  make revised-evaluation  Generate revised RMSE/CRPS tables, LaTeX, figures, and diagnostics.'
	@printf '%s\n' '  make legacy-fitted-evaluation  Reproduce the invalid fitted-value evaluation for audit only.'
	@printf '%s\n' '  make crps-thesis         Run the original thesis CRPS workflow.'
	@printf '%s\n' '  make crps-plots          Run the CRPS workflow with thesis plots.'
	@printf '%s\n' '  make crps-teaching       Run the teaching CRPS demonstration.'
	@printf '%s\n' '  make julia-export        Export fertility Julia results to CSV.'
	@printf '%s\n' '  make all                 Run RMSE validation and revised evaluation tables.'
	@printf '%s\n' '  make show-config         Display the resolved input and output paths.'
	@printf '%s\n' 'Use DATA_DIR=/path/to/data to read inputs from another location.'

show-config:
	@printf '%s\n' "DATA_DIR=$(DATA_DIR)"
	@printf '%s\n' "OUTPUT_DIR=$(OUTPUT_DIR)"
	@printf '%s\n' "RESULTS_DIR=$(RESULTS_DIR)"

install-r-packages:
	$(R) src/install_r_packages.R

all: rmse revised-evaluation

rmse:
	$(R) src/RMSE.r

legacy-rmse:
	ALLOW_INVALID_FITTED_EVALUATION=1 $(R) src/RMSE.r

revised-evaluation:
	$(R) src/revised_evaluation_tables.R

legacy-fitted-evaluation:
	ALLOW_INVALID_FITTED_EVALUATION=1 $(R) src/revised_evaluation_tables.R

crps-thesis:
	$(R) src/CRPS_Workflow_thesis.R

crps-plots:
	$(R) src/CRPS_Workflow_thesis_plots.R

crps-teaching:
	$(R) src/CRPS_Workflow_teaching.R

julia-export:
	$(JULIA) --project=. src/Fertility_Julia_to_csv.jl
