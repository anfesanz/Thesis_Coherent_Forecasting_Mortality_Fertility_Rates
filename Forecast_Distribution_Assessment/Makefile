R ?= Rscript
JULIA ?= julia

-include config/local.mk
MORTALITY_FORECAST_DIR ?=
export MORTALITY_FORECAST_DIR

.PHONY: help mortality-holdout-rmse fertility-jls-to-csv check-r check-julia

help:
	@printf '%s\n' \
		'make mortality-holdout-rmse  Generate corrected mortality RMSE, CRPS, and Table 4.5/6.3.' \
		'make fertility-jls-to-csv      Convert fertility posterior chains from JLS to CSV.' \
		'make check-r                   Parse all R source files without running analyses.' \
		'make check-julia               Parse the Julia fertility conversion script.' \
		'' \
		'Configure MORTALITY_FORECAST_DIR in config/local.mk before running mortality-holdout-rmse.'

mortality-holdout-rmse:
	$(R) src/mortality_holdout_rmse.R

fertility-jls-to-csv:
	$(JULIA) src/Fertility_Julia_to_csv.jl

check-r:
	$(R) -e 'for (f in list.files("src", pattern = "[.]R$$", full.names = TRUE)) parse(f); cat("All R source files parsed successfully.\\n")'

check-julia:
	$(JULIA) --project=. -e 'Meta.parseall(read("src/Fertility_Julia_to_csv.jl", String)); println("Julia source parsed successfully.")'
