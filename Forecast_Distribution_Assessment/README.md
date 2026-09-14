# Forecast Distribution Assessment

Reproducible scripts for assessing mortality and fertility forecast distributions.

## Requirements

- R with `scoringRules`, `ggplot2`, and `reshape2`.
- Julia 1.10 for conversion of serialized fertility chains, using the pinned
  dependencies in `Project.toml` and `Manifest.toml`.
- The rerun mortality forecast exports from the companion `HPC_thesis` project
  to reproduce the corrected mortality evaluation.

## Configuration

Copy `config/local.mk.example` to `config/local.mk`, then set the paths for the
current computer. `MORTALITY_FORECAST_DIR` must point to the
`output/evaluation_local_pieces` directory from the rerun HPC project. It must
contain the `mortality_a`, `mortality_b`, `mortality_c`, and `mortality_d`
subdirectories.

For portable use, place the project data and the required HPC forecast exports
under the shared OneDrive thesis folder, then reference those copies in
`config/local.mk`. Do not commit machine-specific paths.

## Commands

```bash
make help
make check-r
make check-julia
make mortality-holdout-rmse
make fertility-jls-to-csv
```

`make mortality-holdout-rmse` validates the labelled 2014--2018 posterior
forecast draws, calculates RMSE, MSE, mean error, and CRPS on log mortality
rates, and writes publication-ready LaTeX tables to (tables 6.3 and 6.4 are now in the new doc 4.5 and 4.6):

```text
outputs/revised_evaluation_tables/latex/revised_table_6_3_mortality_holdout_rmse.tex
outputs/revised_evaluation_tables/latex/mortality_holdout_crps.tex
```

Supporting CSV results and scale-validation diagnostics are written below
`outputs/revised_evaluation_tables/`. This workflow uses forecast values from
the rerun models, rather than fitted values; all Models A--D are checked to be
on a common native log-rate scale before scoring.

The older CRPS scripts in `src/` read the chain CSV files from `data/`. The
plotting workflow writes figures to `results/thesis_plots/`.
