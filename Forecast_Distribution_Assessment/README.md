# Forecast Distribution Assessment

This repository evaluates saved mortality and fertility forecast chains. It is
portable when the code is cloned locally and the large input data are stored in
the shared OneDrive location.

## First-time setup

1. Install R and the required R packages:
   `Rscript src/install_r_packages.R`
2. Install Julia 1.10 and use the checked-in `Project.toml` / `Manifest.toml`
   for the fertility export workflow.
3. Copy `config/local.mk.example` to `config/local.mk` and set `DATA_DIR` to
   the local OneDrive path. Alternatively, pass the path for one command:
   `make rmse DATA_DIR=/path/to/data`.
4. Run `make help` to list workflows.

The default shared-data layout is:

```
OneDrive/Thesis/Forecast_Distribution_Assessment/data/
  mortality/
  fertility/
```

## Reproducibility notes

`make rmse` and `make revised-evaluation` resolve paths from the repository,
not the current working directory. Generated output remains in `outputs/` and
`results/` in the clone unless `OUTPUT_DIR` and/or `RESULTS_DIR` is set.

The current mortality chain exports cannot produce a defensible 2014--2018
holdout comparison for all four models: Models C and D require a `log(1000)`
scale conversion, Model B's saved export is invalid, and the required forecast
draws/year mappings are absent. `make revised-evaluation` writes an audit and
stops rather than overwrite the criticised table. The source code needed to
refit/export the mortality models is not included in this repository.

`make legacy-rmse` and `make legacy-fitted-evaluation` exist only to audit the
historical calculations; their outputs must not be used in the thesis.
