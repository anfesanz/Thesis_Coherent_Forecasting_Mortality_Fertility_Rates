# Mortality Holdout Forecast Validation

- Training period: 1998-2013; holdout: 2014-2018.
- Combined model-piece files: 13; posterior draws per forecast cell: 500.
- Forecast log rates are the HPC exporter formula `alpha + beta * kf`.
- Observed holdout values match across every model key.
- All model forecasts passed the common log-rate scale check; no post-hoc scaling offset was applied.

| Model | RMSE, 2014-2018 | RMSE, 2018 only |
| --- | ---: | ---: |
| Model A | 0.4701 | 0.7333 |
| Model B | 0.4637 | 0.7097 |
| Model C | 0.5107 | 0.6674 |
| Model D | 0.5113 | 0.6688 |
