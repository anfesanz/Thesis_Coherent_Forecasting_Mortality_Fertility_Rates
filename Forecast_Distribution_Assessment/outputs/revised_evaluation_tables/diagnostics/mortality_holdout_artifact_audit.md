# Mortality Holdout Artifact Audit

The available mortality files do not support a valid 2014-2018 temporal-holdout comparison across Models A-D.

- Model B has only 10 saved draws in each sex-specific chain and its values are not on a credible log mortality-rate scale.
- Models C and D have no saved `logmuf` forecast block; their 21-year `logmu` arrays are fitted latent values, not documented holdout forecasts.
- Model A contains a `logmuf` block, but the repository does not document its calendar-year mapping.
- The serialized chains require the original Julia 1.10/Turing environment and do not deserialize under the currently available project stack without the original compatible dependencies.

Do not use the existing revised Table 6.3 as an out-of-sample comparison. A valid replacement requires regenerated or re-exported 2014-2018 posterior forecast draws for every model, with explicit year metadata and a common per-1,000 log-rate scale.

## Artifact Summary
| model | chain_file | expected_structure | n_draws | has_logmuf_forecast_block | forecast_calendar_mapping_documented | valid_for_2014_2018_holdout | issue |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Model A | mortality/chain_model1f1.csv | Individual model by sex and education | 1000 | TRUE | FALSE | FALSE | no documented mapping from forecast columns to calendar years 2014-2018 |
| Model B | mortality/chain_model2f.csv | Shared beta model, female chain | 10 | TRUE | FALSE | FALSE | only 10 saved draws; no documented mapping from forecast columns to calendar years 2014-2018 |
| Model B | mortality/chain_model2m.csv | Shared beta model, male chain | 10 | TRUE | FALSE | FALSE | only 10 saved draws; no documented mapping from forecast columns to calendar years 2014-2018 |
| Model C | mortality/chain_model3f.csv | Shared beta and kappa model, female chain | 1000 | FALSE | FALSE | FALSE | no saved logmuf forecast block; no documented mapping from forecast columns to calendar years 2014-2018 |
| Model C | mortality/chain_model3m.csv | Shared beta and kappa model, male chain | 1000 | FALSE | FALSE | FALSE | no saved logmuf forecast block; no documented mapping from forecast columns to calendar years 2014-2018 |
| Model D | mortality/chain_model4.csv | Joint sex, beta, and kappa model | 1000 | FALSE | FALSE | FALSE | no saved logmuf forecast block; no documented mapping from forecast columns to calendar years 2014-2018 |
