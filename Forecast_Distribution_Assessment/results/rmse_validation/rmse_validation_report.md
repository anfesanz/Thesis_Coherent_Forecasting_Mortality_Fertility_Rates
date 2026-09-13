# RMSE Validation Report

## Scope
- Reused the same cleaned observation panels and posterior forecast draws used in the CRPS workflow.
- Evaluated the 2014-2018 holdout period explicitly, with a separate 2018-only recalculation.
- Computed RMSE from row-level squared errors on the appropriate evaluation scale.

## Mortality
- Corrected pooled holdout RMSE ranking: Model A (0.0182), Model D (6.9082), Model C (6.9615), Model B (12.1615).
- Corrected 2018-only RMSE ranking: Model A (0.0272), Model D (6.9079), Model C (6.9971), Model B (12.4287).
- `rmse == sqrt(mse)` check passed for all mortality model/sex/education groups: yes.
- Appendix rounded RMSE matches: 0/32 groups.
- Appendix rounded MSE matches: 0/32 groups.
- Closest attempted reproduction of Table 6.3: `misaligned_holdout` (minimum absolute gap 1.2659).
- The `chain_model1*_eval.csv` files contain 16 forecast cells per age group and therefore align with the training period rather than the 2014-2018 holdout.

## Fertility
- Corrected pooled holdout RMSE ranking on log ASFR: Model C (0.0545), Model B (0.1356), Model A (0.1386).
- Corrected 2018-only RMSE ranking on log ASFR: Model C (0.0884), Model A (0.1529), Model B (0.1691).
- Closest attempted reproduction of Table 7.4: `education_specific_tfr` (minimum absolute gap 6.4754).
- Separate RMSE outputs were produced for log ASFR, ASFR, education-specific TFR, and aggregate TFR.

## Output Files
- Mortality tables M1-M6, appendix validation, and diagnostics are in `results/rmse_validation`.
- Fertility RMSE tables, metric comparisons, and diagnostics are in `results/rmse_validation`.
- Combined RMSE/CRPS summaries are in `results/rmse_validation/rmse_crps_merged_summaries.csv`.

## Mortality Candidate Diagnostics
### Correct holdout pooled RMSE
| model | value |
|---|---:|
| Model A | 0.018195 |
| Model B | 12.161514 |
| Model C | 6.961474 |
| Model D | 6.908152 |

### Correct 2018-only RMSE
| model | value |
|---|---:|
| Model A | 0.027198 |
| Model B | 12.428749 |
| Model C | 6.997149 |
| Model D | 6.907919 |


## Fertility Candidate Diagnostics
### Correct holdout pooled RMSE on log ASFR
| model | value |
|---|---:|
| Model A | 0.138580 |
| Model B | 0.135581 |
| Model C | 0.054498 |

### Correct 2018-only RMSE on log ASFR
| model | value |
|---|---:|
| Model A | 0.152901 |
| Model B | 0.169055 |
| Model C | 0.088404 |

