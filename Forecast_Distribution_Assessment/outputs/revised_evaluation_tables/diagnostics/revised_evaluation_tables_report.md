# Revised Evaluation Tables Report

## Scripts and Datasets Used
- New pipeline script: `src/revised_evaluation_tables.R`.
- Source logic inspected and reused from `src/RMSE.r` and `src/CRPS_Workflow_thesis.R`.
- Mortality cleaned row-level data were rebuilt from `mxf*`, `mxm*`, `exf*`, and `exm*` files on the log mortality-rate scale.
- Fertility cleaned row-level data were rebuilt from `fx*` and `exfe*` files on the log age-specific fertility-rate scale.
- Posterior draws were extracted from `chain_model1*`, `chain_model2*`, `chain_model3*`, and `chain_model4*` CSV files using the same column-matching logic as the existing CRPS workflow.
- Mortality posterior draws for Models C and D were shifted by `log(1000)` before scoring to align their stored rate scale with the observed and Model A log-rate scale. Model B was not shifted because its discrepancy is not consistent with this offset.

## Row-level Join Keys
- Mortality joins used: `year`, `age_group`, `sex`, `education`.
- Fertility joins used: `year`, `age_group`, `education`.
- Predictions and observations were joined explicitly by keys. They were not compared by row order.

## Estimation and Holdout Periods
- Estimation period: 1998-2013.
- Temporal holdout period: 2014-2018.

## Number of Evaluation Observations
| model | n_rows |
| --- | --- |
| Model A | 440 |
| Model B | 440 |
| Model C | 440 |
| Model D | 440 |

| model | n_rows |
| --- | --- |
| Model A | 120 |
| Model B | 120 |
| Model C | 120 |

## Revised Table 6.3
| model | rmse_2014_2018 | rmse_2018_only | n_observations_2014_2018 | n_observations_2018_only |
| --- | --- | --- | --- | --- |
| Model A | 0.018195 | 0.027198 | 440.000000 | 88.000000 |
| Model B | 12.161514 | 12.428749 | 440.000000 | 88.000000 |
| Model C | 0.143514 | 0.173228 | 440.000000 | 88.000000 |
| Model D | 0.005858 | 0.006969 | 440.000000 | 88.000000 |

## Revised Tables E.1-E.4
### E.1
| sex | education | rmse_model_a | rmse_model_b | rmse_model_c | rmse_model_d |
| --- | --- | --- | --- | --- | --- |
| Female | No Formal Education | 0.041593 | 10.392838 | 0.011514 | 0.007974 |
| Female | Post Secondary | 0.005441 | 11.830558 | 0.003864 | 0.003532 |
| Female | Primary | 0.018369 | 9.736855 | 0.021659 | 0.008070 |
| Female | Secondary | 0.010787 | 18.787444 | 0.004176 | 0.008981 |
| Male | No Formal Education | 0.018730 | 9.101243 | 0.260375 | 0.003087 |
| Male | Post Secondary | 0.002454 | 13.599225 | 0.233174 | 0.003797 |
| Male | Primary | 0.004640 | 11.070813 | 0.167179 | 0.002477 |
| Male | Secondary | 0.007533 | 9.855828 | 0.118408 | 0.004747 |

### E.2
| year | rmse_model_a | rmse_model_b | rmse_model_c | rmse_model_d |
| --- | --- | --- | --- | --- |
| 2014.000000 | 0.014819 | 12.123042 | 0.168538 | 0.005949 |
| 2015.000000 | 0.014648 | 11.803255 | 0.135387 | 0.005640 |
| 2016.000000 | 0.013931 | 12.246558 | 0.110365 | 0.004768 |
| 2017.000000 | 0.016952 | 12.197343 | 0.118565 | 0.005750 |
| 2018.000000 | 0.027198 | 12.428749 | 0.173228 | 0.006969 |

### E.3
| age_group | rmse_model_a | rmse_model_b | rmse_model_c | rmse_model_d |
| --- | --- | --- | --- | --- |
| Age 01 | 0.029670 | 13.587120 | 0.135153 | 0.011140 |
| Age 02 | 0.030813 | 14.214993 | 0.114313 | 0.008281 |
| Age 03 | 0.036554 | 10.296052 | 0.112519 | 0.008414 |
| Age 04 | 0.014536 | 8.140188 | 0.100767 | 0.006606 |
| Age 05 | 0.006745 | 14.961300 | 0.111524 | 0.005102 |
| Age 06 | 0.008422 | 15.059841 | 0.136735 | 0.002917 |
| Age 07 | 0.008191 | 7.142835 | 0.115362 | 0.002776 |
| Age 08 | 0.006071 | 6.887211 | 0.183525 | 0.002079 |
| Age 09 | 0.002182 | 13.230037 | 0.123534 | 0.002287 |
| Age 10 | 0.006084 | 16.617807 | 0.153185 | 0.004170 |
| Age 11 | 0.001525 | 8.228062 | 0.235711 | 0.001055 |

### E.4
| model | sex | education | mean_crps | mean_log_score | coverage_80 | coverage_95 | mean_interval_width_80 | mean_interval_width_95 | n_observations |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Model A | Female | No Formal Education | 0.019687 | -2.044563 | 0.927273 | 0.981818 | 0.124116 | 0.188469 | 55.000000 |
| Model A | Female | Post Secondary | 0.009016 | -2.397022 | 1.000000 | 1.000000 | 0.096236 | 0.146885 | 55.000000 |
| Model A | Female | Primary | 0.009209 | -2.681234 | 1.000000 | 1.000000 | 0.076174 | 0.116098 | 55.000000 |
| Model A | Female | Secondary | 0.008072 | -2.584207 | 1.000000 | 1.000000 | 0.073528 | 0.112295 | 55.000000 |
| Model A | Male | No Formal Education | 0.011825 | -2.319300 | 1.000000 | 1.000000 | 0.101872 | 0.155203 | 55.000000 |
| Model A | Male | Post Secondary | 0.006603 | -2.669356 | 1.000000 | 1.000000 | 0.071418 | 0.109029 | 55.000000 |
| Model A | Male | Primary | 0.005325 | -2.963095 | 1.000000 | 1.000000 | 0.054774 | 0.083363 | 55.000000 |
| Model A | Male | Secondary | 0.006059 | -2.846672 | 1.000000 | 1.000000 | 0.055572 | 0.084593 | 55.000000 |
| Model B | Female | No Formal Education | 7.717593 | Inf | 0.000000 | 0.000000 | 0.000228 | 0.000232 | 55.000000 |
| Model B | Female | Post Secondary | 8.877989 | Inf | 0.000000 | 0.000000 | 0.000019 | 0.000020 | 55.000000 |
| Model B | Female | Primary | 8.298646 | Inf | 0.000000 | 0.000000 | 0.019888 | 0.020243 | 55.000000 |
| Model B | Female | Secondary | 16.116051 | Inf | 0.000000 | 0.000000 | 0.000069 | 0.000070 | 55.000000 |
| Model B | Male | No Formal Education | 8.019698 | Inf | 0.000000 | 0.000000 | 0.000000 | 0.000000 | 55.000000 |
| Model B | Male | Post Secondary | 10.078606 | Inf | 0.000000 | 0.000000 | 0.000000 | 0.000000 | 55.000000 |
| Model B | Male | Primary | 8.790207 | Inf | 0.000000 | 0.000000 | 0.001241 | 0.001263 | 55.000000 |
| Model B | Male | Secondary | 9.473321 | Inf | 0.000000 | 0.000000 | 0.000000 | 0.000000 | 55.000000 |
| Model C | Female | No Formal Education | 0.013761 | -2.112605 | 1.000000 | 1.000000 | 0.144528 | 0.221050 | 55.000000 |
| Model C | Female | Post Secondary | 0.009000 | -2.384010 | 1.000000 | 1.000000 | 0.097214 | 0.146943 | 55.000000 |
| Model C | Female | Primary | 0.010370 | -2.638988 | 1.000000 | 1.000000 | 0.077014 | 0.117015 | 55.000000 |
| Model C | Female | Secondary | 0.007204 | -2.606642 | 1.000000 | 1.000000 | 0.076716 | 0.116540 | 55.000000 |
| Model C | Male | No Formal Education | 0.207607 | Inf | 0.072727 | 0.145455 | 0.111651 | 0.170342 | 55.000000 |
| Model C | Male | Post Secondary | 0.145745 | Inf | 0.272727 | 0.345455 | 0.072065 | 0.109607 | 55.000000 |
| Model C | Male | Primary | 0.143073 | Inf | 0.054545 | 0.090909 | 0.055594 | 0.084540 | 55.000000 |
| Model C | Male | Secondary | 0.096669 | 50.548516 | 0.054545 | 0.090909 | 0.057499 | 0.087425 | 55.000000 |
| Model D | Female | No Formal Education | 0.013270 | -2.124608 | 1.000000 | 1.000000 | 0.143554 | 0.218344 | 55.000000 |
| Model D | Female | Post Secondary | 0.008941 | -2.388078 | 1.000000 | 1.000000 | 0.096337 | 0.147108 | 55.000000 |
| Model D | Female | Primary | 0.007881 | -2.683646 | 1.000000 | 1.000000 | 0.080785 | 0.122870 | 55.000000 |
| Model D | Female | Secondary | 0.007751 | -2.602734 | 1.000000 | 1.000000 | 0.074779 | 0.114353 | 55.000000 |
| Model D | Male | No Formal Education | 0.010168 | -2.301943 | 1.000000 | 1.000000 | 0.110154 | 0.167011 | 55.000000 |
| Model D | Male | Post Secondary | 0.006736 | -2.662432 | 1.000000 | 1.000000 | 0.071567 | 0.110424 | 55.000000 |
| Model D | Male | Primary | 0.005247 | -2.943074 | 1.000000 | 1.000000 | 0.056193 | 0.084974 | 55.000000 |
| Model D | Male | Secondary | 0.005514 | -2.873686 | 1.000000 | 1.000000 | 0.056328 | 0.085579 | 55.000000 |

## Revised Table 7.4
| model | rmse_2014_2018 | rmse_2018_only | n_observations_2014_2018 | n_observations_2018_only |
| --- | --- | --- | --- | --- |
| Model A | 0.138580 | 0.152901 | 120.000000 | 24.000000 |
| Model B | 0.135581 | 0.169055 | 120.000000 | 24.000000 |
| Model C | 0.054498 | 0.088404 | 120.000000 | 24.000000 |

## Revised Tables E.5-E.10
### E.5
| education | rmse_model_a | rmse_model_b | rmse_model_c |
| --- | --- | --- | --- |
| No Formal Education | 0.219141 | 0.215855 | 0.046465 |
| Post Secondary | 0.079027 | 0.062716 | 0.024958 |
| Primary | 0.075660 | 0.098549 | 0.093340 |
| Secondary | 0.129712 | 0.115283 | 0.019644 |

### E.6
| year | rmse_model_a | rmse_model_b | rmse_model_c |
| --- | --- | --- | --- |
| 2014.000000 | 0.086623 | 0.073535 | 0.051108 |
| 2015.000000 | 0.115685 | 0.097561 | 0.032207 |
| 2016.000000 | 0.134261 | 0.163990 | 0.024479 |
| 2017.000000 | 0.183660 | 0.146673 | 0.052786 |
| 2018.000000 | 0.152901 | 0.169055 | 0.088404 |

### E.7
| age_group | rmse_model_a | rmse_model_b | rmse_model_c |
| --- | --- | --- | --- |
| 15-19 | 0.000712 | 0.000824 | 0.000198 |
| 20-24 | 0.001768 | 0.001901 | 0.000505 |
| 25-29 | 0.003360 | 0.002578 | 0.000607 |
| 30-34 | 0.007971 | 0.006542 | 0.002069 |
| 35-39 | 0.045744 | 0.035936 | 0.015984 |
| 40-44 | 0.336238 | 0.330073 | 0.132513 |

### E.8
| model | education | mean_crps | mean_log_score | coverage_80 | coverage_95 | mean_interval_width_80 | mean_interval_width_95 | n_observations |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Model A | No Formal Education | 0.077994 | 0.062865 | 0.833333 | 0.833333 | 0.144129 | 0.218565 | 30.000000 |
| Model A | Post Secondary | 0.028167 | -2.753201 | 0.833333 | 0.900000 | 0.074213 | 0.111978 | 30.000000 |
| Model A | Primary | 0.020925 | -2.201375 | 0.933333 | 0.933333 | 0.059247 | 0.091313 | 30.000000 |
| Model A | Secondary | 0.039081 | -2.135250 | 0.900000 | 0.933333 | 0.087063 | 0.133890 | 30.000000 |
| Model B | No Formal Education | 0.073209 | -0.953989 | 0.900000 | 0.900000 | 0.143685 | 0.219311 | 30.000000 |
| Model B | Post Secondary | 0.020704 | -2.988836 | 0.900000 | 0.966667 | 0.080109 | 0.123120 | 30.000000 |
| Model B | Primary | 0.032253 | -2.265576 | 0.833333 | 0.933333 | 0.068179 | 0.103006 | 30.000000 |
| Model B | Secondary | 0.035978 | -2.421898 | 0.900000 | 0.933333 | 0.082363 | 0.126588 | 30.000000 |
| Model C | No Formal Education | 0.023219 | -1.975583 | 1.000000 | 1.000000 | 0.220516 | 0.339162 | 30.000000 |
| Model C | Post Secondary | 0.011378 | -3.111450 | 1.000000 | 1.000000 | 0.105627 | 0.160862 | 30.000000 |
| Model C | Primary | 0.028136 | -0.572938 | 0.900000 | 0.933333 | 0.056960 | 0.086307 | 30.000000 |
| Model C | Secondary | 0.011312 | -3.058506 | 1.000000 | 1.000000 | 0.110899 | 0.168824 | 30.000000 |

### E.9
| education | rmse_model_a | rmse_model_b | rmse_model_c |
| --- | --- | --- | --- |
| No Formal Education | 0.400188 | 0.527233 | 0.425263 |
| Post Secondary | 0.583960 | 0.287464 | 0.314817 |
| Primary | 0.125180 | 0.062866 | 0.216386 |
| Secondary | 0.118250 | 0.090109 | 0.142886 |

### E.10
| model | n_observations | mse | rmse |
| --- | --- | --- | --- |
| Model A | 5.000000 | 0.015528 | 0.124611 |
| Model B | 5.000000 | 0.000988 | 0.031433 |
| Model C | 5.000000 | 0.001834 | 0.042826 |

## Mortality and Fertility Probabilistic Evaluation Results
- `mean_log_score` is reported using `scoringRules::logs_sample`, and lower values indicate better predictive performance.
### Mortality Table 6.4
| model | mean_crps | mean_log_score | coverage_80 | coverage_95 | mean_interval_width_80 | mean_interval_width_95 | n_observations |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Model A | 0.009475 | -2.563181 | 0.990909 | 0.997727 | 0.081712 | 0.124492 | 440.000000 |
| Model B | 9.671514 | Inf | 0.000000 | 0.000000 | 0.002681 | 0.002729 | 440.000000 |
| Model C | 0.079179 | Inf | 0.556818 | 0.584091 | 0.086535 | 0.131683 | 440.000000 |
| Model D | 0.008189 | -2.572525 | 1.000000 | 1.000000 | 0.086212 | 0.131333 | 440.000000 |

### Fertility Table 7.5
| model | mean_crps | mean_log_score | coverage_80 | coverage_95 | mean_interval_width_80 | mean_interval_width_95 | n_observations |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Model A | 0.041542 | -1.756740 | 0.875000 | 0.900000 | 0.091163 | 0.138937 | 120.000000 |
| Model B | 0.040536 | -2.157575 | 0.883333 | 0.933333 | 0.093584 | 0.143006 | 120.000000 |
| Model C | 0.018511 | -2.179619 | 0.975000 | 0.983333 | 0.123500 | 0.188789 | 120.000000 |

## Validation Checks
| check | passed |
| --- | --- |
| mortality_rmse_equals_sqrt_mse | TRUE |
| fertility_rmse_equals_sqrt_mse | TRUE |
| mortality_same_n_observations_per_model | TRUE |
| fertility_same_n_observations_per_model | TRUE |
| mortality_joins_are_key_based_and_one_to_one | TRUE |
| fertility_joins_are_key_based_and_one_to_one | TRUE |
| mortality_no_duplicate_join_keys | TRUE |
| fertility_no_duplicate_join_keys | TRUE |
| original_table_63_values_not_reused | TRUE |
| original_table_74_values_not_reused | TRUE |

- Mortality `rmse == sqrt(mse)` passed for all checked groupings: yes.
- Fertility `rmse == sqrt(mse)` passed for all checked groupings: yes.
- Mortality models all use the same number of holdout rows: 440.
- Fertility models all use the same number of holdout rows: 120.
- Original Table 6.3 values reused: no.
- Original Table 7.4 values reused: no.

## Ranking Summary
- Mortality pooled 2014-2018 ranking: Model D (0.005858), Model A (0.018195), Model C (0.143514), Model B (12.161514).
- Fertility pooled 2014-2018 ranking: Model C (0.054498), Model B (0.135581), Model A (0.138580).
- Detailed ranking changes by year, age group, sex, and education were saved as diagnostic CSV files.

## Unresolved Issues
- The legacy headline tables and appendix values in the thesis text are not reproduced by the corrected key-joined holdout calculations.
- The existing project does not contain a validated legacy script that reproduces the original published Tables 6.3 or 7.4 exactly.
- Mortality Model B produces an infinite mean log score under `logs_sample`, which indicates zero or numerically negligible predictive density at some observed holdout values on the evaluated log-rate scale. Its stored forecast object/scale needs separate review because a `log(1000)` shift does not resolve its RMSE discrepancy.

## Tables to Remove and Replace
- Remove old Table 6.3 and replace it with revised Table 6.3 in `revised_table_6_3_mortality_holdout_rmse`.
- Remove old appendix Tables E.1 and E.2 and replace them with regenerated Tables E.1-E.4.
- Remove old Table 7.4 and replace it with revised Table 7.4 in `revised_table_7_4_fertility_holdout_rmse`.
- Replace previous fertility appendix tables with regenerated Tables E.5-E.10.

## Created Paths
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/src/revised_evaluation_tables.R`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/diagnostics/mortality_joined_holdout_rows.csv`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/diagnostics/fertility_joined_holdout_rows.csv`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/csv/revised_table_6_3_mortality_holdout_rmse.csv`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/latex/revised_table_6_3_mortality_holdout_rmse.tex`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/csv/revised_table_e1_mortality_rmse_by_sex_education_wide.csv`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/latex/revised_table_e1_mortality_rmse_by_sex_education_wide.tex`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/csv/revised_table_e1_mortality_rmse_by_sex_education_long.csv`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/latex/revised_table_e1_mortality_rmse_by_sex_education_long.tex`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/csv/revised_table_e2_mortality_rmse_by_year_wide.csv`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/latex/revised_table_e2_mortality_rmse_by_year_wide.tex`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/csv/revised_table_e2_mortality_rmse_by_year_long.csv`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/latex/revised_table_e2_mortality_rmse_by_year_long.tex`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/csv/revised_table_e3_mortality_rmse_by_age_group_wide.csv`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/latex/revised_table_e3_mortality_rmse_by_age_group_wide.tex`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/csv/revised_table_e3_mortality_rmse_by_age_group_long.csv`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/latex/revised_table_e3_mortality_rmse_by_age_group_long.tex`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/csv/mortality_rmse_detailed_by_year_age_sex_education.csv`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/latex/mortality_rmse_detailed_by_year_age_sex_education.tex`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/csv/new_table_6_4_mortality_probabilistic_evaluation.csv`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/latex/new_table_6_4_mortality_probabilistic_evaluation.tex`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/csv/new_table_e4_mortality_crps_by_sex_education_wide.csv`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/latex/new_table_e4_mortality_crps_by_sex_education_wide.tex`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/csv/new_table_e4_mortality_crps_by_sex_education_long.csv`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/latex/new_table_e4_mortality_crps_by_sex_education_long.tex`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/csv/revised_table_7_4_fertility_holdout_rmse.csv`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/latex/revised_table_7_4_fertility_holdout_rmse.tex`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/csv/revised_table_e5_fertility_rmse_by_education_wide.csv`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/latex/revised_table_e5_fertility_rmse_by_education_wide.tex`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/csv/revised_table_e5_fertility_rmse_by_education_long.csv`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/latex/revised_table_e5_fertility_rmse_by_education_long.tex`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/csv/revised_table_e6_fertility_rmse_by_year_wide.csv`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/latex/revised_table_e6_fertility_rmse_by_year_wide.tex`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/csv/revised_table_e6_fertility_rmse_by_year_long.csv`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/latex/revised_table_e6_fertility_rmse_by_year_long.tex`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/csv/revised_table_e7_fertility_rmse_by_age_group_wide.csv`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/latex/revised_table_e7_fertility_rmse_by_age_group_wide.tex`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/csv/revised_table_e7_fertility_rmse_by_age_group_long.csv`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/latex/revised_table_e7_fertility_rmse_by_age_group_long.tex`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/csv/fertility_rmse_detailed_by_year_age_education.csv`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/latex/fertility_rmse_detailed_by_year_age_education.tex`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/csv/new_table_7_5_fertility_probabilistic_evaluation.csv`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/latex/new_table_7_5_fertility_probabilistic_evaluation.tex`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/csv/new_table_e8_fertility_crps_by_education_wide.csv`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/latex/new_table_e8_fertility_crps_by_education_wide.tex`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/csv/new_table_e8_fertility_crps_by_education_long.csv`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/latex/new_table_e8_fertility_crps_by_education_long.tex`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/csv/new_table_e9_education_specific_tfr_rmse_wide.csv`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/latex/new_table_e9_education_specific_tfr_rmse_wide.tex`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/csv/new_table_e9_education_specific_tfr_rmse_long.csv`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/latex/new_table_e9_education_specific_tfr_rmse_long.tex`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/csv/new_table_e10_aggregate_tfr_rmse.csv`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/latex/new_table_e10_aggregate_tfr_rmse.tex`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/diagnostics/validation_summary.csv`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/diagnostics/mortality_rmse_identity_checks.csv`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/diagnostics/fertility_rmse_identity_checks.csv`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/diagnostics/mortality_model_row_counts.csv`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/diagnostics/fertility_model_row_counts.csv`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/diagnostics/mortality_ranking_by_year.csv`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/diagnostics/mortality_ranking_by_age_group.csv`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/diagnostics/mortality_ranking_by_sex_education.csv`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/diagnostics/fertility_ranking_by_year.csv`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/diagnostics/fertility_ranking_by_age_group.csv`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/diagnostics/fertility_ranking_by_education.csv`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/mortality/mortality_rmse_by_forecast_year.png`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/mortality/mortality_rmse_by_forecast_year.pdf`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/mortality/mortality_rmse_by_age_group.png`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/mortality/mortality_rmse_by_age_group.pdf`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/mortality/mortality_rmse_by_sex_education_model.png`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/mortality/mortality_rmse_by_sex_education_model.pdf`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/mortality/mortality_crps_by_sex_education_model.png`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/mortality/mortality_crps_by_sex_education_model.pdf`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/fertility/fertility_rmse_by_forecast_year.png`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/fertility/fertility_rmse_by_forecast_year.pdf`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/fertility/fertility_rmse_by_age_group.png`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/fertility/fertility_rmse_by_age_group.pdf`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/fertility/fertility_rmse_by_education_model.png`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/fertility/fertility_rmse_by_education_model.pdf`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/fertility/fertility_crps_by_education_model.png`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/fertility/fertility_crps_by_education_model.pdf`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/fertility/fertility_tfr_rmse_by_education_model.png`
- `/Users/felipesanchez/Documents/GitHub/thesis/Forecast_Distribution_Assessment/outputs/revised_evaluation_tables/fertility/fertility_tfr_rmse_by_education_model.pdf`
