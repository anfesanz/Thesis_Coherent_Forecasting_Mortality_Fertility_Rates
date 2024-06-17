
library(tidyverse)
#Logistic transformation
logit <- qlogis
logistic <- plogis

#Reproduction of Box 3.3.2 Example of Computation of Education Transitions in the Reconstruction for Italy (Men), 2010
#Page 34 Speringer(2019)

#To obtain this data I used inversed engenering using the information at the end of this strip
data <- tibble(
  Time = c("t", "t", "t", "t"),
  Age = c("15-19", "20-24", "25-29", "30-34"),
  Pe1 = c(0.0006460419, 0.0008983967, 0.0013762715, 0.0044694965),
  Pe2 = c(0.003413328, 0.004749746, 0.007272275, 0.006629927),
  Pe3 = c(0.01954784, 0.01294855, 0.01951508, 0.02710721),
  Pe4 = c(0.7726552, 0.2557258, 0.2747594, 0.3148844),
  Pe5 = c(0.2037276, 0.6411361, 0.5305808, 0.4546926),
  Pe6 = c(9.979562e-06, 8.454143e-02, 1.664962e-01, 1.922164e-01)
)


#cumulative educational distribution (in %)

data_bTR <- data %>%
  mutate(
    Pe1c= Pe1+Pe2+Pe3+Pe4+Pe5+Pe6,
    Pe2c= Pe2+Pe3+Pe4+Pe5+Pe6,
    Pe3c= Pe3+Pe4+Pe5+Pe6,
    Pe4c= Pe4+Pe5+Pe6,
    Pe5c= Pe5+Pe6,
    Pe6c= Pe6
    ) %>%
  mutate(
    EAPR12 = Pe2c/Pe1c,
    EAPR23 = Pe3c/Pe2c,
    EAPR34 = Pe4c/Pe3c,
    EAPR45 = Pe5c/Pe4c,
    EAPR56 = Pe6c/Pe5c
    ) %>%
  mutate(
    logitEAPR12 = logit(EAPR12),
    logitEAPR23 = logit(EAPR23),
    logitEAPR34 = logit(EAPR34),
    logitEAPR45 = logit(EAPR45),
    logitEAPR56 = logit(EAPR56)
  ) %>%
  select(Age, logitEAPR12, logitEAPR23, logitEAPR34, logitEAPR45, logitEAPR56) %>%
  mutate(index=seq_len(nrow(data)))


# Create an index variable
data_bTR$index <- seq_len(nrow(data_bTR))

# Perform regression for each logitEAPR variable
regressions <- lapply(data_bTR[, grep("^logitEAPR", names(data_bTR))], function(column) {
  lm(column ~ index, data = data_bTR)
})

# Create an empty data frame to store the fitted values
fitted_df <- data.frame(index = data_bTR$index)

# Obtain and store fitted values for each logitEAPR variable in the data frame
for (i in seq_along(regressions)) {
  fitted_df[[names(regressions)[i]]] <- fitted(regressions[[i]])
}

# Print the fitted data frame
print(fitted_df)


fitted_df1 <- fitted_df %>%
  mutate(
    EAPR12 = logistic(logitEAPR12),
    EAPR23 = logistic(logitEAPR23),
    EAPR34 = logistic(logitEAPR34),
    EAPR45 = logistic(logitEAPR45),
    EAPR56 = logistic(logitEAPR56)) %>%
  mutate(
    Pe1c = EAPR12/EAPR12,
    Pe2c = EAPR12,
    Pe3c = Pe2c*EAPR23,
    Pe4c = Pe3c*EAPR34,
    Pe5c = Pe4c*EAPR45,
    ePe6 = Pe5c*EAPR56) %>%
  mutate(
    ePe5 = Pe5c - ePe6,
    ePe4 = Pe4c - ePe6 - ePe5,
    ePe3 = Pe3c - ePe6 - ePe5 - ePe4,
    ePe2 = Pe2c - ePe6 - ePe5 - ePe4 - ePe3,
    ePe1 = Pe1c - ePe6 - ePe5 - ePe4 - ePe3 - ePe2) %>%
  select(index, ePe1, ePe2, ePe3, ePe4, ePe5, ePe6) 
      
  








data_logit <- tibble(
  Time = c("t", "t", "t", "t"),
  Age = c("15-19", "20-24", "25-29", "30-34"),
  logitEAPR12 = c(7.344, 7.014, 6.587, 5.406),
  logitEAPR23 = c(5.676, 5.344, 4.915, 5.005),
  logitEAPR34 = c(3.911, 4.328, 3.908, 3.569),
  logitEAPR45 = c(-1.333, 1.043, 0.931, 0.720),
  logitEAPR56 = c(-9.924, -2.026, -1.159, -0.861)
)

data <- data_logit %>%
  mutate(
    EAPR12=logistic(logitEAPR12),
    EAPR23=logistic(logitEAPR23),
    EAPR34=logistic(logitEAPR34),
    EAPR45=logistic(logitEAPR45),
    EAPR56=logistic(logitEAPR56)
  ) %>%
  mutate(
    Pe1c = EAPR12/EAPR12,
    Pe2c = EAPR12,
    Pe3c = Pe2c*EAPR23,
    Pe4c = Pe3c*EAPR34,
    Pe5c = Pe4c*EAPR45,
    Pe6 = Pe5c*EAPR56) %>%
  mutate(
    Pe5 = Pe5c - Pe6,
    Pe4 = Pe4c - Pe6 - Pe5,
    Pe3 = Pe3c - Pe6 - Pe5 - Pe4,
    Pe2 = Pe2c - Pe6 - Pe5 - Pe4 - Pe3,
    Pe1 = Pe1c - Pe6 - Pe5 - Pe4 - Pe3 - Pe2) %>%
  select(Time, Age, Pe1, Pe2, Pe3, Pe4, Pe5, Pe6)
  

