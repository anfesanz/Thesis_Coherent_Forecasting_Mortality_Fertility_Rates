
#Calculate \pi its pi1 is the sum of edu2, edu3 and edu4, pi2 is the sum of edu 3 and edu4 pi3 is edu4 
# Calculate weights by age_group df_weights 
#and after apply logistic transformation in df_log
df_log <- df %>%
  filter(str_detect(age_group, "^(1[5-9]|[2-9][0-9])")) %>% # Excluding ages bellow 15
  rowwise() %>%
  mutate(across(starts_with("edu"), ~ ./sum(c_across(starts_with("edu"))))) %>%# Calculate weights by row, ensuring the sum of each row is 1
  mutate(
    pi1 = edu2 + edu3 + edu4,
    pi2 = edu3 + edu4,
    pi3 = edu4) %>%
  mutate(
    lpi1 = logit(pi1),
    lpi2 = logit(pi2),
    lpi3 = logit(pi3)
  ) %>%
  select(sex, age_group, lpi1, lpi2, lpi3) %>%
  pivot_wider(names_from = sex, values_from = c(lpi1, lpi2, lpi3), names_sep = "_")


# Extract rows starting from age_start_reg to second to last record
df_reg_prep <- df_log %>%
  filter(age_at_least(age_group,age_start_reg)) %>% # Extract rows starting from age_start_reg until the last one
  slice(1:(n()-1)) %>%#Drop the open interval
  mutate(age_index = 1:n())

#add new index to replace the open interval to the close estimated
new_age_indexes <- max(df_reg_prep$age_index)+1
# Generate new age groups. In this case generate "85-89" 
#But could be usefull to generate n more intervals changing 1:1 by 1:n
new_age_groups <- sapply(1:1, function(i) {
  start_age <- age_last + (i - 1) * 5
  end_age <- start_age + 4
  paste(start_age, "-", end_age, sep="")
})

df_reg_fit <- df_reg_prep %>% #Adding age index (1, 2, ...,
  pivot_longer(-c(age_group, age_index), names_to = "variable", values_to = "value")   %>%#data to long format
  group_by(variable) %>%
  do(model = lm(value ~ age_index, data = .)) %>%
  rowwise() %>%
  do(data.frame(age_group = new_age_groups,
                variable = .$variable,
                value = predict(.$model, newdata = data.frame(age_index = new_age_indexes)))) %>%
  ungroup() %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(age_group=as.factor(age_group))

#Estimation of the close interval
df_log_est=df_log

# Convert the 'age_group' column to character
df_log_est$age_group <- as.character(df_log_est$age_group)

# Find the index where the age_group is an open interval (ends with '+')
open_interval_index <- which(str_detect(df_log_est$age_group, "\\+$"))

# Replace the row with the open interval in df_logrec with the row from data_reg_fit
df_log_est[open_interval_index,] <- df_reg_fit

# Convert the 'age_group' column back to factor (optional)
df_log_est$age_group <- factor(df_log_est$age_group)

#Convert to original scale
##########################
df_est <- df_log_est %>%
  pivot_longer(-c(age_group), names_to = "variable", values_to = "value") %>% # Reshape the data to a longer format
  mutate(value=logistic(value)) %>% #Inverse transformation
  mutate(variable = str_sub(variable, 2)) %>%#Drop "l"
  separate(variable, into=c("var","sex"), sep="_", remove = TRUE) %>%
  pivot_wider(names_from = var, values_from = value) %>%
  mutate(
    edu4 = pi3,
    edu3 = pi2-edu4,
    edu2 = pi1-edu3-edu4,
    edu1 = 1-pi1
  ) %>% #pi to edu
  select(-c(pi1, pi2, pi3))
  
  
