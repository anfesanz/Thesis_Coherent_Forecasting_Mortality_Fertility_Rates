
library(tidyverse)

#Linear interpolation for 1998 survey
#There are two cases. 
#Females 80-84 in 1998
#Females 85+ in 1998

#Females 80-84 in 1998 (interpolation function approx())
data <- data.frame(
  edu3 = c(45976, 31041, 15995, 10223, 4503, 3338, 1768, 591, NA, 410),
  index = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
)

# Interpolate the missing value
data$edu3 <- ifelse(is.na(data$edu3), 
                    approx(data$index, data$edu3, xout = data$index)$y,  
                    data$edu3)

# Print the dataframe to check the results
print(data)

#Females 85+ in 1998 (fill with the previous)
data <- data.frame(
  edu4 = c(114306, 75892, 33415, 15280, 10686, 4101, 1432, 350, 850, NA),
  index = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
)

# Fill the missing value with the last non-missing value
data$edu4[is.na(data$edu4)] <- tail(data$edu4[!is.na(data$edu4)], n=1)

# Print the dataframe to check the results
print(data)


