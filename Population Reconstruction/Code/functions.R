################################################################################
# R Script Header
# 
# Created by: Felipe Sanchez
# Contact: anfesanz@gmail.com
# Date: July 2023
#
# Description:
# This script contains several functions that are used throughout all the subprograms.
#
# age_at_least: This function generates a list of age groups for performing regressions, excluding the last open interval.
#               It takes a starting age "age_start_reg" and a list of age groups, for example, from "0-4" to "85+".
#               If age_start_reg is set to 40, the output will be the age groups from "40-44" to "80-84".
#
# Logit and logistic functions: These functions are renamed from functions already defined in RStats to avoid confunsion.
#
# ltable: This function calculates a life table based on Preston (2001), page 49. An example given by Preston (2001) is replicated at the end under "dataexample".
#
################################################################################

#Logistic transformation
logit <- qlogis
logistic <- plogis

# Custom filter function to regress
age_at_least <- function(age_group,age_start_reg) {
  age <- as.numeric(str_extract(age_group, "\\d+"))
  return(ifelse(is.na(age), FALSE, age >= age_start_reg))
}
#data is a data frame that contains ages (x), mid-year population (N_x), deads count (D_x) and a previous calculation of a from the data
#This could be a borrowed table (see Preston(2001) page 45) combined with Coale-Demeny equations for ages 0 and 1 
#I calculate this based on Preston(2001) page 49
ltable <- function(data) {
  ltable = data %>%mutate(
    n = lead(x) - x,
    m_x = D_x/N_x,
    q_x = n*m_x/(1+(n-a_x)*m_x), # probability of dying between age x and x + 1
    q_x = replace_na(q_x, 1),
    p_x = 1 - q_x, # probability of surviving between age x and x + 1
    l_x = c(100000, 100000 * cumprod(p_x)[-length(p_x)]), # Calculate l_x - number of person-years lived
    d_x = l_x - lead(l_x, default = 0), # Calculate d_x
    L_x = ifelse(row_number() < n(), n * lead(l_x, default = 0) + a_x * d_x, l_x / m_x), # Calculate L_x - number of person-years lived between age x and x + 1
    T_x = sum(L_x) - cumsum(L_x) + L_x, # person-years remaining after age x
    e_x = T_x / l_x # life expectancy at age x
  )
  return(ltable)
}


dataexample <- data.frame(
  x  = c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85),
  N_x = c(47925, 189127, 234793, 238790, 254996, 326831, 355086, 324222, 269963, 261971, 238011, 261612, 181385, 187962, 153832, 105169, 73694, 57512, 32248),
  D_x = c(419, 70, 36, 46, 249, 420, 403, 441, 508, 769, 1154, 1866, 2043, 3496, 4366, 4337, 5279, 6460, 6146),
  a_x = c(0.068, 1.626, 2.5, 3.143, 2.724, 2.52, 2.481, 2.601, 2.701, 2.663, 2.698, 2.676, 2.645, 2.624, 2.619, 2.593, 2.518, 2.423, 5.247)
)

ax_example=ltable(dataexample)






