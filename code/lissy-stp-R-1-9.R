### LIS Cross-section Data center in Luxembourg

# email: usersupport@lisdatacenter.org

### LIS Self Teaching Package 2020
### Part I: Inequality, poverty, and social policy
### R version

# last change of this version of the syntax: 31-07-2020


## Exercise 9: Producing graphs

# Prepare session

library(readr) 
library(dplyr)   
library(magrittr)   
library(purrr)   
library(ggplot2)   
 
all_lissyrtools_scripts <- fs::dir_ls("/media/user/lissyrtools/")   
 
invisible(purrr::map(all_lissyrtools_scripts, ~ source(.x)))   
 
 
# Script -------------------------- 
 
# Read files 
lissy_datasets <- read_lissy_files(files = c("gt06h", "us04h", "dk04h"), full_year_name = TRUE)
 
 
# Data management 
lissy_datasets %<>% 
  transform_negative_values_to_zero(variable = "dhi") %>% 
  transform_equivalise(variable = "dhi") %>% 
  transform_top_code_with_iqr(variable = "dhi", times = 3) %>%
  transform_weight_by_hh_size(variable = "dhi")
 
 
# Plot Lorenz Curve 
lissy_datasets %>% 
  plot_lorenz_curve(variable = "dhi", na.rm = TRUE, plot_theme = "lis")
  