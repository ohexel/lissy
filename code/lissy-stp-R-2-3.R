### LIS Cross-section Data center in Luxembourg

# email: usersupport@lisdatacenter.org

### LIS Self Teaching Package 2020
### Part II: Gender, employment, and wages
### R version

# last change of this version of the syntax: 31-07-2020


## Exercise 3: Family structure and employment

get_stack <- function(datasets, varp, varh, subset) {
  # READ DATASETS
  pp <- read.LIS(paste(datasets, 'p', sep = ''), labels = FALSE, vars = varp, subset = subset)
  hh <- read.LIS(paste(datasets, 'h', sep = ''), labels = FALSE, vars = varh)
  df <- merge(pp, hh,  by = c("dname", "hid"))
  # MAP NEW VARIABLES

df$achildcat <- ifelse(df$ageyoch < 6, 1, ifelse( df$ageyoch > 5 & df$ageyoch < 18,  2, 0))
df$achildcat [is.na(df$achildcat)] <- 0

  return(df)
}
#---------------------------------------- RUN SCRIPTS -------------------------------------------
datasets <- c('us04', 'de04', 'gr04') 
varh     <- c('hid', 'dname', 'own')
varp     <- c('hid', 'dname', 'ppopwgt', 'age', 'sex', 'relation', 'emp', 'ptime1', 'ageyoch', 'partner', 'status1')
subset   <- 'age >= 25 & age <= 54 & relation <= 2200'
df       <- get_stack(datasets, varp, varh, subset)
  
round(with(df[df$sex == 2 & !is.na(df$emp), ], tapply(emp * ppopwgt, list(dname, achildcat, partner), sum) / tapply(ppopwgt , list(dname, achildcat, partner), sum)) * 100, digits = 2)
