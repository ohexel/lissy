### LIS Cross-section Data center in Luxembourg

# email: usersupport@lisdatacenter.org

### LIS Self Teaching Package 2020
### Part II: Gender, employment, and wages
### R version

# last change of this version of the syntax: 31-07-2020


## Exercise 1: Merging person and household data, selecting a sample

setups <- function(ccyy) {
  # READ DATASETS
  varh   <- c('hid', 'dname', 'own')
  varp   <- c('hid', 'dname', 'ppopwgt', 'age', 'sex', 'relation', 'emp', 'ptime1', 'ageyoch', 'partner', 'status1', 'gross1', 'educ', 'immigr')
  subset <- 'age >= 25 & age <= 54 & relation <= 2200'
  dh     <- read.LIS(paste(ccyy, 'h', sep = ''), labels = FALSE, vars = varh)
  dp     <- read.LIS(paste(ccyy, 'p', sep = ''), labels = FALSE, vars = varp, subset = subset)
  df     <- merge(dh, dp, by = c('hid'))
  # MAP NEW VARIABLES
  df$home <- ifelse(df$own %in% 100:199, 1, ifelse(df$own %in% 200:299, 0, NA))  
  return(df)
}
#---------------------------------------- RUN SCRIPTS -------------------------------------------
datasets <- c('us04', 'de04', 'gr04')
for (ccyy in datasets) {
  df  <- setups(paste(ccyy, sep = ''))
  res <- round(with(df[!is.na(df$home),], sum(home*ppopwgt) / sum(ppopwgt[!is.na(home)])) *100, digits=2)
  print(c(ccyy, res))
}
