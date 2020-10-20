### LIS Cross-section Data center in Luxembourg

# email: usersupport@lisdatacenter.org

### LIS Self Teaching Package 2020
### Part II: Gender, employment, and wages
### R version

# last change of this version of the syntax: 31-07-2020


## Exercise 2: Stacking data, employment rates by gender

get_stack <- function(datasets, varp, varh, subset) {
  # READ DATASETS
   pp <- read.LIS(paste(datasets, 'p', sep = ''), labels = FALSE, vars = varp, subset = subset)
   hh <- read.LIS(paste(datasets, 'h', sep = ''), labels = FALSE, vars = varh)
   df <- merge(pp, hh,  by = c("dname", "hid"))
   return(df)
}
#------------------------------ RUN SCRIPTS -----------------------------
varh  <- c('hid', 'dname', 'own')
varp  <- c('hid', 'dname', 'ppopwgt', 'age', 'sex', 'relation', 'emp', 'ptime1')
subset   <- 'age >= 25 & age <= 54 & relation <= 2200'
datasets <- c('us04', 'de04', 'gr04')
df       <- get_stack(datasets, varp, varh, subset)

'Female Employment Rate'
round(with(df[df$sex==2 & !is.na(df$emp),], tapply(emp*ppopwgt, list(dname), sum) / tapply(ppopwgt, list(dname), sum)) *100, digits=2)

'Partime Employment rate among employed women'
round(with(df[df$sex==2 & df$emp==1 & !is.na(df$ptime1),], tapply(ptime1*ppopwgt, list(dname), sum) / tapply(ppopwgt, list(dname), sum)) *100, digits=2)
