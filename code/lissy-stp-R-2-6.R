### LIS Cross-section Data center in Luxembourg

# email: usersupport@lisdatacenter.org

### LIS Self Teaching Package 2020
### Part II: Gender, employment, and wages
### R version

# last change of this version of the syntax: 31-07-2020


## Exercise 6: Immigration and wages, understanding harmonisation

get_stack <- function(datasets, varp, varh, subset) {
# READ DATASETS
pp <- read.LIS(paste(datasets, 'p', sep = ''), labels=FALSE, vars=varp, subset=subset)
hh <- read.LIS(paste(datasets, 'h', sep = ''), labels = FALSE, vars = varh)
df <- merge(pp, hh,  by = c("dname", "hid"))

# MAP NEW VARIABLES
df$sex  <- ifelse(df$sex == 1, 0, 1) 
df$dept <- ifelse(df$status1 %in% c(100:120), 1, ifelse(is.na(df$status1)  , NA, 0))
df$hrwg <- df$gross1
df$hrwg <- ifelse(df$hrwg <= 0, NA, df$hrwg)
for (i in 1:length(datasets)) {
topline <- with(df[!is.na(df$hrwg) & df$dname==datasets[i],], 10*wNtile(hrwg, ppopwgt,0.5))
df$hrwg <- with(df[!is.na(df$hrwg) & df$dname==datasets[i],], ifelse(df$hrwg > topline, topline, df$hrwg))
}
 return(df)
}
wNtile <- function(var, wgt, split) {
  x  <- var[order(var)]
  y  <- wgt[order(var)] 
  z  <- cumsum(y) / sum(y)
  cop  <- rep(NA,length(split))
  for (i in 1:length(cop)) {
    cop[i] <- x[Find(function(h) z[h] > split[i], seq_along(z))]
  }
  return(cop)
}
#-------------------------------------- RUN SCRIPTS -----------------------------------
datasets <- c('us04', 'de04', 'gr04') 
varh     <- c('hid', 'dname', 'own')
varp     <- c('hid', 'dname', 'ppopwgt', 'age', 'sex', 'relation', 'emp', 'ptime1', 'ageyoch', 'partner', 'status1', 'gross1', 'educ', 'immigr')
subset   <- 'age >= 25 & age <= 54 & relation <= 2200'
df       <- get_stack(datasets, varp, varh, subset)

# GENDER WAGE GAP
ctry_list <- list()
  for (k in (1:length(datasets))) {
    mat <- matrix(NA, length(unique(df$sex[!is.na(df$sex)])),
    length(unique(df$immigr[!is.na(df$immigr)])))
    colnames(mat) <- c('Non Immigrant', 'Immigrant')
    for (j in (1:length(unique(df$immigr[!is.na(df$immigr)])))) {
      for (i in (1:length(unique(df$sex[!is.na(df$sex)])))) {
        mat[i,j] <- with(df[df$dname == datasets[k] & !is.na(df$hrwg) & !is.na(df$sex)
        & !is.na(df$immigr) & df$sex== i-1 & df$immigr== j-1,],
        wNtile(hrwg,ppopwgt,0.5))
      }
    }
    ctry_list[[datasets[k]]] <- round(mat[2, ] / mat[1, ], digits = 2)
  }

  'Gender Wage Gap'
  ctry_list
