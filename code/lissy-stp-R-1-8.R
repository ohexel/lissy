### LIS Cross-section Data center in Luxembourg

# email: usersupport@lisdatacenter.org

### LIS Self Teaching Package 2020
### Part I: Inequality, poverty, and social policy
### R version

# last change of this version of the syntax: 31-07-2020


## Exercise 8: Producing compact and concise output

gini <- function(x,weight) {
  ox     <- order(x)
  x      <- x[ox]
  weight <- weight[ox]/sum(weight)
  p      <- cumsum(weight)
  nu     <- cumsum(weight*x)
  n      <- length(nu)
  nu     <- nu/nu[n]
  res    <- sum(nu[-1]*p[-n])-sum(nu[-n]*p[-1])
return(res)
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
setups <- function(data_file) {
  vars     <- c('dhi', 'hifactor', 'hi33', 'hpublic', 'hpub_i', 'hpub_u', 'hpub_a', 'hiprivate', 'hxitsc', 'hpopwgt', 'nhhmem', 'grossnet')
  subset <- 'complete.cases(dhi,hifactor,hi33,hpub_i,hpub_u,hpub_a,hiprivate,hxitsc)' 
  df       <- read.LIS(data_file, labels = FALSE, vars = vars, subset = subset) 
  df$dhi   <-  ifelse(df$dhi < 0, 0, df$dhi)
  df$edhi  <- df$dhi / (df$nhhmem^0.5)
  df$mi    <- df$hifactor + df$hiprivate+df$hi33
  df$mi <- ifelse(df$mi < 0, 0, df$mi) 
  df$emi   <-  df$mi / (df$nhhmem^0.5)
  df$siti  <- df$hifactor + df$hiprivate  + df$hi33 + df$hpub_i + df$hpub_u - df$hxitsc
  df$siti <- ifelse(df$siti < 0, 0, df$siti)
  df$esiti <- df$siti /(df$nhhmem^0.5)
  df$sa <- df$hifactor + df$hiprivate + df$hi33 + df$hpub_a
  df$sa <- ifelse(df$sa < 0, 0, df$sa)
  df$esa <- df$sa / (df$nhhmem^0.5)  
  return(df)
}

datasets <- c('gt06', 'us04', 'dk04', 'hu05', 'il05')
maxline <- 0.5
result           <-  matrix(NA,length(datasets),8)
rownames(result) <- datasets
colnames(result) <- colnames(result) <-c('gini_mi','gini_siti','gini_sa','gini_dhi','pov_mi','pov_siti','pov_sa','pov_dhi') 

for (ccyy in datasets) {  
    df <- setups(paste(ccyy,'h',sep=''))  
   # For net income datasets, blank out market income measures, since these cannot be calculated correctly
    for(var in c( 'esiti', 'esa', 'edhi')) {  
if (df$grossnet ==200) {  
      result[match(ccyy, datasets),     match(var, c('emi', 'esiti', 'esa', 'edhi'))] <- round(gini(df[[var]], df$hpopwgt*df$nhhmem), digits = 3)  
      result[match(ccyy, datasets),  4+ match(var, c('emi',  'esiti', 'esa', 'edhi'))] <- round(100 * (sum((df[[var]] < maxline * wNtile(df$edhi, df$hpopwgt * df$nhhmem, 0.5)) * df$hpopwgt * df$nhhmem) / sum(df$hpopwgt * df$nhhmem)), digits = 2)  
} else {  
    for(var in c('emi', 'esiti', 'esa', 'edhi')) {  
      result[match(ccyy, datasets),     match(var, c('emi', 'esiti', 'esa', 'edhi'))] <- round(gini(df[[var]], df$hpopwgt*df$nhhmem), digits = 3)  
      result[match(ccyy, datasets), 4 + match(var, c('emi', 'esiti', 'esa', 'edhi'))] <- round(100 * (sum((df[[var]] < maxline * wNtile(df$edhi, df$hpopwgt * df$nhhmem, 0.5)) * df$hpopwgt * df$nhhmem) / sum(df$hpopwgt * df$nhhmem)), digits = 2)  
  }
}  
}  
} 
print(write.csv(result))
