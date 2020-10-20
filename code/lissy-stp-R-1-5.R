### LIS Cross-section Data center in Luxembourg

# email: usersupport@lisdatacenter.org

### LIS Self Teaching Package 2020
### Part I: Inequality, poverty, and social policy
### R version

# last change of this version of the syntax: 31-07-2020


## Exercise 5: Relative poverty rates

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
topBottom <- function(var, botline, topline) {
  tb               <- ifelse(var < botline, botline, var)
  tb[tb > topline] <- topline
  return(tb)
}
setups <- function(data_file) {
  vars    <- c('dhi', 'hifactor', 'hpub_i', 'hpub_u', 'hpub_a', 'hiprivate', 'hxitsc', 'hpopwgt', 'nhhmem', 'grossnet')
  subset  <- 'complete.cases(dhi, hifactor, hpub_i, hpub_u, hpub_a, hiprivate, hxitsc)'
  df      <- read.LIS(data_file, labels=FALSE, vars=vars, subset=subset)
  botline <- 0 
  topline <- 10 * wNtile(df$dhi, df$hpopwgt, 0.5)
  df$dhi  <- topBottom(df$dhi, botline, topline)
  df$edhi <- df$dhi / df$nhhmem^0.5
  df$cdhi <- df$dhi / df$nhhmem
  return(df)
}
df <- setups('gt06h')
maxline <- 0.5
round(100 * (sum((df$edhi < maxline * wNtile(df$edhi, df$hpopwgt * df$nhhmem, 0.5)) * df$hpopwgt) / sum(df$hpopwgt)), digits = 2)
round(100 * (sum((df$edhi < maxline * wNtile(df$edhi, df$hpopwgt * df$nhhmem, 0.5)) * df$hpopwgt * df$nhhmem) / sum(df$hpopwgt * df$nhhmem)), digits = 2)
