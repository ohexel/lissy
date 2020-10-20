### LIS Cross-section Data center in Luxembourg

# email: usersupport@lisdatacenter.org

### LIS Self Teaching Package 2020
### Part I: Inequality, poverty, and social policy
### R version

# last change of this version of the syntax: 31-07-2020


## Exercise 3: Working with household income variables (top and bottom coding and equivalence scales)

wmean <- function(x, weight= NULL) { 
  if (is.null(weight))  
    weight <- rep(1, length(x)) 
  y     <- x[which(!is.na(x))] 
  wgt   <- weight[which(!is.na(x))] 
  wmean <- sum(y*wgt/sum(wgt)) 
  return(wmean) 
} 
wNtile <- function(var, wgt = NULL, split) { 
  if (is.null(wgt))  
      wgt <- rep(1, length(var)) 
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
  df$dhiT  <- topBottom(df$dhi, botline, topline) 
  df$edhi <- df$dhiT / df$nhhmem^0.5 
  df$cdhi <- df$dhiT / df$nhhmem 
  return(df) 
} 
 
df <- setups('gt06h') 
for (x in c('dhi', 'dhiT')) { 
  cat(paste("VARIABLE: ", toupper(x), sep=""), sep = '\n') 
  cat(paste("Average: " , format(round(wmean(df[[x]]    , df$hpopwgt)     , digits = 2), big.mark = ",")), sep = '\n') 
  cat(paste("Median : " , format(round(wNtile(df[[x]], df$hpopwgt, 0.5), digits = 2), big.mark = ",")), sep = '\n') 
  cat(paste("Minimum: " , format(round(min(df[[x]]), digits = 2), big.mark = ",")), sep = '\n') 
  cat(paste("Maximum: " , format(round(max(df[[x]]), digits = 2), big.mark = ",")), sep = '\n') 
  cat(" ", sep = '\n') 
} 
for (x in c('cdhi', 'edhi')) { 
  cat(paste("VARIABLE: ", toupper(x), sep=""), sep = '\n') 
  cat(paste("Average: " , format(round(wmean(df[[x]]    , df$hpopwgt * df$nhhmem)     , digits = 2), big.mark = ",")), sep = '\n') 
  cat(paste("Median : " , format(round(wNtile(df[[x]], df$hpopwgt * df$nhhmem, 0.5), digits = 2), big.mark = ",")), sep = '\n') 
  cat(paste("Minimum: " , format(round(min(df[[x]]), digits = 2), big.mark = ",")), sep = '\n') 
  cat(paste("Maximum: " , format(round(max(df[[x]]), digits = 2), big.mark = ",")), sep = '\n') 
  cat(" ", sep = '\n') 
  }
  