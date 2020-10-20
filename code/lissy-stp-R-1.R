### LIS Cross-section Data center in Luxembourg

# email: usersupport@lisdatacenter.org

### LIS Self Teaching Package 2020
### Part I: Inequality, poverty, and social policy
### R version

# last change of this version of the syntax: 31-07-2020

# The exercises in Part I demonstrate the use of household income data along with 
# useful programming techniques for working with the LIS data. With a focus on 
# descriptive statistics, the exercises will lead you through the process of developing
# a complete program that examines inequality and poverty across countries.


## Exercise 1: Accessing the LIS databases

df <- read.LIS('gt06h')
print(summary(df$dhi, digits=10))
print(sum(!is.na(df$dhi)))


## Exercise 2:	Sample selection and weighting

wmean <- function(x, weight) { 
  y     <- x[which(!is.na(x))] 
  wgt   <- weight[which(!is.na(x))] 
  wmean <- sum(y*wgt/sum(wgt)) 
  return(wmean) 
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
 vars <- c('dhi','hifactor','hpub_i', 'hpub_u','hpub_a','hiprivate','hxitsc','hpopwgt','nhhmem','grossnet') 
df   <- read.LIS('gt06h', labels=FALSE, vars=vars) 
subset <- 'complete.cases(dhi,hifactor,hpub_i,hpub_u,hpub_a,hiprivate,hxitsc)'
df2 <- read.LIS('gt06h', labels=FALSE, vars=vars, subset=subset)
print(row_total <- nrow(df)) 
print(row_drop  <- with(df, length(which((complete.cases(dhi,hifactor,hpub_i,hpub_u,hpub_a,hiprivate,hxitsc) == TRUE))))) 
print(miss_income<- row_total- row_drop  )
round(((row_total - row_drop) / row_total) * 100, digits = 2) 
print(c(toupper('grossnet'))) 
print(table(df$grossnet, useNA = 'ifany')) 
 print(paste(round(prop.table(table(df$grossnet, useNA = 'ifany')) * 100, digits = 2), "%", sep = "")) 
cat(paste(" "), sep = '\n')
for (x in c('hpopwgt','dhi','hifactor','hpub_i', 'hpub_u','hpub_a','hiprivate','hxitsc')) { 
 df1 <- df[!is.na(df[[x]]), ] 
 print(c(toupper(x))) 
 print(c(nb_obs = sum(!is.na(df1[[x]])))) 
 print(summary(df1[,x], digits = 10)) 
 print(c(weighted_mean = round(wmean(df1[[x]], df1$hpopwgt*df1$nhhmem), digits = 2),   weighted_median = round(wNtile(df1[[x]], df1$hpopwgt * df1$nhhmem, split = 0.5), digits = 2))) 
  cat(" ", sep = '\n')   
}  
 for (x in c('hpopwgt','dhi','hifactor','hpub_i', 'hpub_u','hpub_a','hiprivate','hxitsc')) { 
print(c(toupper(x))) 
 print(c(nb_obs = sum(!is.na(df2[[x]])))) 
 print(summary(df2[,x], digits = 10)) 
 print(c(weighted_mean = round(wmean(df2[[x]], df2$hpopwgt*df2$nhhmem), digits = 2),   weighted_median = round(wNtile(df2[[x]], df2$hpopwgt * df2$nhhmem, split = 0.5), digits = 2))) 
  cat(" ", sep = '\n')   
} 

df <- read.LIS('gt06h')
print(summary(df$currency, digits=10))


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


## Exercise 4: Inequality: The Gini Index

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
  round(gini(df$dhi , df$hpopwgt)          , digits = 4)
  round(gini(df$cdhi, df$hpopwgt*df$nhhmem), digits = 4)
  round(gini(df$edhi, df$hpopwgt*df$nhhmem), digits = 4)


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


## Exercise 6: Comparing income concepts

gini <- function(x, weight) {
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
  vars     <- c('dhi', 'hifactor', 'hi33', 'hpublic','hpub_i', 'hpub_u', 'hpub_a', 'hiprivate', 'hxitsc', 'hpopwgt', 'nhhmem', 'grossnet')
  subset   <- 'complete.cases(dhi, hifactor, hi33, hpublic, hpub_i, hpub_u, hpub_a, hiprivate, hxitsc)'
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
df <- setups('gt06h')
maxline <- 0.5
for(var in c('emi', 'esiti', 'esa', 'edhi')) {
  cat(paste("VARIABLE: ", var), sep = '\n')
  cat(paste("Gini Coefficient :" , round(gini(df[[var]], df$hpopwgt*df$nhhmem), digits = 3)), sep = '\n')
  cat(paste("Poverty Rate     :", round(100 * (sum((df[[var]] < maxline * wNtile(df$edhi, df$hpopwgt * df$nhhmem, 0.5)) * df$hpopwgt * df$nhhmem) / sum(df$hpopwgt * df$nhhmem)), digits = 2)), sep = '\n')
 cat(" ", sep = '\n')
}
for (var in c('hpublic','hpub_i','hpub_u','hpub_a')) {  
cat(paste("Mean: ", var, round(mean(df[[var]]), digits = 2))) 
 cat(" ", sep = '\n') 
}  


## Exercise 7: Comparing multiple countries

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
  for (ccyy in datasets) {
    df <- setups(paste(ccyy,'h',sep=''))
    for(var in c('emi', 'esiti', 'esa', 'edhi')) {
      cat(paste("VARIABLE: ", var), sep = '\n')
      cat(paste("Gini Coefficient :" , round(gini(df[[var]], df$hpopwgt*df$nhhmem), digits = 3)), sep = '\n')
      cat(paste("Poverty Rate     :", round(100 * (sum((df[[var]] < maxline * wNtile(df$edhi, df$hpopwgt * df$nhhmem, 0.5)) * df$hpopwgt * df$nhhmem) / sum(df$hpopwgt * df$nhhmem)), digits = 2)), sep = '\n')
      cat(" ", sep = '\n')
    }
print(c(toupper('grossnet')))
print(table(df$grossnet, useNA = 'ifany')) 
 print(paste(round(prop.table(table(df$grossnet, useNA = 'ifany')) * 100, digits = 2), "%", sep = "")) 
cat(paste(" "), sep = '\n')
for (var in c('hpublic','hpub_i','hpub_u','hpub_a')) {  
cat(paste("Mean: ", var, round(mean(df[[var]]), digits = 2))) 
 cat(" ", sep = '\n') 
} 
}


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
  