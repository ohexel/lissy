### LIS Cross-section Data center in Luxembourg

# email: usersupport@lisdatacenter.org

### LIS Self Teaching Package 2020
### Part I: Inequality, poverty, and social policy
### R version

# last change of this version of the syntax: 31-07-2020


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
