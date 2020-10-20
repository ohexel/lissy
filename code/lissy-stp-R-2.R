### LIS Cross-section Datacenter in Luxembourg

# email: usersupport@lisdatacenter.org

### LIS Self Teaching Package 2020
### Part II: Gender, employment, and wages
### R version

# last change of this version of the syntax: 31-07-2020

# The exercises in Part II emphasises the use of person-level data, including wages, 
# demographics, and labour market information. Building-up on the techniques presented
# in Part I, they introduce regression modelling and continue to lead you through 
# the process of developing a comparative analysis on inequality and poverty across countries.


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


## Exercise 4: Dependent employment and hourly wages

get_stack <- function(datasets, varp, varh, subset) {

# READ DATASETS
pp <- read.LIS(paste(datasets,'p',sep=''),labels=FALSE, vars=varp, subset = subset)
hh  <- read.LIS(paste(datasets,'h', sep=''), labels=FALSE, vars=varh)
df  <- merge(pp, hh,  by = c("dname", "hid"))
  
# MAP NEW VARIABLES
df$sex  <- ifelse(df$sex == 1, 0, 1) 
df$dept <- ifelse(df$status1 %in% c(100:120),1,ifelse(is.na(df$status1),NA,0))
df$hrwg <- df$gross1
df$hrwg <- ifelse(df$hrwg <= 0, NA, df$hrwg)
for (i in 1:length(datasets)) {
topline <- with(df[!is.na(df$hrwg) & df$dname==datasets[i],], 10 * wNtile(hrwg, ppopwgt, 0.5))
df$hrwg <- with(df[!is.na(df$hrwg) & df$dname==datasets[i],], ifelse(df$hrwg > topline, topline, df$hrwg))
}
  return(df)
}
wNtile <- function(var, wgt, split) {
  x   <- var[order(var)]
  y   <- wgt[order(var)] 
  z   <- cumsum(y) / sum(y)
  cop <- rep(NA,length(split))
  for (i in 1:length(cop)) {
    cop[i] <- x[Find(function(h) z[h] > split[i], seq_along(z))]
  }
  return(cop)
}
#-------------------------------- RUN SCRIPTS ----------------------------------
datasets <- c('us04', 'de04', 'gr04') 
varh     <- c('hid', 'did','dname', 'own')
varp     <- c('hid', 'dname', 'ppopwgt', 'age', 'sex', 'relation', 'emp', 'ptime1', 'ageyoch', 'partner', 'status1', 'gross1')
subset   <- 'age >= 25 & age <= 54 & relation <= 2200'
df       <- get_stack(datasets, varp, varh, subset)

# EMPLOYMENT RATES
print('Employment Rate')  
round(with(df[df$emp == 1 & !is.na(df$dept), ], tapply(dept * ppopwgt, list(dname, sex), sum) / tapply(ppopwgt, list(dname,sex), sum)) * 100, digits=2)

# GENDER WAGE GAP
mat <- matrix(NA, 2 ,3)
colnames(mat) <- datasets
for (i in (1:length(datasets))) {
  for (j in 1:length(unique(df$sex))) {
    mat[j,i] <- with(df[df$dname==datasets[i] & !is.na(df$hrwg) & df$sex== j-1,] , wNtile(hrwg, ppopwgt, 0.5))
  }
}
'Gender Wage Gap'
round(mat[2, ] / mat[1, ], digits = 2)


## Exercise 5: Hourly wages, education, and country-specific variables

get_stack <- function(datasets, varp, varh, subset) {
  # READ DATASETS
  pp      <- read.LIS(paste(datasets, 'p', sep = ''), labels = FALSE, vars = varp, subset = subset)
  hh      <- read.LIS(paste(datasets, 'h', sep = ''), labels = FALSE, vars = varh)
  df      <- merge(pp, hh,  by = c("dname", "hid"))
  # MAP NEW VARIABLES	
  df$sex  <- ifelse(df$sex == 1               , 0, 1) 
  df$hrwg <- df$gross1
  df$hrwg <- ifelse(df$hrwg <= 0, NA, df$hrwg)
  for (i in 1:length(datasets)) {
    topline <- with(df[!is.na(df$hrwg) & df$dname == datasets[i], ] , 10 * wNtile(hrwg, ppopwgt, 0.5))
    df$hrwg <- with(df[!is.na(df$hrwg) & df$dname == datasets[i], ] , ifelse(df$hrwg > topline, topline, df$hrwg))
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
#---------------------------------- RUN SCRIPTS ---------------------------------------
datasets <- c('us04', 'de04', 'gr04') 
varh     <- c('hid', 'dname', 'own')
varp     <- c('hid', 'dname', 'ppopwgt', 'age', 'sex', 'relation', 'emp', 'ptime1', 'ageyoch', 'partner', 'status1', 'gross1', 'educ')
subset   <- 'age >= 25 & age <= 54 & relation <= 2200'
df       <- get_stack(datasets, varp, varh, subset)

# GENDER WAGE GAP
ctry_list <- list()
for (k in (1:length(datasets))) {
mat <- matrix(NA,length(unique(df$sex[!is.na(df$sex)])),length(unique(df$educ[!is.na(df$educ)])))
  colnames(mat) <- c('Low', 'Medium', 'high')
  for (j in (1:length(unique(df$educ[!is.na(df$educ)])))) {
    for (i in (1:length(unique(df$sex[!is.na(df$sex)])))) {
mat[i,j] <- with(df[df$dname == datasets[k] & !is.na(df$hrwg) & !is.na(df$sex) & !is.na(df$educ) & df$sex == i - 1 & df$educ == j, ], wNtile(hrwg, ppopwgt, 0.5))
    }
  }
  ctry_list[[datasets[k]]] <- round(mat[2, ] / mat[1, ], digits = 2)
}

'Gender Wage Gap'
ctry_list


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


## Exercise 7: Wage regressions

get_stack <- function(datasets, varp, varh, subset) {
# READ DATASETS
  pp <- read.LIS(paste(datasets, 'p', sep=''), labels=FALSE, vars=varp, subset=subset)
  hh <- read.LIS(paste(datasets, 'h', sep = ''), labels = FALSE, vars = varh)
  df <- merge(pp, hh,  by = c("dname", "hid"))
  
# MAP NEW VARIABLES
df$homeowner <- ifelse(df$own %in% 100:199, 1, ifelse(df$own %in% 200:299, 0, NA))  
df$achildcat <- ifelse(df$ageyoch < 6, 1, ifelse( df$ageyoch > 5 & df$ageyoch < 18,  2, 0))
df$achildcat [is.na(df$achildcat)] <- 0
df$ychild <- ifelse(df$achildcat==1, 1, ifelse(is.na(df$achildcat), NA, 0))
df$ochild <- ifelse(df$achildcat==2, 1, ifelse(is.na(df$achildcat), NA, 0))
df$meduc  <- ifelse(df$educ==2 , 1, ifelse(is.na(df$educ), NA, 0))
df$heduc  <- ifelse(df$educ==3 , 1, ifelse(is.na(df$educ), NA, 0))
df$hrwg   <- df$gross1
df$hrwg   <- ifelse(df$hrwg <= 0, NA, df$hrwg)
for (i in 1:length(datasets)) {
 topline <- with(df[!is.na(df$hrwg) & df$dname==datasets[i],],
 10*wNtile(hrwg,ppopwgt,0.5))
 df$hrwg <- with(df[!is.na(df$hrwg) & df$dname==datasets[i],], 
 ifelse(df$hrwg>topline, topline, df$hrwg))
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
#------------------------------------------ RUN SCRIPTS ------------------------------ 
options(scipen = 2)  
datasets <- c('us04', 'de04', 'gr04') 
varh     <- c('hid', 'dname', 'own')
varp     <- c('hid', 'dname', 'ppopwgt', 'age', 'sex', 'relation', 'emp', 'ptime1',    
              'ageyoch', 'partner', 'status1', 'gross1', 'educ', 'immigr')
subset   <- 'age >= 25 & age <= 54 & relation <= 2200'
df       <- get_stack(datasets, varp, varh, subset)

# REGRESSION MODEL
gender <- c('Male', 'Female')
model <- formula(I(log(hrwg))~age + I(age^2) + meduc+heduc+immigr+partner+ychild+ochild+ptime1+homeowner)
for(s in (1:length(unique(df$sex[!is.na(df$sex)])))) for(d in datasets) {
  res <- glm(model, data = df, weights = df$ppopwgt, subset = sex == s & dname == d)
  print('-------------------------------------------------------------------------')
  print(paste(gender[s], d, sep = " : "))
  print(summary(res),digits=1)
 }

 
## Exercise 8: Pooled regressions and normalised weights

get_stack <- function(datasets, varp, varh, subset) {
  # READ DATASETS
  pp      <- read.LIS(paste(datasets, 'p', sep = ''), labels = FALSE, vars = varp, subset = subset)
  hh      <- read.LIS(paste(datasets, 'h', sep = ''), labels = FALSE, vars = varh)
  df      <- merge(pp, hh,  by = c("dname", "hid"))
  # MAP NEW VARIABLES
  df$homeowner <- ifelse(df$own %in% 100:199, 1, ifelse(df$own %in% 200:299, 0, NA))  
  df$achildcat <- ifelse(df$ageyoch < 6, 1, ifelse( df$ageyoch > 5 & df$ageyoch < 18,  2, 0))
  df$achildcat [is.na(df$achildcat)] <- 0
  df$ychild    <- ifelse(df$achildcat == 1, 1, ifelse(is.na(df$achildcat), NA, 0))
  df$ochild    <- ifelse(df$achildcat == 2, 1, ifelse(is.na(df$achildcat), NA, 0))
  df$meduc     <- ifelse(df$educ == 2, 1, ifelse(is.na(df$educ), NA, 0))
  df$heduc     <- ifelse(df$educ == 3, 1, ifelse(is.na(df$educ), NA, 0))
  df$ppp       <- ifelse(df$dname == 'de04', 0.74, ifelse(df$dname == 'gr04', 0.62, 1))
  df$hrwg      <- df$gross1
  df$hrwg      <- ifelse(df$hrwg <= 0, NA, df$hrwg)
  df$germany   <- ifelse (df$dname == 'de04', 1, 0)
  df$greece    <- ifelse (df$dname == 'gr04', 1, 0)
  for (i in 1:length(datasets)) {
    df$hrwg <- df$hrwg / df$ppp
    topline <- with(df[!is.na(df$hrwg) & df$dname==datasets[i],], 10 * wNtile(hrwg,ppopwgt,0.5))
    df$hrwg <- with(df[!is.na(df$hrwg) & df$dname==datasets[i],],
               ifelse(df$hrwg>topline,topline,df$hrwg))
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
#------------------------------------------ RUN SCRIPTS -----------------------------------
options(scipen = 2)  
datasets <- c('us04', 'de04', 'gr04') 
varh <- c('hid', 'dname', 'own')
varp <- c('hid','dname','pwgt','ppopwgt','age','sex','relation','emp','ptime1','ageyoch',
          'partner','status1','gross1','educ','immigr')
subset   <- 'age >= 25 & age <= 54 & relation <= 2200'
df       <- get_stack(datasets, varp, varh, subset)

# REGRESSION MODEL
gender <- c('Male', 'Female')
model <- formula(I(log(hrwg))~age + I(age^2) + meduc + heduc + immigr + partner + ychild + ochild + ptime1 + homeowner + germany + greece)
for(s in (1:length(unique(df$sex[!is.na(df$sex)])))) {
  res <- glm(model, data = df, weights = df$pwgt, subset = sex == s)
  print('-------------------------------------------------------------------------')
  print(gender[s])
  print(summary(res),digits=1)
}

