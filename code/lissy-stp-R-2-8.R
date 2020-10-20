### LIS Cross-section Data center in Luxembourg

# email: usersupport@lisdatacenter.org

### LIS Self Teaching Package 2020
### Part II: Gender, employment, and wages
### R version

# last change of this version of the syntax: 31-07-2020

 
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
  df$ppp       <- ifelse(df$dname == 'de04', 0.79, ifelse(df$dname == 'gr04', 0.65, 1))
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
