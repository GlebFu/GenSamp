library(tidyverse)
library(stargazer)
library(htmlTable)
library(xtable)
library(Hmisc)

rm(list = ls())

######################
# SUPPORT FUNCTIONS
######################

# Calculates grand mean centering
GMC <- function(x) x - mean(x, na.rm = T)
# Standardizes scores
STAND <- function(x) (x - mean(x, na.rm = T))/sd(x, na.rm = T)

logit <- function(x) log(x / (1 - x)) 
expit <- function(x) exp(x) / (1 + exp(x))

# Samples from binomial distribution
genE <- function(ps) rbinom(length(ps), 1, prob = ps)

###################
# SIM FUNCTIONS
###################

calcPS <- function(Bs, MRR, vars, data, exclude, getint = F, int = NULL) {
  vars <- vars[!(vars %in% exclude)]
  X <- as.matrix(data[,vars])
  
  XB <- as.numeric(X %*% Bs)
  
  # int = uniroot(function(b0) mean(expit(b0 + XB)) - MRR, c(-20,20))
  # tryCatch({
  if(is.null(int)){
    int = uniroot(function(b0) mean(expit(b0 + XB)) - MRR, interval = c(-30,30))
    int <- int$root
  }
  # },
  # error = function(err) {
  #   print(err)
  #   print(Bs)
  # })
  
  
  dY <- as.numeric(X %*% Bs) + int
  
  PS <- expit(dY)
  
  if (getint) return(list(PS = PS, Intercept = int))
  
  return(PS)
}

# calcPS(Bs = dBs,
#        MRR = .5,
#        vars = dVars,
#        data = df.dists,
#        exclude = dEx)
# 
# undebug(calcPS)



# Calculates the Population mean and sd, and the sampled/unsampled mean and SMD from population mean

calcSMD <- function(Bs, MRR, vars, data, exclude, calcPS = T, goal = NULL) {
  
  # Generate Propensity scores if not already present
  if (calcPS) data$PS <- calcPS(Bs = Bs, MRR = MRR, vars = vars, data = data, exclude = exclude)
  
  # Calcualte IPSW
  data$dIPSW0 <- 1/(1-data$PS)
  data$dIPSW1 <- 1/data$PS
  # data$dIPSW1 <- data$PS
  
  
  dMeans <- data[,c(vars, "dIPSW1", "dIPSW0")] %>%
    gather(key = wType, value = weight, dIPSW0:dIPSW1) %>%
    gather(key = Var, value = val, -weight, -wType) %>%
    group_by(wType, Var) %>%
    summarise(wM = weighted.mean(val, weight)) %>%
    spread(key = wType, value = wM)
  
  dMeans <- data[,vars] %>%
    gather(key = Var, value = Val) %>%
    group_by(Var) %>%
    summarise(m = mean(Val), sd = sd(Val)) %>%
    merge(dMeans) %>%
    mutate(smdS0 = (dIPSW0 - m)/sd, smdS1 = (dIPSW1 - m)/sd)
  
  if (!is.null(goal)) {
    dMeans <- merge(dMeans, data.frame(Var = names(goal), goal)) %>%
      mutate(dif = smdS1 - goal)
  }
  
  return(dMeans)
  
}



# calcSMD(Bs = dBs,
#         MRR = intResp,
#         vars = dVars,
#         data = df.dists,
#         exclude = dEx,
#         goal = dGoal)
# 
# undebug(calcSMD)

testGoal <- function(Bs, MRR, vars, data, exclude, goal = NULL) {
  
  # Calculate SMD
  trial <- calcSMD(Bs = Bs, MRR = MRR, vars = vars, 
                   data = data, exclude = exclude, 
                   goal = goal)
  
  
  goal <- data.frame(Var = names(goal), goal = goal)
  
  trial <- merge(trial, goal)
  difs <- trial$goal - trial$smdS1
  names(difs) <- c(trial$Var)
  
  return(sum(difs^2))
}

# testGoal(Bs = dBs,
#          MRR = RR,
#          vars = dVars,
#          data = df.dists,
#          exclude = dEx,
#          goal = dGoal)
# 
# undebug(testGoal)

getVals <- function (bs, vars, data, exclude, goal) {
  ps_int <- calcPS(Bs = bs$par, MRR = bs$resp, 
                   vars = vars, data = data,
                   exclude = exclude, getint = T)
  
  data$PS <- ps_int$PS
  
  pars <- data.frame(Var = c("Intercept", vars[!(vars %in% exclude)]), pars = c(ps_int$Intercept, bs$par))
  
  values <- merge(calcSMD(vars = vars, data = data, calcPS = F, goal = goal), pars, all = T)
  
  values$RR <- mean(bs$resp)
  values$trueRR <- mean(mean(ps_int$PS))
  values$distRR <- bs$dresp
  
  return(values)
  
}