library(dplyr)
library(tidyr)
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

###################
# DATA PREP
###################

# Read Data
df <- read.csv("final data.csv", stringsAsFactors = F)


# Create ELL and ED variable district data when school is unavailable
# Create Minority variable
# Create Town/Rural Variable
df <- mutate(df,
             pELL = ifelse(is.na(pELL), pELL_D, pELL), 
             pED = ifelse(is.na(pED), pTotfrl, pED),
             pMin = 1-ethWhite,
             ToRu = Town + Rural)


vars <- c("LSTATE", "LEANM", "SCHNAM", "n", "ULOCAL", "pTotfrl", "pIEP_D", "pELL_D", 
          "Urban", "Suburban", "ToRu", "schPrimary", "schMIDDLE", "schHigh", 
          "schOther", "pELL", "pED", "pELA", "pMath", "pMin", "MEDINC")

df <- df[,vars] %>% na.omit

# Create district level data
df.dists <- select(df, -SCHNAM) %>%
  group_by(LSTATE, LEANM) %>%
  summarise_if(is.numeric, mean, na.rm = T) %>%
  ungroup() %>%
  mutate(MEDINC = STAND(MEDINC))


# Set District SMD Goals
dGoal <- c(.43, -.6, .22, .95, .67, .56, -.66)
dVars <- c("Urban", "Suburban", "ToRu", "pELL", "pED", "pMin", "MEDINC")
names(dGoal) <- dVars

# Initial District Betas
dBs <- c(0, 0, 0, 0, 0, 0)

# Set urban as focus
dEx <- "Urban"


###################
# DESCRIPTIVES
###################

df %>%
  group_by(LSTATE) %>%
  summarise(nDists = length(unique(LEANM)), nSch = length(SCHNAM), Students = sum(n, na.rm = T)) %>%
  xtable(summary = F)

###################
# SIM FUNCTIONS
###################

calcPS <- function(Bs, MRR, vars, data, exclude, getint = F) {
  vars <- vars[!(vars %in% exclude)]
  X <- as.matrix(data[,vars])
  
  XB <- as.numeric(X %*% Bs)
  
  int = uniroot(function(b0) mean(expit(b0 + XB)) - MRR, c(-10,10))
  
  dY <- as.numeric(X %*% Bs) + int$root
  
  PS <- expit(dY)
  
  if (getint) return(list(PS = PS, Intercept = int$root))
  
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
  data$dIPSW1 <- 1/(1-data$PS)
  data$dIPSW0 <- 1/data$PS
  
  
  data[,c(dVars, "dIPSW1", "dIPSW0")] %>%
    gather(key = wType, value = weight, dIPSW0:dIPSW1) %>%
    gather(key = Var, value = val, -weight, -wType) %>%
    group_by(wType, Var) %>%
    summarise(wM = weighted.mean(val, weight)) %>%
    spread(key = wType, value = wM) -> dMeans
  
  data[,vars] %>%
    gather(key = Var, value = Val) %>%
    group_by(Var) %>%
    summarise(m = mean(Val), sd = sd(Val)) %>%
    merge(dMeans) %>%
    mutate(smdS0 = (dIPSW0 - m)/sd, smdS1 = (dIPSW1 - m)/sd)  -> dMeans
  
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
  
  trial <- calcSMD(Bs = Bs, MRR = MRR, vars = vars, 
                   data = data, exclude = exclude, goal = goal)
  
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


# Create district level data
df.dists.gmc <- select(df, -SCHNAM) %>%
  group_by(LSTATE, LEANM) %>%
  summarise_if(is.numeric, mean, na.rm = T) %>%
  ungroup() %>%
  mutate_if(is.numeric, GMC) %>%
  mutate(MEDINC = STAND(MEDINC))


# Set District SMD Goals
dGoal <- c(.43, -.6, .22, .95, .67, .56, -.66)
dVars <- c("Urban", "Suburban", "ToRu", "pELL", "pED", "pMin", "MEDINC")
names(dGoal) <- dVars

# Initial District Betas
dBs <- c(0, 0, 0, 0, 0, 0)

# Set urban as focus
dEx <- "Urban"



# load("171019.rdata")

######################
# .3 Response Rate
######################
resp30 <- .3

# bs_RR30 <- optim(par = dBs, fn = testGoal,
#                  MRR = resp30, vars = dVars,
#                  data = df.dists.gmc, exclude = dEx, goal = dGoal)


calcPS(Bs = bs_RR30$par,
       MRR = resp30,
       vars = dVars,
       data = df.dists.gmc,
       exclude = dEx,
       getint = T) %>% 
  lapply(summary)

calcSMD(Bs = bs_RR30$par,
        MRR = resp30,
        vars = dVars,
        data = df.dists.gmc,
        exclude = dEx,
        goal = dGoal) 

######################
# .2 Response Rate
######################
resp20 <- .2

# bs_RR20 <- optim(par = dBs, fn = testGoal,
#                  MRR = resp20, vars = dVars,
#                  data = df.dists.gmc, exclude = dEx, goal = dGoal)


calcPS(Bs = bs_RR20$par,
       MRR = resp20,
       vars = dVars,
       data = df.dists.gmc,
       exclude = dEx,
       getint = T) %>% 
  lapply(summary)

calcSMD(Bs = bs_RR20$par,
        MRR = resp20,
        vars = dVars,
        data = df.dists.gmc,
        exclude = dEx,
        goal = dGoal) 

######################
# .1 Response Rate
######################
resp10 <- .1

# bs_RR10 <- optim(par = dBs, fn = testGoal,
#                  MRR = resp10, vars = dVars,
#                  data = df.dists.gmc, exclude = dEx, goal = dGoal)


calcPS(Bs = bs_RR10$par,
       MRR = resp10,
       vars = dVars,
       data = df.dists.gmc,
       exclude = dEx,
       getint = T) %>% 
  lapply(summary)


calcSMD(Bs = bs_RR10$par,
        MRR = resp10,
        vars = dVars,
        data = df.dists.gmc,
        exclude = dEx,
        goal = dGoal) 


params <- data_frame(
  coef = dVars[!(dVars %in% dEx)],
  RR10 = bs_RR10$par, 
  RR20 = bs_RR20$par, 
  RR30 = bs_RR30$par
)

save.image("171019.rdata")




