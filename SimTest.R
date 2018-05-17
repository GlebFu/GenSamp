library(tidyverse)

rm(list = ls())

# source("SimSource.R")
# 
# vars <- c("n", "urbanicity", "pELL", "pED", "pELA", "pMath", "pMin", "MEDINC")
# 
# frm <- as.formula(paste("Eij ~ ", paste(vars, collapse = " + ")))
# 
# df.Bindex <- df %>% ungroup() %>% select(DSID, vars)

reps <- 1000

runSim <- function(reps) {
  source("SimSource.R")
  
  vars <- c("n", "urbanicity", "pELL", "pED", "pELA", "pMath", "pMin", "MEDINC")
  
  frm <- as.formula(paste("Eij ~ ", paste(vars, collapse = " + ")))
  
  df.Bindex <- df %>% ungroup() %>% select(DSID, vars)
  
  replicate(reps, testRun(df.select,  pop.PS = df.Bindex, frm = frm, vars = vars))
  
}

# seed <- runif(1,0,1)*10^8
# set.seed(42987117)
# 
# # undebug(testRun)
# # undebug(getBindex)
#
#
# test <- testRun(df.select %>% filter(sch.RR %in% c(25)), pop.PS = df %>% ungroup() %>% select(DSID, vars))


runtimeFile <- "Data/2018-5-17/runtime r1000.rdata"
resultsFile <- "Data/2018-5-17/results r1000.rdata"

library(parallel)

no_cores <- detectCores() - 1

minreps <- 10
reps <- rep((minreps + (no_cores - minreps %% no_cores)) / no_cores, each = no_cores)

# Initiate cluster
cl <- makeCluster(no_cores)

seed <- runif(1,0,1)*10^8
set.seed(42987117)

runtime <- system.time(results <- parSapply(cl, reps, runSim))

stopCluster(cl)

save(runtime, file = runtimeFile)

ind1 <- 1 + 4 * 0:(reps[1]-1)
ind2 <- 2 + 4 * 0:(reps[1]-1)
ind3 <- 3 + 4 * 0:(reps[1]-1)
ind4 <- 4 + 4 * 0:(reps[1]-1)

df_responses <- bind_rows(results[ind1,]) %>% data.frame
df_dist_smd <- bind_rows(results[ind2,]) %>% data.frame
df_sch_smd <- bind_rows(results[ind3,]) %>% data.frame
df_Bindex <- bind_rows(results[ind4,]) %>% data.frame

save(df_responses, df_dist_smd, df_sch_smd, df_Bindex, file = resultsFile)

load(runtimeFile)
load(resultsFile)
avgRun <- runtime/sum(reps)
avgRun * 1000 / 60 / 60 # Hours
avgRun * 1000 / 60      # Minutes


