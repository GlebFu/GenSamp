library(tidyverse)

rm(list = ls())


#----------------
# SETUP
#----------------

source("SimSource.R")

load("Data/Simulation Data/Sim Data.Rdata")

minreps <- 100

Run_Sim <- function(reps) {
  library(tidyverse)
  
  source("SimSource.R")
  
  load("Data/Simulation Data/Sim Data.Rdata")
  
  frm <- as.formula(paste("accepted ~", paste(c(covariates[!(covariates %in% c("Urban", "Suburban", "ToRu"))], "urbanicity"), collapse = " + ")))
  RR.list <- unique(df.PS$RR)
  K.list <- "K_06"
  
  results <- replicate(reps,
                       Sim_Driver(sim.data = df.sim,
                                  PS.data = df.PS,
                                  cluster.data = df.clusters,
                                  B.index.formula = frm,
                                  list.covariates = covariates,
                                  RR.list = RR.list,
                                  K.list = K.list)) %>%
    apply(1, bind_rows)
  
  
  return(results)
}


#----------------
# RUN SIM
#----------------

library(parallel)

no_cores <- detectCores() - 1

reps <- rep((minreps + (no_cores - minreps %% no_cores)) / no_cores, each = no_cores)

# Initiate cluster
cl <- makeCluster(no_cores)

seed <- runif(1,0,1)*10^8
set.seed(27770460)

runtime <- system.time(results <- parSapply(cl, reps, Run_Sim))

stopCluster(cl)

#----------------
# Export Results
#----------------

results <- apply(results, 1, bind_rows)

runtimeFile <- paste("Data/Results/Time - ", sum(reps), " Reps.rdata", sep = "")
resultsFile <- paste("Data/Results/Results - ", sum(reps), " Reps.rdata", sep = "")

with(results, save(list = names(results), file = resultsFile))
save(runtime, file = runtimeFile)

load(runtimeFile)
load(resultsFile)
avgRun <- runtime/sum(reps)
avgRun * 1000 / 60 / 60 # Hours
avgRun * 1000 / 60      # Minutes

