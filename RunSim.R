library(tidyverse)

rm(list = ls())


#----------------
# SETUP
#----------------

source("SimSource.R")

load("Data/Simulation Data/Sim Data.Rdata")

minreps <- 1000
whichK <- c("K_05")
whichRR <- NULL




Run_Sim <- function(reps, RR.list = NULL, K.list = NULL) {
  library(tidyverse)
  
  source("SimSource.R")
  
  load("Data/Simulation Data/Sim Data.Rdata")
  
  frm <- as.formula(paste("accepted ~", paste(c(covariates[!(covariates %in% c("Urban", "Suburban", "ToRu"))], "urbanicity"), collapse = " + ")))
  
  if(is.null((RR.list))) RR.list <- unique(df.PS$RR)
  if(is.null((K.list))) K.list <- unique(df.clusters$K)
  
  
  
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


# Run_Sim(1, K.list = whichK)

#----------------
# RUN SIM
#----------------

library(parallel)

no_cores <- detectCores() - 1

reps <- rep((minreps + (no_cores - minreps %% no_cores)) / no_cores, each = no_cores)

# Initiate cluster
cl <- makeCluster(no_cores)

seed <- runif(1,0,1)*10^8
set.seed(75702217)

runtime <- system.time(results <- parSapply(cl, reps, Run_Sim, whichRR, whichK))

stopCluster(cl)

#----------------
# Export Results
#----------------

results <- apply(results, 1, bind_rows)


whichK <- paste(parse_number(unique(c(whichK[1], whichK[length(whichK)]))), collapse = " to ")


resultsFile <- paste("Data/Results/", sum(reps), " Reps - K", whichK, ".rdata", sep = "")

with(results, save(list = c(names(results), "runtime", "seed"), file = resultsFile))

write_file(x = resultsFile, "Data/Results/LastResults.txt")

load(resultsFile)
avgRun <- runtime/sum(reps)
avgRun * 1000 / 60 / 60 # Hours
avgRun * 1000 / 60      # Minutes

