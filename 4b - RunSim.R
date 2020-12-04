library(tidyverse)

rm(list = ls())


#----------------
# SETUP
#----------------

source("0 - Functions - Simulation.R")

load("Data/Simulation Data/Sim Data.Rdata")

minreps <- 1000
whichK <- c("K_05")
whichRR <- NULL
whichSB <- NULL
# whichRR <- c("RR_10")
# whichSB <- c(1)




Run_Sim <- function(reps, RR.list = NULL, K.list = NULL, SB.list = NULL) {
  library(tidyverse)
  
  source("0 - Functions - Simulation.R")
  
  load("Data/Simulation Data/Sim Data.Rdata")
  
  frm <- as.formula(paste("accepted ~", paste(c(covariates[!(covariates %in% c("Urban", "Suburban", "ToRu"))], "urbanicity"), collapse = " + ")))
  
  if(is.null((RR.list))) RR.list <- unique(df.PS$RR)
  if(is.null((K.list))) K.list <- unique(df.clusters$K)
  if(is.null((SB.list))) SB.list <- unique(df.PS$scale_factor)
  
  
  
  results <- replicate(reps,
                       Sim_Driver(sim.data = df.sim,
                                  PS.data = df.PS,
                                  cluster.data = df.clusters,
                                  B.index.formula = frm,
                                  list.covariates = covariates,
                                  RR.list = RR.list,
                                  K.list = K.list,
                                  SB.list = SB.list)) %>%
    apply(1, bind_rows)
  
  
  return(results)
}


# test <- Run_Sim(1, K.list = whichK, RR.list = c("RR_90"), SB.list = c(1, .25))
# test <- Run_Sim(1, K.list = whichK, RR.list = NULL, SB.list = NULL)
# test$df.samp.counts

#----------------
# RUN SIM
#----------------

library(parallel)

no_cores <- detectCores() - 2
# no_cores <- 3

reps <- rep((minreps + (no_cores - minreps %% no_cores)) / no_cores, each = no_cores)

# Initiate cluster
cl <- makeCluster(no_cores)

seed <- runif(1,0,1)*10^8
# seed <- 75702217
set.seed(seed)

runtime <- system.time(results <- parSapply(cl, reps, Run_Sim, whichRR, whichK, whichSB))

stopCluster(cl)

#----------------
# Export Results
#----------------

results <- apply(results, 1, bind_rows)


whichK <- paste(parse_number(unique(c(whichK[1], whichK[length(whichK)]))), collapse = " to ")

date <- paste(lubridate::year(Sys.time()), lubridate::month(Sys.time()), lubridate::day(Sys.time()), sep = "")

resultsFile <- paste("Data/Results/", date, " - ", sum(reps), " Reps - K", whichK, ".rdata", sep = "")

with(results, save(list = c(names(results), "runtime", "seed"), file = resultsFile))

write_file(x = resultsFile, "Data/Results/LastResults.txt")

load(resultsFile)
avgRun <- runtime/sum(reps)
avgRun * 1000 / 60 / 60 # Hours
avgRun * 1000 / 60      # Minutes

