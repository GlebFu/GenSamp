library(tidyverse)

rm(list = ls())


#----------------
# SETUP
#----------------

source("0 - Functions - Simulation.R")

load("Data/Simulation Data/Sim Data.Rdata")

frm <- as.formula(paste("accepted ~", paste(c(covariates[!(covariates %in% c("Urban", "Suburban", "ToRu"))], "urbanicity"), collapse = " + ")))

params <- list(
  iterations = 12,
  RR.condition = unique(df.PS$RR),
  K.condition = c("K_05"),
  SB.condition = unique(df.PS$scale_factor)
)

param_df <- 
  cross_df(params)

seed <- 20201208

#----------------
# RUN SIM
#----------------
library(future)
library(furrr)
plan(multicore)

runtime <- system.time(
  results <- 
    param_df %>%
    mutate(
      res = future_pmap(., .f = Sim_Driver_JP, 
                        sim.data = df.sim, 
                        PS.data = df.PS, 
                        cluster.data = df.clusters, 
                        B.index.formula = frm, 
                        list.covariates = covariates,
                        .options = furrr_options(seed = NULL))
    ) %>%
    unnest(res)
)
  



#----------------
# Export Results
#----------------

date <- paste(lubridate::year(Sys.time()), lubridate::month(Sys.time()), lubridate::day(Sys.time()), sep = "")

resultsFile <- paste("Data/Results/GenSamp - ", date, " - ", params$iterations, ".rdata", sep = "")

save(params, results, runtime, seed, file = resultsFile)

avgRun <- runtime/params$iterations
avgRun * 1000 / 60 / 60 # Hours
avgRun * 1000 / 60      # Minutes

