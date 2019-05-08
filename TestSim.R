library(tidyverse)

rm(list = ls())




source("SimSource.R")

load("Data/Simulation Data/Sim Data.Rdata")


frm <- as.formula(paste("accepted ~", paste(c(covariates[!(covariates %in% c("Urban", "Suburban", "ToRu"))], "urbanicity"), collapse = " + ")))
RR.list <- unique(df.PS$RR)
K.list <- "K_06"



  

debug(Run_Iteration)

results <- Sim_Driver(sim.data = df.sim,
                      PS.data = df.PS,
                      cluster.data = df.clusters,
                      B.index.formula = frm,
                      list.covariates = covariates,
                      RR.list = RR.list,
                      K.list = K.list) %>%
  apply(1, bind_rows)
