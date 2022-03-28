library(tidyverse)

rm(list = ls())

source("0 - Functions - Simulation.R")

load("Data/Simulation Data/Sim Data.Rdata")


sim.data <- df.sim %>% 
  select(DSID, all_of(covariates))

B.index.covariates <- c(covariates[!(covariates %in% c("Urban", "Suburban", "ToRu"))], "urbanicity")

B.index.formula <- as.formula(paste("accepted ~", paste(B.index.covariates, collapse = " + ")))

B.index.data <- df.sim %>%
  select(DSID, all_of(B.index.covariates))

df.K.ranks <- df.clusters %>%
  nest(cluster.ranks = -c(K)) %>%
  rename(K.condition = K)

params <- list(
  iterations = 12,
  RR.condition = unique(df.PS$RR),
  K.condition = c("K_05"),
  SB.condition = unique(df.PS$scale_factor)
)


param_df <- df.PS %>%
  nest(PS.data = -c(scale_factor, RR)) %>%
  rename(SB.condition = scale_factor,
         RR.condition = RR) %>%
  right_join(cross_df(params)) %>%
  left_join(df.K.ranks) %>%
  mutate(B.index.formula = list(B.index.formula),
         B.index.data = list(B.index.data),
         sim.data = map(cluster.ranks, function(x) select(x, strata, DSID) %>% left_join(sim.data)))


test <- param_df %>% filter(SB.condition == 1 ,
                            RR.condition %in% c("RR_20", "RR_50"))

PS.data <- test$PS.data[[1]]
cluster.ranks <- test$cluster.ranks[[1]]
sim.data <- test$sim.data[[1]]

PS.data$PS <- 1
df.responses <- Generate_Responses(PS.data, cluster.ranks)

df.sampled <- Create_Samples(df.responses)
