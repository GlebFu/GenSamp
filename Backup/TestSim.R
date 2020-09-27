library(tidyverse)

rm(list = ls())

source("SimSource.R")

load("Data/Simulation Data/Sim Data.Rdata")

#---------------------------------
# SETUP
#---------------------------------

frm <- as.formula(paste("accepted ~", paste(c(covariates[!(covariates %in% c("Urban", "Suburban", "ToRu"))], "urbanicity"), collapse = " + ")))
RR.list <- unique(df.PS$RR)
K.list <- "K_05"

sim.data <- df.sim
PS.data <- df.PS
cluster.data <- df.clusters
B.index.formula <- frm
list.covariates <- covariates
RR.list <- RR.list
K.list <- K.list

K.condition <- "K_05"
RR.condition <- "RR_10"


#---------------------------------
# Generate Potential Responses
#---------------------------------

df.responses <- Generate_Responses(PS.data = PS.data, 
                                   cluster.data = cluster.data,
                                   K.condition = K.condition,
                                   RR.condition = RR.condition)

#---------------------------------
# Select Samples
#---------------------------------

df.sampled <- Create_Samples(df.responses)


df.sampled %>%
  group_by(K, RR, sample_method, Ej, accepted) %>%
  summarise(n = n()) %>%
  spread(sample_method, n)


#---------------------------------
# Calculate Recruitment Statistics
#---------------------------------

df.recruitment.stats <- Calc_Recruitment_Stats(df.sampled) 

#---------------------------------
# Track Sampled Schools
#---------------------------------

df.samp.counts <- df.sampled %>%
  filter(accepted) %>%
  select(sample_method, DSID) 

#---------------------------------
# Calculate Sample Statistics
#---------------------------------

df.sampled <- sim.data %>% 
  full_join(df.sampled)

df.samp.stats <- Calc_Sample_Statistics(df.sampled, list.covariates)

full_join(df.samp.stats, df.pop.stats) %>%
  mutate(smd = (samp.mean - pop.mean) / pop.sd) %>%
  arrange(var) %>%
  as.data.frame

#---------------------------------
# Calculate B-Index
#---------------------------------

df.B.indicies <- df.sampled %>%
  mutate(accepted = ifelse(accepted, 1, 0)) %>%
  group_by(sample_method) %>%
  nest() %>%
  mutate(PS_sample = map(data, glm, formula = B.index.formula, family = quasibinomial()),
         PS_sample = map(PS_sample, fitted)) %>%
  unnest(cols = c(data, PS_sample)) %>%
  select(sample_method, PS_sample, accepted) %>%
  group_by(sample_method) %>%
  summarise(Bs = Calc_Bindex(PS_sample, accepted))

