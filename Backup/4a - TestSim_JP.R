library(tidyverse)

rm(list = ls())

source("0 - Functions - Simulation.R")

load("Data/Simulation Data/Sim Data.Rdata")

# ggplot(df.clusters, aes(UCS_Rank, SCS_Rank, color = factor(strata))) + 
#   geom_point() + 
#   facet_wrap(~ K) + 
#   theme_minimal()


#---------------------------------
# SETUP
#---------------------------------

frm <- as.formula(paste("accepted ~", paste(c(covariates[!(covariates %in% c("Urban", "Suburban", "ToRu"))], "urbanicity"), collapse = " + ")))
RR.list <- unique(df.PS$RR)
SB.list <- unique(df.PS$scale_factor)
K.list <- "K_05"

sim.data <- df.sim
PS.data <- df.PS
cluster.data <- df.clusters
B.index.formula <- frm
list.covariates <- covariates
RR.list <- RR.list
K.list <- K.list
SB.list <- SB.list

K.condition <- "K_05"
RR.condition <- "RR_10"
SB.condition <- 1

#---------------------------------
# Generate Potential Responses
#---------------------------------

df.responses <- Generate_Responses(PS.data = PS.data, 
                                   cluster.data = cluster.data,
                                   K.condition = K.condition,
                                   RR.condition = RR.condition,
                                   SB.condition = SB.condition)

df.responses %>% 
  group_by(strata) %>%
  summarise(
    N = n(),
    PS = sum(PS),
    Ej = sum(Ej)
  ) %>%
  mutate(
    RR = Ej / N
  )

#---------------------------------
# Select Samples
#---------------------------------

df.sampled <- Create_Samples(df.responses)

sampled_by_stratum <- 
df.sampled %>%
  group_by(K, RR, scale_factor, sample_method, strata) %>%
  summarise(
    N = n(),
    Contacted = sum(contacted),
    Potential = sum(Ej),
    Accepted = sum(accepted),
    .groups = "drop_last"
  )

sampled_by_stratum

sampled_by_stratum %>%
  summarise(across(c(N, Contacted, Potential, Accepted), sum))

df.sampled %>%
  group_by(K, RR, scale_factor, sample_method, Ej, accepted) %>%
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
  select(sample_method, DSID, strata, PS, RR) 

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

#---------------------------------
# Return Data
#---------------------------------

tibble(
  df.recruitment.stats = list(df.recruitment.stats), 
  df.samp.counts = list(df.samp.counts), 
  df.B.indicies = list(df.B.indicies), 
  df.samp.stats = list(df.samp.stats)
)

#---------------------------------
# Run Full Iteration
#---------------------------------

K.condition <- "K_05"
RR.condition <- "RR_10"
SB.condition <- 1

# undebug(Run_Iteration_JP)

results <- Run_Iteration_JP(sim.data = sim.data, 
                            PS.data = PS.data, 
                            cluster.data = cluster.data, 
                            K.condition = K.condition, 
                            RR.condition = RR.condition, 
                            SB.condition = SB.condition, 
                            B.index.formula = B.index.formula, 
                            list.covariates = list.covariates)

#---------------------------------
# Run All Conditions
#`--------------------------------

results <- Sim_Driver_JP(sim.data, PS.data, cluster.data, B.index.formula, list.covariates, K.list, RR.list, SB.list)
