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
  bind_rows(mutate(., strata = 0)) %>%
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

df.sampled$sample_method %>% unique()

sampled_by_stratum <- 
df.sampled %>%
  group_by(K, RR, scale_factor, sample_method, strata) %>%
  summarise(
    N = n(),
    Contacted = sum(contacted),
    Potential = sum(Ej),
    Accepted = sum(accepted),
    .groups = "drop_last" )

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

df.recruitment.stats <- Calc_Recruitment_Stats(df.sampled, include_strata = T) 

#---------------------------------
# Track Sampled Schools
#---------------------------------

df.samp.counts <- df.sampled %>%
  filter(accepted) %>%
  select(sample_method, DSID) 

#---------------------------------
# Calculate Sample Statistics
#---------------------------------

df.samp.stats <- df.sampled %>%
  filter(accepted) %>%
  left_join(sim.data) %>%
  Calc_Sample_Statistics(list.covariates)


# full_join(df.samp.stats, df.pop.stats) %>%
#   mutate(smd = (samp.mean - pop.mean) / pop.sd) %>%
#   select(sample_method, strata, var, smd) %>%
#   ggplot(aes(x = strata, y = smd, color = sample_method)) +
#   geom_point() +
#   facet_wrap(~var)

#---------------------------------
# Calculate B-Index
#---------------------------------

df.B.indicies <- df.sampled %>%
  full_join(sim.data) %>%
  mutate(accepted = ifelse(accepted, 1, 0)) %>%
  nest(data = -sample_method) %>%
  mutate(PS_sample = map(data, function(x) glm(data = x, formula = B.index.formula, family = quasibinomial()) %>% fitted())) %>%
  unnest(cols = c(data, PS_sample)) %>%
  select(sample_method, PS_sample, accepted) %>%
  group_by(sample_method) %>%
  summarise(Bs = Calc_Bindex(PS_sample, accepted))

#---------------------------------
# Run Full Iteration for One Condition
#---------------------------------

K.condition <- "K_05"
RR.condition <- "RR_10"
SB.condition <- 1

results <- Run_Iteration(sim.data, PS.data, cluster.data, K.condition, RR.condition, SB.condition, B.index.formula, list.covariates)

#undebug(Run_Iteration)

#---------------------------------
# Run All Conditions Once
#`--------------------------------

Run_Iteration <- function(sim.data, PS.data, cluster.data, K.condition, RR.condition, SB.condition, B.index.formula, list.covariates) {
  
  df.responses <- Generate_Responses(PS.data = PS.data, 
                                     cluster.data = cluster.data,
                                     K.condition = K.condition,
                                     RR.condition = RR.condition,
                                     SB.condition = SB.condition)
  
  df.sampled <- Create_Samples(df.responses)
  
  df.recruitment.stats <- Calc_Recruitment_Stats(df.sampled) 
  
  df.samp.counts <- df.sampled %>%
    filter(accepted) %>%
    select(sample_method, DSID, strata, PS, RR) 
  
  df.samp.stats <- df.sampled %>%
    filter(accepted) %>%
    left_join(sim.data) %>%
    Calc_Sample_Statistics(list.covariates)
  
  
  
  # df.samp.stats <- df.samp.counts %>%
  #   left_join(df.sampled) %>%
  #   Calc_Sample_Statistics(list.covariates)
  
  df.B.indicies <- df.sampled %>%
    full_join(sim.data) %>%
    mutate(accepted = ifelse(accepted, 1, 0)) %>%
    nest(data = -sample_method) %>%
    mutate(PS_sample = map(data, function(x) glm(data = x, formula = B.index.formula, family = quasibinomial()) %>% fitted())) %>%
    unnest(cols = c(data, PS_sample)) %>%
    select(sample_method, PS_sample, accepted) %>%
    group_by(sample_method) %>%
    summarise(Bs = Calc_Bindex(PS_sample, accepted))
  
  results <- list(df.recruitment.stats = df.recruitment.stats, 
                  df.samp.counts = df.samp.counts, 
                  df.B.indicies = df.B.indicies, 
                  df.samp.stats = df.samp.stats) %>%
    lapply(function(x) mutate(x, K = K.condition, RR = RR.condition, SB = SB.condition))
  
  return(results)
}


PS.data %>%
  nest(PS.data = c(DSID, PS)) %>%
  filter(scale_factor == 1,
         RR == "RR_50") %>%
  mutate(results = pmap(.l = list(RR.condition = RR,
                                  SB.condition = scale_factor),
                        .f = Run_Iteration,
                        sim.data = sim.data,
                        PS.data = PS.data,
                        cluster.data = cluster.data,
                        K.condition = "K_05",
                        B.index.formula = B.index.formula,
                        list.covariates = list.covariates))


#undebug(Run_Iteration)




#---------------------------------
# Test Full Sim
#`--------------------------------

# results <- Sim_Driver(sim.data, PS.data, cluster.data, B.index.formula, list.covariates, K.list, RR.list, SB.list)

results <- Sim_Driver(sim.data, PS.data, cluster.data, B.index.formula, list.covariates, K.list = "K_05", RR.list, SB.list = 1)



prop_allocations %>%
  filter(K == "K_05") %>%
  select(strata, n) %>%
  right_join(results$df.recruitment.stats) %>%
  mutate(proportion = value / n) %>%
  filter(measure == "sch.contacted") %>%
  ggplot(aes(x = RR, y = proportion, color = sample_method, group = sample_method)) +
  geom_line() +
  theme_minimal() +
  facet_wrap(~strata)



df.clusters %>%
  filter(K == "K_05") %>%
  left_join(df.PS) %>%
  filter(RR %in% c("RR_10", "RR_20", "RR_30", "RR_40")) %>%
  ggplot(aes(x = PS, fill = factor(strata))) +
  geom_histogram() +
  facet_wrap(~RR)
