library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

rm(list = ls())

#-----------------
# Fixed data
#-----------------
#tets

# Base data set
load("Data/simData.Rdata")

# Response generating variables and goal SMD
load("Data/RGM Vars.Rdata")



#-----------------
# Functions
#-----------------

sampleBinomial <- function(ps) rbinom(length(ps), 1, prob = ps)

generateE <- function(data) {
  data %>%
    group_by(dist.RR, sch.RR, DID) %>%
    mutate(Ej = sampleBinomial(mean(dist.PS))) %>%
    group_by(dist.RR, sch.RR) %>%
    mutate(Eij = ifelse(Ej == 1, sampleBinomial(sch.PS), 0)) %>%
    return()
}

propAllocation <- function(cluster) {
  allocations <- round((table(df$cluster) / nrow(df)) * 60,0)
  return(allocations[cluster])
}

# Creates dataset with samples for each method and response rate
createSample <- function(data) {
  
  # Simple Random Sampling
  SRS_Sample <- data %>%
    group_by(dist.RR, sch.RR) %>%
    mutate(rank.SRS = sample(1:n())) %>%
    arrange(rank.SRS) %>%
    mutate(count = cumsum(Eij)) %>% 
    filter(count <= 60) %>%
    left_join(df, by = c("DSID", "DID", "SID", "cluster")) %>%
    mutate(sample = "SRS")
  
  #Convenience Sampling
  CS_Sample <- data %>%
    group_by(dist.RR, sch.RR) %>%
    arrange(-dist.PS, -sch.PS) %>%
    mutate(count = cumsum(Eij)) %>% 
    filter(count <= 60) %>%
    left_join(df, by = c("DSID", "DID", "SID", "cluster")) %>%
    mutate(sample = "CS")
  
  #Cluster Analyusis Stratified Sampling
  CASS_Sample <- data %>%
    group_by(dist.RR, sch.RR, cluster) %>%
    arrange(rankC) %>%
    mutate(count = cumsum(Eij)) %>%
    filter(count <= propAllocation(cluster)) %>%
    left_join(df, by = c("DSID", "DID", "SID", "cluster")) %>%
    mutate(sample = "CASS")
  
  return(rbind(SRS_Sample, CS_Sample, CASS_Sample))
}

# Calculates response rates and other recruiting statistics
calcResponseRates <- function(data, cluster = F) {
  if(!cluster){
    data <- data %>%
      group_by(sample, dist.RR, sch.RR, Ej)
  } else {
    data <- data %>%
      filter(sample == "CASS") %>%
      group_by(dist.RR, sch.RR, Ej, cluster)

  }
  data %>%
    summarise(sch_contacted = n(),
            sch_accepted = sum(Eij),
            dist_contacted = length(unique(DID))) %>%
    mutate(sch_rejected = sch_contacted * Ej - sch_accepted) %>%
    mutate(dist_rejected = sum(dist_contacted) - dist_contacted,
           sch_RR = sch_accepted/sch_contacted,
           dist_accepted = dist_contacted,
           dist_contacted = sum(dist_contacted),
           dist_RR = dist_accepted/dist_contacted) %>%
    filter(Ej == 1) %>%
    select(-Ej) %>%
    return()
}



# Calculates weighted standard deviation
weighted.sd <- function(x, w) sqrt(sum((w * (x - weighted.mean(x, w)))^2) / (sum(w) - 1))
# weighted.sd <- function(x, w) sd(x[w == 1])

# calculates standardized mean diferences between sample and population
calcDistSMDs <- function(sample) {
  sample[, c("dist.RR", "sch.RR", "sample","DID", "Ej", "Eij", names(distGoal))] %>%
    gather(key = "Variable", value = "Value", names(distGoal)) %>%
    group_by(sample, dist.RR, sch.RR, Variable, DID) %>%
    summarise(sampled = mean(Ej),
              contributed = as.numeric(sum(Eij) > 0),
              rejected = 1 - sampled,
              dist_mean = mean(Value)) %>%
    gather(key = "Group", value = "Weight", sampled:rejected) %>%
    group_by(sample, dist.RR, sch.RR, Variable, Group) %>%
    summarise(sample_mean = weighted.mean(dist_mean, Weight),
              sample_sd = weighted.sd(dist_mean, Weight)) %>% 
    left_join(dist_stats) %>%
    mutate(sim_SMD = (sample_mean - pop_mean) / pop_sd,
           miss = sim_SMD - goal_SMD)
}

calcSchSMDs <- function(sample) {
  sample[, c("dist.RR", "sch.RR", "sample","DID", "Ej", "Eij", names(schGoal))] %>%
    gather(key = "Variable", value = "Value", names(schGoal)) %>%
    mutate(sampled = Eij,
              rejected = Ej - Eij) %>%
    gather(key = "Group", value = "Weight", sampled:rejected) %>%
    group_by(sample, dist.RR, sch.RR, Variable, Group) %>%
    summarise(sample_mean = weighted.mean(Value, Weight),
              sample_sd = weighted.sd(Value, Weight)) %>% 
    left_join(sch_stats) %>%
    mutate(sim_SMD = (sample_mean - pop_mean) / pop_sd,
           miss = sim_SMD - goal_SMD)
}

#-----------------
# Test Sim Stages
#-----------------

# set.seed(1010)
# test_approached <- generateE(df.select)
# test_sample <- createSample(test_approached)
# test_responses <- calcResponseRates(test_sample) 
# test_dist_smds <- calcDistSMDs(test_sample)
# test_dist_smds <- test_dist_smds %>% select(sample:Group, goal_SMD:miss)
# test_sch_smds <- calcSchSMDs(test_sample)
# test_sch_smds <- test_sch_smds %>% select(sample:Group, goal_SMD:miss)
# 
# 
# # Visualize Sampling
# 
# test_responses %>%
#   filter(sch.RR == 30) %>%
#   gather(key = measure, value = value, -sample, -sch.RR, -dist.RR) %>%
#   mutate(level = str_split(measure, "_", simplify = T)[,1],
#          measure =  str_split(measure, "_", simplify = T)[,2]) %>%
#   ggplot(aes(x = dist.RR, y = value, group = sample, color = sample)) +
#   geom_point() +
#   geom_line() +
#   facet_grid(measure ~ level, scales = "free_y") +
#   theme_bw()
# 
# # Visualize District SMDs
# test_dist_smds %>%
#   filter(sch.RR == 30) %>%
#   ggplot(aes(x = dist.RR, y = abs(sim_SMD), group = sample, color = sample)) +
#   geom_point() +
#   geom_line() +
#   geom_hline(yintercept = 0) +
#   geom_hline(yintercept = c(-.1, .1), linetype = "dashed") +
#   facet_grid(Group ~ Variable) +
#   theme_bw()
# 
# # Visualize School SMDs
# test_sch_smds %>%
#   filter(sch.RR == 30) %>%
#   ggplot(aes(x = dist.RR, y = abs(sim_SMD), group = sample, color = sample)) +
#   geom_point() +
#   geom_line() +
#   geom_hline(yintercept = 0) +
#   geom_hline(yintercept = c(-.1, .1), linetype = "dashed") +
#   facet_grid(Group ~ Variable) +
#   theme_bw()
# 
# 
# # Compare to goal SMDs
# test_dist_smds %>%
#   filter(sch.RR == 30) %>%
#   ggplot(aes(x = dist.RR, y = miss, group = sample, color = sample)) +
#   geom_point() +
#   geom_line() +
#   geom_hline(yintercept = 0) +
#   geom_hline(yintercept = c(-.1, .1), linetype = "dashed") +
#   facet_grid(Group ~ Variable) +
#   theme_bw()

testRun <- function(data) {
  approached <- generateE(data)
  sample <- createSample(approached)
  
  responses <- calcResponseRates(sample) %>%
    gather(key = variable, value = value, -sample, -dist.RR, -sch.RR)
  
  dist_smds <- calcDistSMDs(sample) %>% 
    select(sample:Group, goal_SMD:miss) %>%
    gather(key = variable, value = value, -sample, -dist.RR, -sch.RR, -Variable, -Group)
  
  sch_smds <- calcSchSMDs(sample) %>% 
    select(sample:Group, goal_SMD:miss) %>% 
    gather(key = variable, value = value, -sample, -dist.RR, -sch.RR, -Variable, -Group)
  

  return(list(responses, dist_smds, sch_smds))
}




set.seed(0115)

runtime <- system.time(results <- replicate(1000, testRun(df.select %>% filter(sch.RR %in% c(30, 50, 70)))))
save(runtime, file = "Data/Sim Test Runtime3.rdata")
load("Data/Sim Test Runtime3.rdata")
avgRun <- runtime/100
avgRun * 10000 / 60 / 60 # Hours
avgRun * 10000 / 60      # Minutes

save(results, file = "Data/results2.rdata")

load("Data/results2.rdata")


df_responses <- bind_rows(results[1,]) %>% data.frame
df_dist_smd <- bind_rows(results[2,]) %>% data.frame
df_sch_smd <- bind_rows(results[3,]) %>% data.frame

# Visualize Sampling
df_responses %>%
  group_by(sample, dist.RR, sch.RR, variable) %>%
  summarise(value = mean(value)) %>%
  spread(key = variable, value = value) %>%
  # filter(sch.RR == 50) %>%
  gather(key = measure, value = value, -sample, -sch.RR, -dist.RR) %>%
  mutate(level = str_split(measure, "_", simplify = T)[,1],
         measure =  str_split(measure, "_", simplify = T)[,2]) %>%
  ggplot(aes(x = dist.RR, y = value, group = sample, color = sample)) +
  geom_point() +
  geom_line() +
  facet_grid(measure + level ~ sch.RR, scales = "free_y") +
  theme_bw() +
  expand_limits(y=0)


# Visualize District SMDs
df_dist_smd %>%
  group_by(sample, dist.RR, sch.RR, Variable, Group, variable) %>%
  summarise(value = mean(value, na.rm = T)) %>% # NAs due to 0 rejections by CS
  spread(key = variable, value = value) %>%
  filter(sch.RR == 30) %>%
  ggplot(aes(x = dist.RR, y = abs(sim_SMD), group = sample, color = sample)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-.1, .1), linetype = "dashed") +
  facet_grid(Group ~ Variable) +
  theme_bw()

# Visualize School SMDs
df_sch_smd %>%
  group_by(sample, dist.RR, sch.RR, Variable, Group, variable) %>%
  summarise(value = mean(value, na.rm = F)) %>% 
  spread(key = variable, value = value) %>%
  filter(sch.RR == 30) %>%
  ggplot(aes(x = dist.RR, y = abs(sim_SMD), group = sample, color = sample)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-.1, .1), linetype = "dashed") +
  facet_grid(Group ~ Variable) +
  theme_bw()


# Compare to goal SMDs
df_dist_smd %>%
  group_by(sample, dist.RR, sch.RR, Variable, Group, variable) %>%
  summarise(value = mean(value, na.rm = T)) %>% # NAs due to 0 rejections by CS
  spread(key = variable, value = value) %>%
  filter(sch.RR == 30) %>%
  ggplot(aes(x = dist.RR, y = miss, group = sample, color = sample)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-.1, .1), linetype = "dashed") +
  facet_grid(Group ~ Variable) +
  theme_bw()

df_sch_smd %>%
  group_by(sample, dist.RR, sch.RR, Variable, Group, variable) %>%
  summarise(value = mean(value, na.rm = T)) %>% # NAs due to 0 rejections by CS
  spread(key = variable, value = value) %>%
  filter(sch.RR == 30) %>%
  ggplot(aes(x = dist.RR, y = miss, group = sample, color = sample)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-.1, .1), linetype = "dashed") +
  facet_grid(Group ~ Variable) +
  theme_bw()
