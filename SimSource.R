library(tidyverse)

# rm(list = ls())

#-----------------
# Fixed data
#-----------------
#tets

# Base data set
load(paste("Data/", file_date, "/simData.Rdata", sep = ""))

# Response generating variables and goal SMD
load(paste("Data/", file_date, "/RGM Vars.Rdata", sep = ""))



#-----------------
# Functions
#-----------------

sampleBinomial <- function(ps) rbinom(length(ps), 1, prob = ps)


Bindex <- function(PS, Eij) {
  
  dat1B <- PS[Eij == 1]
  dat2B <- PS[Eij == 0]
  ##Baklizi and Eidous (2006) estimator
  # bandwidth
  h = function(x){
    n = length(x)
    return((4*sqrt(var(x))^5/(3*n))^(1/5)) 
  }
  
  # kernel estimators of the density and the distribution
  kg = function(x,data){
    hb = h(data) #bin width
    k = r = length(x)
    for(i in 1:k) r[i] = mean(dnorm((x[i]-data)/hb))/hb
    return(r )
  } 
  
  return( as.numeric(integrate(function(x) sqrt(kg(x,dat1B)*kg(x,dat2B)),-Inf,Inf)$value))
  
}

# getBindex <- function(sample, pop.PS) {
#   
#   forBindex <- sample %>%
#     ungroup() %>%
#     filter(Eij == 1) %>%
#     select(DSID, sample, dist.RR, sch.RR) %>%
#     left_join(pop.PS)
#   
#   Bindicies <- data.frame(B = NA, sample = NA, dist.RR = NA, sch.RR = NA)
#   
#   for(i in unique(sample$sample)) {
#     for(j in unique(sample$dist.RR)) {
#       for(k in unique(sample$sch.RR)) {
#         d1 <- filter(forBindex, sample == i, dist.RR == j, sch.RR == k)
#         d2 <- filter(pop.PS, dist.RR == j, sch.RR == k)
#         
#         B <- Bindex(d1$sch.PS, d2$sch.PS)
#         
#         Bindicies <- rbind(Bindicies, data.frame(B = B, sample = i, dist.RR = j, sch.RR = k))
#       }
#     }
#   }
#   
#   return(na.omit(Bindicies))
# }
  

generateE <- function(data) {
  data %>%
    group_by(sch.RR) %>%
    mutate(Eij = sampleBinomial(sch.PS)) %>%
    return()
}


propAllocation <- function(cluster, whichCass) {
  allocations <- round((table(df[,whichCass]) / nrow(df)) * 60,0)
  return(allocations[cluster])
}

# Creates dataset with samples for each method and response rate
createSample <- function(data) {
  
  # Simple Random Sampling
  SRS_Sample <- data %>%
    group_by(sch.RR) %>%
    mutate(rank.SRS = sample(1:n())) %>%
    arrange(rank.SRS) %>%
    mutate(count = cumsum(Eij)) %>% 
    filter(count <= 60) %>%
    left_join(df) %>%
    mutate(sample = "SRS",
           cluster = NA)
  
  #Convenience Sampling
  CS_Sample <- data %>%
    group_by(sch.RR) %>%
    sample_frac(size = 1, weight = sch.PS) %>%
    mutate(count = cumsum(Eij)) %>% 
    filter(count <= 60) %>%
    left_join(df) %>%
    mutate(sample = "CS",
           cluster = NA)
  
  #OV Cluster Analyusis Stratified Sampling
  # SUBS_OV_Sample <- data %>%
  #   group_by(dist.RR, sch.RR, cluster_ov) %>%
  #   arrange(rank_ov) %>%
  #   mutate(count = cumsum(Eij)) %>%
  #   filter(count <= propAllocation(cluster_ov, "cluster_ov")) %>%
  #   left_join(df) %>%
  #   mutate(sample = "SUBS_OV",
  #          cluster = cluster_ov)
  
  #Full Cluster Analyusis Stratified Sampling
  SUBS_F_Sample <- data %>%
    group_by(sch.RR, cluster_full) %>%
    arrange(rank_full) %>%
    mutate(count = cumsum(Eij)) %>%
    filter(count <= propAllocation(cluster_full, "cluster_full")) %>%
    left_join(df) %>%
    mutate(sample = "SUBS_F",
           cluster = cluster_full)
  
  #Full Cluster Analyusis Stratified Sampling
  SUBS_SRS_Sample <- data %>%
    group_by(sch.RR, cluster_full) %>%
    mutate(rank.SRS = sample(1:n())) %>%
    arrange(rank.SRS) %>%
    mutate(count = cumsum(Eij)) %>% 
    filter(count <= propAllocation(cluster_full, "cluster_full")) %>%
    left_join(df) %>%
    mutate(sample = "SUBS_SRS",
           cluster = cluster_full)
  
  #Full Cluster Analyusis Stratified Sampling
  SUBS_CS_Sample <- data %>%
    group_by(sch.RR, cluster_full) %>%
    sample_frac(size = 1, weight = sch.PS) %>%
    mutate(count = cumsum(Eij)) %>% 
    filter(count <= propAllocation(cluster_full, "cluster_full")) %>%
    left_join(df) %>%
    mutate(sample = "SUBS_CS",
           cluster = cluster_full)
  
  return(rbind(SRS_Sample, CS_Sample, SUBS_F_Sample, SUBS_SRS_Sample, SUBS_CS_Sample))
}

# Calculates response rates and other recruiting statistics
calcResponseRates <- function(data, cluster = F) {
  if(!cluster){
    data <- data %>%
      mutate(cluster = NA) %>%
      group_by(sample, sch.RR, cluster)
  } else {
    data <- data %>%
      filter(sample %in% c("SUBS_F", "SUBS_OV")) %>%
      group_by(sample, sch.RR, cluster)

  }
  
  data %>%
    summarise(sch_contacted = n(),
            sch_accepted = sum(Eij)) %>%
    mutate(sch_rejected = sch_contacted - sch_accepted) %>%
    mutate(sch_RR = sch_accepted/sch_contacted) %>%
    group_by(sample, sch.RR, cluster) %>%
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
  sample[, c("sch.RR", "sample","DID", "Eij", names(schGoal))] %>%
    gather(key = "Variable", value = "Value", names(schGoal)) %>%
    mutate(sampled = Eij,
           rejected = 1 - Eij) %>%
    gather(key = "Group", value = "Weight", sampled:rejected) %>%
    group_by(sample, sch.RR, Variable, Group) %>%
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
# df.select <- df.select %>%
#   gather(key = sch.RR, value = sch.PS, PS25:PS75)
# 
# test_approached <- generateE(df.select)
# test_sample <- createSample(test_approached) # undebug(createSample)
# test_responses <- calcResponseRates(test_sample)
# # test_dist_smds <- calcDistSMDs(test_sample)
# # test_dist_smds <- test_dist_smds %>% select(sample:Group, goal_SMD:miss)
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

testRun <- function(data, pop.PS, frm, vars) {
  
  approached <- generateE(data)
  samp <- createSample(approached)
  
  responses <- calcResponseRates(samp) %>%
    gather(key = variable, value = value, -sample, -sch.RR, -cluster)
  
  responses <- calcResponseRates(samp, cluster = T) %>%
    gather(key = variable, value = value, -sample, -sch.RR, -cluster) %>%
    rbind(responses)
  
  sch_smds <- calcSchSMDs(samp) %>% 
    select(sample:Group, goal_SMD:miss) %>% 
    gather(key = variable, value = value, -sample, -sch.RR, -Variable, -Group)

  Bindicies <- samp %>%
    select(sample, DSID, Eij, vars, sch.RR) %>%
    group_by(sample, sch.RR) %>%
    nest() %>%
    mutate(data = map(data, full_join, pop.PS)) %>%
    unnest() %>%
    mutate(Eij = ifelse(is.na(Eij), 0, Eij)) %>% 
    group_by(sample, sch.RR) %>%
    nest() %>% 
    mutate(PS_sample = map(data, glm, formula = frm, family = quasibinomial()),
           PS_sample = map(PS_sample, fitted)) %>%
    unnest() %>%
    group_by(sample, sch.RR) %>%
    summarise(Bs = Bindex(PS_sample, Eij))
  
  samp_counts <- samp %>%
    select(sample, DSID, Eij) %>%
    filter(Eij == 1)
  
  # test <- samp %>%
  #   select(sample, DSID, Eij, vars, sch.RR) %>%
  #   group_by(sample, sch.RR) %>%
  #   nest() %>%
  #   mutate(data = map(data, full_join, pop.PS)) %>%
  #   unnest() %>%
  #   mutate(Eij = ifelse(is.na(Eij), 0, Eij)) %>% 
  #   group_by(sample, sch.RR) %>%
  #   filter(sample == "CS")
  # 
  # summary(test)
    # 
    # glm(formula = frm, family = quasibinomial(), data = test) %>%
    #   fitted
  
  return(list(responses = responses, sch_smds = sch_smds, Bindicies = Bindicies, samp_counts))
  # return(list(responses = responses, dist_smds = dist_smds, sch_smds = sch_smds))
  
  # return(samp)
}



