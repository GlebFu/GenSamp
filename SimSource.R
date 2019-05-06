library(tidyverse)

file_date <- "2019-03-26"


#-----------------
# Fixed data
#-----------------
#tets

# Base data set
load(paste("Data/", file_date, "/simData.Rdata", sep = ""))



#-----------------
# Functions
#-----------------

sampleBinomial <- function(ps) rbinom(length(ps), 1, prob = ps)

# This version is inaccurate!!!! Don't use it!!!
# 
# Bindex <- function(PS, Eij) {
#   
#   dat1B <- PS[Eij == 1]
#   dat2B <- PS[Eij == 0]
#   ##Baklizi and Eidous (2006) estimator
#   # bandwidth
#   h = function(x){
#     n = length(x)
#     return((4*sqrt(var(x))^5/(3*n))^(1/5)) 
#   }
#   
#   # kernel estimators of the density and the distribution
#   kg = function(x,data){
#     hb = h(data) #bin width
#     k = r = length(x)
#     for(i in 1:k) r[i] = mean(dnorm((x[i]-data)/hb))/hb
#     return(r )
#   } 
#   
#   return( as.numeric(integrate(function(x) sqrt(kg(x,dat1B)*kg(x,dat2B)),-Inf,Inf)$value))
#   
# }

h <- function(x){
  n <- length(x)
  (4 * sqrt(var(x))^5 / (3 * n))^(1/5) 
}

Bindex <- function(PS, Eij) {
  
  tryCatch({
    dat1B <- PS[Eij == 1]
    dat2B <- PS[Eij == 0]
    ##Baklizi and Eidous (2006) estimator
    # bandwidth
    h1 <- h(dat1B)
    h2 <- h(dat2B)
    
    # kernel estimators of the density and the distribution
    kg = function(x, data, hb = h(data)){
      k = r = length(x)
      for(i in 1:k) r[i] = mean(dnorm((x[i]-data)/hb))/hb
      return(r)
    } 
    
    h_max <- max(h1, h2)
    min_x <- min(PS) - 3 * h_max
    max_x <- max(PS) + 3 * h_max
    
    integrate(function(x) sqrt(kg(x, dat1B, h1) * kg(x, dat2B, h2)), min_x, max_x)$value %>%
      as.numeric() %>%
      return
  },
  error = function(cond) {return(NA)})


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




# Creates dataset with samples for each method and response rate
createSample <- function(data, df.cov) {
  
  
  
  # Simple Random Sampling
  SRS_Sample <- data %>%
    filter(is.na(K)) %>%
    group_by(sch.RR) %>%
    mutate(rank.SRS = sample(1:n())) %>%
    arrange(rank.SRS) %>%
    # or do sample_frac(size = 1)
    mutate(count = cumsum(Eij)) %>% 
    filter(count <= 60) %>%
    left_join(df.cov) %>%
    mutate(sample = "SRS",
           cluster = NA)
  
  #Convenience Sampling
  CS_Sample <- data %>%
    filter(is.na(K)) %>%
    group_by(sch.RR) %>%
    sample_frac(size = 1, weight = sch.PS) %>%
    mutate(count = cumsum(Eij)) %>% 
    filter(count <= 60) %>%
    left_join(df.cov) %>%
    mutate(sample = "CS",
           cluster = NA)
  

  #Full Cluster Analysis Stratified Sampling
  SUBS_F_Sample <- data %>%
    group_by(sch.RR, K, strata) %>%
    arrange(rank_full) %>%
    mutate(count = cumsum(Eij)) %>%
    filter(count <= pa) %>%
    left_join(df.cov) %>%
    mutate(sample = "SUBS_F",
           cluster = strata)
  
  #Full Cluster Analysis Stratified Sampling
  SUBS_SRS_Sample <- data %>%
    group_by(sch.RR, K, strata) %>%
    mutate(rank.SRS = sample(1:n())) %>%
    arrange(rank.SRS) %>%
    # or do sample_frac(size = 1)
    mutate(count = cumsum(Eij)) %>% 
    filter(count <= pa) %>%
    left_join(df.cov) %>%
    mutate(sample = "SUBS_SRS",
           cluster = strata)
  
  #Full Cluster Analysis Stratified Sampling
  SUBS_CS_Sample <- data %>%
    group_by(sch.RR, K, strata) %>%
    sample_frac(size = 1, weight = sch.PS) %>%
    mutate(count = cumsum(Eij)) %>% 
    filter(count <= pa) %>%
    left_join(df.cov) %>%
    mutate(sample = "SUBS_CS",
           cluster = strata)
  
  return(rbind(SRS_Sample, CS_Sample, SUBS_F_Sample, SUBS_SRS_Sample, SUBS_CS_Sample))
}

# Calculates response rates and other recruiting statistics
calcResponseRates <- function(data, cluster = F) {
  if(!cluster){
    data <- data %>%
      mutate(strata = NA) %>%
      group_by(sample, sch.RR, strata, K)
  } else {
    data <- data %>%
      filter(sample %in% c("SUBS_F", "SUBS_OV")) %>%
      group_by(sample, sch.RR, strata, K)

  }
  
  data %>%
    summarise(sch_contacted = n(),
            sch_accepted = sum(Eij)) %>%
    mutate(sch_rejected = sch_contacted - sch_accepted) %>%
    mutate(sch_RR = sch_accepted/sch_contacted) %>%
    group_by(sample, sch.RR, strata, K) %>%
    return()
}



# Calculates weighted standard deviation
weighted.sd <- function(x, w) sqrt(sum((w * (x - weighted.mean(x, w)))^2) / (sum(w) - 1))
# weighted.sd <- function(x, w) sd(x[w == 1])


# calcSchSMDs <- function(sample) {
#   sample[, c("sch.RR", "sample","DID", "Eij", names(schGoal))] %>%
#     gather(key = "Variable", value = "Value", names(schGoal)) %>%
#     mutate(sampled = Eij,
#            rejected = 1 - Eij) %>%
#     gather(key = "Group", value = "Weight", sampled:rejected) %>%
#     group_by(sample, sch.RR, Variable, Group) %>%
#     summarise(sample_mean = weighted.mean(Value, Weight),
#               sample_sd = weighted.sd(Value, Weight)) %>% 
#     left_join(sch_stats) %>%
#     mutate(sim_SMD = (sample_mean - pop_mean) / pop_sd,
#            miss = sim_SMD - goal_SMD)
# }



runSim <- function(data, df.cov, frm, vars) {
  df.unstrat <- data %>% 
    ungroup %>% 
    select(-K, -strata, -rank_full, -pa) %>% 
    unique 
  
  approached <- generateE(df.unstrat)
  approached <- data %>% left_join(approached) %>% bind_rows(approached)
  samp <- createSample(approached, df.cov)
  
  responses <- calcResponseRates(samp) %>%
    gather(key = variable, value = value, -sample, -sch.RR, -strata, -K)
  
  responses <- calcResponseRates(samp, cluster = T) %>%
    gather(key = variable, value = value, -sample, -sch.RR, -strata, -K) %>%
    rbind(responses)
  
  samp.stats <- samp %>%
    ungroup() %>%
    select(sch.RR, sample, vars, K) %>%
    gather(key = var, value = val, -sample, -sch.RR, -K) %>%
    group_by(sch.RR, sample, var, K) %>%
    summarise(samp.mean = mean(val),
              samp.sd = sd(val))

  Bindicies <- samp %>%
    select(sample, DSID, Eij, vars, sch.RR, K) %>%
    group_by(sample, sch.RR, K) %>%
    nest() %>%
    mutate(data = map(data, full_join, df.cov)) %>%
    unnest() %>%
    mutate(Eij = ifelse(is.na(Eij), 0, Eij)) %>% 
    group_by(sample, sch.RR, K) %>%
    nest() %>% 
    mutate(PS_sample = map(data, glm, formula = frm, family = quasibinomial()),
           PS_sample = map(PS_sample, fitted)) %>%
    unnest() %>%
    group_by(sample, sch.RR, K) %>%
    summarise(Bs = Bindex(PS_sample, Eij))
  
  samp_counts <- samp %>%
    select(sample, DSID, Eij, K, sch.RR) %>%
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
  
  return(list(responses = responses, samp.stats = samp.stats, Bindicies = Bindicies, samp_counts))
  # return(list(responses = responses, dist_smds = dist_smds, sch_smds = sch_smds))
  
  # return(samp)
}



