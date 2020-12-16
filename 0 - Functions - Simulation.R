

#-----------------
# Functions
#-----------------

# Calculates weighted standard deviation
Weighted_SD <- function(x, w) sqrt(sum((w * (x - weighted.mean(x, w)))^2) / (sum(w) - 1))

Sample_Binomial <- function(ps) rbinom(length(ps), 1, prob = ps)



Generate_Responses <- function(PS.data, cluster.data, K.condition, RR.condition, SB.condition) {
  df.responses <- PS.data %>%
    ungroup() %>%
    filter(RR == RR.condition,
           scale_factor == SB.condition) %>%
    mutate(Ej = Sample_Binomial(PS))
  
  df.responses <- cluster.data %>%
    ungroup() %>%
    filter(K == K.condition) %>%
    left_join(df.responses)
  
  return(df.responses)
}

Calc_H <- function(x){
  n <- length(x)
  (4 * sqrt(var(x))^5 / (3 * n))^(1/5) 
}

Calc_Bindex <- function(PS, sampled) {
  
  tryCatch({
    dat1B <- PS[sampled == 1]
    dat2B <- PS[sampled == 0]
    ##Baklizi and Eidous (2006) estimator
    # bandwidth
    h1 <- Calc_H(dat1B)
    h2 <- Calc_H(dat2B)
    
    # kernel estimators of the density and the distribution
    kg = function(x, data, hb = Calc_H(data)){
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



# Creates dataset with samples for each method and response rate
Create_Samples <- function(data) {
  
  
  
  bind_rows(
    # Unstratified Random Sampling
    data %>%
      sample_frac(size = 1) %>%
      mutate(contacted = cumsum(Ej) <= 60,
             accepted = contacted & Ej == 1,
             sample_method = "U_RS_RR"),
    
    # Stratified Random
   data %>%
      group_by(strata) %>%
      sample_frac(size = 1) %>%
      mutate(contacted = cumsum(Ej) <= pa,
             accepted = contacted & Ej == 1,
             sample_method = "S_RS_RR"),
    
    # Unstratified Convenience Sampling
    data %>%
      sample_frac(size = 1, weight = UCS_Rank) %>%
      mutate(contacted = cumsum(Ej) <= 60,
             accepted = contacted & Ej == 1,
             sample_method = "U_CS_UR"),
    
    # Stratified Convenience Unstratified Ranks
    data %>%
      group_by(strata)  %>%
      sample_frac(size = 1, weight = UCS_Rank) %>%
      mutate(contacted = cumsum(Ej) <= pa,
             accepted = contacted & Ej == 1,
             sample_method = "S_CS_UR"),
    
    # Stratified Convenience Stratified Ranks
    data %>%
      group_by(strata)  %>%
      sample_frac(size = 1, weight = SCS_Rank) %>%
      mutate(contacted = cumsum(Ej) <= pa,
             accepted = contacted & Ej == 1,
             sample_method = "S_CS_SR"),
    
    # Unstratified Convenience Sampling Squared Ranks
    data %>%
      sample_frac(size = 1, weight = UCS_Rank^2) %>%
      mutate(contacted = cumsum(Ej) <= 60,
             accepted = contacted  & Ej == 1,
             sample_method = "U_CS_UR2"),
    
    # Stratified Convenience Squared Unstratified Ranks
    data %>%
      group_by(strata)  %>%
      sample_frac(size = 1, weight = UCS_Rank^2) %>%
      mutate(contacted = cumsum(Ej) <= pa,
             accepted = contacted & Ej == 1,
             sample_method = "S_CS_UR2"),
    
    # Stratified Convenience Squared Stratified Ranks
    data %>%
      group_by(strata)  %>%
      sample_frac(size = 1, weight = SCS_Rank^2) %>%
      mutate(contacted = cumsum(Ej) <= pa,
             accepted = contacted & Ej == 1,
             sample_method = "S_CS_SR2"),
    
    # Stratified Balanced Sampling
    data %>%
      group_by(strata) %>%
      arrange(SBS_Rank) %>%
      mutate(contacted = cumsum(Ej) <= pa,
             accepted = contacted  & Ej == 1,
             sample_method = "S_BS_BR"),
  )
  
}

# Calculates response rates and other recruiting statistics
Calc_Recruitment_Stats <- function(data, include_strata = T) {
  if(!include_strata){
    data <- data %>%
      group_by(sample_method)
  } else {
    data <- data %>%
      group_by(sample_method, strata)

  }
  
  data %>%
    summarise(sch.contacted = sum(contacted),
              sch.accepted = sum(accepted)) %>%
    return()

}


Calc_Sample_Statistics <- function(sample.data, list.covariates) {
  sample.data %>%
    bind_rows(mutate(., strata = 0)) %>%
    select(strata, sample_method, all_of(list.covariates)) %>%
    gather(key = var, value = val, -sample_method, -strata) %>%
    group_by(sample_method, strata, var) %>%
    summarise(samp.mean = mean(val),
              samp.sd = sd(val))
}

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




Sim_Driver <- function(sim.data, PS.data, cluster.data, B.index.formula, list.covariates, K.list, RR.list, SB.list) {
  results <- list()
  
  for(cond.K in K.list) {
    for(cond.RR in RR.list) {
      for(cond.SB in SB.list) {
        results <- Run_Iteration(sim.data = sim.data,
                                 PS.data = PS.data,
                                 cluster.data = cluster.data,
                                 K.condition = cond.K,
                                 RR.condition = cond.RR,
                                 SB.condition = cond.SB,
                                 B.index.formula = B.index.formula,
                                 list.covariates = list.covariates) %>%
          as.matrix %>%
          cbind(results)
      }
    }
  }
  
  # results <- apply(results, 2, bind_cols)
  results %>%
    apply(1, bind_rows) %>%
    return()
  
}

# Run_Iteration_JP <- function(x, sim.data, PS.data, cluster.data, K.condition, RR.condition, SB.condition, B.index.formula, list.covariates) {
#   
#   df.responses <- Generate_Responses(PS.data = PS.data, 
#                                      cluster.data = cluster.data,
#                                      K.condition = K.condition,
#                                      RR.condition = RR.condition,
#                                      SB.condition = SB.condition)
#   
#   df.sampled <- Create_Samples(df.responses)
#   
#   df.recruitment.stats <- Calc_Recruitment_Stats(df.sampled) 
#   
#   df.samp.counts <- df.sampled %>%
#     filter(accepted) %>%
#     select(sample_method, DSID, strata, PS, RR) 
#   
#   df.sampled <- sim.data %>%
#     full_join(df.sampled)
# 
#   df.samp.stats <- Calc_Sample_Statistics(df.sampled, list.covariates)
#   
#   # df.samp.stats <- df.samp.counts %>%
#   #   left_join(df.sampled) %>%
#   #   Calc_Sample_Statistics(list.covariates)
#   
#   df.B.indicies <- df.sampled %>%
#     mutate(accepted = ifelse(accepted, 1, 0)) %>%
#     group_by(sample_method) %>%
#     nest() %>%
#     mutate(PS_sample = map(data, glm, formula = B.index.formula, family = quasibinomial()),
#            PS_sample = map(PS_sample, fitted)) %>%
#     unnest(cols = c(data, PS_sample)) %>%
#     select(sample_method, PS_sample, accepted) %>%
#     group_by(sample_method) %>%
#     summarise(Bs = Calc_Bindex(PS_sample, accepted))
#   
#   tibble(
#     df.recruitment.stats = list(df.recruitment.stats), 
#     df.samp.counts = list(df.samp.counts), 
#     df.B.indicies = list(df.B.indicies), 
#     df.samp.stats = list(df.samp.stats)
#   )
# }


# Sim_Driver_JP <- function(iterations, 
#                           K.condition, RR.condition, SB.condition, 
#                           sim.data, PS.data, cluster.data, 
#                           B.index.formula, list.covariates) {
#   
#   
#   results <- map_dfr(
#     1:iterations, 
#     Run_Iteration_JP,
#     sim.data = sim.data,
#     PS.data = PS.data,
#     cluster.data = cluster.data,
#     K.condition = K.condition,
#     RR.condition = RR.condition,
#     SB.condition = SB.condition,
#     B.index.formula = B.index.formula,
#     list.covariates = list.covariates
#   )
#   
#   # calculate summary stats across iterations here
# 
# }