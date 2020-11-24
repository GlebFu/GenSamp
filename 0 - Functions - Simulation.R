

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
  
  # Random Sampling
  U.RS <- data %>%
    sample_frac(size = 1) %>%
    mutate(contacted = cumsum(Ej) <= 60,
           accepted = contacted  & Ej == 1,
           sample_method = "Unstratified_RS")
  
  # Convenience Sampling
  U.CS <- data %>%
    sample_frac(size = 1, weight = UCS_Rank) %>%
    mutate(contacted = cumsum(Ej) <= 60,
           accepted = contacted  & Ej == 1,
           sample_method = "Unstratified_CS")

  # Stratified Balanced Sampling
  S.BS <- data %>%
    group_by(strata) %>%
    arrange(SBS_Rank ) %>%
    mutate(contacted = cumsum(Ej) <= pa,
           accepted = contacted  & Ej == 1,
           sample_method = "Stratified_BS")
  
  # Stratified Random
  S.RS <- data %>%
    group_by(strata) %>%
    sample_frac(size = 1) %>%
    mutate(contacted = cumsum(Ej) <= pa,
           accepted = contacted  & Ej == 1,
           sample_method = "Stratified_RS")
  
  # Stratified Convenience
  S.CS <- data %>%
    group_by(strata)  %>%
    sample_frac(size = 1, weight = SCS_Rank) %>%
    mutate(contacted = cumsum(Ej) <= pa,
           accepted = contacted  & Ej == 1,
           sample_method = "Stratified_CS")
  
  return(bind_rows(U.RS, U.CS, S.BS, S.RS, S.CS))
}

# Calculates response rates and other recruiting statistics
Calc_Recruitment_Stats <- function(data, include_strata = F) {
  if(!include_strata){
    data <- data %>%
      group_by(sample_method)
  } else {
    data <- data %>%
      group_by(sample_method, strata)

  }
  
  data %>%
    summarise(sch.contacted = sum(contacted),
              sch.accepted = sum(accepted),
              sch.rejected = sch.contacted - sch.accepted,
              sch.response.rate = sch.accepted/sch.contacted) %>%
    gather(key = measure, value = value, -sample_method) %>%
    return()

}


Calc_Sample_Statistics <- function(sample.data, list.covariates) {
  sample.data %>%
    filter(accepted) %>%
    select(sample_method, all_of(list.covariates)) %>%
    gather(key = var, value = val, -sample_method) %>%
    group_by(sample_method, var) %>%
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
  
  df.sampled <- sim.data %>% 
    full_join(df.sampled)
  
  df.samp.stats <- Calc_Sample_Statistics(df.sampled, list.covariates)
  
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

