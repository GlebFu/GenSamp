

#-----------------
# Functions
#-----------------

# Calculates weighted standard deviation
Weighted_SD <- function(x, w) sqrt(sum((w * (x - weighted.mean(x, w)))^2) / (sum(w) - 1))

Sample_Binomial <- function(ps) rbinom(length(ps), 1, prob = ps)



Generate_Responses <- function(PS.data, cluster.ranks) {
  PS.data %>%
    mutate(Ej = Sample_Binomial(PS)) %>%
    full_join(cluster.ranks)
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

Bindex_Summary <- function(sampled, B.index.data, B.index.formula) {
  sampled %>%
    select(DSID, accepted, sample_method) %>%
    left_join(B.index.data) %>%
    mutate(accepted = as.numeric(accepted)) %>%
    nest(data = -sample_method) %>%
    mutate(PS_sample = map(data, function(x) glm(data = x, formula = B.index.formula, family = quasibinomial()) %>% fitted()),
           accepted = map(data, select, "accepted")) %>%
    select(-data) %>%
    unnest(cols = c(accepted, PS_sample)) %>%
    group_by(sample_method) %>%
    summarise(Bs = Calc_Bindex(PS_sample, accepted))
}


# Creates dataset with samples for each method and response rate
Create_Samples <- function(data) {
  
  bind_rows(
    # Unstratified Random Sampling
    data %>%
      sample_frac(size = 1) %>%
      mutate(contacted = cumsum(Ej) <= 60,
             accepted = contacted & Ej == 1,
             sample_method = "U_RS_UR_X1"),
    
    # Stratified Random Sampling
   data %>%
      group_by(strata) %>%
      sample_frac(size = 1) %>%
      mutate(contacted = cumsum(Ej) <= pa,
             accepted = contacted & Ej == 1,
             sample_method = "S_RS_SR_X1"),
    
    # Unstratified Convenience Sampling
    data %>%
      sample_frac(size = 1, weight = UCS_Rank) %>%
      mutate(contacted = cumsum(Ej) <= 60,
             accepted = contacted & Ej == 1,
             sample_method = "U_CS_UR_X1"),
    
    # Stratified Convenience Unstratified Ranks
    data %>%
      group_by(strata)  %>%
      sample_frac(size = 1, weight = UCS_Rank) %>%
      mutate(contacted = cumsum(Ej) <= pa,
             accepted = contacted & Ej == 1,
             sample_method = "S_CS_UR_X1"),
    
    # Stratified Convenience Stratified Ranks
    data %>%
      group_by(strata)  %>%
      sample_frac(size = 1, weight = SCS_Rank) %>%
      mutate(contacted = cumsum(Ej) <= pa,
             accepted = contacted & Ej == 1,
             sample_method = "S_CS_SR_X1"),
    
    # Unstratified Convenience Sampling Squared Ranks
    data %>%
      sample_frac(size = 1, weight = UCS_Rank^2) %>%
      mutate(contacted = cumsum(Ej) <= 60,
             accepted = contacted  & Ej == 1,
             sample_method = "U_CS_UR_X2"),
    
    # Stratified Convenience Squared Unstratified Ranks
    data %>%
      group_by(strata)  %>%
      sample_frac(size = 1, weight = UCS_Rank^2) %>%
      mutate(contacted = cumsum(Ej) <= pa,
             accepted = contacted & Ej == 1,
             sample_method = "S_CS_UR_X2"),
    
    # Stratified Convenience Squared Stratified Ranks
    data %>%
      group_by(strata)  %>%
      sample_frac(size = 1, weight = SCS_Rank^2) %>%
      mutate(contacted = cumsum(Ej) <= pa,
             accepted = contacted & Ej == 1,
             sample_method = "S_CS_SR_X2"),
    
    # Stratified Balanced Sampling
    data %>%
      group_by(strata) %>%
      arrange(SBS_Rank) %>%
      mutate(contacted = cumsum(Ej) <= pa,
             accepted = contacted  & Ej == 1,
             sample_method = "S_BS_SR_X1"),
  )
  
}

# Calculates response rates and other recruiting statistics
Calc_Recruitment_Stats <- function(data, include_strata = T) {
  data %>%
    bind_rows(mutate(., strata = 0)) %>%
    group_by(sample_method, strata) %>%
    summarise(contacted = sum(contacted),
              accepted = sum(accepted))

}


Calc_Sample_Statistics <- function(sample.counts) {
  sample.counts %>%
    bind_rows(mutate(., strata = 0)) %>%
    gather(key = var, value = val, -sample_method, -strata, -DSID) %>%
    group_by(sample_method, strata, var) %>%
    summarise(samp.mean = mean(val),
              samp.sd = sd(val))
}



Run_Iteration <- function(x, PS.data, cluster.ranks, sim.data, B.index.data, B.index.formula) {
  
  df.responses <- Generate_Responses(PS.data, cluster.ranks)
  
  
  df.sampled <- Create_Samples(df.responses)
  
  df.recruitment.stats <- Calc_Recruitment_Stats(df.sampled) 
  
  df.samp.counts <- df.sampled %>%
    filter(accepted) %>%
    select(sample_method, DSID) 
  
  df.samp.stats <- df.samp.counts %>%
    left_join(sim.data) %>%
    Calc_Sample_Statistics
  
  df.B.indicies <- Bindex_Summary(df.sampled, B.index.data, B.index.formula)
  
  tibble(
    df.recruitment.stats = list(df.recruitment.stats),
    df.samp.counts = list(df.samp.counts),
    df.B.indicies = list(df.B.indicies),
    df.samp.stats = list(df.samp.stats)
  )
  
}

Summarise_Condition_Results <- function(results) {
  r.stats <- 
    results$df.recruitment.stats %>%
    bind_rows() %>%
    group_by(sample_method, strata) %>%
    summarise_all(mean)
  
  samp.counts <-
    results$df.samp.counts %>%
    bind_rows() %>%
    group_by(sample_method, DSID) %>%
    count() %>%
    as.data.frame()
  
  B <- 
    results$df.B.indicies %>%
    bind_rows()
  
  smd.stats <- 
    results$df.samp.stats %>%
    bind_rows() %>%
    group_by(sample_method, strata, var) %>%
    summarise(sim.mean = mean(samp.mean),
              sim.sd = sd(samp.mean))
  
  tibble(r.stats = list(r.stats),
         samp.counts = list(samp.counts), 
         B = list(B),
         smd.stats = list(smd.stats))
}


Sim_Driver  <- function(iterations, PS.data, cluster.ranks, sim.data, B.index.data, B.index.formula) {
  
  sim.stats <- list()
  
  sim.stats$condition.runtime <-
    system.time(
      results <- 
        map_dfr(
          .x = 1:iterations,
          .f = Run_Iteration,
          PS.data = PS.data, 
          cluster.ranks = cluster.ranks, 
          sim.data = sim.data,
          B.index.data = B.index.data, 
          B.index.formula = B.index.formula
        )
    )
  
  sim.stats$summary.runtime <-
    system.time(
      results <- Summarise_Condition_Results(results)
    )
  
  results %>%
    mutate(sim.stats = list(sim.stats))
  
}