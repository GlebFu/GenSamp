library(tidyverse)

rm(list = ls())

#------------------------------
# utility functions
#------------------------------

logit <- function(x) log(x / (1 - x)) 
expit <- function(x) 1 / (1 + exp(-x))


#------------------------------
# Setup School-level analysis
#------------------------------

file_date <- "2019-02-28"
load(paste("data/", file_date, "/base data.rdata", sep = ""))

# Set School SMD Goals
# schGoal <- c(.374, .433, .007, -.403, .081, .538, .412, -.3, -.3)
schGoal <- c(.4, .4, 0, -.4, 0.1, .5, .4, -.3, -.3)
schVars <- c("n", "Urban", "Suburban", "ToRu", "pED", "pMin", "pELL", "pELA", "pMath")
names(schGoal) <- schVars

schGoal

# Set urban as focus
#schEx <- "Urban"
schEx <- NULL

# Standardize X matrix

df_standardized <- 
  df.sch %>%
  select(schVars) %>%
  scale() %>%
  as_tibble()

# dat <- df_standardized[1:2]
# data_mat <- as.matrix(dat)
# N = nrow(data_mat)
# goal <- schGoal[1:2]
# sample_n <- 500
# RR <- 0.2
# seed <- 20190325
# reps <- 100
# Bs <- rep(0, ncol(dat))
# opt_method <- "Nelder-Mead"
# control <- list(trace = 4, maxit = 100)

convenience_sample <- function(Bs, RR, sample_n, data_mat, N = nrow(data_mat), seed = NULL) {
  
  if (!is.null(seed)) set.seed(seed)
  
  logit_ps <- as.numeric(data_mat %*% Bs)
  intercept <- uniroot(function(x) mean(expit(x + logit_ps)) - RR, lower = -1000, upper = 1000)$root
  ps <- expit(logit_ps + intercept)
  participate <- runif(N) < ps
  
  recruit_ord <- sample.int(n = N, prob = ps)
  recruit_cum <- cumsum(participate[recruit_ord])
  sampled <- recruit_ord[recruit_cum < sample_n + 1]
  
  data_mat[sampled,]
}

convenience_discrepancy <- function(Bs, goal, RR, sample_n, reps,
                                    data_mat, N = nrow(data_mat), 
                                    seed = NULL) {
  
  if (!is.null(seed)) set.seed(seed)
  
  sample_dat <- rerun(reps, {
    convenience_sample(Bs = Bs, RR = RR, 
                       sample_n = sample_n, 
                       data_mat = data_mat, N = N)
  })
    
  map(sample_dat, ~ enframe(colMeans(.))) %>% 
    bind_rows() %>% 
    group_by(name) %>%
    summarise(value = mean(value)) %>%
    mutate(diff = value - goal) %>%
    summarise(
      SS = sum(diff^2)
    ) %>% 
    pull(SS)
  
}

generate_parameters <- function(goal, RR, sample_n, dat, 
                                Bs = rep(0, ncol(dat)),
                                reps = 100, 
                                seed = NULL, 
                                opt_method = "Nelder-Mead",
                                control = list(trace = 4, maxit = 1000)) {
 
  require(dplyr)
  require(purrr)
  
  data_mat <- as.matrix(dat)
  N <- nrow(data_mat)

  beta_opt <- optim(par = Bs, fn = convenience_discrepancy,
                    goal = goal, RR = RR, 
                    sample_n = sample_n, reps = reps,
                    data_mat = data_mat, N = N, 
                    seed = seed, 
                    method = opt_method, control = control)
  
  tibble(beta = list(beta_opt$par), 
         value = beta_opt$value, 
         counts_fn = beta_opt$counts[[1]],
         convergence = beta_opt$convergence)
  
}

generate_parameters(goal = schGoal[1:3], RR = 0.2,
                    sample_n = 200, dat = df_standardized[1:3],
                    reps = 500, seed = 20190325)
