library(tidyverse)

rm(list = ls())

#------------------------------
# utility functions
#------------------------------

logit <- function(x) log(x / (1 - x)) 
expit <- function(x) 1 / (1 + exp(-x))


file_date <- "2019-02-28"
load(paste("data/", file_date, "/base data.rdata", sep = ""))



#------------------------------
# Setup School-level analysis
#------------------------------

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

data_mat <- as.matrix(df_standardized)
N = nrow(data_mat)
goal <- schGoal
sample_n <- 200
RR <- 0.2
seed <- 20190325

convenience_sample <- function(Bs, goal, RR, sample_n, data_mat, N = nrow(data_mat), seed = NULL) {
  
  if (!is.null(seed)) set.seed(seed)
  
  logit_ps <- as.numeric(data_mat %*% Bs)
  intercept <- uniroot(function(x) mean(expit(x + logit_ps)) - RR, lower = -1000, upper = 1000)$root
  ps <- expit(logit_ps + intercept)
  participate <- rbinom(N, size = 1, prob = ps)
  
  recruit_ord <- sample.int(n = nrow(data_mat), prob = ps)
  recruit_cum <- cumsum(participate[recruit_ord])
  sampled <- recruit_ord[recruit_cum < sample_n + 1]
  
  data_mat[sampled,]
}

convenience_discrepancy <- function(Bs, goal, RR, sample_n, data_mat, N = nrow(data_mat), seed = NULL) {
  
  sample_dat <- convenience_sample(Bs = Bs, goal = goal, RR = RR,
                                   sample_n = sample_n, data_mat = data_mat,
                                   N = N, seed = seed)
  
  sum((colMeans(sample_dat) - goal)^2)
  
}

generate_parameters <- function(goal, RR, sample_n, data, seed = NULL) {
  
  data_mat <- as.matrix(data)
  N <- nrow(data_mat)
  Bs <- rep(0, ncol(data_mat))
  
  beta_opt <- optim(par = Bs, fn = convenience_discrepancy,
                    goal = goal, RR = RR, sample_n = sample_n, 
                    data_mat = data_mat, N = N, 
                    seed = seed, control = list())
  
  convenience_sample(Bs = beta_opt$par, goal = goal, RR = RR, sample_n = sample_n, data_mat = data_mat, seed = seed) %>%
    colMeans()

}

generate_parameterS(goal = schGoal)
