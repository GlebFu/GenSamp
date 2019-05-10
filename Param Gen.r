rm(list = ls())

library(tidyverse)
library(Hmisc)
library(snow)

source("ParGenSource.r")

load("Data/Population Data/Cleaned Data.rdata")
load("Data/Cluster Analysis/Clusters k2-10.rdata")

#-----------------------
# Schools
#-----------------------

covariates

# LogOdds
# DGM.Bs <- c(-.03, 0, -.02, .46, .04, -.01, .01, .01, 0, -.1, 0, .02)

# SMDs
DGM.Bs <- c(.019, .374, .081, .433, .007, -.403, -.538, .291, .395, -.019, -.101, .520, .412)

# Set focus to Town/Rural
# DGM.exclude <- "ToRu"
DGM.exclude <- ""

names(DGM.Bs) <- covariates[!(covariates %in% DGM.exclude)]

DGM.Bs

#-----------------------
# Generate Intercept
# -----------------------
df.sim.standardized <- 
  df.sim[,covariates] %>%
  mutate_all(stand)

# response.rates <- 8:1/20
response.rates <- 9:1/10
# response.rates <- c(.1, .2, .3)
response.rates.names <- paste("RR_", formatC(response.rates*100, width = 2, format = "d", flag = "0"), sep = "")

PS.Int <- sapply(response.rates, calcPS, Bs = DGM.Bs, vars = covariates, data = df.sim.standardized, exclude = DGM.exclude, getint = T)

df.PS <- 
  bind_cols(PS.Int[1,]) %>%
  setNames(response.rates.names)

df.PS %>% colMeans()

intercepts <- 
  PS.Int[2,] %>%
  unlist %>%
  setNames(response.rates.names)


df.PS %>%
  gather(key = RR, value = PS) %>%
  ggplot(aes(x = PS)) +
  geom_histogram() +
  facet_wrap(~RR)

df.PS <- 
  df.sim %>%
  select(DSID) %>%
  cbind(df.PS) %>%
  gather(key = RR, value = PS, - DSID)

#-----------------------
# Generate Within Cluster Ranks and Proportional Allocations
#-----------------------

prop_allocations <-
  df.clusters %>%
  gather(key = K, value = strata, -DSID) %>%
  group_by(K, strata) %>%
  summarise(n = n()) %>%
  # filter(n != 0) %>%
  group_by(K) %>%
  mutate(p = n / sum(n),
         pa = round(p * 60),
         t = sum(pa),
         pa = propAllocation(pa, t, p))

prop_allocations %>%
  group_by(K) %>%
  summarise(t = sum(pa))


df.clusters <- df.sim %>%
  select(DSID, covariates) %>%
  mutate(n = log(n),
         dSCH = log(dSCH),
         ST.ratio = log(ST.ratio)) %>%
  gather(key = variables, value = value, -DSID) %>%
  left_join(df.clusters) %>%
  gather(key = K, value = strata, -DSID, -variables, -value) %>%
  group_by(K, strata, variables) %>%
  mutate(w = 1/var(value)) %>%
  filter(w != Inf) %>%
  mutate(value = w * (value - mean(value))^2) %>%
  # mutate(value = (value - mean(value))^2) %>%
  group_by(K, DSID, strata) %>%
  summarise(value = sqrt(sum(value))) %>%
  group_by(K, strata) %>%
  arrange(K, strata, value) %>%
  mutate(cluster_rank = 1:n(),
         cluster_percentile = cluster_rank/n() * 100) %>%
  select(-value)


# df.clusters %>%
#   filter(K == "K_05") %>%
#   full_join(df.PS) %>%
#   ggplot(aes(x = cluster_percentile, y = PS)) +
#   geom_point() +
#   facet_grid(RR ~ strata)


df.clusters %>%
  filter(K == "K_05") %>%
  full_join(df.PS) %>%
  filter(strata == 4,
         RR == "RR_10") %>%
  left_join(df.sim %>% select(DSID, covariates)) %>%
  gather(key = var, value = val, T1:pELL) %>%
  ggplot(aes(x = cluster_percentile, y = val, color = PS)) +
  geom_point() +
  facet_wrap(~ var, scales = "free")
  
# 
# df.strat4 <- df.clusters %>%
#   filter(K == "K_06") %>%
#   full_join(df.PS) %>%
#   filter(strata == 4,
#          RR == "RR_10") %>%
#   left_join(df.sim.standardized %>% mutate(DSID = df.sim$DSID))
# 
# X.strat4 <- df.strat4 %>%
#   ungroup() %>%
#   select(covariates) %>%
#   as.matrix
# 
# df.strat4$PS == (expit(intercepts["RR_10"] + X.strat4 %*% (DGM.Bs %>% as.matrix())))



df.clusters <- prop_allocations %>%
  select(K, strata, pa) %>%
  right_join(df.clusters)


#-----------------------
# Export Data
#-----------------------

df.pop.stats <- df.sim %>%
  ungroup() %>%
  select(covariates) %>%
  gather(key = var, value = val) %>%
  group_by(var) %>%
  summarise(pop.mean = mean(val),
            pop.sd = sd(val))



save(df.sim, df.PS, df.clusters, covariates, df.pop.stats, DGM.Bs, intercepts, file = "Data/Simulation Data/Sim Data.Rdata")
