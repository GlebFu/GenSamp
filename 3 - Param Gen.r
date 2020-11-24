rm(list = ls())

library(tidyverse)
library(Hmisc)
library(snow)

source("0 - Functions - Parameter Generation.r")

load("Data/Population Data/Cleaned Data.rdata")
load("Data/Cluster Analysis/Clusters k2-10.rdata")

#-----------------------
# Schools
#-----------------------

covariates

# LogOdds
# DGM.Bs <- c(-.03, 0, -.02, .46, .04, -.01, .01, .01, 0, -.1, 0, .02)

# SMDs
DGM.Bs <- tibble(Bs = c(.019, .374, .081, .433, .007, -.403, -.538, .291, .395, -.019, -.101, .520, .412),
                 var = c("T1", "n", "pTotfrl", "Urban", "Suburban",  "ToRu", "ethWhite", "ethBlack", "ethHisp", "pFem", "ST.ratio", "dSCH", "pELL")) %>%
  mutate(Bs = set_names(Bs, var))



# Set focus to Town/Rural
# DGM.exclude <- "ToRu"
DGM.exclude <- ""

#-----------------------
# Standardize
# -----------------------
df.sim.standardized <- df.sim %>%
  select(DSID, all_of(covariates)) %>%
  mutate_if(is.numeric, stand)

df.pop.stats <- df.sim %>%
  ungroup() %>%
  select(all_of(covariates)) %>%
  gather(key = var, value = val) %>%
  group_by(var) %>%
  summarise(pop.mean = mean(val),
            pop.sd = sd(val))

#-----------------------
# Generate Intercept
# -----------------------

scale_factor <- c(1/4, 1/2, 1/1)
# scale_factor <- c(1/2, 1/1)

response.rates <- 9:1/10
# response.rates <- c(.1, .2, .3)

response_grid <- expand.grid(scale_factor = scale_factor, MRR = response.rates) %>%
  tibble() %>%
  mutate(RR = paste("RR_", formatC(MRR*100, width = 2, format = "d", flag = "0"), sep = ""),
         Bs = list(DGM.Bs$Bs),
         vars = list(covariates),
         DSID = list(df.sim.standardized$DSID)) %>%
  mutate(PS.Int = pmap(.l = list(Bs = Bs,
                                 MRR = MRR,
                                 scale_factor = scale_factor,
                                 vars = vars),
                       .f = calcPS,
                       data = df.sim.standardized,
                       exclude = DGM.exclude,
                       getint = T),
         Int = map_dbl(PS.Int, function(x) x$Intercept),
         PS = map(PS.Int, function(x) x$PS))



df.PS <- response_grid %>%
  select(scale_factor, RR, DSID, PS) %>%
  unnest(cols = c(DSID, PS))
  
df.PS %>% 
  group_by(scale_factor, RR ) %>%
  summarise_if(is.numeric, mean)

intercepts <- 
  response_grid %>%
  select(scale_factor, RR, Int)

# df.PS %>%
#   mutate(PS.log = log(PS / (1-PS))) %>%
#   ggplot(aes(x = PS)) +
#   geom_histogram() +
#   facet_grid(scale_factor~RR)

df.PS %>%
  mutate(PS.log = log(PS / (1-PS))) %>%
  ggplot(aes(x = PS.log)) +
  geom_histogram() +
  facet_grid(scale_factor~RR )


df.smd.ipsw <- df.PS %>%
  left_join(df.sim %>% select(DSID, covariates)) %>%
  gather(key = var, value = val, T1:pELL) %>%
  mutate(w1 = 1 / PS,
         w0 = 1 / (1 - PS)) %>%
  group_by(scale_factor, RR, var) %>%
  summarise(m = weighted.mean(val, PS)) %>%
  left_join(df.pop.stats) %>%
  mutate(smd = (m - pop.mean) / pop.sd)

df.smd.ipsw %>%
  select(RR, var, smd) %>%
  left_join(DGM.Bs) %>%
  mutate(dif = Bs - smd,
         scale_d = abs(dif / Bs)) %>%
  ggplot(aes(x = RR,
             y = scale_d,
             color = scale_factor,
             group = scale_factor)) +
  geom_point() +
  geom_line() +
  facet_wrap(~var)



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
  select(DSID, all_of(covariates)) %>%
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
  mutate(SBS_Rank = 1:n()) %>%
  select(-value)


df.clusters <- prop_allocations %>%
  select(K, strata, pa) %>%
  right_join(df.clusters) 

df.clusters <- df.PS %>%
  group_by(scale_factor, RR) %>%
  arrange(PS) %>%
  mutate(UCS_Rank = 1:n()) %>%
  ungroup() %>%
  select(DSID, UCS_Rank) %>%
  unique() %>%
  right_join(df.clusters) %>%
  group_by(K, strata) %>%
  arrange(UCS_Rank) %>%
  mutate(SCS_Rank = 1:n())


# df.clusters %>%
#   filter(K == "K_06") %>%
#   full_join(df.PS) %>%
#   ggplot(aes(x = cluster_percentile, y = PS)) +
#   geom_point() +
#   facet_grid(RR ~ strata)
# 
# 
# df.clusters %>%
#   filter(K == "K_06") %>%
#   full_join(df.PS) %>%
#   filter(strata == 4,
#          RR == "RR_10") %>%
#   left_join(df.sim %>% select(DSID, covariates)) %>%
#   gather(key = var, value = val, T1:pELL) %>%
#   ggplot(aes(x = cluster_percentile, y = val, color = PS)) +
#   geom_point() +
#   facet_wrap(~ var, scales = "free")


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





#-----------------------
# Export Data
#-----------------------

save(df.sim, df.PS, df.clusters, covariates, df.pop.stats, DGM.Bs, intercepts, df.smd.ipsw, prop_allocations, file = "Data/Simulation Data/Sim Data.Rdata")
