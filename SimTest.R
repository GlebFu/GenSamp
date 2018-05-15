library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

rm(list = ls())

source("SimSource.R")

reps <- 1000


seed <- runif(1,0,1)*10^8
set.seed(42987117)

# test <- testRun(df.select %>% filter(sch.RR %in% c(25)))
# 
# 
# undebug(testRun)
# undebug(calcResponseRates)

runtimeFile <- "Data/2018-5-15/runtime r1000.rdata"
resultsFile <- "Data/2018-5-15/results r1000.rdata"

runtime <- system.time(results <- replicate(reps, testRun(df.select)))
save(runtime, file = runtimeFile)


df_responses <- bind_rows(results[1,]) %>% data.frame
df_dist_smd <- bind_rows(results[2,]) %>% data.frame
df_sch_smd <- bind_rows(results[3,]) %>% data.frame

save(df_responses, df_dist_smd, df_sch_smd, file = resultsFile)

load(runtimeFile)
load(resultsFile)
avgRun <- runtime/reps
avgRun * 1000 / 60 / 60 # Hours
avgRun * 1000 / 60      # Minutes

df.dist %>% 
  select(-n, -MEDINC, -m) %>%
  gather(cov, val, Urban:pMin) %>%
  # gather(RR, PS, PS25:PS75) %>%
  mutate(big_PS = PS50 > 0.9) %>%
  ggplot(aes(val, fill = big_PS)) + 
  geom_density(alpha = 0.5) + 
  facet_wrap(~ cov)

df.dist %>% 
  select(PS25:PS75) %>% 
  gather(key = RR, value = PS) %>% 
  ggplot(aes(x = PS)) + 
  geom_histogram() + 
  facet_wrap(~RR)

df.sch %>% 
  select(RR2525:RR7575) %>% 
  gather(key = RR, value = PS) %>% 
  ggplot(aes(x = PS)) + 
  geom_histogram() + 
  facet_wrap(~RR)


# Visualize Sampling
samplot <- df_responses %>%
  filter(is.na(cluster)) %>%
  group_by(sample, dist.RR, sch.RR, variable) %>%
  summarise(value = mean(value)) %>%
  spread(key = variable, value = value)
# filter(sch.RR == 50) %>%

df_responses %>%
  filter(is.na(cluster)) %>%
  ggplot(aes(x = dist.RR, y = value, color = sample)) +
  geom_boxplot() +
  facet_grid(variable ~ sch.RR, scales = "free_y")

samplot %>%
  # filter(sch.RR == 50) %>%
  gather(key = measure, value = value, -sample, -sch.RR, -dist.RR) %>%
  mutate(level = str_split(measure, "_", simplify = T)[,1],
         measure =  str_split(measure, "_", simplify = T)[,2]) %>%
  ggplot(aes(x = dist.RR, y = value, group = sample, color = sample)) +
  geom_point() +
  geom_line() +
  facet_grid(level + measure ~ sch.RR, scales = "free_y") +
  theme_bw() +
  expand_limits(y=0)

samplot_SUBS <- df_responses %>%
  filter(!is.na(cluster)) %>%
  group_by(sample, dist.RR, sch.RR, variable, cluster) %>%
  summarise(value = mean(value)) %>%
  spread(key = variable, value = value)
# filter(sch.RR == 50) %>%

samplot_SUBS %>%
  filter(sch.RR == 25) %>%
  gather(key = measure, value = value, -sample, -sch.RR, -dist.RR, -cluster) %>%
  mutate(level = str_split(measure, "_", simplify = T)[,1],
         measure =  str_split(measure, "_", simplify = T)[,2]) %>%
  ggplot(aes(x = dist.RR, y = value, group = sample, color = sample)) +
  geom_point() +
  geom_line() +
  facet_grid(level + measure ~ cluster, scales = "free_y") +
  theme_bw() +
  expand_limits(y=0)

samplot_SUBS %>%
  filter(sch.RR == 50) %>%
  gather(key = measure, value = value, -sample, -sch.RR, -dist.RR, -cluster) %>%
  mutate(level = str_split(measure, "_", simplify = T)[,1],
         measure =  str_split(measure, "_", simplify = T)[,2]) %>%
  ggplot(aes(x = dist.RR, y = value, group = sample, color = sample)) +
  geom_point() +
  geom_line() +
  facet_grid(level + measure ~ cluster, scales = "free_y") +
  theme_bw() +
  expand_limits(y=0)




# Visualize District SMDs
dSMDplot <- df_dist_smd %>%
  group_by(sample, dist.RR, sch.RR, Variable, Group, variable) %>%
  summarise(value = mean(value, na.rm = T)) %>% # NAs due to 0 rejections by CS
  spread(key = variable, value = value)

dSMDplot %>%
  filter(sch.RR == 50) %>%
  ggplot(aes(x = dist.RR, y = abs(sim_SMD), group = sample, color = sample)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-.1, .1), linetype = "dashed") +
  facet_grid(Group ~ Variable) +
  theme_bw()

# Visualize School SMDs
sSMDplot <- df_sch_smd %>%
  group_by(sample, dist.RR, sch.RR, Variable, Group, variable) %>%
  summarise(value = mean(value, na.rm = F)) %>% 
  spread(key = variable, value = value)

sSMDplot %>%
  filter(Group == "sampled") %>%
  ggplot(aes(x = dist.RR, y = abs(sim_SMD), group = sample, color = sample)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-.1, .1), linetype = "dashed") +
  facet_grid(Variable ~ sch.RR) +
  theme_bw()


# Compare to goal SMDs
df_dist_smd %>%
  group_by(sample, dist.RR, sch.RR, Variable, Group, variable) %>%
  summarise(value = mean(value, na.rm = T)) %>% # NAs due to 0 rejections by CS
  spread(key = variable, value = value) %>%
  filter(sch.RR == 75) %>%
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
  filter(sch.RR == 75) %>%
  ggplot(aes(x = dist.RR, y = miss, group = sample, color = sample)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-.1, .1), linetype = "dashed") +
  facet_grid(Group ~ Variable) +
  theme_bw()
