library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

rm(list = ls())

source("SimSource.R")

reps <- 1000


with(filter(df.select, RR.S.D == "RR9090"), table(cluster_ov, cluster_full))

# seed <- runif(1,0,1)*10^8
# set.seed(42987117)
# # 
# # test <- testRun(df.select %>% filter(sch.RR %in% c(30, 50, 70)))
# # 
# # undebug(calcResponseRates)
# # undebug(testRun)
# 
# runtimeFile <- "Data/runtime r1000.rdata"
# resultsFile <- "Data/results r1000.rdata"
# 
# runtime <- system.time(results <- replicate(reps, testRun(df.select %>% filter(sch.RR %in% c(30, 50, 70)))))
# save(runtime, file = runtimeFile)
# 
# #
# df_responses <- bind_rows(results[1,]) %>% data.frame
# df_dist_smd <- bind_rows(results[2,]) %>% data.frame
# df_sch_smd <- bind_rows(results[3,]) %>% data.frame
# 
# save(df_responses, df_dist_smd, df_sch_smd, file = resultsFile)

load(runtimeFile)
load(resultsFile)
avgRun <- runtime/reps
avgRun * 1000 / 60 / 60 # Hours
avgRun * 1000 / 60      # Minutes


# Visualize Sampling
samplot <- df_responses %>%
  filter(is.na(cluster)) %>%
  group_by(sample, dist.RR, sch.RR, variable) %>%
  summarise(value = mean(value)) %>%
  spread(key = variable, value = value)
# filter(sch.RR == 50) %>%

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
  filter(sch.RR == 30) %>%
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
  filter(sch.RR == 30) %>%
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
  filter(sch.RR == 30) %>%
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
  filter(sch.RR == 30) %>%
  ggplot(aes(x = dist.RR, y = abs(sim_SMD), group = sample, color = sample)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-.1, .1), linetype = "dashed") +
  facet_grid(Group ~ Variable) +
  theme_bw()


# Compare to goal SMDs
df_dist_smd %>%
  group_by(sample, dist.RR, sch.RR, Variable, Group, variable) %>%
  summarise(value = mean(value, na.rm = T)) %>% # NAs due to 0 rejections by CS
  spread(key = variable, value = value) %>%
  filter(sch.RR == 70) %>%
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
  filter(sch.RR == 70) %>%
  ggplot(aes(x = dist.RR, y = miss, group = sample, color = sample)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-.1, .1), linetype = "dashed") +
  facet_grid(Group ~ Variable) +
  theme_bw()