library(tidyverse)

rm(list = ls())

source("SimSource.R")

frm <- as.formula(paste("Eij ~ ", paste(covariates, collapse = " + ")))

df.cov <- df %>% ungroup() %>% select(DSID, covariates) %>% unique

#----------------------
# Run Single Iteration
#----------------------
df.test <- filter(df.select, sch.RR == 10, K %in% c(6))

#69590806 - SBS B-Index < .001
#74069381 - SBS B-Index = .941
# seed <- runif(1,0,1)*10^8
seed <- 74069381
set.seed(seed)

# debug(Bindex)


#--------
dat <- df.test
vars <- covariates
df.cov
frm

df.unstrat <- dat %>% 
  ungroup() %>% 
  select(-K, -strata, -rank_full, -pa) %>% 
  unique 

approached <- generateE(df.unstrat)
approached <- dat %>% left_join(approached) %>% bind_rows(approached)
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


#--------










results <- runSim(df.test,  df.cov = df.cov, frm = frm, vars = covariates)



df_responses <- bind_rows(results[1]) %>% data.frame
df_sch_stats <- bind_rows(results[2]) %>% data.frame
df_Bindex <- bind_rows(results[3]) %>% data.frame
df_counts <- bind_rows(results[4]) %>% data.frame


#-----------------------
# Generate participation responess
#-----------------------

set.seed(seed)

approached.test <- generateE(df.test)


#-----------------------
# Run sampling mechanisms
#-----------------------

samp.test <- createSample(approached.test)

#-----------------------
# Calculate responses
#-----------------------

responses.test <- calcResponseRates(samp.test) %>%
  gather(key = variable, value = value, -sample, -sch.RR, -cluster)

responses.test <- calcResponseRates(samp.test, cluster = T) %>%
  gather(key = variable, value = value, -sample, -sch.RR, -cluster) %>%
  rbind(responses.test)


#-----------------------
# Calculate SMDs
#-----------------------

sch_smds.test <- calcSchSMDs(samp.test) %>% 
  select(sample:Group, goal_SMD:miss) %>% 
  gather(key = variable, value = value, -sample, -sch.RR, -Variable, -Group)


#-----------------------
# Calculate B-index
#-----------------------

Bindicies.test <- samp.test %>%
  select(sample, DSID, Eij, vars, sch.RR) %>%
  group_by(sample, sch.RR) %>%
  nest() %>%
  mutate(data = map(data, full_join, df.Bindex)) %>%
  unnest() %>%
  mutate(Eij = ifelse(is.na(Eij), 0, Eij)) %>% 
  group_by(sample, sch.RR) %>%
  nest() %>% 
  mutate(PS_sample = map(data, glm, formula = frm, family = quasibinomial()),
         PS_sample = map(PS_sample, fitted)) %>%
  unnest() %>%
  group_by(sample, sch.RR) %>%
  summarise(Bs = Bindex(PS_sample, Eij))

# Examine SBS Sample

test.df <- samp.test %>%
  select(sample, DSID, Eij, vars, sch.RR) %>%
  group_by(sample, sch.RR) %>%
  nest() %>%
  mutate(data = map(data, full_join, df.Bindex)) %>%
  unnest() %>%
  mutate(Eij = ifelse(is.na(Eij), 0, Eij)) %>%
  group_by(sample, sch.RR) %>%
  filter(sample == "SUBS_F") # Identify sampling method: SRS | CS | SUBS_F | SUBS_SRS | SUBS_CS 



test.df.glm <- glm(formula = frm, family = quasibinomial(), data = test.df)
test.fitted <- fitted(test.df.glm) 
hist(test.fitted) #distribution of propensity score for caluclating B-Index
Bindex(test.fitted, test.df$Eij)
Bindex2(test.fitted, test.df$Eij)


#-----------------------
# Track sampling counts
#-----------------------

samp_counts.test <- samp.test %>%
  select(sample, DSID, Eij) %>%
  filter(Eij == 1)




