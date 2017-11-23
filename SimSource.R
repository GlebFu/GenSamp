library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

rm(list = ls())

load("Data/simData.Rdata")


# Pull out necesary variables for generating selections
dfPS <- select(df, DSID, DID, SID, distPS10:distPS30, schPS10:schPS30, cluster, rank) %>%
  rename(rankC = rank) %>%
  gather(key = unitRR, value = PS, distPS10:schPS30) %>%
  mutate(unit = str_sub(unitRR, end = -3),
         RR = str_sub(unitRR, start = -2)) %>%
  select(-unitRR) %>%
  spread(key = unit, value = PS)

# 
sampleBinom <- function(ps) rbinom(length(ps), 1, prob = ps)

genE <- function(data) {
  data %>%
    group_by(RR, DID) %>%
    mutate(Ej = sampleBinom(mean(distPS))) %>%
    group_by(RR) %>%
    mutate(Eij = ifelse(Ej == 1, sampleBinom(schPS), 0)) %>%
    # merge(select(df, DSID, cluster, n, Urban, Suburban, ToRu, pELL, pED, pELA, pMath, pMin, MEDINC, rank, rankp)) %>%
    return()
}

propAl <- function(cluster) {
  allocations <- round((table(df$cluster) / nrow(df)) * 60,0)
  return(allocations[cluster])
}


#-------------------------------------
# Building sample generation function
#-------------------------------------
approached <- genE(dfPS)

set.seed(1010)

SRS_Sample <- approached %>%
  select(DSID:RR, Ej:Eij) %>%
  group_by(RR) %>%
  mutate(rankSRS = sample(1:n())) %>%
  arrange(rankSRS) %>%
  mutate(count = cumsum(Eij)) %>% 
  filter(count <= 60) %>%
  left_join(df, by = c("DSID", "DID", "SID", "cluster"))

SRS_Sample %>% 
  group_by(RR, DID) %>% 
  summarise(
    Ej = mean(Ej),
    n_schools = n(),
    schRR = mean(Eij),
    accepted = sum(Eij),
    rejected = n() - accepted
  ) %>%
  summarise(
    distRR = mean(Ej),
    schRR = weighted.mean(schRR, w = n_schools),
    schRR_con = sum(schRR * Ej * n_schools) / sum(Ej * n_schools),
    accepted = sum(accepted), 
    rejected = sum(rejected),
    district_sampled = n(),
    schools_sampled = sum(n_schools)
  )

CS_Sample <- approached %>%
  group_by(RR) %>%
  arrange(-distPS, -schPS) %>%
  mutate(count = cumsum(Eij)) %>% 
  filter(count <= 60) %>%
  left_join(df, by = c("DSID", "DID", "SID", "cluster"))

CS_Sample %>% group_by(RR) %>% summarise(schRR = mean(Eij), accepted = sum(Eij), rejected = n() - accepted, sampled = n())

CASS_Sample <- approached %>%
  group_by(RR, cluster) %>%
  arrange(rankC) %>%
  mutate(count = cumsum(Eij)) %>%
  filter(count <= propAl(cluster)) %>%
  left_join(df, by = c("DSID", "DID", "SID", "cluster"))

CASS_Sample %>% group_by(RR, cluster) %>% summarise(schRR = mean(Eij), accepted = sum(Eij), rejected = n() - accepted, sampled = n())
CASS_Sample %>% group_by(RR) %>% summarise(schRR = mean(Eij), accepted = sum(Eij), rejected = n() - accepted, sampled = n())




sim_driver <- function(RR, )