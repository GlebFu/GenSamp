library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

rm(list = ls())

load("Data/simData.Rdata")


# Pull out necesary variables for generating selections
dfPS <- select(df, DSID, DID, SID, distPS10:distPS30, schPS10:schPS30) %>%
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
    merge(select(df, DSID, cluster, n, Urban, Suburban, ToRu, pELL, pED, pELA, pMath, pMin, MEDINC, rank, rankp)) %>%
    return()
}


approached <- genE(dfPS) %>%
  select(DSID:RR, Ej:Eij) %>%
  group_by(RR) %>%
  mutate(rankSRS = sample(1:n())) %>%
  arrange(rankSRS) %>%
  mutate(count = cumsum(Eij)) %>% 
  filter(count <= 60)

approached %>% group_by(RR) %>% summarise(mean(Eij), sum(Eij), n())
