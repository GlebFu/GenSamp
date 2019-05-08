library(tidyverse)

rm(list = ls())

df.source <- read.csv("Data/Population Data/Source Data.csv", stringsAsFactors = F)
df.source$urbanicity <- factor(df.source$urbanicity)
levels(df.source$urbanicity) <- c("ToRu", "Suburban", "ToRu", "Urban")


# Drop large schools
# Fix binary variables
# Fix percentage variables

df.sim <- df.source %>% 
  filter(n < 4000) %>%      
  unique() %>%
  mutate(pELL = ifelse(is.na(pELL), pELL_D, pELL), 
         pED = ifelse(is.na(pED), pTotfrl, pED),
         pMin = 1 - ethWhite,
         ToRu = Town + Rural,
         MEDINC = as.numeric(MEDINC),
         DID = as.numeric(as.factor(LEAID)) + 1000,
         T1 = TITLEISTAT == 5,
         ST.ratio = n/FTE,
         pFem = genFemale) %>%
  group_by(DID) %>%
  mutate(SID = 1:n() + 10000) %>%
  ungroup() %>%
  mutate(DSID = paste(DID, SID, sep = "-")) %>%
  filter(ST.ratio <= 50,
         n <= 3000)


vars <- c("LSTATE", "LEANM", "SCHNAM", "DID", "SID", "DSID", "n", "pTotfrl",
          "Urban", "Suburban", "ToRu",
          "pELL", "pED", "pELA", "pMath", "pMin", "MEDINC", "urbanicity", 
          "T1", "ethBlack", "ethHisp", "ethWhite", "FTE", "ST.ratio", "pFem", "dSCH")

covariates <- c("T1", "n", "pTotfrl", "Urban", "Suburban", "ToRu",
                "ethWhite", "ethBlack", "ethHisp", "pFem",
                "ST.ratio", "dSCH","pELL")


df.sim <- df.sim[,vars] %>% na.omit %>% unique()


df.sim %>%
  select(DSID, covariates) %>%
  gather(key = var, value = value, -DSID) %>%
  group_by(var) %>%
  mutate(z = (value - mean(value)) / sd(value)) %>%
  filter(abs(z) > 4) %>%
  summarise(min = min(value),
            max = max(value),
            minz = min(abs(z)),
            maxz = max(abs(z)))


#Convert to Percents

df.sim <- df.sim %>%
  mutate(pTotfrl = 100 * pTotfrl,
         ethBlack = 100 * ethBlack,
         ethHisp = 100 * ethHisp,
         ethWhite = 100 * ethWhite,
         pFem = 100 * pFem,
         pELL = 100 * pELL)

df.sim[,covariates]

save(df.sim, covariates, file = "Data/Population Data/Cleaned Data.rdata")

