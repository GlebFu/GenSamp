library(tidyverse)

file_date <- "2019-02-28"
file_dir <- paste("data/", file_date, "/", sep = "")


df <- read.csv("Data/final data.csv", stringsAsFactors = F)
df$urbanicity <- factor(df$urbanicity)
levels(df$urbanicity) <- c("ToRu", "Suburban", "ToRu", "Urban")

# Create ELL and ED variable district data when school is unavailable
# Create Minority variable
# Create Town/Rural Variable
df <- df %>% 
  filter(n < 4000) %>%
  unique() %>%
  mutate(pELL = ifelse(is.na(pELL), pELL_D, pELL), 
         pED = ifelse(is.na(pED), pTotfrl, pED),
         pMin = 1-ethWhite,
         ToRu = Town + Rural,
         MEDINC = as.numeric(MEDINC),
         DID = as.numeric(as.factor(LEAID)) + 1000) %>%
  group_by(DID) %>%
  mutate(SID = 1:n() + 10000) %>%
  ungroup() %>%
  mutate(DSID = paste(DID, SID, sep = "-"))

vars <- c("LSTATE", "LEANM", "SCHNAM", "DID", "SID", "DSID", "n", "pTotfrl",
          "Urban", "Suburban", "ToRu",
          "pELL", "pED", "pELA", "pMath", "pMin", "MEDINC", "urbanicity")

covariates <- c("n", "Urban", "Suburban", "ToRu", "pTotfrl",
                "pELL", "pED", "pELA", "pMath", "pMin", "MEDINC")

df <- df[,vars] %>% na.omit %>% unique()

# Covarites 
# subs_f_vars <- c("n", "urbanicity",
#                  "pELL", "pED", "pELA", "pMath", "pMin", "MEDINC")
subs_f_vars <- c("n", "urbanicity",
                 "pELL", "pED", "pELA", "pMath", "pMin")
 
# School Level Data
df.sch <- df[,vars]


save(df, df.sch, subs_f_vars, file_date, file = paste(file_dir, "base data.rdata", sep = ""))

