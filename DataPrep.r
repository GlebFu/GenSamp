library(tidyverse)

file_date <- "2019-03-26"
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

df <- df[,vars] %>% na.omit %>% unique()




df %>%
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

df <- df %>%
  mutate(pTotfrl = 100 * pTotfrl,
         ethBlack = 100 * ethBlack,
         ethHisp = 100 * ethHisp,
         ethWhite = 100 * ethWhite,
         pFem = 100 * pFem,
         pELL = 100 * pELL)

df[,covariates]

save(df, covariates, file_date, file = paste(file_dir, "base data.rdata", sep = ""))

