library(tidyverse)

rm(list = ls())

file_date <- "2019-02-28"

source("SimSource.R")

# vars <- c("n", "urbanicity", "pELL", "pED", "pELA", "pMath", "pMin", "MEDINC")
# 
# frm <- as.formula(paste("Eij ~ ", paste(vars, collapse = " + ")))
# 
# df.Bindex <- df %>% ungroup() %>% select(DSID, vars)

# reps <- 1000

runSim <- function(reps) {
  source("SimSource.R")
  # debug(testRun)
  
  # vars <- c("n", "urbanicity", "pELL", "pED", "pELA", "pMath", "pMin", "MEDINC")
  vars <- c("n", "urbanicity", "pELL", "pED", "pELA", "pMath", "pMin")
  
  frm <- as.formula(paste("Eij ~ ", paste(vars, collapse = " + ")))
  
  df.Bindex <- df %>% ungroup() %>% select(DSID, vars)
  
  # replicate(reps, testRun(df.select %>% filter(sch.RR %in% c(50)),  pop.PS = df.Bindex, frm = frm, vars = vars))
  replicate(reps, testRun(df.select,  pop.PS = df.Bindex, frm = frm, vars = vars))
  
}

# runSim(1)

runtimeFile <- paste("Data/", file_date, "/runtime r1000.rdata", sep = "")
resultsFile <- paste("Data/", file_date, "/results r1000.rdata", sep = "")

library(parallel)

no_cores <- detectCores() - 1

minreps <- 1000
reps <- rep((minreps + (no_cores - minreps %% no_cores)) / no_cores, each = no_cores)

# Initiate cluster
cl <- makeCluster(no_cores)

seed <- runif(1,0,1)*10^8
set.seed(42987117)

runtime <- system.time(results <- parSapply(cl, reps, runSim))

stopCluster(cl)

save(runtime, file = runtimeFile)

types = 4

ind1 <- 1 + types * 0:(reps[1]-1)
ind2 <- 2 + types * 0:(reps[1]-1)
ind3 <- 3 + types * 0:(reps[1]-1)
ind4 <- 4 + types * 0:(reps[1]-1)

df_responses <- bind_rows(results[ind1,]) %>% data.frame
df_sch_smd <- bind_rows(results[ind2,]) %>% data.frame
df_Bindex <- bind_rows(results[ind3,]) %>% data.frame
df_counts <- bind_rows(results[ind4,]) %>% data.frame

df_counts %>%
  group_by(sample, sch.RR, DSID) %>%
  summarise(n = sum(Eij),
            perc = n/sum(reps)) %>%
  ggplot(aes(x = perc)) +
  geom_histogram() + 
  facet_grid(sample ~ sch.RR)

save(df_responses, df_sch_smd, df_Bindex, df_counts, file = resultsFile)

load(runtimeFile)
load(resultsFile)
avgRun <- runtime/sum(reps)
avgRun * 1000 / 60 / 60 # Hours
avgRun * 1000 / 60      # Minutes


