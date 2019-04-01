rm(list = ls())

library(tidyverse)
library(Hmisc)
library(Pusto)
library(snow)

source("ParGenSource.r")


file_date <- "2019-03-26"

load(paste("data/", file_date, "/base data.rdata", sep = ""))

#-----------------------
# Schools
#-----------------------

covariates

# LogOdds
schBs <- c(-.03, 0, -.02, .46, .04, -.01, .01, .01, 0, -.1, 0, .02)
# schBs <- schBs*2

# Set focus to Town/Rural
schEx <- "ToRu"

names(schBs) <- covariates[!(covariates %in% schEx)]



#-----------------------
# Generate Intercept
# -----------------------
rescale <- function(x) {
  x[x == 0] <- min(x[x != 0])
  x
}


# sch.resps <- 8:1/20
sch.resps <- 9:1/10
# sch.resps <- c(.1, .2, .3)
sch.respNames <- paste("PS", formatC(sch.resps*100, width = 2, format = "d", flag = "0"), sep = "")

PS.Int <- sapply(sch.resps, calcPS, Bs = schBs, vars = covariates, data = df, exclude = schEx, getint = T)

schPS <- bind_cols(PS.Int[1,])
  # mutate_all(rescale)

intercepts <- PS.Int[2,] %>%
  unlist



names(intercepts) <- names(schPS) <- sch.respNames


df.sch <- cbind(df, schPS)

schPS %>%
  gather(key = RR, value = PS) %>%
  ggplot(aes(x = PS)) +
  geom_histogram() +
  facet_wrap(~RR)

#-----------------------
# Run Cluster Analysis
#-----------------------

# Cluster.Rmd

#-----------------------
# Generate Within Cluster Ranks
#-----------------------
load(paste("data/", file_date, "/base data.rdata", sep = ""))

load(paste("Paper Data/", file_date, "/Clusters.rdata", sep = ""))
# load(paste("Paper Data/", file_date, "/clusters-full-logs.rdata", sep = ""))

df <- cbind(df, cls)

to_matrix <- function(data, vars, add.vars = NA) {
  frm <- as.formula(paste("~", paste(vars, collapse = " + ")))
  data_matrixed <- model.matrix(frm, data = data) %>%
    as.data.frame
  
  if(sum(is.na(add.vars)) == 0) data_matrixed <- cbind(data_matrixed, data[,add.vars])
  
  return(data_matrixed %>% select(-`(Intercept)`))
}

df <- to_matrix(data = df, vars = cluster_vars, add.vars = c("DSID", names(cls))) %>%
  gather(key = var, value = value, -one_of(c("DSID", names(cls)))) %>%
  gather(key = K, value = strata, - var, -DSID, -value) %>%
  group_by(K, var, strata) %>%
  mutate(w = 1/var(value)) %>%
  filter(w != Inf) %>%
  group_by(K, strata, var) %>%
  mutate(value = w * (value - mean(value))^2) %>%
  # mutate(value = (value - mean(value))^2) %>%
  group_by(K, DSID, strata) %>%
  summarise(value = sqrt(sum(value))) %>%
  group_by(K, strata) %>%
  arrange(K, strata, value) %>%
  mutate(rank_full = 1:n(),
         rankp_full = rank_full/n() * 100) %>%
  select(-value) %>%
  left_join(df)



df$K <- str_split(df$K, pattern = "_", simplify = T)[,2] %>% as.numeric
df$strata <- as.factor(df$strata)

df %>%
  filter(K == 2) %>%
  mutate(urbanicity = ifelse(urbanicity == "Rural" | urbanicity == "Town", "ToRu", as.character(urbanicity))) %>%
  ggplot(aes(x = pED, y = pMin, color = strata, alpha = (1 - rankp_full)/100)) +
  facet_wrap(~urbanicity) +
  geom_point()

df %>%
  filter(K == 7) %>%
  mutate(urbanicity = ifelse(urbanicity == "Rural" | urbanicity == "Town", "ToRu", as.character(urbanicity))) %>%
  ggplot(aes(x = pMin, y = pELL, color = pED, alpha = (1 - rankp_full)/100)) +
  facet_grid(urbanicity ~ strata) +
  geom_point()


#-----------------------
# Export Data
#-----------------------

# schVals %>%
#   select(Var, pars, RR) %>%
#   mutate(pars = round(pars,2)) %>%
#   write.csv(paste("Params/", file_date, "/School Parameters.csv", sep = ""))


head(df.sch)
sch.PS <- df.sch %>%
  select(DID, DSID, SID, sch.respNames)

df.PS <- sch.PS

# Pull out necesary variables for generating selections
df.select <- select(df, DSID, DID, SID, K, strata, rank_full)

df.select <- left_join(df.select, df.PS) %>%
  gather(key = sch.RR, value = sch.PS, sch.respNames) %>%
  mutate(sch.RR = as.numeric(str_sub(sch.RR, start = 3)))

propAllocation <- function(goal, total, perc) {
  if(mean(total) < 60) goal[perc == min(perc)] <- goal[perc == min(perc)] + 1
  return(goal)
}

df.select <- df.select %>%
  group_by(K, strata) %>%
  summarise(n = n()) %>%
  filter(n != 0) %>%
  group_by(K) %>%
  mutate(p = n / sum(n),
         pa = round(p * 60),
         t = sum(pa),
         pa = propAllocation(pa, t, p)) %>%
  select(K, strata, pa) %>%
  right_join(df.select)


pop.stats <- df %>%
  ungroup() %>%
  select(covariates) %>%
  gather(key = var, value = val) %>%
  group_by(var) %>%
  summarise(pop.mean = mean(val),
            pop.sd = sd(val))



save(df, df.select, df.sch, sch.PS, df.PS, PS.Int, covariates, cluster_vars, pop.stats, schBs, file = paste("Data/", file_date, "/simData.Rdata", sep = ""))
