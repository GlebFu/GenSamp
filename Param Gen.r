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
names(schBs) <- covariates



#-----------------------
# Generate Intercept
# -----------------------

# sch.resps <- 8:1/20
sch.resps <- 9:1/10
# sch.resps <- c(.1, .2, .3)
sch.respNames <- paste("PS", formatC(sch.resps*100, width = 2, format = "d", flag = "0"), sep = "")

schPS <- sapply(sch.resps, calcPS, Bs = schBs, vars = covariates, data = df, exclude = NULL, getint = F) %>%
  as.data.frame

intercepts <- sapply(sch.resps, calcPS, Bs = schBs, vars = covariates, data = df, exclude = NULL, getint = T)[2,] %>%
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
load(paste("Paper Data/", file_date, "/clusters-full-logs.rdata", sep = ""))
load(paste("Params/", file_date, "/schPars.rdata", sep = ""))

df <- df %>%
  mutate(cluster_full = cls[,6])

to_matrix <- function(data, vars, add.vars = NA) {
  frm <- as.formula(paste("~", paste(vars, collapse = " + ")))
  data_matrixed <- model.matrix(frm, data = data) %>%
    as.data.frame
  
  if(sum(is.na(add.vars)) == 0) data_matrixed <- cbind(data_matrixed, data[,add.vars])
  
  return(data_matrixed %>% select(-`(Intercept)`))
}

df <- to_matrix(data = df, vars = subs_f_vars, add.vars = c("DSID", "cluster_full")) %>%
  gather(key = var, value = value, -one_of(c("DSID", "cluster_full"))) %>%
  group_by(var) %>%
  mutate(w = 1/var(value)) %>%
  group_by(cluster_full, var) %>%
  mutate(value = w * (value - mean(value))^2) %>%
  group_by(DSID, cluster_full) %>%
  summarise(value = sqrt(sum(value))) %>%
  group_by(cluster_full) %>%
  arrange(cluster_full, value) %>%
  mutate(rank_full = 1:n(),
         rankp_full = rank_full/n() * 100) %>%
  select(-value) %>%
  left_join(df)



df$cluster_full <- as.factor(df$cluster_full)


df %>%
  mutate(urbanicity = ifelse(urbanicity == "Rural" | urbanicity == "Town", "ToRu", as.character(urbanicity))) %>%
  ggplot(aes(x = pED, y = pMin, color = cluster_full, alpha = (1 - rankp_full)/100)) +
  facet_wrap(~urbanicity) +
  geom_point()

df %>%
  mutate(urbanicity = ifelse(urbanicity == "Rural" | urbanicity == "Town", "ToRu", as.character(urbanicity))) %>%
  ggplot(aes(x = pMin, y = pELL, color = pED, alpha = (1 - rankp_full)/100)) +
  facet_grid(urbanicity~cluster_full) +
  geom_point()


#-----------------------
# Export Data
#-----------------------

# save.image(file = paste("Params/", file_date, "/schPars.rdata", sep = ""))
load(paste("Params/", file_date, "/schPars.rdata", sep = ""))

schVals %>%
  select(Var, pars, RR) %>%
  mutate(pars = round(pars,2)) %>%
  write.csv(paste("Params/", file_date, "/School Parameters.csv", sep = ""))


head(df.sch)
sch.PS <- df.sch %>%
  select(DID, DSID, SID, sch.respNames)

df.PS <- sch.PS

# Pull out necesary variables for generating selections
df.select <- select(df, DSID, DID, SID, cluster_full, rank_full)

df.select <- left_join(df.select, df.PS) %>%
  gather(key = sch.RR, value = sch.PS, sch.respNames) %>%
  mutate(sch.RR = as.numeric(str_sub(sch.RR, start = 3)))

# # District statistics
# dist_stats <- df[,c("DID", names(distGoal))] %>%
#   gather(key = "Variable", value = "Value", names(distGoal)) %>%
#   group_by(Variable, DID) %>%
#   summarise(dist_mean = mean(Value)) %>%
#   summarise(pop_mean = mean(dist_mean),
#             pop_sd = sd(dist_mean)) %>%
#   left_join(data.frame(Variable = names(distGoal), goal_SMD = distGoal, row.names = NULL, stringsAsFactors = F))

# School statistics
sch_stats <- df[,c("DSID", names(schGoal))] %>%
  gather(key = "Variable", value = "Value", names(schGoal)) %>%
  group_by(Variable) %>%
  summarise(pop_mean = mean(Value),
            pop_sd = sd(Value)) %>%
  left_join(data.frame(Variable = names(schGoal), goal_SMD = schGoal, row.names = NULL, stringsAsFactors = F))

save(df, df.select, df.sch, sch.PS, df.PS, sch_stats, file = paste("Data/", file_date, "/simData.Rdata", sep = ""))
save(schGoal, schVals, file = paste("Data/", file_date, "/RGM Vars.Rdata", sep = ""))
