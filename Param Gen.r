rm(list = ls())

source("ParGenSource.r")

library(Pusto)
library(snow)


file_date <- "2019-02-28"

load(paste("data/", file_date, "/base data.rdata", sep = ""))

#-----------------------
# Schools
#-----------------------
# Set School SMD Goals
# schGoal <- c(.374, .433, .007, -.403, .081, .538, .412, -.3, -.3)
schGoal <- c(.4, .4, 0, -.4, 0, .5, .4, -.3, -.3)
schVars <- c("n", "Urban", "Suburban", "ToRu", "pED", "pMin", "pELL", "pELA", "pMath")
names(schGoal) <- schVars

schGoal

# Initial School Betas
schBs <- c(0, 0, 0, 0, 0, 0, 0, 0, 0)

# Set urban as focus
#schEx <- "Urban"
schEx <- NULL


#-----------------------
# Generate Parameters
# -----------------------
generate_parameters <- function(RR, data, Bs, vars, exclude = NULL, goal) {
  source("ParGenSource.r")
  
  pars <- optim(par = Bs, fn = testGoal,
              MRR = RR, vars = vars,
              data = data, exclude = exclude, goal = goal) %>%
          c(resp = RR)
  
  getVals(bs = pars, vars = vars, data = data, exclude = exclude, goal = goal)
}


sch.resps <- 8:1/20
# sch.resps <- 9:1/10
# sch.resps <- c(.1, .2, .3)
sch.respNames <- paste("PS", formatC(sch.resps*100, width = 2, format = "d", flag = "0"), sep = "")

# Standardize X matrix
df.sch.sd <- df.sch[,schVars] %>% mutate_all(STAND)

no_cores <- min(length(sch.resps), 7)
cl <- start_parallel(no_cores, packages = c("tidyverse"))

seed <- runif(1,0,1)*10^8
set.seed(seed)

runtime <- system.time(schPars <- parSapply(cl, 
                                            sch.resps, 
                                            generate_parameters, 
                                            data = df.sch.sd, 
                                            Bs = schBs,
                                            vars = schVars,
                                            goal = schGoal))

stop_parallel(cl)


schPars <- apply(schPars, 2, bind_rows)

schPS <- lapply(schPars, calcPS_RRs, x.data = df.sch.sd) %>% 
  data.frame

schVals <- bind_rows(schPars)


names(schPS) <- sch.respNames
df.sch <- cbind(df.sch, schPS)

schPS %>%
  gather(key = RR, value = PS) %>%
  ggplot(aes(x = PS)) +
  geom_histogram() +
  facet_wrap(~RR)

schVals %>%
  ggplot(aes(x = RR, y = pars, group = Var)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Var) +
  geom_hline(yintercept = 0, linetype = "dotted")

schVals %>%
  ggplot(aes(x = RR, y = pars, group = Var, color = Var)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dotted")

schVals %>%
  ggplot(aes(x = RR, y = dif, color = Var)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-1, 1) * .25, linetype = "dashed")

save(schVals, df.sch, sch.respNames, sch.resps, schGoal, file = paste("Params/", file_date, "/schPars.rdata", sep = ""))

load(paste("Params/", file_date, "/schPars.rdata", sep = ""))


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
