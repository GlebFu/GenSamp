rm(list = ls())

source("ParGenSource.r")



load("data/base data.rdata")


#-----------------------
# DESCRIPTIVES
#-----------------------

df %>%
  group_by(LSTATE) %>%
  summarise(nDists = length(unique(DID)), nSch = length(DSID), Students = sum(n, na.rm = T)) %>%
  xtable(summary = F)

#-----------------------
# Schools
#-----------------------
# Set School SMD Goals
schGoal <- c(.374, .433, -.007, -.403, .081, .538, .412, -.25, -.25)
schVars <- c("n", "Urban", "Suburban", "ToRu", "pED", "pMin", "pELL", "pELA", "pMath")
names(schGoal) <- schVars

# Initial School Betas
schBs <- c(0, 0, 0, 0, 0, 0, 0, 0, 0)

# Set urban as focus
#schEx <- "Urban"
schEx <- NULL


#-----------------------
# Gen District Params
#-----------------------
sch.resps <- 9:1/10
# sch.resps <- c(.25, .5, .75)

sch.respNames <- paste("PS", sch.resps*100, sep = "")

calcschParams <- function(resp, data) {
  optim(par = schBs, fn = testGoal,
        MRR = resp, vars = schVars,
        data = data, exclude = schEx, goal = schGoal) %>%
    c(resp = resp)
}

calcPS_RRs <- function(pars, data) {
  calcPS(Bs = pars$par, MRR = pars$resp, vars = schVars, data = data, exclude = schEx)
}

df.sch.sd <- df.sch[,schVars] %>% mutate_all(STAND)

# schPars <- calcschParams(sch.resps[[2]], data = df.sch.sd)
#
# schPars <- lapply(sch.resps, function(x) {
#   schPars$resp <- x
#   return(schPars)
#   })

schPars <- lapply(sch.resps, calcschParams, data = df.sch.sd)

# undebug(calcPS)

schVals <- lapply(schPars, getVals, vars = schVars, data = df.sch.sd, exclude = schEx, goal = schGoal) %>%
  Reduce(function(dtf1,dtf2) rbind(dtf1,dtf2), .) %>%
  data.frame
schPS <- sapply(schPars, calcPS_RRs, data = df.sch.sd) %>% data.frame

schPS %>%
  ggplot(aes(x = X1, y = X2, color = abs(X1-X2))) +
  geom_point()

schPS %>%
  ggplot(aes(x = X2, y = X3, color = abs(X2-X3))) +
  geom_point()

schPS %>%
  ggplot(aes(x = X1, y = X3, color = abs(X1-X3))) +
  geom_point()

names(schPS) <- sch.respNames
df.sch <- cbind(df.sch, schPS)

apply(schPS, 2, mean)

schPS %>%
  gather(key = RR, value = PS) %>%
  ggplot(aes(x = PS)) +
  geom_histogram() +
  facet_wrap(~RR)

save(schPars, schVals, df.sch, sch.respNames, sch.resps, file = "Params/2018-07-11/schPars.rdata")

load("Params/2018-07-11/schPars.rdata")

schVals %>%
  select(Var, RR, smdS1:goal) %>%
  gather(key = SMD, value = value, smdS1:goal) %>%
  ggplot(aes(x = RR, y = value, color = SMD)) +
  geom_point() +
  facet_wrap(~Var)

schVals %>%
  filter(Var %in% c("ToRu", "Suburban")) %>%
  ggplot(aes(x = RR, y = pars, color = Var)) +
  geom_point()

schVals %>%
  ggplot(aes(x = RR, y = dif, color = Var)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-1, 1) * .25, linetype = "dashed")


#-----------------------
# Run Cluster Analysis
#-----------------------

# Cluster.Rmd

#-----------------------
# Generate Within Cluster Ranks
#-----------------------
load("data/base data.rdata")

load("Params/Clusters.rdata")
load("Params/2018-07-11/schPars.rdata")

df <- df %>%
  left_join(df.clusts) %>%
  rename(cluster_full = cluster_full_6,
         cluster_ov = cluster_OV_6)

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

df <- to_matrix(data = df, vars = subs_ov_vars, add.vars = c("DSID", "cluster_ov")) %>%
  gather(key = var, value = value, -one_of(c("DSID", "cluster_ov"))) %>%
  group_by(var) %>%
  mutate(w = 1/var(value)) %>%
  group_by(cluster_ov, var) %>%
  mutate(value = w * (value - mean(value))^2) %>%
  group_by(DSID, cluster_ov) %>%
  summarise(value = sqrt(sum(value))) %>%
  group_by(cluster_ov) %>%
  arrange(cluster_ov, value) %>%
  mutate(rank_ov = 1:n(),
         rankp_ov = rank_ov/n() * 100) %>%
  select(-value) %>%
  left_join(df)

df$cluster_full <- as.factor(df$cluster_full)
df$cluster_ov <- as.factor(df$cluster_ov)

df %>%
  mutate(urbanicity = ifelse(urbanicity == "Rural" | urbanicity == "Town", "ToRu", as.character(urbanicity))) %>%
  ggplot(aes(x = pED, y = pMin, color = cluster_ov, alpha = (1 - rankp_ov)/100)) +
  facet_wrap(~urbanicity) +
  geom_point()

df %>%
  mutate(urbanicity = ifelse(urbanicity == "Rural" | urbanicity == "Town", "ToRu", as.character(urbanicity))) %>%
  ggplot(aes(x = pMin, y = pELL, color = pED, alpha = (1 - rankp_ov)/100)) +
  facet_grid(urbanicity~cluster_ov) +
  geom_point()

df %>%
  mutate(urbanicity = ifelse(urbanicity == "Rural" | urbanicity == "Town", "ToRu", as.character(urbanicity))) %>%
  ggplot(aes(x = pMin, y = pELL, color = pED, alpha = (1 - rankp_ov)/100)) +
  facet_grid(urbanicity~cluster_full) +
  geom_point()

# df %>%
#   select(cluster_full, cluster_ov) %>%
#   table

df %>%
  filter(rankp_ov < 50) %>%
  mutate(urbanicity = ifelse(urbanicity == "Rural" | urbanicity == "Town", "ToRu", as.character(urbanicity))) %>%
  ggplot(aes(x = pED, y = pMin, color = cluster_ov)) +
  facet_wrap(~urbanicity) +
  geom_point()

df %>%
  filter(rankp_ov < 50) %>%
  mutate(urbanicity = ifelse(urbanicity == "Rural" | urbanicity == "Town", "ToRu", as.character(urbanicity))) %>%
  ggplot(aes(x = pMin, y = pELL, color = pED)) +
  facet_grid(urbanicity~cluster_ov) +
  geom_point()

#-----------------------
# Export Data
#-----------------------

# save.image(file = "Params/2018-07-11/image.rdata")
load("Params/2018-07-11/image.rdata")

schVals %>%
  select(Var, pars, RR) %>%
  mutate(pars = round(pars,2)) %>%
  write.csv("Params/School Parameters.csv")


head(df.sch)
sch.PS <- df.sch %>%
  select(DID, DSID, SID, sch.respNames)

df.PS <- sch.PS

# Pull out necesary variables for generating selections
df.select <- select(df, DSID, DID, SID, cluster_ov, cluster_full, rank_ov, rank_full)

df.select <- left_join(df.select, df.PS) %>%
  gather(key = sch.RR, value = sch.PS, PS10:PS90) %>%
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

save(df, df.select, df.dist, df.sch, sch.PS, df.PS, sch_stats, file = "Data/simData.Rdata")
save(schGoal, schVals, file = "Data/RGM Vars.Rdata")
