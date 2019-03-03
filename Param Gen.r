rm(list = ls())

source("ParGenSource.r")

file_date <- "2019-02-28"

load(paste("data/", file_date, "/base data.rdata", sep = ""))



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
schGoal <- c(.374, .433, .007, -.403, .081, .538, .412, -.3, -.3)
schVars <- c("n", "Urban", "Suburban", "ToRu", "pED", "pMin", "pELL", "pELA", "pMath")
names(schGoal) <- schVars

# Initial School Betas
schBs <- c(0, 0, 0, 0, 0, 0, 0, 0, 0)

# Set urban as focus
#schEx <- "Urban"
schEx <- NULL


#-----------------------
# Gen District Params
# -----------------------
# sch.resps <- 9:1/10
sch.resps <- 10:2/20
# sch.resps <- c(.1, .2, .3)

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

# schPS %>%
#   ggplot(aes(x = X1, y = X2, color = abs(X1-X2))) +
#   geom_point()
# 
# schPS %>%
#   ggplot(aes(x = X2, y = X3, color = abs(X2-X3))) +
#   geom_point()
# 
# schPS %>%
#   ggplot(aes(x = X1, y = X3, color = abs(X1-X3))) +
#   geom_point()

names(schPS) <- sch.respNames
df.sch <- cbind(df.sch, schPS)

apply(schPS, 2, mean)

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


schPS$PS90

# df %>%
#   select(Urban) %>%
#   mutate(ps = schPS$PS10) %>%
#   summarise(m1 = weighted.mean(Urban, ps),
#          m2 = weighted.mean(Urban, 1/ps),
#          m3 = sum((1/ps) * Urban)/sum(1/ps),
#          m4 = sum((ps) * Urban)/sum(ps),
#          m = mean(Urban))

df %>%
  select(Urban) %>%
  mutate(E = rbinom(n = length(schPS$PS50), size = 1, prob = schPS$PS10)) %>%
  filter(E == 1) %>%
  summarise(m = mean(Urban))

save(schPars, schVals, df.sch, sch.respNames, sch.resps, schGoal, file = paste("Params/", file_date, "/schPars.rdata", sep = ""))

load(paste("Params/", file_date, "/schPars.rdata", sep = ""))

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

select(df, DSID, cluster_full) %>%
  full_join(select(df.sch, DSID, PS50:PS10)) %>%
  gather(key = rr, value = ps, -DSID, -cluster_full) %>%
  group_by(cluster_full, rr) %>%
  summarise(m = mean(ps)) %>%
  spread(key = rr, value = m)

select(df, DSID, cluster_full) %>%
  full_join(select(df.sch, DSID, PS50:PS10)) %>%
  gather(key = rr, value = ps, -DSID, -cluster_full) %>%
  group_by(cluster_full, rr) %>%
  summarise(m = mean(ps)) %>%
  ggplot(aes(x = rr, y = m, color = cluster_full, group = cluster_full)) +
  geom_line()


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
  gather(key = sch.RR, value = sch.PS, PS10:PS50) %>%
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
