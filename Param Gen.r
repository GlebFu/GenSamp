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
# District
#-----------------------

# Set District SMD Goals
distGoal <- c(.43, -.6, .22, .95, .67, .56, -.66)
distVars <- c("Urban", "Suburban", "ToRu", "pELL", "pED", "pMin", "MEDINC")
names(distGoal) <- distVars

# Initial District Betas
distBs <- c(0, 0, 0, 0, 0, 0, 0)

# Set urban as focus
# distEx <- "Urban"
distEx <- NULL


#-----------------------
# Gen District Params
#-----------------------
# dist.resps <- 9:1/10
dist.resps <- c(.25, .5, .75)

dist.respNames <- paste("PS", dist.resps*100, sep = "")

calcDistParams <- function(resp, data) {
  optim(par = distBs, fn = testGoal,
        MRR = resp, vars = distVars,
        data = data, exclude = distEx, goal = distGoal) %>%
    c(resp = resp)
}

calcPS_RRs <- function(pars, data) {
  calcPS(Bs = pars$par, MRR = pars$resp, vars = distVars, data = data, exclude = distEx)
}

df.dist.sd <- df.dist[,distVars] %>% mutate_all(STAND)

distPars <- lapply(dist.resps[2], calcDistParams, data = df.dist.sd)

# undebug(calcPS)

distVals <- lapply(distPars, getVals, vars = distVars, data = df.dist.sd, exclude = distEx, goal = distGoal) %>%
  Reduce(function(dtf1,dtf2) rbind(dtf1,dtf2), .) %>%
  data.frame
distPS <- sapply(distPars, calcPS_RRs, data = df.dist.sd) %>% data.frame
names(distPS) <- dist.respNames
df.dist <- cbind(df.dist, distPS)

save(distPars, distVals, df.dist, file = "Params/2018-05-07/distPars.rdata")

load("Params/2018-05-07/distPars.rdata")

distVals %>%
  select(Var, RR, smdS1:goal) %>%
  gather(key = SMD, value = value, smdS1:goal) %>%
  ggplot(aes(x = RR, y = value, color = SMD)) +
  geom_point() +
  facet_wrap(~Var)

distVals %>%
  filter(Var %in% c("ToRu", "Suburban")) %>%
  ggplot(aes(x = RR, y = pars, color = Var)) +
  geom_point()

distVals %>%
  ggplot(aes(x = RR, y = dif, color = Var)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-1, 1) * .25, linetype = "dashed")

#-----------------------
# Schools
#-----------------------
df.sch <- df %>%
  mutate(n = STAND(n))

# Set School SMD Goals
schGoal <- c(.374, .433, -.007, -.403, .081, .538, .412)
schVars <- c("n", "Urban", "Suburban", "ToRu", "pED", "pMin", "pELL")
names(schGoal) <- schVars

# Initial School Betas
schBs <- c(0, 0, 0, 0, 0, 0)

# Set urban as focus
schEx <- "Urban"

#-----------------------
# Gen School Params
#-----------------------
sch.resps <- 9:1/10
# # sch.resps <- c(.5)
# 
# 
# 
# calcSchParams <- function(sch.resps, distp = NULL, distrr) {
#   optim(par = schBs, fn = testGoal,
#         MRR = sch.resps, vars = schVars,
#         data = df.sch, exclude = schEx,
#         goal = schGoal, distp = distp) %>%
#     c(resp = sch.resps,
#       dresp = distrr) %>%
#     c(list(distp = distp))
# }
# 
# calcPS_RRs <- function(pars, distp = NULL) {
#   calcPS(Bs = pars$par, MRR = pars$resp,
#          vars = schVars, data = df.sch,
#          exclude = schEx, distp = distp,
#          int = pars$value)
# }
# 
# test <- df.dist[, dist.respNames] %>% as.data.frame
# row.names(test) <- df.dist$DID
# test <- test[as.character(df.sch$DID),] %>% as.data.frame
# names(test) <- dist.respNames
# 
# schPars <- list()
# 
# for(i in 1:length(dist.resps)) {
#   for(j in 1:length(sch.resps)) {
#     tryCatch(
#       {
#         schPars <- c(schPars, list(calcSchParams(sch.resps[j], distp = test[, i], distrr = dist.resps[i])))
#       },
#       error=function(e){cat(# "ERROR :",conditionMessage(e), "\n",
#                             "District RR: ", dist.resps[i], " School RR: ", sch.resps[j], "\n")})
#   }
# 
# 
# }
# 
# schVals <- lapply(schPars, getVals, vars = schVars, data = df.sch, exclude = schEx, goal = schGoal) %>%
#   Reduce(function(dtf1,dtf2) rbind(dtf1,dtf2), .)
# schPS <- sapply(schPars, calcPS_RRs) %>% data.frame
# 
# 
sch.respNames <- schVals[, c("RR", "distRR")] %>% unique
sch.respNames <- paste("RR", sch.respNames$RR*100, sch.respNames$distRR*100, sep = "")
# 
# names(schPS) <- sch.respNames
# df.sch <- cbind(df.sch, schPS)
# 
# save(schVals, schPS, schPars, file = "Params/2018-05-07/schVals.rdata")

load("Params/2018-05-07/schVals.rdata")

schVals %>%
  ggplot(aes(x = RR, y = pars)) +
  geom_point() + geom_smooth(method = "lm", formula = y ~ poly(x,2)) + geom_line(aes(color = factor(distRR))) +
  facet_wrap(~Var)

schVals %>%
  select(Var, RR, distRR, smdS1:goal) %>%
  gather(key = SMD, value = value, smdS1:goal) %>%
  ggplot(aes(x = RR, y = value, color = SMD)) +
  geom_point() +
  facet_grid(distRR ~ Var)

schVals %>%
  ggplot(aes(x = distRR, y = pars, color = Var)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ RR)

schVals %>%
  ggplot(aes(x = RR, y = dif, color = Var)) +
  geom_point() +
  geom_line() + 
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-1, 1) * .25, linetype = "dashed") +
  facet_wrap(~ distRR)

schVals %>%
  ggplot(aes(x = RR, y = dif, color = distRR, group = distRR)) +
  geom_point() +
  geom_line() + 
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-1, 1) * .25, linetype = "dashed") +
  facet_wrap(~ Var)

schVals %>%
  ggplot(aes(x = RR, y = pars, color = distRR)) +
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ I(x^2)) +
  facet_wrap(~ Var)

smoothSchVals <- lm(pars ~ Var * (RR + I(RR^2)), data = schVals)
schVals$parSmooth[!is.na(schVals$pars)] <- smoothSchVals$fitted.values

schVals %>%
  ggplot(aes(x = RR, y = parSmooth, color = distRR)) +
  geom_point() + 
  facet_wrap(~ Var)

schVals$pars <- schVals$parSmooth

schVals %>%
  ggplot(aes(x = distRR, y = pars, color = Var)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ RR)



#-----------------------
# Examin Propensity Scores
#-----------------------
df.dist %>%
  select(DID, one_of(dist.respNames)) %>%
  gather(key = "dist.RR", value = "dist.PS", -DID) %>%
  ggplot(aes(x = dist.PS)) +
  geom_density() +
  facet_wrap(~ dist.RR, scales = "free_y")

df.sch %>%
  select(DID, SID, DSID, one_of(sch.respNames)) %>%
  gather(key = "RR.S.D", value = "sch.PS", -DID, -SID, -DSID) %>%
  mutate(dist.RR = str_sub(RR.S.D, start = 5),
         sch.RR = str_sub(RR.S.D, start = 3, end = 4)) %>%
  ggplot(aes(x = sch.PS,)) + 
  geom_density() +
  facet_wrap(sch.RR ~ dist.RR, scales = "free_y", ncol = 9)



# df.select <- df.dist %>%
#   select(DID, one_of(dist.respNames)) %>%
#   gather(key = "dist.RR", value = "dist.PS", -DID) %>%
#   mutate(dist.RR = str_sub(dist.RR, start = 3))
# 
# df.select <- df.sch %>%
#   select(DID, SID, DSID, one_of(sch.respNames)) %>%
#   gather(key = "RR.S.D", value = "sch.PS", -DID, -SID, -DSID) %>%
#   mutate(dist.RR = str_sub(RR.S.D, start = 5),
#          sch.RR = str_sub(RR.S.D, start = 3, end = 4)) %>%
#   left_join(df.select)
# 
# save(df.select, file = "Params/2018-05-07/select.rdata")

load("Params/2018-05-07/select.rdata")
#-----------------------
# Convenience Sample SMDs
#-----------------------



# sampleCS <- function(data, n = 60) {
#   data <- data %>%
#     mutate(Ej = genE(dist.PS),
#            Eij = ifelse(Ej == 1, genE(sch.PS), 0)) %>%
#     group_by(RR.S.D) %>%
#     arrange(RR.S.D, -Eij, -dist.PS, -sch.PS) %>%
#     mutate(Rank = 1:n(),
#            Select = Rank <= n) %>%
#     ungroup() %>%
#     arrange(DSID, RR.S.D)
# 
#   return(data$Select)
# 
# }
# 
# #create cluster
# library(parallel)
# cl <- makeCluster(detectCores() - 1)
# #get library support needed to run the code
# clusterEvalQ(cl, library(dplyr))
# #put objects in place that might be needed for the code
# clusterExport(cl,c("sampleCS", "df.select", "genE"))
# #... then parallel replicate...
# results <- apply(parSapply(cl, 1:1000, function(i,...) { sampleCS(df.select) } ), 1, mean)
# #stop the cluster
# stopCluster(cl)
# 
# 
# results <- replicate(10, sampleCS(df.select))

# save(results, file = "Params/CS Selection2.rData")


# load("Params/CS Selection2.rData")
# 
# sPlot <- df.select %>%
#   ungroup() %>%
#   arrange(DSID, RR.S.D) %>%
#   mutate(selectRate = apply(results, 1, mean))
# 
# sPlot %>%
#   filter(selectRate > 0) %>%
#   ggplot(aes(x = selectRate)) +
#   geom_histogram() +
#   facet_grid(dist.RR ~ sch.RR)
# 
# sPlot %>%
#   group_by(dist.RR, sch.RR) %>%
#   summarise(mean(selectRate == 0))
# 
# rm(list = "results")

#-----------------------
# Run Full Cluster Analysis
#-----------------------
library(cluster)

K = 10

IVs <- c("n", "urbanicity", "pED", "pMin", "pELL", "MEDINC")
# 
# 
# distance <- daisy(x = df[,IVs], metric = "gower")
# 
# clusters <- list()
# 
# for(i in 1:K) {
# 
#   clusters <- append(clusters, list(kmeans(distance, i)))
# }
# 
# save(clusters, file = "Params/2018-05-07/clusters-full.rdata")

load("Params/2018-05-07/clusters-full.rdata")

classign <- data.frame(sapply(clusters, function(x) x$cluster))
names(classign) <- paste("k", 1:K, sep = "")

df_cluster <- cbind(df, classign)

Vratio <- function(clstr){
  Vw <- clstr$tot.withinss
  Vb <- clstr$betweenss
  Vb / (Vw + Vb)
  
}

data.frame(k = 1:K, var = sapply(clusters, Vratio)) %>%
  ggplot(aes(x = k, y = var)) +
  geom_point() +
  ggtitle("Between cluster variance by number of strata") +
  labs(y = "Between Cluster Variance",
       x = "Number of Strata (k)") +
  geom_line() +
  theme_minimal() +
  scale_x_discrete(limits = c(1:K)) +
  scale_y_continuous(breaks = seq(0, 1, .1))

ggsave("Params/Scree1.png", dpi = 500, width = 5, height = 5)


df$cluster_full <- df_cluster$k6

#-----------------------
# Run OV Cluster Analysis
#-----------------------
# library(cluster)
# 
# K = 10
# 
# IVs <- c("n", "urbanicity", "pED", "pMin", "pELL")
# 
# 
# distance <- daisy(x = df[,IVs], metric = "gower")
# 
# clusters <- list()
# 
# for(i in 1:K) {
#   
#   clusters <- append(clusters, list(kmeans(distance, i)))
# }
# 
# save(clusters, file = "Params/2018-05-07/clusters-OV.rdata")

load("Params/2018-05-07/clusters-OV.rdata")

classign <- data.frame(sapply(clusters, function(x) x$cluster))
names(classign) <- paste("k", 1:K, sep = "")

df_cluster <- cbind(df, classign)

Vratio <- function(clstr){
  Vw <- clstr$tot.withinss
  Vb <- clstr$betweenss
  Vb / (Vw + Vb)
  
}

data.frame(k = 1:K, var = sapply(clusters, Vratio)) %>%
  ggplot(aes(x = k, y = var)) +
  geom_point() +
  ggtitle("Between cluster variance by number of strata") +
  labs(y = "Between Cluster Variance",
       x = "Number of Strata (k)") +
  geom_line() +
  theme_minimal() +
  scale_x_discrete(limits = c(1:K)) +
  scale_y_continuous(breaks = seq(0, 1, .1))

ggsave("Params/Scree2.png", dpi = 500, width = 5, height = 5)


df$cluster_ov <- df_cluster$k6

#-----------------------
# Generate Within Cluster Ranks
#-----------------------

ranks_full <- df %>%
  select(DSID, cluster_full, IVs[-2]) %>%
  gather(key = "Var", value = "Val", -DSID, -cluster_full) %>%
  group_by(Var) %>%
  mutate(SD = sd(Val, na.rm = T),
         zVal = (Val - mean(Val, na.rm = T)) / SD) %>%
  group_by(Var, cluster_full) %>%
  mutate(dististance_full = (zVal - mean(zVal, na.rm = T))^2) %>%
  group_by(DSID, cluster_full) %>%
  summarise(dististance_full = sqrt(sum(dististance_full))) %>%
  arrange(dististance_full) %>%
  group_by(cluster_full) %>%
  mutate(rank_full = 1:n(),
         rankp_full = (1:n())/n() * 100)


df <- merge(df, ranks_full)
df$cluster_full <- factor(df$cluster_full, ordered = F)

ranks_ov <- df %>%
  select(DSID, cluster_ov, IVs[-2]) %>%
  gather(key = "Var", value = "Val", -DSID, -cluster_ov) %>%
  group_by(Var) %>%
  mutate(SD = sd(Val, na.rm = T),
         zVal = (Val - mean(Val, na.rm = T)) / SD) %>%
  group_by(Var, cluster_ov) %>%
  mutate(distance_ov = (zVal - mean(zVal, na.rm = T))^2) %>%
  group_by(DSID, cluster_ov) %>%
  summarise(distance_ov = sqrt(sum(distance_ov))) %>%
  arrange(distance_ov) %>%
  group_by(cluster_ov) %>%
  mutate(rank_ov = 1:n(),
         rankp_ov = (1:n())/n() * 100)


df <- merge(df, ranks_ov)
df$cluster_ov <- factor(df$cluster_ov, ordered = F)




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

# save.image(file = "Params/2018-05-07/image.rdata")
# load("Params/2018-05-07/image.rdata")

# District statistics
dist_stats <- df[,c("DID", names(distGoal))] %>%
  gather(key = "Variable", value = "Value", names(distGoal)) %>%
  group_by(Variable, DID) %>%
  summarise(dist_mean = mean(Value)) %>%
  summarise(pop_mean = mean(dist_mean),
            pop_sd = sd(dist_mean)) %>%
  left_join(data.frame(Variable = names(distGoal), goal_SMD = distGoal, row.names = NULL, stringsAsFactors = F))

# School statistics
sch_stats <- df[,c("DSID", names(schGoal))] %>%
  gather(key = "Variable", value = "Value", names(schGoal)) %>%
  group_by(Variable) %>%
  summarise(pop_mean = mean(Value),
            pop_sd = sd(Value)) %>%
  left_join(data.frame(Variable = names(schGoal), goal_SMD = schGoal, row.names = NULL, stringsAsFactors = F))

# Pull out necesary variables for generating selections
df.select <- select(df, DSID, DID, SID, cluster_ov, cluster_full, rank_ov, rank_full) %>%
  left_join(df.select)

save(df, df.select, dist_stats, sch_stats, file = "Data/simData.Rdata")
save(schGoal, distGoal, file = "Data/RGM Vars.Rdata")
