library(dplyr)

rm(list = ls())

source("ParGenSource.r")

genE <- function(ps) rbinom(length(ps), 1, prob = ps)

#-----------------------
# DATA PREP
#-----------------------

# Read Data
df <- read.csv("Data/final data.csv", stringsAsFactors = F)
df$urbanicity <- factor(df$urbanicity)
levels(df$urbanicity) <- c("ToRu", "Suburban", "ToRu", "Urban")

# Create ELL and ED variable district data when school is unavailable
# Create Minority variable
# Create Town/Rural Variable
df <- df %>% 
  filter(n < 4000) %>%
  mutate(pELL = ifelse(is.na(pELL), pELL_D, pELL), 
         pED = ifelse(is.na(pED), pTotfrl, pED),
         pMin = 1-ethWhite,
         ToRu = Town + Rural,
         MEDINC = STAND(as.numeric(MEDINC)),
         DID = as.numeric(as.factor(LEAID)) + 1000) %>%
  group_by(DID) %>%
  mutate(SID = 1:n() + 10000) %>%
  ungroup() %>%
  mutate(DSID = paste(DID, SID, sep = "-"))


vars <- c("LSTATE", "LEANM", "SCHNAM", "DID", "SID", "DSID", "n", "ULOCAL", "pTotfrl", "pIEP_D", "pELL_D", 
          "Urban", "Suburban", "ToRu", "schPrimary", "schMIDDLE", "schHigh", 
          "schOther", "pELL", "pED", "pELA", "pMath", "pMin", "MEDINC", "urbanicity")

# apply(df[,vars], 2, function(x) sum(is.na(x)))

df <- df[,vars] %>% na.omit



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


# Create district level data
df.dist <- select(df, -SCHNAM, -LSTATE, -LEANM, -SID) %>%
  group_by(DID) %>%
  summarise_if(is.numeric, mean, na.rm = T) %>%
  ungroup()


# Set District SMD Goals
distGoal <- c(.43, -.6, .22, .95, .67, .56, -.66)
distVars <- c("Urban", "Suburban", "ToRu", "pELL", "pED", "pMin", "MEDINC")
names(distGoal) <- distVars

# Initial District Betas
distBs <- c(0, 0, 0, 0, 0, 0)

# Set urban as focus
distEx <- "Urban"


#-----------------------
# Gen District Params
#-----------------------
# dist.resps <- 9:1/10
# # dist.resps <- c(.4, .6)
# 
# dist.respNames <- paste("PS", dist.resps*100, sep = "")
# 
# calcDistParams <- function(resp) {
#   optim(par = distBs, fn = testGoal,
#         MRR = resp, vars = distVars,
#         data = df.dist, exclude = distEx, goal = distGoal) %>%
#     c(resp = resp)
# }
# 
# calcPS_RRs <- function(pars) {
#   calcPS(Bs = pars$par, MRR = pars$resp, vars = distVars, data = df.dist, exclude = distEx)
# }
# 
# distPars <- lapply(dist.resps, calcDistParams)
# distVals <- lapply(distPars, getVals, vars = distVars, data = df.dist, exclude = distEx, goal = distGoal)
# distPS <- sapply(distPars, calcPS_RRs) %>% data.frame
# names(distPS) <- dist.respNames
# df.dist <- cbind(df.dist, distPS)
# 
# save.image("Params/180127.rdata")

load("Params/180127.rdata")

distVals <- distVals %>%
  Reduce(function(dtf1,dtf2) rbind(dtf1,dtf2), .)

distVals %>%
  select(Var, RR, smdS1:goal) %>%
  gather(key = SMD, value = value, smdS1:goal) %>%
  ggplot(aes(x = RR, y = value, color = SMD)) +
  geom_point() +
  facet_wrap(~Var)

distVals %>%
  ggplot(aes(x = RR, y = dif)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-1, 1) * .25, linetype = "dashed") +
  facet_wrap(~Var)

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
# sch.resps <- 9:1/10
# # sch.resps <- c(.3, .5)
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
# sch.respNames <- schVals[, c("RR", "distRR")] %>% unique
# sch.respNames <- paste("RR", sch.respNames$RR*100, sch.respNames$distRR*100, sep = "")
# 
# names(schPS) <- sch.respNames
# df.sch <- cbind(df.sch, schPS)
# 
# save.image("Params/180127.rdata")

load("Params/180127.rdata")

schVals %>%
  select(Var, RR, distRR, smdS1:goal) %>%
  gather(key = SMD, value = value, smdS1:goal) %>%
  ggplot(aes(x = RR, y = value, color = SMD)) +
  geom_point() +
  facet_grid(distRR~Var)

schVals %>%
  ggplot(aes(x = RR, y = dif)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-1, 1) * .25, linetype = "dashed") +
  facet_grid(distRR~Var)

#-----------------------
# Convenience Sample SMDs
#-----------------------

df.select <- df.dist %>%
  select(DID, one_of(dist.respNames)) %>%
  gather(key = "RR", value = "dPS", -DID)

df.select <- df.sch %>%
  select(DID, SID, DSID, one_of(sch.respNames)) %>%
  gather(key = "RR", value = "sPS", -DID, -SID, -DSID) %>%
  left_join(df.select)


sampleCS <- function(data, n = 60) {
  data <- data %>%
    mutate(Ej = genE(dPS),
           Eij = ifelse(Ej == 1, genE(sPS), 0)) %>%
    group_by(RR) %>%
    arrange(RR, -Eij, -dPS, -sPS) %>%
    mutate(Rank = 1:n(),
           Select = Rank <= n) %>%
    ungroup() %>%
    arrange(DSID)

  return(data$Select)

}

#create cluster
library(parallel)
cl <- makeCluster(detectCores()-1)  
#get library support needed to run the code
clusterEvalQ(cl, library(dplyr))
#put objects in place that might be needed for the code
clusterExport(cl,c("sampleCS", "df.select", "genE"))
#... then parallel replicate...
results <- parSapply(cl, 1:100, function(i,...) { sampleCS(df.select) } )
#stop the cluster
stopCluster(cl)


results <- replicate(10, sampleCS(df.select))

save(results, file = "Params/CS Selection2.rData")


load("Params/CS Selection2.rData")

sPlot <- df.select %>%
  ungroup() %>%
  arrange(DSID) %>%
  mutate(selectRate = apply(results, 1, mean))
  
sPlot %>%
  filter(selectRate > 0) %>%
  ggplot(aes(x = selectRate)) +
  geom_histogram() +
  facet_wrap(~ RR, scales = "free_x")

sPlot %>%
  group_by(RR) %>%
  summarise(mean(selectRate == 0))

#-----------------------
# Run Cluster Analysis
#-----------------------
library(cluster)

K = 10

IVs <- c("n", "urbanicity", "pED", "pMin", "pELL")



# distance <- daisy(x = df[,IVs], metric = "gower")
# 
# clusters <- list()
# 
# for(i in 1:K) {
# 
#   clusters <- append(clusters, list(kmeans(distance, i)))
# }
# 
# save(clusters, file = "Params/clusters10.rData")
# 
# beepr::beep(1)
# beepr::beep(1)
# beepr::beep(1)

load("Params/clusters10.rData")

classign <- data.frame(sapply(clusters, function(x) x$cluster))
names(classign) <- paste("k", 1:K, sep = "")

df <- cbind(df, classign)

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


df$cluster <- df$k6

#-----------------------
# Generate Within Cluster Ranks
#-----------------------

ranks <- df %>%
  select(DSID, cluster, IVs[-2]) %>%
  gather(key = "Var", value = "Val", -DSID, -cluster) %>%
  group_by(Var) %>%
  mutate(SD = sd(Val, na.rm = T),
         zVal = (Val - mean(Val, na.rm = T)) / SD) %>%
  group_by(Var, cluster) %>%
  mutate(dist = (zVal - mean(zVal, na.rm = T))^2) %>%
  group_by(DSID, cluster) %>%
  summarise(dist = sqrt(sum(dist))) %>%
  arrange(dist) %>%
  group_by(cluster) %>%
  mutate(rank = 1:n(),
         rankp = (1:n())/n() * 100)


df <- merge(df, ranks)
df$cluster <- factor(df$cluster, ordered = F)

df %>%
  mutate(urbanicity = ifelse(urbanicity == "Rural" | urbanicity == "Town", "ToRu", as.character(urbanicity))) %>%
  ggplot(aes(x = pED, y = pMin, color = cluster, alpha = (1 - rankp)/100)) +
  facet_wrap(~urbanicity) +
  geom_point()

df %>%
  mutate(urbanicity = ifelse(urbanicity == "Rural" | urbanicity == "Town", "ToRu", as.character(urbanicity))) %>%
  ggplot(aes(x = pMin, y = pELL, color = pED, alpha = (1 - rankp)/100)) +
  facet_grid(urbanicity~cluster) +
  geom_point()

df %>%
  filter(rankp < 50) %>%
  mutate(urbanicity = ifelse(urbanicity == "Rural" | urbanicity == "Town", "ToRu", as.character(urbanicity))) %>%
  ggplot(aes(x = pED, y = pMin, color = cluster)) +
  facet_wrap(~urbanicity) +
  geom_point()

df %>%
  filter(rankp < 50) %>%
  mutate(urbanicity = ifelse(urbanicity == "Rural" | urbanicity == "Town", "ToRu", as.character(urbanicity))) %>%
  ggplot(aes(x = pMin, y = pELL, color = pED)) +
  facet_grid(urbanicity~cluster) +
  geom_point()

#-----------------------
# Export Data
#-----------------------


df <- merge(df, df.dist[, c("DID","PS10", "PS20", "PS30")])

df <- transmute(df.sch, DID = DID, SID = SID, schPS10 = PS10, schPS20 = PS20, schPS30 = PS30) %>%
  merge(df)

df <- rename(df, distPS10 = PS10, distPS20 = PS20, distPS30 = PS30)

save(df, file = "Data/simData.Rdata")
save(schGoal, distGoal, file = "Data/RGM Vars.Rdata")
