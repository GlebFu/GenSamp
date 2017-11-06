library(dplyr)

rm(list = ls())

source("ParGenSource.r")

genE <- function(ps) rbinom(length(ps), 1, prob = ps)

#-----------------------
# DATA PREP
#-----------------------

# Read Data
df <- read.csv("Data/final data.csv", stringsAsFactors = F)


# Create ELL and ED variable district data when school is unavailable
# Create Minority variable
# Create Town/Rural Variable
df <- mutate(df,
             pELL = ifelse(is.na(pELL), pELL_D, pELL), 
             pED = ifelse(is.na(pED), pTotfrl, pED),
             pMin = 1-ethWhite,
             ToRu = Town + Rural,
             MEDINC = STAND(MEDINC),
             DID = as.numeric(as.factor(LEAID)) + 1000) %>%
  group_by(DID) %>%
  mutate(SID = 1:n() + 10000) %>%
  ungroup() %>%
  mutate(DSID = paste(DID, SID, sep = "-"))

vars <- c("LSTATE", "LEANM", "SCHNAM", "DID", "SID", "DSID", "n", "ULOCAL", "pTotfrl", "pIEP_D", "pELL_D", 
          "Urban", "Suburban", "ToRu", "schPrimary", "schMIDDLE", "schHigh", 
          "schOther", "pELL", "pED", "pELA", "pMath", "pMin", "MEDINC")

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
df.dist <- select(df, -SCHNAM, -LSTATE, -LEANM) %>%
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
# resp30 <- .3
# resp20 <- .2
# resp10 <- .1
# 
# bs_RR30 <- optim(par = distBs, fn = testGoal,
#                  MRR = resp30, vars = distVars,
#                  data = df.dist, exclude = distEx, goal = distGoal) %>%
#   c(resp = resp30)
# 
# bs_RR20 <- optim(par = distBs, fn = testGoal,
#                  MRR = resp20, vars = distVars,
#                  data = df.dist, exclude = distEx, goal = distGoal) %>%
#   c(resp = resp20)
# 
# bs_RR10 <- optim(par = distBs, fn = testGoal,
#                  MRR = resp10, vars = distVars,
#                  data = df.dist, exclude = distEx, goal = distGoal) %>%
#   c(resp = resp10)
# 
# distVals <- rbind(getVals(bs_RR30, vars = distVars, data = df.dist, exclude = distEx, goal = distGoal),
#                   getVals(bs_RR20, vars = distVars, data = df.dist, exclude = distEx, goal = distGoal),
#                   getVals(bs_RR10, vars = distVars, data = df.dist, exclude = distEx, goal = distGoal))
# 
# df.dist$PS30 <- calcPS(Bs = bs_RR30$par, MRR = bs_RR30$resp, vars = distVars, data = df.dist, exclude = distEx)
# df.dist$PS20 <- calcPS(Bs = bs_RR20$par, MRR = bs_RR20$resp, vars = distVars, data = df.dist, exclude = distEx)
# df.dist$PS10 <- calcPS(Bs = bs_RR10$par, MRR = bs_RR10$resp, vars = distVars, data = df.dist, exclude = distEx)
# 
<<<<<<< HEAD
# save.image("Params/171031.rdata")

load("Params/171031.rdata")

#-----------------------
=======
# save.image("Params/171103.rdata")
# 
# 
# 
# 
# 
# distVals <- rbind(getVals(bs_RR30, vars = distVars, data = df.dist, exclude = distEx, goal = distGoal),
#                   getVals(bs_RR20, vars = distVars, data = df.dist, exclude = distEx, goal = distGoal),
#                   getVals(bs_RR10, vars = distVars, data = df.dist, exclude = distEx, goal = distGoal))
# 
# df.dist$PS30 <- calcPS(Bs = bs_RR30$par, MRR = bs_RR30$resp, vars = distVars, data = df.dist, exclude = distEx)
# df.dist$PS20 <- calcPS(Bs = bs_RR20$par, MRR = bs_RR20$resp, vars = distVars, data = df.dist, exclude = distEx)
# df.dist$PS10 <- calcPS(Bs = bs_RR10$par, MRR = bs_RR10$resp, vars = distVars, data = df.dist, exclude = distEx)

# RR_PS <- df.dist %>%
#   select(PS30:PS10) %>%
#   gather(key = "RR", value = "PS")
# 

# 
# reps <- 10000
# RR_PS$selectRate <- replicate(reps, distSelect(RR_PS)) %>% apply(1, sum)/reps 
#   
# RR_PS %>%
#   filter(selectRate > .5) %>%
#   ggplot(aes(x = selectRate, fill = RR)) +
#   geom_histogram() +
#   facet_wrap(~ RR)
# 
# RR_PS %>%
#   ggplot(aes(x = selectRate, y = PS, color = selectRate)) +
#   geom_point()

###################
>>>>>>> e040028599937f6755ede435ff0cb0ec4a6d74fb
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

# resp30 <- .3
# resp20 <- .2
# resp10 <- .1
# 
# schBs_RR30 <- optim(par = schBs, fn = testGoal,
#                  MRR = resp30, vars = schVars,
#                  data = df.sch, exclude = schEx, goal = schGoal) %>%
#   c(resp = resp30)
# 
# schBs_RR20 <- optim(par = schBs, fn = testGoal,
#                  MRR = resp20, vars = schVars,
#                  data = df.sch, exclude = schEx, goal = schGoal) %>%
#   c(resp = resp20)
# 
# schBs_RR10 <- optim(par = schBs, fn = testGoal,
#                  MRR = resp10, vars = schVars,
#                  data = df.sch, exclude = schEx, goal = schGoal) %>%
#   c(resp = resp10)
# 
# 
# 
# 
# 
# 
# schVals <- rbind(getVals(schBs_RR30, vars = schVars, data = df.sch, exclude = schEx, goal = schGoal),
#                   getVals(schBs_RR20, vars = schVars, data = df.sch, exclude = schEx, goal = schGoal),
#                   getVals(schBs_RR10, vars = schVars, data = df.sch, exclude = schEx, goal = schGoal))
# 
# df.sch$PS30 <- calcPS(Bs = schBs_RR30$par, MRR = schBs_RR30$resp, vars = schVars, data = df.sch, exclude = schEx)
# df.sch$PS20 <- calcPS(Bs = schBs_RR20$par, MRR = schBs_RR20$resp, vars = schVars, data = df.sch, exclude = schEx)
# df.sch$PS10 <- calcPS(Bs = schBs_RR10$par, MRR = schBs_RR10$resp, vars = schVars, data = df.sch, exclude = schEx)
# 
# 
# save.image("Params/171103.rdata")

load("Params/171103.rdata")


#-----------------------
# Convenience Sample SMDs
#-----------------------

# df.select <- df.dist %>% 
#   select(DID, PS30:PS10) %>% 
#   gather(key = "RR", value = "dPS", PS30:PS10)
# 
# df.select <- df.sch %>%
#   select(DID, SID, DSID, PS30:PS10) %>%
#   gather(key = "RR", value = "sPS", PS30:PS10) %>%
#   merge(df.select)
# 
# 
# sampleCS <- function(data, n = 60) {
#   data <- data %>% 
#     mutate(Ej = genE(dPS), 
#            Eij = ifelse(Ej == 1, genE(sPS), 0)) %>%
#     group_by(RR) %>%
#     arrange(RR, -Eij, -dPS, -sPS) %>%
#     mutate(Rank = 1:n(),
#            Select = Rank <= n) %>%
#     ungroup() %>%
#     arrange(DSID)
#   
#   return(data$Select)
#   
# }
# 
# results <- replicate(10000, sampleCS(df.select))
# 
# write.csv(results, "Params/CS Selection.csv")
# 
# results <- read.csv("Params/CS Selection.csv")
# 
# df.select %>%
#   ungroup() %>%
#   arrange(DSID) %>%
#   mutate(selectRate = apply(results, 1, mean)) %>%
#   ggplot(aes(x = selectRate)) + 
#   geom_histogram() +
#   facet_wrap(~ RR)


#-----------------------
# Run Cluster Analysis
#-----------------------
library(cluster)

K = 10



# IVs <- c("urbanicity", "G08", "FTE", "LEVEL", "edp", "ellp", "spedp", "ethOther", "ethWhite", "ethBlack", "ethHisp", "ethAsian", "genMale", "genFemale")
# 
# # pop <- pop %>% filter(LEVEL == "schMIDDLE",
# #                       MAGNET == 2)
# 
# pop <- pop %>% filter(LEVEL == "schMIDDLE")
# 
# pop$region <- factor(pop$region)
# 
# # distance <- daisy(x = cbind(pop[,IVs], pop[,IVs[-c(1,4)]]^2, pop[,IVs[-c(1,4)]]^3),
# #                   metric = "gower") #ORDER 3
# distance <- daisy(x = pop[,IVs], metric = "gower")    #ORDER 1
# 
# 
# clusters <- list()
# 
# for(i in 1:K) {
# 
#   clusters <- append(clusters, list(kmeans(distance, i)))
# }
# 
# classign <- data.frame(sapply(clusters, function(x) x$cluster))
# names(classign) <- paste("k", 1:K, sep = "")
# 
# pop <- cbind(pop, classign)
# 
# with(pop, table(LSTATE, k5))
# 
# save(pop, IVs, expandedIVs, clusters, file = "Clusters Order 1 Mid-Only.Rdata")


findDist <- function(pat) {
  pop[, c("LEANM", "LSTATE")][str_detect(pop$LEANM, ignore.case(pat)),] %>%
    group_by(LEANM, LSTATE) %>%
    summarise(length(LSTATE)) %>%
    data.frame()
}


findSch <- function(pat) {
  pop[, c("SCHNAM", "LSTATE")][str_detect(pop$SCHNAM, ignore.case(pat)),] %>%
    group_by(SCHNAM, LSTATE) %>%
    summarise(length(LSTATE)) %>%
    data.frame()
}

# #
# # findDist("austin")
# 
# 
# # Vratio <- function(clstr){
# #   Vw <- clstr$tot.withinss/(nrow(pop) - length(table(clstr$cluster)))
# #   Vb <- clstr$betweenss/(length(table(clstr$cluster)) - 1)
# #   Vb / (Vw + Vb)
# #
# # }

Vratio <- function(clstr){
  Vw <- clstr$tot.withinss
  Vb <- clstr$betweenss
  Vb / (Vw + Vb)
  
}

<<<<<<< HEAD
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

ggsave("Figures/Scree2.png", dpi = 500, width = 5, height = 5)


pop$cluster <- pop$k5

pop$schShrt <- str_sub(pop$SCHNAM, end = 10)




#-----------------------
# Export Data
#-----------------------


# df <- merge(df, df.dist[, c("DID","PS10", "PS20", "PS30")])
#   
# df <- transmute(df.sch, DID = DID, SID = SID, schPS10 = PS10, schPS20 = PS20, schPS30 = PS30) %>%
#   merge(df)
# 
# save(df, file = "Data/simData.Rdata")
=======
# results <- replicate(10000, sampleCS(df.select))
# 
# results <- data.frame(m = apply(results, 1, mean), sd = apply(results, 1, sd))
# 
# write.csv(results, "CS Selection.csv")

results <- read.csv("CS selection.csv")



df.select <- ungroup(df.select) %>%
  arrange(DSID) %>%
  mutate(m = results$m, 
         sd = results$sd,
         mGroups = factor(cut(m, breaks = seq(0,1,.05), labels = seq(0,.95,.05), right = T)))

# df.select %>% select(m, mGroups)
# 
# df.select %>%
#   ggplot(aes(x = mGroups, y = m)) +
#   geom_point() +
#   facet_wrap(~RR)

df.select %>%
  group_by(mGroups, RR) %>%
  summarise(n = n()) %>%
  arrange(-as.numeric(mGroups)) %>%
  mutate(nC = cumsum(n)) %>%
  group_by(RR) %>%
  arrange(RR) %>%
  mutate(p = n / sum(n),
         pC = cumsum(p)) %>% 
  filter(!is.na(mGroups)) %>%
  ggplot(aes(x = mGroups, y = pC)) +
  geom_bar(stat="identity") +
  geom_bar(aes(y = p), stat="identity", fill = "red") +
  facet_grid(RR~., scales = "free_y")


df <- df.select %>%
  select(DSID, RR, m) %>%
  spread(key = RR, value = m) %>%
  merge(df)


dMeans <- df[, c(schVars, "PS10", "PS20", "PS30")] %>%
  gather(key = wRR, value = weight, PS10:PS30) %>%
  gather(key = Var, value = val, -weight, -wRR) %>%
  group_by(wRR, Var) %>%
  summarise(wM = weighted.mean(val, weight)) %>%
  spread(key = wRR, value = wM)
  
df[, schVars] %>%
  gather(key = Var, value = Val) %>%
  group_by(Var) %>%
  summarise(m = mean(Val), sd = sd(Val)) %>%
  merge(dMeans) %>%
  mutate(SMD10 = (PS10 - m)/sd,
         SMD20 = (PS20 - m)/sd,
         SMD30 = (PS30 - m)/sd) %>%
  merge(data.frame(Var = names(schGoal), goal = schGoal))

>>>>>>> e040028599937f6755ede435ff0cb0ec4a6d74fb
