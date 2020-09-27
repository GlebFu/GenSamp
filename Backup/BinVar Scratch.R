rm(list = ls())

logit <- function(x) log(x / (1 - x)) 
expit <- function(x) exp(x) / (1 + exp(x))
genE <- function(ps) rbinom(length(ps), 1, prob = ps)


reps <- function(p, n) {
  x <- rnorm(n, logit(p), 1)
  e <- rnorm(n, 0, 10)
  ps1 <- expit(x)
  ps2 <- expit(x + e)
  
  E1 <- genE(ps1)
  E2 <- as.numeric(ps2 > .5)
  
  c(targetV = p * (1 - p),
    V1 = var(E1),
    V2 = var(E2),
    M1 = mean(E1),
    M2 = mean(E2),
    PS1 = mean(ps1),
    PS2 = mean(ps2))
}

p <- .5
rbind(bign = reps(p, 1000000), sim = apply(replicate(100, reps(p, 1000)), 1, mean))
