library(MASS)
library(ggplot2)
library(dplyr)
library(tidyr)

rm(list = ls())



n = 200
p = 4
i <- diag(rep(1,4))
bz <- c(210, 27.4, 13.7, 13.7, 13.7)
bp <- c(-1, .5, -.25, .1)
Z <- cbind(1, mvrnorm(n, rep(0,p), i))
e <- rnorm(n)
y <- Z %*% bz + e
pi <- t(exp(bp %*% t(Z[,-1])) / (1 + exp(bp %*% t(Z[,-1]))))

t <- as.numeric(runif(n,0,1) < pi)
# t <- as.numeric(rnorm(n) < pi)
# t <- as.numeric(.5 <= pi)

solve(t(Z) %*% Z) %*% t(Z) %*% y

(u <- mean(y))
(u1 <- mean(y[t == 1]))
(u0 <- mean(y[t == 0]))

X <- cbind(1, exp(Z[,2]/2), Z[,3]/(1 + exp(Z[,2])) + 10, (Z[,2] * Z[,4] / 25 + .6)^3, (Z[,3] + Z[,5] + 20)^2)

data.frame(y, t = as.factor(t), X[,-1]) %>% 
  gather(key = var, value = val, -(y:t)) %>%
  ggplot(aes(x = val,
             y = y,
             color = t)) +
  geom_point() +
  facet_wrap(~var, scales = "free_x")

bx <- solve(t(X) %*% X) %*% t(X) %*% y

# bp2 <- t(solve(t(X[,-1]) %*% X[,-1]) %*% t(X[,-1]) %*% t)
# pi2 <- t(exp(bp2 %*% t(X[,-1])) / (1 + exp(bp2 %*% t(X[,-1]))))

bp2 <- t(solve(t(X) %*% X) %*% t(X) %*% t)
pi2 <- t(exp(bp2 %*% t(X)) / (1 + exp(bp2 %*% t(X))))

summary(lm(y ~ X1 + X2 + X3 + X4, data.frame(y, X[,-1])[t == 1,]))

fyt <- Z %*% bz
fyf <- X %*% bx

cor(fyt, fyf)
cor(pi, pi2)

sum(t * (pi^-1) * y) / sum(t * (pi^-1)) - u
sum(t * (pi^-1) * (1 - pi) * y) / sum(t * (pi^-1) * (1 - pi)) - u0

sum(t * (pi2^-1) * y) / sum(t * (pi2^-1)) - u
sum(t * (pi2^-1) * (1 - pi2) * y) / sum(t * (pi2^-1) * (1 - pi2)) - u0

