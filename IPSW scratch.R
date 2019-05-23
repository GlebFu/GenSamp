rm(list = ls())

library(tidyverse)
library(MASS)


logit <- function(x) log(x / (1 - x)) 
expit <- function(x) exp(x) / (1 + exp(x))

n <- 10000


Bs <- c(.5, .1)


# Variables are independant

df <- data.frame(x1 = rnorm(n),
              x2 = rnorm(n)) %>%
  mutate(x1 = (x1 - mean(x1)) / sd(x1),
         x2 = (x2 - mean(x2)) / sd(x2),
         PS = cbind(x1, x2) %*% Bs %>% expit,
         w1 = 1 / PS,
         w0 = 1 / (1 - PS))

df %>%
  gather(key = type, value = weight, w1:w0) %>%
  gather(key = var, value = val, -PS, -type, -weight) %>%
  group_by(var, type) %>%
  summarise(m = weighted.mean(val, weight),
            mps = weighted.mean(val, PS))

df %>%
  mutate(Tx = rbinom(n(), 10000, PS)) %>%
  summarise(x1 = sum(x1 * Tx) / sum(Tx),
            x2 = sum(x2 * Tx) / sum(Tx))



# Variables are correlated

df <- data.frame(x1 = rnorm(n)) %>%
  mutate(x2 = rnorm(n, x1 * .1)) %>%
  mutate(x1 = (x1 - mean(x1)) / sd(x1),
         x2 = (x2 - mean(x2)) / sd(x2),
         PS = cbind(x1, x2) %*% Bs %>% expit,
         w1 = 1 / PS,
         w0 = 1 / (1 - PS))

df %>%
  gather(key = type, value = weight, w1:w0) %>%
  gather(key = var, value = val, -PS, -type, -weight) %>%
  group_by(var, type) %>%
  summarise(m = weighted.mean(val, weight),
            mps = weighted.mean(val, PS))

df %>%
  mutate(Tx = rbinom(n(), 10000, PS)) %>%
  summarise(x1 = sum(x1 * Tx) / sum(Tx),
            x2 = sum(x2 * Tx) / sum(Tx))
