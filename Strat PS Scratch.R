rm(list = ls())

library(tidyverse)

logit <- function(x) log(x / (1 - x)) 
expit <- function(x) exp(x) / (1 + exp(x))


N <- 10000
k <- 5
n <- N/20
nj <- n/k

df <- data.frame(strata = rep(1:k, each = N/k)) %>%
  mutate(mx = strata - (k + 1) / 2,
         x = rnorm(n = n(), mean = mx),
         PS = expit(x),
         Ej = rbinom(n = n(), size = 1, prob = PS)) %>%
  as_tibble

df %>%
  ggplot(aes(x = PS)) +
  geom_density() +
  facet_grid(~ strata)

df.ucs <- df %>%
  sample_frac(size = 1, weight = PS) %>%
  mutate(contacted = cumsum(Ej) <= n,
         accepted = contacted  & Ej == 1,
         sample_method = "U_CS")

df.scs <- df %>%
  group_by(strata)  %>%
  sample_frac(size = 1, weight = PS) %>%
  mutate(contacted = cumsum(Ej) <= nj,
         accepted = contacted  & Ej == 1,
         sample_method = "S_CS")


df.samp <- bind_rows(df.ucs, df.scs)

df.samp %>%
  filter(accepted) %>%
  ggplot() +
  geom_density(aes(x = PS, fill = sample_method), alpha = .1) +
  geom_density(aes(x = PS), data = df) +
  facet_grid(strata ~ .,
             scales = "free_y")
