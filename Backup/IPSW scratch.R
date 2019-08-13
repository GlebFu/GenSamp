rm(list = ls())

library(tidyverse)

logit <- function(x) log(x / (1 - x)) 
expit <- function(x) exp(x) / (1 + exp(x))

stand <- function(x) (x - mean(x)) / sd(x)

gen_dat <- function(n, m1, m2, s1, s2, p, b1, b2) {
  data.frame(x1 = rnorm(n, m1, s1)) %>%
    mutate(x2 = rnorm(n, m2 + p * x1, s2),
           x1_z = stand(x1),
           x2_z = stand(x2),
           PS = (cbind(x1_z, x2_z) %*% c(b1, b2)) %>% expit,
           w1 = 1 / PS,
           w0 = 1 / (1 - PS))
}

calc_ipsw <- function(df.data) {
  df.data %>%
    gather(key = type, value = weight, w1:w0, PS) %>%
    gather(key = var, value = val, -type, -weight) %>%
    group_by(var, type) %>%
    summarise(m = weighted.mean(val, weight)) %>%
    ungroup()
}


#-------------------
# Example
#-------------------

# Generate Data
df.ex <- gen_dat(n = 1000,
                 m1 = 1,   # Mean of x1
                 m2 = 1,   # Mean of x2
                 s1 = 1,   # SD of x1
                 s2 = 1,   # SD of x2
                 p = 0,    # Manipulate correlation between x1 and x2
                 b1 = 1,   # x1 coeficient (target smd)
                 b2 = 1)   # x2 coefficent (target smd)


# Correlation between x1 and x2
cor(df.ex$x1, df.ex$x2)

# Summary statistics
df.ex %>%
  gather(var, value) %>%
  group_by(var) %>%
  summarise(m = mean(value) %>% round(2),
            sd = sd(value) %>% round(2))

# Calculate SMDs using IPSW and PS as weights
calc_ipsw(df.ex) %>%
  spread(type, m)

df.ex %>%
  gather(key = type, value = weight, w1:w0, PS) %>%
  gather(key = var, value = val, -type, -weight) %>%
  group_by(var, type) %>%
  summarise(m = weighted.mean(val, weight)) %>%
  ungroup() %>%
  spread(type, m)

#-------------------
# Simulation
#-------------------

df.cond <- list(n = 1000, 
     m1 = c(1), 
     m2 = c(1),
     s1 = c(1),
     s2 = c(1),
     p = c(0),
     b1 = (1:20) / 10,
     b2 = c(1)) %>%
  expand.grid %>%
  as_tibble()


df.data <- df.cond %>%
  mutate(generated_data = pmap(., gen_dat))


df.results <- df.data %>%
  mutate(results = map(generated_data, calc_ipsw)) %>%
  unnest(results) %>%
  mutate(transform = str_detect(var, "z") %>% if_else("Z", "None"),
         var = str_split(var, "_", simplify = T)[,1],
         B = ifelse(var == "x1", b1 ,b2),
         dif = m - B)


df.results %>%
  ggplot(aes(x = b1, y = dif, color = type, linetype = transform)) +
  geom_line() +
  facet_grid(p ~ var) +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed")


df.results %>%
  ggplot(aes(x = b1, y = dif, color = var, linetype = as.factor(p))) +
  geom_line() +
  facet_grid(type ~ transform) +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed")


df.results %>%
  filter(p == 0) %>%
  ggplot(aes(x = b1, y = dif, color = var, linetype = transform)) +
  geom_line() +
  facet_grid( ~ type) +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed")


