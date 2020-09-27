
######################
# SUPPORT FUNCTIONS
######################

rename_conditions <- function(orig.data) {
  orig.data %>%
    separate(col = sample_method, into = c("Clustering", "Sampling"), remove = F)
}

boxErrors <- function(x) {
  v <- c(min(x), mean(x) - sd(x), mean(x), mean(x) + sd(x), max(x))
  names(v) <- c("ymin", "lower", "middle", "upper", "ymax")
  v
}

save.figures <- function(x, w = 10, h = 6, ...) ggsave(filename = paste("Figs/Paper/", x, ".jpg", sep = ""), width = w, height = h, ...)

###################
# PLOT FUNCTIONS
###################

plot_smd <- function(data) {
  data %>% 
    ggplot(aes(x = RR, y = mSMD, color = Sampling, linetype = Clustering, group = sample_method)) +
    geom_line(size = 1) +
    geom_hline(yintercept = .25, linetype = "dotted") +
    facet_wrap( ~ Variables, scales = "free", ncol = 3) +
    labs(y = lab.vars$smd,
         x = lab.vars$rr) + 
    scale_x_continuous(breaks = seq(10, 90, 20)) +
    theme(legend.position = "bottom")
}

plot_smd2 <- function(data) {
  data %>% 
    ggplot(aes(x = RR, y = mSMD, color = Sampling, linetype = Clustering, group = sample_method)) +
    geom_line(size = 1) +
    geom_hline(yintercept = .25, linetype = "dotted") +
    labs(y = lab.vars$smd,
         x = lab.vars$rr) + 
    scale_x_continuous(breaks = seq(10, 90, 20)) +
    theme(legend.position = "right")
}
