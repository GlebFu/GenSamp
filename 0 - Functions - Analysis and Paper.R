
######################
# SUPPORT FUNCTIONS
######################


boxErrors <- function(x) {
  v <- c(min(x), mean(x) - sd(x), mean(x), mean(x) + sd(x), max(x))
  names(v) <- c("ymin", "lower", "middle", "upper", "ymax")
  v
}

save.figures <- function(x, w = 10, h = 6, ...) ggsave(filename = paste("Figs/Paper/", x, ".jpg", sep = ""), width = w, height = h, ...)

row_groups <- function(x, blank = F) { 
  index <- table(x)[unique(x)]
  
  if(blank){names(index) <- lapply(1:length(index), function(x) rep(" ", x) %>% paste(collapse = ""))}
  
  return(index)
}

###################
# PLOT FUNCTIONS
###################

plot_smd <- function(data, yvar = "mSMD", l.pos = "bottom", g.title) {
  data %>% 
    ggplot(aes(x = RR, y = get(yvar), 
               color = Sampler, 
               linetype = Sampler, 
               group = Sampler)) +
    geom_line(size = 1) +
    geom_hline(yintercept = .25, linetype = "dotted") +
    facet_wrap( ~ Variables, ncol = 3) +
    labs(y = lab.vars$smd,
         x = lab.vars$rr,
         title = g.title) + 
    scale_x_continuous(breaks = seq(10, 90, 20)) +
    theme(legend.position = l.pos)
}

plot_smd2 <- function(data, yvar = "mSMD", l.pos = "right", g.title) {
  data %>% 
    ggplot(aes(x = RR, y = get(yvar), 
               color = Sample, 
               linetype = Cluster, 
               group = Sampler)) +
    geom_line(size = 1) +
    geom_hline(yintercept = c(.25), linetype = "dotted") +
    geom_hline(yintercept = 0, linetype = "solid") +
    labs(y = lab.vars$smd,
         x = lab.vars$rr,
         title = g.title) + 
    scale_x_continuous(breaks = seq(10, 90, 20),
                       limits = c(10, 100)) +
    theme(legend.position = l.pos) +
    facet_wrap(~SB, nrow = 1) 
}

apa_style_plot <- function(x) {
  x +
    theme_apa() + 
    (if (grey.plots) {scale_colour_grey(start = 0, end = .7)}) +
    guides(linetype = guide_legend(ncol = 1, title.position = "top"),
           color = guide_legend(ncol = 1, title.position = "top")) +
    theme(legend.position = "right") +
    scale_color_brewer(type = "qual", palette = 6)
}