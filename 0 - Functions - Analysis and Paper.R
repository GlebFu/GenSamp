
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

common_line <- function(x, ...) {
  x + 
    geom_line(aes(color = Sample,
                  group = Method_Label,
                  ...))
}
  
common_point <- function(x, ...) {
  x +
    geom_point(aes(color = Sample,
                   shape = Cluster),
               ...) +
    scale_shape_manual(values = c(1, 16))
}

plot_smd <- function(data, l.pos = "right", g.title = waiver(), x.title = waiver(), y.title = waiver()) {
  data %>% 
    ggplot(aes(x = RR, y = Value,
               alpha = alpha.group)) %>%
    common_line() %>%
    common_point() +
    geom_hline(yintercept = c(.25), linetype = "dotted") +
    geom_hline(yintercept = 0, linetype = "solid") +
    labs(y = y.title,
         x = x.title,
         title = g.title) + 
    scale_x_continuous(breaks = seq(10, 90, 20),
                       limits = c(10, 100)) +
    scale_alpha_continuous(guide = "none", range = c(.5, 1)) +
    theme(legend.position = l.pos) +
    facet_wrap(~SF_fac, nrow = 1) 
}

plot_smd2 <- function(data, l.pos = "right", g.title = waiver(), x.title = waiver(), y.title = waiver()) {
  data %>% 
    ggplot(aes(x = RR, y = Value,
               alpha = alpha.group)) %>%
    common_line() %>%
    common_point() +
    geom_hline(yintercept = c(.25), linetype = "dotted") +
    geom_hline(yintercept = 0, linetype = "solid") +
    labs(y = y.title,
         x = x.title,
         title = g.title) + 
    scale_x_continuous(breaks = seq(10, 90, 20),
                       limits = c(10, 100)) +
    scale_alpha_continuous(guide = "none", range = c(.5, 1)) +
    theme(legend.position = l.pos) +
    facet_wrap(~SF_fac + Group, nrow = 1) 
}

plot_smd3 <- function(data, l.pos = "right", g.title = waiver(), x.title = waiver(), y.title = waiver()) {
  data %>% 
    ggplot(aes(x = RR, y = Value,
               alpha = alpha.group)) %>%
    common_line() %>%
    common_point() +
    geom_hline(yintercept = c(.25), linetype = "dotted") +
    geom_hline(yintercept = 0, linetype = "solid") +
    labs(y = y.title,
         x = x.title,
         title = g.title) + 
    scale_x_continuous(breaks = seq(10, 90, 20),
                       limits = c(10, 100)) +
    scale_alpha_continuous(guide = "none", range = c(.5, 1)) +
    theme(legend.position = l.pos) +
    facet_wrap(~SF_fac + Group, nrow = 1) 
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

smd.limits <- 
  function(x)  {
    newlim <- c(-.1, ceiling(max(x) * 10) / 10)
    newlim
  }

smd.break <- 
  function(x) {
    x.max <- max(smd.limits(x))
    x.break <- ceiling(x.max * 2) / 10
    newbreak <- seq(0, x.max, x.break)
    newbreak
  }