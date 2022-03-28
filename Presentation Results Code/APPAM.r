
# APPAM PLOTS

## B-index
```{r}
appam <- list()


appam$B.label <- tabs$B %>%
  ungroup() %>%
  filter(RR == 10) %>%
  mutate(RR = 5)  %>%
  arrange(M) %>%
  mutate(M = c(.53, .60, .7, .77, .9))

appam$B <- tabs$B %>%
  ggplot(aes(x = RR, y = M, color = Sampler, 
             linetype = Sampler, group = Sampler)) +
  geom_line(size = 1.5) +
  # geom_label(data = tabs$B.label, aes(label = Sampler)) +
  geom_label(data = appam$B.label,
             aes(label = Sampler), size = 4) +
  labs(y = lab.vars$B ,
       x = lab.vars$rr) +
  geom_hline(yintercept = .95, linetype = "dotted") +
  geom_hline(yintercept = .8, linetype = "dashed") +
  scale_x_continuous(breaks = seq(0, 100, 10)) + 
  scale_y_continuous(limits = c(0.5, 1.0), expand = c(0,0)) +
  theme_apa() +
  scale_color_brewer(type = "qual", palette = 6) +
  theme(legend.position = "none",
        text = element_text(size = 15))

ggsave(plot = appam$B , filename = "Figs/APPAM 2020/B index.jpg", dpi = 1000, width = (6), height = (6))

```

## Sample
```{r}
appam$samples <- tabs$samples %>%
  filter(measure == "sch.contacted") %>%
  ggplot(aes(x = RR, y = value, group = Sampler, color = Sampler, linetype = Sampler)) +
  geom_line() +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  scale_y_continuous(breaks = seq(0, 4000, 500),
                     limits = c(0, 4000),
                     labels = function(x) x / 1000) +
  labs(x = lab.vars$rr,
       y = lab.vars$contacts) +
  geom_label(data = tabs$samples.contacts.labels,
             aes(label = Sampler),
             size = 4) +
  theme_apa() +
  scale_color_brewer(type = "qual", palette = 6) +
  theme(legend.position = "none",
        text = element_text(size = 15))

ggsave(plot = appam$samples, filename = "Figs/APPAM 2020/Contacts.jpg", dpi = 1000, width = (6), height = (6))
```

## Non-Response

```{r}
appam$nonresp.lab <-  tabs$samples %>%
  ungroup() %>%
  filter(measure == "sch.response.rate") %>%
  filter(RR == 10) %>%
  mutate(RR = 45) %>%
  # filter(RR == 10) %>%
  arrange(value) %>%
  mutate(value = c(93, 78, 65, 47, 30))



appam$nonresp <- tabs$samples %>%
  filter(measure == "sch.response.rate") %>%
  mutate(value = (1-value) * 100) %>%
  ggplot(aes(x = RR, y = value, group = Sampler, color = Sampler, linetype = Sampler)) +
  geom_line(size = 1.5) +
  geom_abline(slope = -1, intercept = 100, linetype = "dotted", alpha = .5) +
  expand_limits(y = 0) +
  theme(legend.position = "none") +
  labs(x = "Generated Participation Rate (%)",
       y = "Nonresponse Rate (%)") +
  scale_x_continuous(breaks = seq(0, 100, 10), limits = c(5, 90)) +
  scale_y_continuous(breaks = seq(10, 100, 15), limits = c(5, 100)) +
  geom_label(data = appam$nonresp.lab,
             aes(label = Sampler)) +
  theme_apa() +
  scale_color_brewer(type = "qual", palette = 6) +
  theme(legend.position = "none",
        text = element_text(size = 15))


ggsave(plot = appam$nonresp, filename = "Figs/APPAM 2020/Non Response.jpg", dpi = 1000, width = (6), height = (6))

```

## Combined

```{r}
appam$combined1 <- plot_grid(plotlist = list(appam$B + labs(x = ""),
                                             appam$samples),
                             ncol = 1, 
                             nrow = 2, 
                             label_x = .5,
                             align = "v",
                             label_fontface = "plain") 


ggsave(plot = appam$combined1, filename = "Figs/APPAM 2020/Combined1.jpg", dpi = 1000, width = (7), height = (6))


appam$combined2 <- plot_grid(plotlist = list(appam$B + labs(x = ""),
                                             appam$nonresp),
                             ncol = 1, 
                             nrow = 2, 
                             label_x = .5,
                             align = "v",
                             label_fontface = "plain") 


ggsave(plot = appam$combined2, filename = "Figs/APPAM 2020/Combined2.jpg", dpi = 1000, width = (7), height = (6))
```

## Individual

```{r}

tfun <- function(x) {
  x +
    theme_apa() + 
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    scale_color_brewer(type = "qual", palette = 6) 
} 

legend_plot <- function(x) {
  x +
    theme_apa() + 
    theme(legend.position = "none",
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_blank()) +
    labs(x = "", y = "") +
    scale_x_continuous(labels = rep("", 5), breaks = 1:5) +
    scale_y_continuous(labels = rep("", 5), breaks = 1:5) +
    scale_color_brewer(type = "qual", palette = 6) 
  
}



appam$smd.list <- list(plot_grid(plotlist = list(tfun(figs$smd.Group1), 
                                                 tfun(figs$smd.Group2)), 
                                 labels = c("Group 1", "Group 2"), 
                                 ncol = 2, 
                                 nrow = 1, 
                                 rel_heights = c(1, 1),
                                 label_x = .2,
                                 align = "h",
                                 label_fontface = "plain" ,
                                 label_size = 10), 
                       plot_grid(plotlist = list(tfun(figs$smd.Group3), legend_plot(figs$smd.label)),
                                 labels = c("Group 3", ""), 
                                 ncol = 2, 
                                 nrow = 1, 
                                 rel_heights = c(1, 1),
                                 label_x = .2,
                                 align = "h",
                                 label_fontface = "plain" ,
                                 label_size = 10)
                       
)

appam$smd <- plot_grid(plotlist = appam$smd.list,
                       ncol = 1, 
                       nrow = 2, 
                       rel_heights = c(1, 1),
                       label_x = .5,
                       align = "v",
                       label_fontface = "plain" ,
                       label_size = 10,
                       scale = .9) 

appam$smd  + #perhaps reduce this for a bit more space
  draw_label(lab.vars$rr,
             x= 0.5,
             y = 0,
             vjust = -0.25,
             angle = 0) +
  draw_label(lab.vars$smd,
             x = 0,
             y = 0.5,
             vjust = 1.5,
             angle = 90)


ggsave( filename = "Figs/APPAM 2020/SMD.jpg", dpi = 1000, width = (8), height = (6))
```