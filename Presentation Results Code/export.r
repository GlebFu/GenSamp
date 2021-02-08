# Export Plots

```{r}
# figs
# 
# # i <- 12
# 
# for(i in 1:length(figs)) {
#   
#   print.fig <- figs[[i]] +
#     scale_color_brewer(type = "qual", palette = 2) +
#     theme_bw() +
#     theme(text = element_text(size = 20)) 
#   
#   f.name <- paste("Figs/Presentation/Fig ", i, " - ", names(figs)[i],".jpg", sep = "")
#   
#   ggsave(plot = print.fig, filename = f.name, dpi = 1000, width = (6*1.6), height = (6))
# }

```

```{r}
# library(cowplot)
# 
# fig.SCS <- list(
#   figs$smd.ell +
#     scale_color_brewer(type = "qual", palette = 2) +
#     theme_bw() +
#     theme(text = element_text(size = 10),
#           legend.position = "none",
#           axis.title.x = element_blank(),
#           axis.ticks = element_blank(),
#           axis.text.x = element_blank()) +
#     labs(y = "SMD"),
#   figs$B +
#     scale_color_brewer(type = "qual", palette = 2) +
#     theme_bw() +
#     theme(text = element_text(size = 10),
#           legend.position = "right",
#           axis.title.x = element_blank(),
#           axis.ticks = element_blank(),
#           axis.text.x = element_blank()) +
#     labs(y = "B-Index"),
#   figs$samples.contacts +
#     scale_color_brewer(type = "qual", palette = 2) +
#     theme_bw() +
#     theme(text = element_text(size = 10),
#           legend.position = "none") +
#     labs(y = "Contacts") +
#     scale_y_continuous(breaks = seq(0, 4000, 1000),
#                        limits = c(0, 4000))
# )
# 
# plot_grid(plotlist = fig.SCS, 
#           ncol = 1, 
#           nrow = 3, 
#           rel_heights = c(1, 1, 1.3),
#           label_x = .5,
#           align = "v",
#           axis = "lr",
#           label_fontface = "plain" ,
#           label_size = 10)

# ggsave(filename = "Figs/Presentation/Fig Middle.jpg", dpi = 1000, width = (6*1.6), height = (6))
```