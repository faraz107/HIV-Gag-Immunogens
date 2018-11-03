library(tidyverse)
library(ggpubr)
library(RColorBrewer)
library(grid)
library(magick)
library(ComplexHeatmap)

pal2 <- brewer.pal(3, "Set1")[2:1]

g3 <- ggplot(PC_roca, aes(x = eigIndx, y = "1", fill = if_else(condition = (abs(eigVec3) > 0.05), true = TRUE, false = FALSE)))
g3 <- g3 + scale_y_discrete(expand = c(0,0), labels = c(expression(paste("PC3", (lambda[3])))))
g3 <- g3 + ylab("PC3")
g3 <- g3 + geom_tile(color = "grey90", size = 0.2)
g3 <- g3 + scale_fill_manual(values = c("white", pal2[1]))
xtcks <- c(25, 73, 122, 169, 211, 252, 286, 331, 379, 426)
g3 <- g3 + scale_x_continuous(breaks = xtcks, labels = true_indices$GAGIndx[true_indices$numIndx %in% xtcks], expand = c(0,0))
g3 <- g3 + theme_minimal() + theme(legend.position = "none") + theme(strip.background = element_blank())
g3 <- g3 + theme(axis.ticks.x = element_blank()) + theme(panel.grid = element_blank()) 
g3 <- g3 + theme(axis.title.x = element_blank())
g3 <- g3 + theme(axis.title.y = element_text(face = "bold", size = 7, vjust = 0.5, hjust = 0.5, angle = 0))
g3 <- g3 + theme(axis.text.y = element_blank())
g3 <- g3 + theme(axis.text.x = element_text(face = "bold", size = 7, colour = "white"))
g3 <- g3 + theme(axis.ticks.x = element_line(linetype = "solid", colour = "black")) 

spl <- setdiff((all_secs %>% filter(case=="PCs9" & sec=="sec7"))$site, (all_secs %>% filter(case=="PCs9" & sec=="sec3"))$site)
g7 <- ggplot(PC_roca, aes(x = eigIndx, y = "1", fill = case_when(eigIndx %in% spl ~ "2", abs(eigVec7) > 0.05 ~ "1", TRUE ~ "0")))
g7 <- g7 + scale_y_discrete(expand = c(0,0), labels = c(expression(paste("PC7", (lambda[7])))))
g7 <- g7 + geom_tile(color = "grey90", size = 0.2)
g7 <- g7 + ylab("PC7")
g7 <- g7 + scale_fill_manual(values = c("white", pal2[1], pal2[2]), breaks = c(1, 2), label = c("Sites represented either only in PC3 or in both PC3 and PC7", "Sites represented only in PC7"))
xtcks <- c(25, 73, 122, 169, 211, 252, 286, 331, 379, 426)
g7 <- g7 + scale_x_continuous(breaks = xtcks, labels = true_indices$GAGIndx[true_indices$numIndx %in% xtcks], expand = c(0,0))
g7 <- g7 + theme_minimal() + theme(legend.position = "bottom") + theme(strip.background = element_blank())
g7 <- g7 + theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank(), legend.text = element_text(size = 9), legend.text.align = 0.5, legend.spacing.x = unit(5, 'mm'))
g7 <- g7 + theme(axis.ticks.y = element_blank()) + theme(panel.grid = element_blank())
g7 <- g7 + theme(axis.title.x = element_blank())
g7 <- g7 + theme(axis.title.y = element_text(face = "bold", size = 7, vjust = 0.5, hjust = 0.5, angle = 0))
g7 <- g7 + theme(axis.text.y = element_blank())
g7 <- g7 + theme(axis.text.x = element_text(face = "bold", size = 7))
g7 <- g7 + theme(axis.ticks.x = element_line(linetype = "solid", colour = "black")) 

tmp_G <- grid.grabExpr(
  {draw(object = G, gap = unit(c(bu), "mm"), heatmap_legend_side = "right", show_heatmap_legend = TRUE, annotation_legend_side = c("bottom"), show_annotation_legend = TRUE)  
  }
)

gempty <- ggplot(data = PC_roca) + theme_transparent()

fig_tmp <- ggarrange(ggarrange(plotlist = list(gempty, GT, gempty), nrow = 3, ncol = 1, heights = c(1,4,1)), gempty, tmp_G, labels = c("A","", "B"),  hjust = c(-0.5,-0.5,1.5), nrow = 1, ncol = 3, widths = c(1, 0.15, 2.5), align = "hv")

fig_tmp <- annotate_figure(fig_tmp, top = text_grob("Model based correlation matrices", face = "bold", size = 14))

ggsave(filename = "Figures/Fig2.png", plot = fig_tmp, width = 6, height = 4.5, units = "in", dpi=600)

grobpdb2 <- rasterGrob(image_read("Figures/hexamer1.png"), just = "center")
grobpdb3 <- rasterGrob(image_read("Figures/pentamer1.png"), just = "center")

fig_tmp <- ggarrange(evdg, gempty, g3, g7, gempty,
                     ggarrange(grobpdb2, grobpdb3, ncol = 2, nrow = 1),
                     nrow = 6, ncol = 1, labels=c("A", "", "B", "", "", "C"), 
                     font.label = list(size=9), vjust = c(1, 0, 0, 0, 0, 0), heights = c(3, 0.25, 0.75, 1.5, 0.25, 5))

ggsave(filename = "Figures/Fig1.png", plot = fig_tmp, height = 7.5, width = 7.5, units = "in", dpi=600)


