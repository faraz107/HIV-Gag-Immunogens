# Archive for ComplexHeatmap


# 
# decorate_heatmap_body(heatmap = "sec1", slice = 1, code = {
#   grid.rect(gp = gpar(fill = "transparent", col = "black", lwd = 2))
# })
# 
# decorate_heatmap_body(heatmap = "sec2", slice = 2, code = {
#   grid.rect(gp = gpar(fill = "transparent", col = "black", lwd = 2))
# })
# 
# decorate_heatmap_body(heatmap = c("sec3", "sec4"), slice = 3, code = {
#   grid.rect(gp = gpar(fill = "transparent", col = "black", lwd = 2))
# })
# 
# decorate_heatmap_body(heatmap = "sec4", slice = 4, code = {
#   grid.rect(gp = gpar(fill = "transparent", col = "black", lwd = 2))
# })
# 
# decorate_heatmap_body(heatmap = "sec5", slice = 5, code = {
#   grid.rect(gp = gpar(fill = "transparent", col = "black", lwd = 2))
# })
# 
# decorate_heatmap_body(heatmap = "sec6", slice = 6, code = {
#   grid.rect(gp = gpar(fill = "transparent", col = "black", lwd = 2))
# })
# 
# decorate_heatmap_body(heatmap = "sec7", slice = 7, code = {
#   grid.rect(gp = gpar(fill = "transparent", col = "black", lwd = 2))
# })
# 
# decorate_heatmap_body(heatmap = "sec8", slice = 8, code = {
#   grid.rect(gp = gpar(fill = "transparent", col = "black", lwd = 2))
# })
# 
# decorate_heatmap_body(heatmap = "sec9", slice = 9, code = {
#   grid.rect(gp = gpar(fill = "transparent", col = "black", lwd = 2))
# })
# 
# 



# 
# h1left = rowAnnotation(heatmap = row_anno_boxplot(C451sec[,1:24]))
# 
# H1 <- Heatmap(matrix = C451sec[,1:24], name = "sec1", col = colorRamp2(colors = colrs, breaks = brks), cluster_rows = FALSE, cluster_columns = FALSE, show_column_names = FALSE, column_names_side = "bottom", column_names_gp = gpar(col="white"), show_row_names = FALSE, rect_gp = gpar(col = "grey90"), split = secInds, combined_name_fun = NULL, gap = unit(bu, "mm"), show_heatmap_legend = FALSE, width = unit(bu*25, "mm"), raster_device = "png", raster_quality = 50, heatmap_legend_param = list(title="", labels_gp = gpar(fontsize = 14), legend_height = unit(75, "mm"), legend_direction = "horizontal"))



# 
# x <- abs(PC_roca9[sec_ind_1_9, c(1, 2, 3, 7, 4, 5, 6, 9, 8)])
# names(x) <- c("PC1", "PC2", "PC3", "PC7", "PC4", "PC5", "PC6", "PC9", "PC8")
# x <- t(as.matrix(x))
# 
# brksPC <- seq(min(x), max(x), by=1/25)
# colrsPC <- colorRampPalette(c("white", brewer.pal(n = 11, name = "Spectral")[11]))(13)
# 
# P1 <- Heatmap(matrix = x[,1:24], col = colorRamp2(colors = colrsPC, breaks = brksPC), cluster_rows = FALSE, cluster_columns = FALSE, show_column_names = FALSE, row_names_side = "left", row_names_gp = gpar(fontsize=9), show_row_names = FALSE, rect_gp = gpar(type = "none"), cell_fun = cell_fun2, combined_name_fun = NULL,show_heatmap_legend = FALSE, gap = unit(1, "mm"), split = seq(1,9))
# 
# P2 <- Heatmap(matrix = x[,25:38], col = colorRamp2(colors = colrsPC, breaks = brksPC), cluster_rows = FALSE, cluster_columns = FALSE, show_column_names = FALSE, row_names_side = "left", row_names_gp = gpar(fontsize=9), show_row_names = FALSE, rect_gp = gpar(type = "none"), cell_fun = cell_fun2, combined_name_fun = NULL,show_heatmap_legend = FALSE, gap = unit(1, "mm"), split = seq(1,9))
# 
# P3 <- Heatmap(matrix = x[,39:71], col = colorRamp2(colors = colrsPC, breaks = brksPC), cluster_rows = FALSE, cluster_columns = FALSE, show_column_names = FALSE, row_names_side = "left", row_names_gp = gpar(fontsize=9), show_row_names = FALSE, rect_gp = gpar(type = "none"), cell_fun = cell_fun2, combined_name_fun = NULL,show_heatmap_legend = FALSE, gap = unit(1, "mm"), split = seq(1,9))
# 
# P4 <- Heatmap(matrix = x[,72:90], col = colorRamp2(colors = colrsPC, breaks = brksPC), cluster_rows = FALSE, cluster_columns = FALSE, show_column_names = FALSE, row_names_side = "left", row_names_gp = gpar(fontsize=9), show_row_names = FALSE, rect_gp = gpar(type = "none"), cell_fun = cell_fun2, combined_name_fun = NULL,show_heatmap_legend = FALSE, gap = unit(1, "mm"), split = seq(1,9))
# 
# P5 <- Heatmap(matrix = x[,91:99], col = colorRamp2(colors = colrsPC, breaks = brksPC), cluster_rows = FALSE, cluster_columns = FALSE, show_column_names = FALSE, row_names_side = "left", row_names_gp = gpar(fontsize=9), show_row_names = FALSE, rect_gp = gpar(type = "none"), cell_fun = cell_fun2, combined_name_fun = NULL,show_heatmap_legend = FALSE, gap = unit(1, "mm"), split = seq(1,9))
# 
# P6 <- Heatmap(matrix = x[,100:136], col = colorRamp2(colors = colrsPC, breaks = brksPC), cluster_rows = FALSE, cluster_columns = FALSE, show_column_names = FALSE, row_names_side = "left", row_names_gp = gpar(fontsize=9), show_row_names = FALSE, rect_gp = gpar(type = "none"), cell_fun = cell_fun2, combined_name_fun = NULL,show_heatmap_legend = FALSE, gap = unit(1, "mm"), split = seq(1,9))
# 
# P7 <- Heatmap(matrix = x[,137:141], col = colorRamp2(colors = colrsPC, breaks = brksPC), cluster_rows = FALSE, cluster_columns = FALSE, show_column_names = FALSE, row_names_side = "left", row_names_gp = gpar(fontsize=9), show_row_names = FALSE, rect_gp = gpar(type = "none"), cell_fun = cell_fun2, combined_name_fun = NULL,show_heatmap_legend = FALSE, gap = unit(1, "mm"), split = seq(1,9))
# 
# P8 <- Heatmap(matrix = x[,142:149], col = colorRamp2(colors = colrsPC, breaks = brksPC), cluster_rows = FALSE, cluster_columns = FALSE, show_column_names = FALSE, row_names_side = "left", row_names_gp = gpar(fontsize=9), show_row_names = FALSE, rect_gp = gpar(type = "none"), cell_fun = cell_fun2, combined_name_fun = NULL,show_heatmap_legend = FALSE, gap = unit(1, "mm"), split = seq(1,9))
# 
# P9 <- Heatmap(matrix = x[,150:159], col = colorRamp2(colors = colrsPC, breaks = brksPC), cluster_rows = FALSE, cluster_columns = FALSE, show_column_names = FALSE, row_names_side = "left", row_names_gp = gpar(fontsize=9), show_row_names = FALSE, rect_gp = gpar(type = "none"), cell_fun = cell_fun2, combined_name_fun = NULL,show_heatmap_legend = FALSE, gap = unit(1, "mm"), split = seq(1,9))
# 
# P <- HeatmapList(P1 + P2 + P3 + P4 + P5 + P6 + P7 + P8 + P9)
# 
# P <- draw(P, gap = unit(c(1, 1, 0, 1, 1, 1, 0, 1, 1), "mm"))
# 
# 
# 

library(tidyverse)
library(ggpubr)
library(RColorBrewer)
library(grid)
library(magick)

library(ComplexHeatmap)

# grobpdb2 <- rasterGrob(image_read("Figures/interhex.png"), just = "center")
# grobpdb3 <- rasterGrob(image_read("Figures/interpent.png"), just = "center")

pal2 <- brewer.pal(3, "Set1")[2:1]

g3 <- ggplot(PC_roca, aes(x = eigIndx, y = "1", fill = if_else(condition = (abs(eigVec3) > 0.05), true = TRUE, false = FALSE)))

g3 <- g3 + scale_y_discrete(expand = c(0,0), labels = c(expression(paste("PC3", (lambda[3])))))

g3 <- g3 + ylab("PC3")

g3 <- g3 + geom_tile(color = "grey90", size = 0.2)

# g3 <- g3 + scale_fill_distiller(palette = "Greens", direction = 1)

g3 <- g3 + scale_fill_manual(values = c("white", pal2[1]))

xtcks <- c(25, 73, 122, 169, 211, 252, 286, 331, 379, 426)

g3 <- g3 + scale_x_continuous(breaks = xtcks, labels = true_indices$GAGIndx[true_indices$numIndx %in% xtcks], expand = c(0,0))

g3 <- g3 + theme_minimal() + theme(legend.position = "none") +
  theme(strip.background = element_blank())

g3 <- g3 + theme(axis.ticks.x = element_blank()) + theme(panel.grid = element_blank()) 

g3 <- g3 + theme(axis.title.x = element_blank())

g3 <- g3 + theme(axis.title.y = element_text(face = "bold", size = 7, vjust = 0.5, hjust = 0.5, angle = 0))

g3 <- g3 + theme(axis.text.y = element_blank())
                   # element_text(face = "bold", size = 9, margin = margin(t = 0, r = 0, b = 0, l = 0)))

g3 <- g3 + theme(axis.text.x = element_text(face = "bold", size = 7, colour = "white"))

g3 <- g3 + theme(axis.ticks.x = element_line(linetype = "solid", colour = "black")) 
# + theme(plot.margin = unit(c(0,0,0,0), "lines"))

spl <- setdiff((all_secs %>% filter(case=="PCs9" & sec=="sec7"))$site, (all_secs %>% filter(case=="PCs9" & sec=="sec3"))$site)

g7 <- ggplot(PC_roca, aes(x = eigIndx, y = "1",
                          fill = case_when(eigIndx %in% spl ~ "2", 
                                           abs(eigVec7) > 0.05 ~ "1", 
                                           TRUE ~ "0")))

g7 <- g7 + scale_y_discrete(expand = c(0,0), labels = c(expression(paste("PC7", (lambda[7])))))

g7 <- g7 + geom_tile(color = "grey90", size = 0.2)

g7 <- g7 + ylab("PC7")

# g3 <- g3 + scale_fill_distiller(palette = "Greens", direction = 1)

g7 <- g7 + scale_fill_manual(values = c("white", pal2[1], pal2[2]), breaks = c(1, 2), label = c("Sites represented either only in PC3 or in both PC3 and PC7", "Sites represented only in PC7"))

xtcks <- c(25, 73, 122, 169, 211, 252, 286, 331, 379, 426)

g7 <- g7 + scale_x_continuous(breaks = xtcks, labels = true_indices$GAGIndx[true_indices$numIndx %in% xtcks], expand = c(0,0))

# g7 <- g7 + theme_minimal() + theme(legend.position = "none") +
#   theme(strip.background = element_blank())
g7 <- g7 + theme_minimal() + theme(legend.position = "bottom") +
  theme(strip.background = element_blank())
g7 <- g7 + theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank(), legend.text = element_text(size = 9), legend.text.align = 0.5, legend.spacing.x = unit(5, 'mm'))

g7 <- g7 + theme(axis.ticks.y = element_blank()) + theme(panel.grid = element_blank())

g7 <- g7 + theme(axis.title.x = element_blank())

g7 <- g7 + theme(axis.title.y = element_text(face = "bold", size = 7, vjust = 0.5, hjust = 0.5, angle = 0))

g7 <- g7 + theme(axis.text.y = element_blank())
                   # element_text(face = "bold", size = 9, margin = margin(t = 0, r = 0, b = 0, l = 0), hjust = 1))

g7 <- g7 + theme(axis.text.x = element_text(face = "bold", size = 7))

g7 <- g7 + theme(axis.ticks.x = element_line(linetype = "solid", colour = "black")) 
# + theme(plot.margin = unit(c(0,0,0,0), "lines"))
# 
# g <- ggarrange(
#   ggarrange(grobpdb1, grobpdb2, labels=c("Pentamer", "Hexamer"), font.label = list(size=9), nrow = 1, ncol = 2),
#   ggarrange(g3, g7, nrow = 2, ncol = 1),
#   nrow = 2, ncol = 1, heights = c(4, 2, 2))
# 
# ggsave(filename = "g.png", plot = g, width = 183, units = "mm", dpi=600)



tmp_G <- grid.grabExpr(
  {draw(object = G, gap = unit(c(bu), "mm"), heatmap_legend_side = "right", show_heatmap_legend = TRUE, annotation_legend_side = c("bottom"), show_annotation_legend = TRUE)
  
  # decorate_annotation(annotation = "PC1", {
  #   grid.text(label = "(f)", x = unit(0, "npc"), y = unit(0, "npc"), hjust = 1.5, vjust = -2,  default.units = "npc", just = "left")
  # })
  }
)

# fig_tmp = ggarrange(tmp_G, nrow = 1, ncol = 1)

gempty <- ggplot(data = PC_roca) + theme_transparent()

fig_tmp <- ggarrange(ggarrange(plotlist = list(gempty, GT, gempty), nrow = 3, ncol = 1, heights = c(1,4,1)), gempty, tmp_G, labels = c("A","", "B"),  hjust = c(-0.5,-0.5,1.5), nrow = 1, ncol = 3, widths = c(1, 0.15, 2.5), align = "hv")

fig_tmp <- annotate_figure(fig_tmp,
                top = text_grob("Model based correlation matrices", face = "bold", size = 14))

ggsave(filename = "Figures/Fig2.png", plot = fig_tmp, width = 6, height = 4.5, units = "in", dpi=600)



# grobpdb2 <- rasterGrob(image_read("Figures/interhex.png"), just = "center", width = 12, default.units = "cm")
# grobpdb3 <- rasterGrob(image_read("Figures/interpent.png"), just = "center", width = 12, default.units = "cm")
# grobevd <- rasterGrob(image_read("evd_spec.png"))

# fig_tmp <- ggarrange(
#   ggarrange(evdg, 
#             ggarrange(g3, g7, nrow = 2, ncol = 1, labels=c("(b)", ""), font.label = list(size=9), vjust = 1.5, heights = c(1, 1)), 
#   labels=c("(a)"), font.label = list(size=9), nrow = 2, ncol = 1,  heights = c(3, 1)),
#   ggarrange(grobpdb2, grobpdb3, labels=c("(d)", "(e)"), font.label = list(size=9), ncol = 2, nrow = 1),
#   nrow = 2, ncol = 1, heights = c(3, 1.3))

grobpdb2 <- rasterGrob(image_read("Figures/hexamer1.png"), just = "center")
grobpdb3 <- rasterGrob(image_read("Figures/pentamer1.png"), just = "center")

fig_tmp <- ggarrange(evdg, gempty, g3, g7, gempty,
                     ggarrange(grobpdb2, grobpdb3, ncol = 2, nrow = 1),
                     nrow = 6, ncol = 1, labels=c("A", "", "B", "", "", "C"), 
                     font.label = list(size=9), vjust = c(1, 0, 0, 0, 0, 0), heights = c(3, 0.25, 0.75, 1.5, 0.25, 5))

ggsave(filename = "Figures/Fig1b.png", plot = fig_tmp, height = 7.5, width = 7.5, units = "in", dpi=600)


