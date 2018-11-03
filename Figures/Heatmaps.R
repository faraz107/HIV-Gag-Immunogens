library(ComplexHeatmap)
library(circlize)
library(multipanelfigure)
library(RColorBrewer)
library(tidyverse)

bu = 190.5/(159+7)
bu = bu * 0.80
# bu = bu * 0.40

cell_lwd <- 0.75
total_anno_height <- unit(5*9*bu, "mm")

anno_cell_height <- rep(1, 9)

colrs <- colorRampPalette(c(brewer.pal(n = 11, name = "Spectral")[1], "white", brewer.pal(n = 11, name = "Spectral")[11]))(28)

colrsPC <- colorRampPalette(c("white", brewer.pal(n = 11, name = "Spectral")[11]))(9)

pc_colrs <- colorRamp2(c(0,0.5), c("white", brewer.pal(n = 9, name = "Greens")[9]))

PC_colors <- list(PC1 = pc_colrs, PC2 = pc_colrs, PC3 = pc_colrs, PC4 = pc_colrs, PC5 = pc_colrs, PC6 = pc_colrs, PC7 = pc_colrs, PC8 = pc_colrs, PC9 = pc_colrs)

x <- data.frame(abs(PC_roca9[sec1_only,c(1, 2, 3, 7, 4, 5, 6, 9, 8)]))
names(x) <- c("PC1", "PC2", "PC3", "PC7", "PC4", "PC5", "PC6", "PC9", "PC8")

# h1a <- HeatmapAnnotation(df = x, col = PC_colors, which = "column", name = "annPC1", annotation_name_side = "left", show_annotation_name = TRUE, gp = gpar(col = "grey90"), annotation_height = rep(1, 9), show_legend = c(TRUE, F, F, F, F, F, F, F, F), annotation_legend_param = list(title="", labels_gp = gpar(fontsize = 10), legend_height = unit(50, "mm"), legend_direction = "horizontal"), gap = unit(bu, "mm"), annotation_name_gp = gpar(fontsize=7.5))

# h1a <- HeatmapAnnotation(df = x, col = PC_colors, which = "column", name = "annPC1", annotation_name_side = "left", show_annotation_name = TRUE, gp = gpar(col = "grey90", lwd = cell_lwd), show_legend = c(TRUE, F, F, F, F, F, F, F, F), annotation_height = anno_cell_height, annotation_legend_param = list(title="", labels_gp = gpar(fontsize = 8), color_bar = "continuous", legend_direction = "horizontal", at = c(-0.1, 0.25, 0.6), labels = c("0", "0.3", "0.6")), gap = unit(bu, "mm"), annotation_name_gp = gpar(fontsize=7.5, col = c(rep("black", 2), rep("red",2), rep("black", 2), rep("red",2), "black"), fontface = c(rep("plain", 2), rep("bold",2), rep("plain", 2), rep("bold",2), "plain")))

h1a <- HeatmapAnnotation(df = x, col = PC_colors, which = "column", name = "annPC1", annotation_name_side = "left", show_annotation_name = TRUE, gp = gpar(col = "grey90", lwd = cell_lwd), show_legend = c(TRUE, F, F, F, F, F, F, F, F), annotation_height = anno_cell_height, annotation_legend_param = list(title="", labels_gp = gpar(fontsize = 8), color_bar = "continuous", legend_direction = "horizontal", at = c(-0.1, 0.25, 0.6), labels = c("0", "0.3", "0.6")), gap = unit(bu, "mm"), annotation_name_gp = gpar(fontsize=7.5, col = c(rep("black", 3), rep("brown",1), rep("black", 3), rep("brown",1), "black"), fontface = c(rep("plain", 3), rep("bold",1), rep("plain", 3), rep("bold",1), "plain")))

x <- data.frame(abs(PC_roca9[sec2_only,c(1, 2, 3, 7, 4, 5, 6, 9, 8)]))
names(x) <- c("PC1", "PC2", "PC3", "PC7", "PC4", "PC5", "PC6", "PC9", "PC8")

# h2a <- HeatmapAnnotation(df = x, col = PC_colors, which = "column", show_annotation_name = FALSE, gp = gpar(col = "grey90"), annotation_height = rep(1, 9), show_legend = FALSE, gap = unit(bu, "mm"), annotation_name_gp = gpar(fontsize=7.5))

h2a <- HeatmapAnnotation(df = x, col = PC_colors, show_annotation_name = FALSE, gp = gpar(col = "grey90", lwd = cell_lwd), annotation_height = anno_cell_height, show_legend = FALSE, gap = unit(bu, "mm"))

x <- data.frame(abs(PC_roca9[sec3_only,c(1, 2, 3, 7, 4, 5, 6, 9, 8)]))
names(x) <- c("PC1", "PC2", "PC3", "PC7", "PC4", "PC5", "PC6", "PC9", "PC8")

h3a <- HeatmapAnnotation(df = x, col = PC_colors, show_annotation_name = FALSE, gp = gpar(col = "grey90", lwd = cell_lwd), annotation_height = anno_cell_height, show_legend = FALSE, gap = unit(bu, "mm"))

x <- data.frame(abs(PC_roca9[sec7_only,c(1, 2, 3, 7, 4, 5, 6, 9, 8)]))
names(x) <- c("PC1", "PC2", "PC3", "PC7", "PC4", "PC5", "PC6", "PC9", "PC8")

h4a <- HeatmapAnnotation(df = x, col = PC_colors, show_annotation_name = FALSE, gp = gpar(col = "grey90", lwd = cell_lwd), annotation_height = anno_cell_height, show_legend = FALSE, gap = unit(bu, "mm"))

x <- data.frame(abs(PC_roca9[sec4_only,c(1, 2, 3, 7, 4, 5, 6, 9, 8)]))
names(x) <- c("PC1", "PC2", "PC3", "PC7", "PC4", "PC5", "PC6", "PC9", "PC8")

h5a <- HeatmapAnnotation(df = x, col = PC_colors, show_annotation_name = FALSE, gp = gpar(col = "grey90", lwd = cell_lwd), annotation_height = anno_cell_height, show_legend = FALSE, gap = unit(bu, "mm"))

x <- data.frame(abs(PC_roca9[sec5_only,c(1, 2, 3, 7, 4, 5, 6, 9, 8)]))
names(x) <- c("PC1", "PC2", "PC3", "PC7", "PC4", "PC5", "PC6", "PC9", "PC8")

h6a <- HeatmapAnnotation(df = x, col = PC_colors, show_annotation_name = FALSE, gp = gpar(col = "grey90", lwd = cell_lwd), annotation_height = anno_cell_height, show_legend = FALSE, gap = unit(bu, "mm"))

x <- data.frame(abs(PC_roca9[sec6_only,c(1, 2, 3, 7, 4, 5, 6, 9, 8)]))
names(x) <- c("PC1", "PC2", "PC3", "PC7", "PC4", "PC5", "PC6", "PC9", "PC8")

h7a <- HeatmapAnnotation(df = x, col = PC_colors, show_annotation_name = FALSE, gp = gpar(col = "grey90", lwd = cell_lwd), show_legend = FALSE, gap = unit(bu, "mm"))

x <- data.frame(abs(PC_roca9[sec9_only,c(1, 2, 3, 7, 4, 5, 6, 9, 8)]))
names(x) <- c("PC1", "PC2", "PC3", "PC7", "PC4", "PC5", "PC6", "PC9", "PC8")

h8a <- HeatmapAnnotation(df = x, col = PC_colors, show_annotation_name = FALSE, gp = gpar(col = "grey90", lwd = cell_lwd), annotation_height = anno_cell_height, show_legend = FALSE, gap = unit(bu, "mm"))

x <- data.frame(abs(PC_roca9[sec8_only,c(1, 2, 3, 7, 4, 5, 6, 9, 8)]))
names(x) <- c("PC1", "PC2", "PC3", "PC7", "PC4", "PC5", "PC6", "PC9", "PC8")

h9a <- HeatmapAnnotation(df = x, col = PC_colors, show_annotation_name = FALSE, gp = gpar(col = "grey90", lwd = cell_lwd), annotation_height = anno_cell_height, show_legend = FALSE, gap = unit(bu, "mm"))

# cell_fun2 = function(j, i, x, y, width, height, fill) {
#   s = min(unit.c(convertWidth(width, "cm"), convertHeight(height, "cm")))
#   # grid.rect(x = x, y = y, width = unit(bu, "mm"), height = unit(bu, "mm"), 
#   # gp = gpar(col = "grey90", fill = fill))
#   grid.rect(x = x, y = y, width = s, height = s, 
#             gp = gpar(col = "grey90", fill = fill))
# }

h1ra <- rowAnnotation(text=row_anno_text(c("(a)", rep("", 158))))

sec_ind_1_9 <- c(sec1_only, sec2_only, sec3_only, sec7_only, sec4_only, sec5_only, sec6_only, sec9_only, sec8_only)

C451sec <- C451[sec_ind_1_9, sec_ind_1_9]

secInds <- c(rep(0,24), rep(1,14), rep(2,33+5), rep(3,19), rep(4,9), rep(5,37+10), rep(6,8))

# H1 <- Heatmap(matrix = C451sec[,1:24], name = "sec1", col = colorRamp2(colors = colrs, breaks = brks), cluster_rows = FALSE, cluster_columns = FALSE, show_column_names = TRUE, column_names_side = "bottom", column_names_gp = gpar(col="white"), show_row_names = FALSE, rect_gp = gpar(col = "grey90"), split = secInds, combined_name_fun = NULL, gap = unit(bu, "mm"), show_heatmap_legend = TRUE, width = unit(bu*25, "mm"), bottom_annotation = h1a, heatmap_legend_param = list(title="", labels_gp = gpar(fontsize = 10), legend_height = unit(50, "mm"), legend_direction = "vertical"))

H1 <- Heatmap(matrix = C451sec[,1:24], name = "sec1", col = colorRamp2(colors = colrs, breaks = brks), cluster_rows = FALSE, cluster_columns = FALSE, show_column_names = TRUE, column_names_gp = gpar(col="white", fontsize = 2), column_names_max_height = unit(bu, "mm"), column_names_side = "bottom", show_row_names = TRUE, row_names_side = "left", row_names_gp = gpar(col="white"), rect_gp = gpar(col = "grey90", lwd = cell_lwd), split = secInds, combined_name_fun = NULL, gap = unit(bu, "mm"), show_heatmap_legend = TRUE, bottom_annotation = h1a, bottom_annotation_height = total_anno_height, heatmap_legend_param = list(title="", labels_gp = gpar(fontsize = 10), legend_direction = "vertical"))

H2 <- Heatmap(matrix = C451sec[,25:38], name = "sec2", col = colorRamp2(colors = colrs, breaks = brks), cluster_rows = FALSE, cluster_columns = FALSE, show_column_names = FALSE, column_names_side = "bottom", column_names_gp = gpar(col="white"), show_row_names = FALSE, rect_gp = gpar(col = "grey90", lwd = cell_lwd), split = secInds, combined_name_fun = NULL, gap = unit(bu, "mm"), show_heatmap_legend = FALSE, bottom_annotation = h2a, bottom_annotation_height = total_anno_height)
# width = unit(bu*15, "mm"),

H3 <- Heatmap(matrix = C451sec[,39:71], name = "sec3", col = colorRamp2(colors = colrs, breaks = brks), cluster_rows = FALSE, cluster_columns = FALSE, show_column_names = FALSE, column_names_side = "bottom", column_names_gp = gpar(col="white"), show_row_names = FALSE, rect_gp = gpar(col = "grey90", lwd = cell_lwd), split = secInds, combined_name_fun = NULL, gap = unit(bu, "mm"), show_heatmap_legend = FALSE, bottom_annotation = h3a, bottom_annotation_height = total_anno_height)
# width = unit(bu*34, "mm"), 

H4 <- Heatmap(matrix = C451sec[,72:76], name = "sec4", col = colorRamp2(colors = colrs, breaks = brks), cluster_rows = FALSE, cluster_columns = FALSE, show_column_names = FALSE, show_row_names = FALSE, rect_gp = gpar(col = "grey90", lwd = cell_lwd), split = secInds, combined_name_fun = NULL, gap = unit(bu, "mm"), show_heatmap_legend = FALSE, bottom_annotation = h4a, bottom_annotation_height = total_anno_height)
# width = unit(bu*6, "mm"),

H5 <- Heatmap(matrix = C451sec[,77:95], name = "sec5", col = colorRamp2(colors = colrs, breaks = brks), cluster_rows = FALSE, cluster_columns = FALSE, show_column_names = FALSE, column_names_side = "bottom", column_names_gp = gpar(col="white"), show_row_names = FALSE, rect_gp = gpar(col = "grey90", lwd = cell_lwd), split = secInds, combined_name_fun = NULL, gap = unit(bu, "mm"), show_heatmap_legend = FALSE, bottom_annotation = h5a, bottom_annotation_height = total_anno_height)
# width = unit(bu*20, "mm"),

H6 <- Heatmap(matrix = C451sec[,96:104], name = "sec6", col = colorRamp2(colors = colrs, breaks = brks), cluster_rows = FALSE, cluster_columns = FALSE, show_column_names = FALSE, column_names_side = "bottom", column_names_gp = gpar(col="white"), show_row_names = FALSE, rect_gp = gpar(col = "grey90", lwd = cell_lwd), split = secInds, combined_name_fun = NULL, gap = unit(bu, "mm"), show_heatmap_legend = FALSE, bottom_annotation = h6a, bottom_annotation_height = total_anno_height)
# width = unit(bu*10, "mm"),

H7 <- Heatmap(matrix = C451sec[,105:141], name = "sec7", col = colorRamp2(colors = colrs, breaks = brks), cluster_rows = FALSE, cluster_columns = FALSE, show_column_names = FALSE, show_row_names = FALSE, rect_gp = gpar(col = "grey90", lwd = cell_lwd), split = secInds, combined_name_fun = NULL, gap = unit(bu, "mm"), show_heatmap_legend = FALSE, bottom_annotation = h7a, bottom_annotation_height = total_anno_height)
# width = unit(bu*38, "mm"), 

H8 <- Heatmap(matrix = C451sec[,142:151], name = "sec8", col = colorRamp2(colors = colrs, breaks = brks), cluster_rows = FALSE, cluster_columns = FALSE, show_column_names = FALSE, column_names_side = "bottom", column_names_gp = gpar(col="white"), show_row_names = FALSE, rect_gp = gpar(col = "grey90", lwd = cell_lwd), split = secInds, combined_name_fun = NULL, gap = unit(bu, "mm"), show_heatmap_legend = FALSE, bottom_annotation = h8a, bottom_annotation_height = total_anno_height)
# width = unit(bu*11, "mm"), 

H9 <- Heatmap(matrix = C451sec[,152:159], name = "sec9", col = colorRamp2(colors = colrs, breaks = brks), cluster_rows = FALSE, cluster_columns = FALSE, show_column_names = FALSE, column_names_side = "bottom", column_names_gp = gpar(col="white"), show_row_names = FALSE, rect_gp = gpar(col = "grey90", lwd = cell_lwd), split = secInds, combined_name_fun = NULL, gap = unit(bu, "mm"), show_heatmap_legend = FALSE, bottom_annotation = h9a, bottom_annotation_height = total_anno_height)
# width = unit(bu*9, "mm"), 

# H <- HeatmapList(h1ra + H1 + H2 + H3 + H4 + H5 + H6 + H7 + H8 + H9)

H <- HeatmapList(H1 + H2 + H3 + H4 + H5 + H6 + H7 + H8 + H9)

# pdf(file = "H.pdf", width = 7.5, height = 10, paper = "a4")

# draw(object = H, gap = unit(c(bu, bu, 0, bu, bu, bu, 0, bu), "mm"), heatmap_legend_side = "right", show_heatmap_legend = TRUE, annotation_legend_side = c("bottom"), show_annotation_legend = TRUE)

# decorate_annotation(annotation = "PC1", {
#   grid.text(label = "(b)", x = unit(0, "npc"), y = unit(0, "npc"), hjust = 1.5, vjust = -2,  default.units = "npc", just = "left")
# })
# 
# dev.off()

png(filename = "Figures/Fig3_small.png", units = "in", width = 7, height = 7.5, res = 600)
draw(object = H, gap = unit(c(bu, bu, 0, bu, bu, bu, 0, bu), "mm"), heatmap_legend_side = "right", show_heatmap_legend = TRUE, annotation_legend_side = c("bottom"), show_annotation_legend = TRUE, column_title = "Gag correlation matrix", column_title_gp = gpar(fontsize = 14, fontface = "bold"))

decorate_heatmap_body(heatmap = "sec3", code = {
  
  grid.lines(gp = gpar(fill = "transparent", col = "black", lwd = 1))
  
}, slice = 3)
dev.off()


png(filename = "Figures/Fig3d.png", units = "in", width = 7, height = 7.5, res = 600)
draw(object = H, gap = unit(c(bu, bu, 0, bu, bu, bu, 0, bu), "mm"), heatmap_legend_side = "right", show_heatmap_legend = TRUE, annotation_legend_side = c("bottom"), show_annotation_legend = TRUE, column_title = "Gag sample correlation matrix", column_title_gp = gpar(fontsize = 14, fontface = "bold"))
dev.off()


draw(object = H, gap = unit(c(bu, bu, bu, 0, bu, bu, bu, 0, bu, bu), "mm"), heatmap_legend_side = "right", show_heatmap_legend = TRUE, annotation_legend_side = c("bottom"), show_annotation_legend = TRUE)

decorate_annotation(annotation = "PC1", {
  grid.text(label = "(b)", x = unit(0, "npc"), y = unit(0, "npc"), hjust = 1.5, vjust = -2,  default.units = "npc", just = "left")
})

dev.off()

# svg(filename = "H.svg", width = 7.5, height = 10)
# 
# draw(object = H, gap = unit(c(bu, bu, bu, 0, bu, bu, bu, 0, bu, bu), "mm"), heatmap_legend_side = "right", show_heatmap_legend = TRUE, annotation_legend_side = c("bottom"), show_annotation_legend = TRUE)
# 
# decorate_annotation(annotation = "PC1", {
#   grid.text(label = "(b)", x = unit(0, "npc"), y = unit(0, "npc"), hjust = 1.5, vjust = -2,  default.units = "npc", just = "left")
# })
# 
# dev.off()

H3 <- Heatmap(matrix = C451sec[,39:71], name = "sec3", col = colorRamp2(colors = colrs, breaks = brks), cluster_rows = FALSE, cluster_columns = FALSE, show_column_names = FALSE, column_names_side = "bottom", column_names_gp = gpar(col="white"), show_row_names = FALSE, rect_gp = gpar(col = "grey90", lwd = cell_lwd), split = secInds, combined_name_fun = NULL, gap = unit(bu, "mm"), show_heatmap_legend = FALSE, bottom_annotation = h3a, bottom_annotation_height = total_anno_height)

draw(object = H3, heatmap_legend_side = "right", show_heatmap_legend = TRUE, annotation_legend_side = c("bottom"), show_annotation_legend = TRUE)

decorate_heatmap_body(heatmap = "sec3", code = {
  
  grid.rect(gp = gpar(fill = "transparent", col = "black", lwd = 1))
  
}, slice = 3)


# GT ----------------------------------------------------------------------


# For extended GT test


bu = (190.5/2)/(57+3)
bu = bu * 0.80

sec22 <- c(52, 156, 159, 211, 224, 236, 240, 242, 244, 319, 320, 321, 322, 323)

sec37 <- c(13,  14,  71, 134, 141, 146, 147, 148, 149, 150, 158, 161, 163, 164, 165, 166, 168, 169, 171, 173, 174, 175, 177, 179, 180, 181, 182, 183, 185, 187, 196, 197, 202, 208, 215, 219, 222, 274, 290, 296, 312, 447, 448)

evd22 <- eigen(C_full22)
pcs22 <- as.data.frame(evd22$vectors)
rownames(pcs22) <- seq(1, 451)

pcs22[which(abs(pcs22[,1])<0.10, arr.ind = TRUE), 1] <- 0
pcs22[which(abs(pcs22[,2])<0.05, arr.ind = TRUE), 2] <- 0
pcs22[which(abs(pcs22[,3])<0.05, arr.ind = TRUE), 3] <- 0

pc_colrs <- colorRamp2(c(0,0.5), c("white", brewer.pal(n = 9, name = "Greens")[9]))
# pc_colrs <- colorRamp2(c(0.01,0.5), c("white", brewer.pal(n = 9, name = "Greens")[9]))


PC_colors <- list(PC1 = pc_colrs, PC2 = pc_colrs, PC3 = pc_colrs)

x <- data.frame(abs(pcs22[c(sec22), c(1, 2, 3)]))
names(x) <- c("PC1", "PC2", "PC3")

# g1a <- HeatmapAnnotation(df = x, col = PC_colors, which = "column", annotation_name_side = "left", show_annotation_name = FALSE, gp = gpar(col = "grey90"), annotation_height = rep(1, 3), show_legend = c(TRUE, F, F), annotation_legend_param = list(title="", labels_gp = gpar(fontsize = 8), color_bar = "continuous", legend_direction = "vertical", at = c(0.1, 0.3, 0.5), labels = c("0.10", "0.30", "0.50")), gap = unit(bu, "mm"), annotation_name_gp = gpar(fontsize=7.5))

g1a <- HeatmapAnnotation(df = x, col = PC_colors, show_annotation_name = TRUE, annotation_name_side = "left", gp = gpar(col = "grey90"), gap = unit(bu, "mm"), annotation_name_gp = gpar(fontsize=8, fontface="bold"), show_legend = c(TRUE, F, F), annotation_legend_param = list(title="", labels_gp = gpar(fontsize = 8), color_bar = "continuous", legend_direction = "horizontal", at = c(-0.1, 0.25, 0.6), labels = c("0", "0.3", "0.6")))

# g1ra <- rowAnnotation(text=row_anno_text(c("(a)", rep("", 57))))
g1ra <- rowAnnotation(text=row_anno_text(c("(e)", rep("", 57))))

secInds <- c(rep(0,14), rep(1,43))

G1 <- Heatmap(matrix = C22[,1:14], name = "sec1", col = colorRamp2(colors = colrs, breaks = brks), cluster_rows = FALSE, cluster_columns = FALSE, show_column_names = TRUE, column_names_gp = gpar(col="white", fontsize = 2), column_names_max_height = unit(bu, "mm"), show_row_names = FALSE, rect_gp = gpar(col = "grey90"), split = secInds, combined_name_fun = NULL, gap = unit(bu, "mm"), show_heatmap_legend = FALSE, bottom_annotation = g1a)


x <- data.frame(abs(pcs22[c(sec37), c(1, 2, 3)]))
names(x) <- c("PC1", "PC2", "PC3")

# g2a <- HeatmapAnnotation(df = x, col = PC_colors, which = "column", annotation_name_side = "left", show_annotation_name = FALSE, gp = gpar(col = "grey90"), annotation_height = rep(1, 3), show_legend = c(F, F, F), annotation_legend_param = list(title="", labels_gp = gpar(fontsize = 8), legend_height = unit(17, "mm"), color_bar = "continuous", legend_direction = "vertical", at = c(0.1, 0.3, 0.5), labels = c("0.10", "0.30", "0.50")), gap = unit(bu, "mm"), annotation_name_gp = gpar(fontsize=7.5))

# G2 <- Heatmap(matrix = C22[,15:57], name = "sec2", col = colorRamp2(colors = colrs, breaks = brks), cluster_rows = FALSE, cluster_columns = FALSE, show_column_names = FALSE, column_names_side = "bottom", column_names_gp = gpar(col="white"), show_row_names = FALSE, rect_gp = gpar(col = "grey90"), split = secInds, combined_name_fun = NULL, gap = unit(bu, "mm"), show_heatmap_legend = TRUE, width = unit(bu*43, "mm"), bottom_annotation = g2a, heatmap_legend_param = list(title="", labels_gp = gpar(fontsize = 8), legend_height = unit(17, "mm"), legend_direction = "vertical"))

g2a <- HeatmapAnnotation(df = x, col = PC_colors, show_annotation_name = FALSE, gp = gpar(col = "grey90"), show_legend = c(F, F, F), gap = unit(bu, "mm"))

G2 <- Heatmap(matrix = C22[,15:57], name = "sec2", col = colorRamp2(colors = colrs, breaks = brks), cluster_rows = FALSE, cluster_columns = FALSE, show_column_names = FALSE, show_row_names = FALSE, rect_gp = gpar(col = "grey90"), split = secInds, combined_name_fun = NULL, gap = unit(bu, "mm"), show_heatmap_legend = TRUE, bottom_annotation = g2a, heatmap_legend_param = list(title="", labels_gp = gpar(fontsize = 8), legend_direction = "vertical"))

# G <- HeatmapList(g1ra + G1 + G2)
G <- HeatmapList(G1 + G2)

# pdf(file = "G.pdf", width = 3.5, height = 4, paper = "a4")

draw(object = G, gap = unit(c(bu), "mm"), heatmap_legend_side = "right", show_heatmap_legend = TRUE, annotation_legend_side = c("bottom"), show_annotation_legend = TRUE)

# decorate_annotation(annotation = "PC1", {
#   grid.text(label = "(b)", x = unit(0, "npc"), y = unit(0, "npc"), hjust = 1.5, vjust = -2,  default.units = "npc", just = "left")
# })

dev.off()

svg(filename = "G.svg", width = 3.5, height = 4.5)

draw(object = G, gap = unit(c(bu, bu, bu), "mm"), heatmap_legend_side = "top", show_heatmap_legend = TRUE, annotation_legend_side = c("bottom"), show_annotation_legend = TRUE)

decorate_annotation(annotation = "PC1", {
  grid.text(label = "(b)", x = unit(0, "npc"), y = unit(0, "npc"), hjust = 1.5, vjust = -2,  default.units = "npc", just = "left")
})

dev.off()


png(filename = "G.png", width = 3.5, height = 4.5, units = "in", res = 600)

draw(object = G, gap = unit(c(bu, bu, bu), "mm"), heatmap_legend_side = "top", show_heatmap_legend = TRUE, annotation_legend_side = c("bottom"), show_annotation_legend = TRUE)

decorate_annotation(annotation = "PC1", {
  grid.text(label = "(b)", x = unit(0, "npc"), y = unit(0, "npc"), hjust = 1.5, vjust = -2,  default.units = "npc", just = "left")
})

dev.off()
