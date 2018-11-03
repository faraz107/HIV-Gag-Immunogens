library(tidyverse)
library(scales)
library(RColorBrewer)
library(ggpubr)

biosecs <- biodom
colnames(biosecs) <- c("DisProg", "DisCont", "P17Mem", "InterHex", "IntraHex", "P24SP1", "P7Zinc")

y<-c(1, 1, 5.6e-7, 1, 4.8e-1, 1, 
     1e-16, 1, 9.6e-1, 1, 1, 7.7e-2, 
     1, 1.4e-6, 1, 1, 1, 1, 
     5.7e-1, 1, 1, 1, 1e-6, 1)

z<-c(1, 1, 1.4e-10, 1, 4.8e-1, 1, 
     2.1e-15, 1, 1, 1, 1, 0.6, 
     1, 9.3e-7, 1, 1, 1, 1, 
     5.6e-1, 1, 1, 1, 1e-6, 1)

x<-c(1, 1, 1e-3, 5e-1, 1, 1, 
     1, 1, 1, 1, 1, 1,
     1, 1, 1, 1, 1, 1,
     1, 1, 1, 1, 1, 1)

assoc_df <- data.frame(approachNames <- c(rep("m1Dahi", 24), rep("m2Old", 24), rep("m3New", 24)), biochemicalDomainNames <- rep(c("P24 Intrahexamer Interface", "P17-Membrane Binding", "P24-SP1 Interface", "P7 Zinc Finger"), each=6), secNames <- rep(c("Sector 1", "Sector 2", "Sector 3", "Sector 4", "Sector 5", "Sector 6"), 12), sectorPvals <- c(-log10(x), -log10(y), -log10(z)), raw <- c(x, y, z)) 

colnames(assoc_df) <- c("Approach", "BiochemicalDomain", "Sector", "pValue", "rawp")

pg <- ggplot(data = assoc_df, mapping = aes(x = Sector, y = pValue, fill = Approach))
pg <- pg + geom_col(position = "dodge")
pg <- pg + facet_grid(.~BiochemicalDomain, labeller = label_wrap_gen(width = 16, multi_line = TRUE), scales = "free")
pg <- pg + scale_x_discrete(labels = c("Sec-1", "Sec-2", "Sec-3", "Sec-4", "Sec-5", "Sec-6"))
pg <- pg + scale_y_continuous(expand = c(0,0))
pg <- pg + scale_fill_manual(values = c(brewer.pal(9, "Paired")[3], brewer.pal(9, "Set1")[2]), aesthetics = "fill")
pg <- pg + theme_minimal() + theme(legend.position = "none" ) 
pg <- pg + theme(strip.background = element_rect(fill = "white", colour = "white"), strip.text = element_text(face = "bold", size = 9))
pg <- pg + theme(axis.ticks.y = element_blank()) + theme(axis.title.x = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + theme(axis.title.y = element_text(face = "bold", size = 9))
pg <- pg + theme(panel.grid.minor.y = element_line(linetype = "dotted", color = "grey90"))
pg <- pg + theme(axis.text.x = element_text(face = "bold", size = 7, angle = 30, hjust = 1, vjust = 1), axis.text.y = element_text(face = "bold", size = 9)) + theme(axis.ticks.x = element_line(color = "grey40", size = 1)) 
pg <- pg + geom_hline(yintercept = 0) + geom_hline(yintercept = 1.30103, linetype = "dashed", color = brewer.pal(9, "Set1")[1], size = 1)
pg <- pg + ylab(expression(paste(-log[10], "(p-value)")))
pg1 <- pg

y<-c(1, 1, 1, 1, 1, 2.5e-6, 
     3.1e-1, 9.7e-2, 1.2e-4, 1, 7.6e-1, 7.9e-1,
     1.7e-1, 4.7e-3, 9.3e-1, 4.3e-1, 7.3e-1, 8.0e-1)

z<-c(1, 1, 1, 1, 1, 1e-9, 
     2.8e-1, 7.4e-2, 4.0e-7, 1, 7.6e-1, 1, 
     1.6e-1, 3.4e-3, 1, 6.0e-1, 7.3e-1, 1)

x<-c(1, 1, 1, 1, 1, 1.3617e-10, 
     2.4636, 4.5540, 1.0186e-05, 0.4814, 2.5750, 1.6934,
     1, 1, 1, 1, 1, 1)

assoc_df2 <- data.frame(approachNames <- c(rep("m1Dahi", 18), rep("m2Old", 18), rep("m3New", 18)), immunoDomainNames <- rep(c("HLA Associated Polymorphism", "Viral Control", "Disease Progression"), each=6), secNames <- rep(c("Sector 1", "Sector 2", "Sector 3", "Sector 4", "Sector 5", "Sector 6"), 9), sectorPvals <- c(-log10(x), -log10(y), -log10(z)), raw <- c(x, y, z)) 

colnames(assoc_df2) <- c("Approach", "ImmunoDomain", "Sector", "pValue", "rawp")

assoc_df2$ImmunoDomain <- factor(assoc_df2$ImmunoDomain, levels = c("Viral Control", "Disease Progression", "HLA Associated Polymorphism"))

pg <- ggplot(data = assoc_df2, mapping = aes(x = Sector, y = pValue, fill = Approach))
pg <- pg + geom_col(position = "dodge")
pg <- pg + facet_grid(cols = vars(ImmunoDomain), labeller = label_wrap_gen(width = 16, multi_line = TRUE), scales = "free")
pg <- pg + scale_x_discrete(labels = c("Sec-1", "Sec-2", "Sec-3", "Sec-4", "Sec-5", "Sec-6"))
pg <- pg + scale_y_continuous(expand = c(0,0), limits = c(0,16), breaks = c(0, 4, 8, 12, 16))
pg <- pg + scale_fill_manual(values = c(brewer.pal(9, "Paired")[3], brewer.pal(9, "Set1")[2], brewer.pal(9, "Paired")[7]), aesthetics = "fill")
pg <- pg + theme_minimal() + theme(legend.position = "none" ) 
pg <- pg + theme(strip.background = element_rect(fill = "white", colour = "white"), strip.text = element_text(face = "bold", size = 9))
pg <- pg + theme(axis.ticks.y = element_blank()) + theme(axis.title.x = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + theme(axis.title.y = element_text(face = "bold", size = 9))
pg <- pg + theme(panel.grid.minor.y = element_line(linetype = "dotted", color = "grey90"))
pg <- pg + theme(axis.text.x = element_text(face = "bold", size = 7, angle = 0, hjust = 1, vjust = 1), axis.text.y = element_text(face = "bold", size = 9)) + theme(axis.ticks.x = element_line(color = "grey40", size = 1)) 
pg <- pg + geom_hline(yintercept = 0) + geom_hline(yintercept = 1.30103, linetype = "dashed", color = brewer.pal(9, "Set1")[1], size = 1)
pg <- pg + ylab(expression(paste(-log[10], "(p-value)")))
pg2 <- pg
fig_tmp <- ggarrange(pg1, pg2, nrow = 2, ncol = 1, labels=c("(a)", "(b)"), font.label = list(size=9), widths = c(2, 1))

# plot --------------------------------------------------------------------

tmp1 <- assoc_df
tmp1 <- rename(tmp1, Domain = BiochemicalDomain)
tmp2 <- assoc_df2
tmp2 <- rename(tmp2, Domain = ImmunoDomain)
tmp <- bind_rows(tmp1, tmp2)
tmp %>% filter(Domain %in% c("Viral Control")) -> tmp
tmp$Domain <- factor(tmp$Domain, levels = c("Viral Control"))

pg <- ggplot(data = tmp, mapping = aes(x = Sector, y = (1/rawp), fill = Approach))
pg <- pg + geom_col(position = "dodge")
pg <- pg + facet_grid(cols = vars(Domain), labeller = label_wrap_gen(width = 20, multi_line = TRUE), scales = "free")
pg <- pg + scale_x_discrete(labels = c("Sector 1", "Sector 2", "Sector 3", "Sector 4", "Sector 5", "Sector 6"))

minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))
pg <- pg + scale_y_continuous(trans = "log10", expand = c(0,0), breaks = c(1e1, 1e3, 1e5, 1e7), limits = c(1, 1e7), labels = trans_format("log10", math_format(10^.x)), minor_breaks = minor_breaks)
pg <- pg + scale_fill_manual(values = c(brewer.pal(9, "Set1")[9], brewer.pal(9, "Set1")[3], brewer.pal(9, "Set1")[2]), aesthetics = "fill", labels = c("Dahirel et al., 2011", "Quadeer et al., 2018", "Present Work"))
pg <- pg + theme_minimal() + theme(legend.position = "bottom") + guides(fill=guide_legend(title="", label.theme = element_text(face = "plain", size = 9)))
pg <- pg + theme(strip.background = element_rect(fill = "white", colour = "white"), strip.text = element_blank()) 
pg <- pg + theme(axis.ticks.y = element_blank()) + theme(axis.title.x = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + theme(axis.title.y = element_text(face = "bold", size = 10))
pg <- pg + theme(panel.grid.minor.y = element_line(linetype = "dotted", color = "grey90")) + theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))
pg <- pg + theme(axis.text.x = element_text(face = "bold", size = 10, angle = 0, hjust = 0.5, vjust = 1), axis.text.y = element_text(face = "bold", size = 10)) + theme(axis.ticks.x = element_line(color = "grey40", size = 1)) 
pg <- pg + geom_hline(yintercept = 0) + geom_hline(yintercept = 1/0.05, linetype = "dashed", color = brewer.pal(9, "Set1")[1], size = 1)
pg <- pg + ylab(expression(paste("Statistical significance (p-value)")^-1))
pg <- pg + ggtitle("Association of sectors with known protective epitopes")
pg3 <- pg
fig_tmp <- pg3

ggsave(filename = "Figures/Fig4a.png", plot = fig_tmp, width = 6, height = 4,  units = "in", dpi=600)
