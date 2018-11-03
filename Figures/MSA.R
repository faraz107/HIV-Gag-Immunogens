# Randomization of MSA ----------------------------------------------------

library(tidyverse)
library(RColorBrewer)
library(purrr)
library(foreach)
library(doParallel)

Bcap <- readxl::read_xlsx("Data files/Bcap.xlsx", col_names = as.character(c(1:451)))
Bcap <- as.data.frame(Bcap)

maxE <- (as.data.frame(map(.x = Bcap, .f = sample)) %>% cor() %>% eigen())$values %>% max()

system.time({(as.data.frame(map(.x = Bcap, .f = sample)) %>% cor() %>% eigen())$values %>% max()})

cl <- makeCluster(detectCores())
registerDoParallel(cl = cl, cores = detectCores())

system.time(
  {
    maxE1 <- foreach(i=1:1e4, .combine = cbind) %dopar% 
    {library(tidyverse)
      max(eigen(cor(as.data.frame(map(.x = Bcap, .f = sample))))$values)}
  }
)

system.time(
  {
    maxE2 <- foreach(i=1:1e4, .combine = cbind) %do% 
    {library(tidyverse)
      max(eigen(cor(as.data.frame(map(.x = Bcap, .f = sample))))$values)}
  }
)

system.time({
  for (i in ncol(Bcap)) {
    Bcap[,i] <- sample(Bcap[,i])  
  }
  (as.data.frame(Bcap) %>% cor() %>% eigen())$values %>% max()
})


# EVD

library(RMTstat)

Bcap %>% cor() %>% eigen() -> evd 

evals <- as.data.frame(evd$values) 
colnames(evals) <- "values"
evectors <- as.data.frame(evd$vectors)

evals$mp_x <- seq(0,6.5,length.out = 451)
evals$mp_y <- dmp(evals$mp_x, svr = 1845/451)

coloRs <- RColorBrewer::brewer.pal(8, "Set2")

c <- ggplot(data = evals)
# c <- c + geom_line(aes(x = mp_x, y = mp_y), size = 1.2, color = coloRs[1])
c <- c + geom_histogram(aes(x = values, y = ..density..), bins = 451, fill = coloRs[3], color = "grey45", show.legend = FALSE)
# c <- c + geom_line(aes(x = mp_x, y = mp_y), color = coloRs[2], size = 1.2)
c <- c + scale_color_discrete(name = "", label = "MP-Law")
c <- c + geom_segment(aes(x = 4, xend = 4, y = 0, yend = 0.80), lineend = "butt", show.legend = FALSE, color = "black", linetype = "dashed", size = 1)

# c <- c + annotate("text", x=evals$values[7], y=0.3, label="lambda[7]", parse=TRUE, size = 5) + annotate("text", x=evals$values[3], y=0.3, label="lambda[3]", parse=TRUE, size = 5)

c <- c + annotate(geom = "text", x = 4, y = 0.90, color = "black", size = 3.5, label = expression(paste(lambda[max]^"rnd")))

# c <- c + annotate(geom = "text", x = 4.969326, y = 0.20, color = "grey45", size = 7, label = expression(paste(lambda[3])))
# c <- c + annotate(geom = "text", x = 3.978187, y = 0.20, color = "grey45", size = 7, label = expression(paste(lambda[7])))

c <- c + guides(color = guide_legend(label.theme = element_text(color = coloRs[1])))

c <- c + xlab(expression("Eigenvalue"~(lambda)))
c <- c + ylab(expression("Density")) 
c <- c + scale_y_continuous(limits = c(0,1), expand = c(0, 0))
c <- c + scale_x_continuous(limits = c(0,6.5), expand = c(0,0), breaks = c(4.97, 3.98), labels = c(expression(paste(lambda[3])), expression(paste(lambda[7]))))
c <- c + theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "white"),
  legend.position = c(0.75, 0.95),
  # legend.box.background = element_rect(size = 1),
  # legend.box.margin = margin(1, 1, 1, 1),
  legend.key = element_blank(),
  # axis.text.x = element_blank(),
  axis.text.x = element_text(angle = 0, size = 10, vjust = 0, face = "bold", color = "black"),
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank(),
  axis.line.y = element_blank(),
  # axis.line.x = element_line(color = coloRs[9]),
  # panel.grid.major.x = element_line(color = "grey80", linetype = "dotted"),
  # panel.grid.minor.x = element_line(color = "grey80", linetype = "dotted"),
  # panel.grid.major.y = element_line(color = "grey80", linetype = "dotted"),
  # panel.grid.minor.y = element_line(color = "grey80", linetype = "dotted"),  
  # axis.line = element_line(color = "black", linetype = "solid"),
  axis.title.x = element_text(face = "bold", size = 12, angle = 0, hjust = 0.5, vjust = 0.5),
  axis.title.y = element_text(face = "bold", size = 12, angle = 90, hjust = 0.5, vjust = 0.5))

c <- c + theme(plot.margin = unit(c(0.5,0,0,0), "lines"))

evdg <- c
ggsave(filename = "Fig1A.png", plot = evdg, width = 7.5, units = "in", dpi=600)


# IPRs --------------------------------------------------------------------

library(readr)
library(tidyverse)
library(RColorBrewer)
library(ggpubr)

sampl37ipr <- read_csv("~/MATLAB/Research/Gag/sampl37ipr", col_names = FALSE)
sampl37ipr <- as.data.frame(sampl37ipr)
# colnames(sampl37ipr) <- paste("ipr", c(1:451), sep = "")
colnames(sampl37ipr) <- c(1:451)

iprs <- gather(data = sampl37ipr, key = "iprNum", value = "iprValue")

iprs %>% group_by(iprNum) %>% mutate(iprMax = max(iprValue)) -> iprs
iprs %>% group_by(iprNum) %>% mutate(iprMin = min(iprValue)) -> iprs
iprs %>% group_by(iprNum) %>% mutate(iprMean = median(iprValue)) -> iprs
iprs %>% group_by(iprNum) %>% mutate(ipr05 = quantile(iprValue, c(0.05))) -> iprs
iprs %>% group_by(iprNum) %>% mutate(ipr95 = quantile(iprValue, c(0.95))) -> iprs

iprs$iprNum <- as.numeric(iprs$iprNum)

iprs %>% select(-iprValue) %>% distinct() %>% ungroup() %>% as.data.frame() -> iprs_range

iprs_range$index <- seq.int(from = 1, to = 451, by = 1)

coloRs1 <- RColorBrewer::brewer.pal(9, "Set1")
coloRsB <- RColorBrewer::brewer.pal(9, "Blues")

# s <- ggplot(data = iprs_range %>% filter(iprNum >= 400), mapping = aes(x = iprNum))
# iprs_range %>% filter(iprNum >= 431) -> tmp
# s <- ggplot(data = tmp, mapping = aes(x = iprNum))

iprs %>% filter(iprNum >= 436) -> tmp2
tmp2 <- tmp2 %>% group_by(iprNum) %>% mutate(outlier = iprValue > median(iprValue) + IQR(iprValue)*1.5) %>% ungroup()
s <- ggplot(data = tmp2, mapping = aes(x = as.factor(iprNum), y = iprValue))

#s <- s + geom_point(aes(y = iprMean), color = coloRsB[9])
# s <- s + geom_segment(aes(y = ipr05, yend = ipr95, xend = iprNum), color = coloRsB[5], size = 0.75) 
#s <- s + geom_ribbon(aes(ymin = ipr05, ymax = ipr95), fill = coloRsB[5], alpha = 0.8) 
# s <- s + geom_point(aes(y = iprMean), color = coloRs1[2], show.legend = TRUE, size = 1)

s <- s + stat_boxplot(geom = "errorbar", coef = 1.5, color = coloRsB[6]) + geom_boxplot(color = coloRsB[7], outlier.shape = NA, outlier.alpha = 0.50, outlier.fill = coloRsB[6], outlier.color = coloRsB[6])
# s <- s + ylim(0, 0.25) + xlim(0,455)
s <- s + ylim(0, 0.15) + xlim(436,455)

# s <- s + scale_y_continuous(expand = c(0, 0.01), breaks = seq(0.05, 0.3, 0.05))
s <- s + scale_y_continuous(breaks = seq(0.02, 0.12, 0.02))
# s <- s + scale_x_continuous(expand = c(0, 3), breaks = c(50, 100, 150, 200, 250, 300, 350, 400, 450), labels = c(400, 350, 300, 250, 200, 150, 100, 50, 1))
s <- s + scale_x_discrete(expand = c(0, 1), breaks = c(seq.int(from = 436, to = 451, by = 1)), labels = c(15, "","","","", 10, "","","","", 5, "","","","", 1))

# s <- s + coord_trans(x = "log10", y = "log10")

s <- s + xlab("Rank of Eigenvlaues") + ylab("Value of IPR")

s <- s + theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "white"),
  legend.position = "bottom",
  legend.key = element_blank(),
  panel.grid.major.x = element_line(linetype = "dotted", color = "grey85"),
  panel.grid.minor.x = element_line(linetype = "dotted", color = "grey85"),
  axis.text.x = element_text(angle = 0, size = 8, vjust = 0),
  axis.text.y = element_text(angle = 0, size = 8),
  axis.line.y = element_line(color = coloRs1[9]),
  axis.line.x = element_line(color = coloRs1[9]),
  axis.title.x = element_text(face = "bold", size = 10, angle = 0, hjust = 0.5, vjust = 0.5),
  axis.title.y = element_text(face = "bold", size = 10, angle = 90, hjust = 0.5, vjust = 0.5))

s <- s + coord_cartesian(ylim = c(0,0.12))


s1 <- ggplot(data = iprs_range %>% filter(iprNum>=444), mapping = aes(x = iprNum))
#s1 <- s1 + geom_ribbon(aes(ymin = ipr05, ymax = ipr95), fill = coloRsB[5], alpha = 0.8) 
s1 <- s1 + geom_segment(aes(y = ipr05, yend = ipr95, xend = iprNum), color = coloRsB[5], size = 1.1) 

s1 <- s1 + geom_point(aes(y = iprMean), color = coloRs1[2], size = 1)
#s1 <- s1 + geom_line(aes(y = iprMean), color = "grey90", linetype = "dotted")

s1 <- s1 + scale_x_continuous(breaks = c(444, 445, 446, 447, 448, 449, 450, 451))
s1 <- s1 + scale_y_continuous(breaks = c(0, 0.05, 0.1))

# s1 <- s1 + coord_trans(x = "log10", y = "log10")

s1 <- s1 + ylim(0, 0.10)

s1 <- s1 + theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "white", color = "grey70"),
  legend.position = "none",
  legend.key = element_blank(),
  axis.text.x = element_text(angle = 0, size = 8, vjust = 0),
  axis.text.y = element_text(angle = 0, size = 8),
  #axis.ticks.y = element_blank(),
  axis.line.y = element_blank(),
  axis.line.x = element_line(color = coloRs1[9]),
  axis.title.x = element_blank(),#element_text(face = "bold", size = 12, angle = 0, hjust = 0.5, vjust = 0.5),
  axis.title.y = element_blank())

s <- s + annotation_custom(ggplotGrob(s1), xmin = 270, ymin = 0.10)

s

# fig6 <- s
# 
# ggsave(filename = "fig6.png", plot = fig6, width = 183, units = "mm", dpi=600)

fig6a <- s

ggsave(filename = "fig6a2.png", plot = fig6a, width = 183, units = "mm", dpi=600)




# 9PCs --------------------------------------------------------------------

library(tidyverse)

undetected_9PCs <- readxl::read_xlsx("undetected_9PCs.xlsx", col_names = FALSE)
undetected_9PCs <- as.data.frame(undetected_9PCs)

colnames(undetected_9PCs) <- seq.int(from = 1, to = 9, by = 1)

tmp3 <- as.data.frame(undetected_9PCs)
tmp3 <- gather(data = tmp3, key = "iprNum", value = "numSites")
tmp3$iprNum <- as.numeric(tmp3$iprNum)

s3 <- ggplot(data = tmp3, mapping = aes(x = as.factor(iprNum), y = numSites))

s3 <- s3 + ylim(0, 0.15) + xlim(436,455)




