# Randomization of MSA ----------------------------------------------------

library(tidyverse)
library(RColorBrewer)
library(purrr)
library(foreach)
library(doParallel)

Bcap <- readxl::read_xlsx("Data files/Bcap.xlsx", col_names = as.character(c(1:451)))
Bcap <- as.data.frame(Bcap)

maxE <- (as.data.frame(map(.x = Bcap, .f = sample)) %>% cor() %>% eigen())$values %>% max()

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
c <- c + geom_histogram(aes(x = values, y = ..density..), bins = 451, fill = coloRs[3], color = "grey45", show.legend = FALSE)
c <- c + scale_color_discrete(name = "", label = "MP-Law")
c <- c + geom_segment(aes(x = 4, xend = 4, y = 0, yend = 0.80), lineend = "butt", show.legend = FALSE, color = "black", linetype = "dashed", size = 1)
c <- c + annotate(geom = "text", x = 4, y = 0.90, color = "black", size = 3.5, label = expression(paste(lambda[max]^"rnd")))

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
  legend.key = element_blank(),
  axis.text.x = element_text(angle = 0, size = 10, vjust = 0, face = "bold", color = "black"),
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank(),
  axis.line.y = element_blank(),
  axis.title.x = element_text(face = "bold", size = 12, angle = 0, hjust = 0.5, vjust = 0.5),
  axis.title.y = element_text(face = "bold", size = 12, angle = 90, hjust = 0.5, vjust = 0.5))

c <- c + theme(plot.margin = unit(c(0.5,0,0,0), "lines"))

evdg <- c
ggsave(filename = "Fig1A.png", plot = evdg, width = 7.5, units = "in", dpi=600)






