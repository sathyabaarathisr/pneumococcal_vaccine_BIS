### Figure 3d - Th1, Th17, & Th2 % at baseline in men vs. women

#load the required libraries
library(ggplot2)
library(RColorBrewer)
library(ggpubr)
library(ggsignif)
library(reshape2)

setwd("$/Figure3/")
Fig3d <- read.csv(file="Fig3d_input.csv", header=TRUE, sep=",")

TH_sexdiff <- ggplot(Fig3d, aes(x=Gender, y=value, fill=Gender)) +
  geom_boxplot() +
  geom_jitter(width=0.1, size=2)+
  scale_fill_manual(values=c("Men"="#5EC3AA", "Women"="#ECC01C"))+
  facet_wrap(~CellType, scales = "free", ncol=3) +
  geom_signif(comparisons = list(c("Men","Women")),
              test="wilcox.test" )+
  theme_bw()+ ylab("Cell (log 2)") +
  theme(strip.background = element_blank(), strip.text.y = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(color="black", size=16 ),
        axis.text.y = element_text(color="black", size=16),
        axis.title.x = element_text(color="black", size=18 ),
        axis.title.y = element_text(color="black", size=18),
        strip.text = element_text(face="bold", size=18,lineheight=4.0)
        ) 
pdf(file="Fig3d.pdf", height = 5, width=8)
TH_sexdiff
dev.off()

