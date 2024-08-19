### Figure3ab
### Figure3a - Plasmablast  in PCV13 and PPSV23
### Figure3b - ICOS+ TFH in PCV13 and PPSV23

#set the working directory
setwd("$/Figure/")

#load the required libraries
library(ggplot2)
library(RColorBrewer)
library(ggpubr)
library(ggsignif)
library(reshape2)

#load the inputs
plasmablast <- read.csv(file="Fig3a_input.csv", header = T,sep=",")
ICOS_TFH <- read.csv(file="Fig3b_input.csv", header = T,sep=",")

plasmablast$Day <- factor(plasmablast$Day, level=unique(plasmablast$Day))
ICOS_TFH$Day <- factor(ICOS_TFH$Day, level=unique(ICOS_TFH$Day))

#color key
collable_ordered <- c("PCV13" ="#002060", "PPSV23"="#911403")

# Figure3a - Plasmablast in PCV13 and PPSV23
Cluster <- as.vector(unique(plasmablast$vaccine))
plasmablast.plot <- lapply(Cluster,
                        Response_bar <- function(Cluster) {
                        colval <- collable_ordered[Cluster]
                          Cell <- subset(plasmablast, vaccine==Cluster)
                          filename <-paste0(print(Cluster), "_DC.pdf")
                          yname <- paste(print(Cluster))
                          
                          boxresponse_percentage <- ggplot(Cell, aes(x=Day, y=Flow)) +
                            geom_boxplot()+
                            geom_line(aes(group=ID), colour="#989898",linetype=1, size=0.2) +
                            geom_point( size=2) +
                            geom_point(colour= colval,size=2) +
                            geom_signif(comparisons = list(c("Baseline","Day1"),
                                                           c("Baseline", "Day10"), c("Baseline", "Day28"), c("Baseline", "Day60")), 
                                        y_position = c(8, 9, 10, 11),
                                        test = "wilcox.test", textsize = 5) + #wilcox test
                            theme_bw()+ xlab("") + ylab("Plasmablast (cells per ul)") +
                            ggtitle(yname)+
                            theme(strip.background = element_blank(), strip.text.y = element_blank(),
                                  legend.position = "none",
                                  axis.text.y = element_text(color="black", size=10 ),
                                  axis.text.x = element_text(color="black", size=10, angle = 45),
                                  axis.title.x = element_text(color="black", size=12 ),
                                  axis.title.y = element_text(color="black", size=12),
                                  strip.text = element_text(face="bold", size=10,lineheight=3.0)
                            ) +
                            coord_cartesian(ylim = c(0, 12))
                          
                          #ggsave(filename)
                          boxresponse_percentage
                        }
)

pb_allplots <- ggarrange(plotlist=plasmablast.plot,
                      ncol = 2, nrow = 1)

pdf(file = "Figure3a_plasmablast_longitudinal.pdf", height = 5, width = 14)
pb_allplots
dev.off()


# Figure3b - ICOS+ TFH in PCV13 and PPSV23
Cluster <- as.vector(unique(ICOS_TFH$vaccine))
ICOS_TFH.plot <- lapply(Cluster,
                           Response_bar <- function(Cluster) {
                             colval <- collable_ordered[Cluster]
                             Cell <- subset(ICOS_TFH, vaccine==Cluster)
                             filename <-paste0(print(Cluster), "_DC.pdf")
                             yname <- paste(print(Cluster))
                             
                             boxresponse_percentage <- ggplot(Cell, aes(x=Day, y=Flow)) +
                               geom_boxplot()+
                               geom_line(aes(group=ID), colour="#989898",linetype=1, size=0.2) +
                               geom_point( size=2) +
                               geom_point(colour= colval,size=2) +
                               geom_signif(comparisons = list(c("Baseline","Day1"),
                                                              c("Baseline", "Day10"), c("Baseline", "Day28"), c("Baseline", "Day60")), 
                                           y_position = c(20, 22, 24, 26),
                                           test = "wilcox.test", textsize = 5) + #wilcox test
                               theme_bw()+ xlab("") + ylab("ICOS+ TFH (cells per ul)") +
                               ggtitle(yname)+
                               theme(strip.background = element_blank(), strip.text.y = element_blank(),
                                     legend.position = "none",
                                     axis.text.y = element_text(color="black", size=10 ),
                                     axis.text.x = element_text(color="black", size=10, angle = 45),
                                     axis.title.x = element_text(color="black", size=12 ),
                                     axis.title.y = element_text(color="black", size=12),
                                     strip.text = element_text(face="bold", size=10,lineheight=3.0)
                               ) +
                               coord_cartesian(ylim = c(0, 28))
                             
                             #ggsave(filename)
                             boxresponse_percentage
                           }
)

allplots_ICOS.TFH <- ggarrange(plotlist=ICOS_TFH.plot,
                      ncol = 2, nrow = 1)

pdf(file = "Figure3a_ICOS_TFH_longitudinal.pdf", height = 5, width = 14)
allplots_ICOS.TFH
dev.off()