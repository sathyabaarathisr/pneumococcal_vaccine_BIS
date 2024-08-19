# Rank vs baseline cellular correlates
# Figure 3C_1 - PCV13 Rank vs Th1, Th17 and Th2 percentages at baseline
# Figure 3C_2 - PPSV23 Rank vs Th1, Th17 and Th2 percentages at baseline

#required libraries
library(ggplot2)
library(RColorBrewer)
library(ggpubr)
library(ggsignif)
library(reshape2)

setwd("$/Figure3c/")

#load the input data
Th1Th17 <- read.csv(file="Fig3c_input.csv", header=TRUE, sep=",")
PCV13 <- subset(Th1Th17, Vaccine=="PCV13")
PPSV23 <- subset(Th1Th17, Vaccine=="PPSV23")

#Figure 3c_1
TH1Rank <- ggscatter(PCV13, x = "Rank", y = "Th1", color = "#002060", 
                      fill="grey90", size=3,
                      add = "reg.line", conf.int = TRUE,
                      cor.coef = TRUE, cor.method = "pearson",
                      xlab = "PCV13 Rank", ylab = "Th1",
                      font.label = c(16, "plain")+
                        theme(strip.background = element_blank(), strip.text.y = element_blank(),
                              legend.position = "none",
                              axis.text.x = element_text(color="black", size=16 ),
                              axis.text.y = element_text(color="black", size=16),
                              axis.title.x = element_text(color="black", size=18 ),
                              axis.title.y = element_text(color="black", size=18),
                              strip.text = element_text(face="bold", size=18,lineheight=4.0)))


TH17Rank <- ggscatter(PCV13, x = "Rank", y = "Th17", color = "#002060", 
                          fill="grey90", size=3,
                          add = "reg.line", conf.int = TRUE,
                          cor.coef = TRUE, cor.method = "pearson",
                          xlab = "PCV13 Rank", ylab = "Th17",
                          font.label = c(16, "plain")+
                            theme(strip.background = element_blank(), strip.text.y = element_blank(),
                                  legend.position = "none",
                                  axis.text.x = element_text(color="black", size=16 ),
                                  axis.text.y = element_text(color="black", size=16),
                                  axis.title.x = element_text(color="black", size=18 ),
                                  axis.title.y = element_text(color="black", size=18),
                                  strip.text = element_text(face="bold", size=18,lineheight=4.0)))

TH2Rank <- ggscatter(PCV13, x = "Rank", y = "Th2", color = "#002060", 
                          fill="grey90", size=3,
                          add = "reg.line", conf.int = TRUE,
                          cor.coef = TRUE, cor.method = "pearson",
                          xlab = "PCV13 Rank", ylab = "Th2",
                          font.label = c(16, "plain")+
                            theme(strip.background = element_blank(), strip.text.y = element_blank(),
                                  legend.position = "none",
                                  axis.text.x = element_text(color="black", size=16 ),
                                  axis.text.y = element_text(color="black", size=16),
                                  axis.title.x = element_text(color="black", size=18 ),
                                  axis.title.y = element_text(color="black", size=18),
                                  strip.text = element_text(face="bold", size=18,lineheight=4.0)))


ggsave(file="Fig3C_1.pdf", 
       ggarrange(TH1Rank, TH17Rank, TH2Rank,
                 ncol = 3, nrow = 1), height = 4, width=12)
dev.off()


########
## Figure3c_2
TH1Rank <- ggscatter(PPSV23, x = "Rank", y = "Th1", color = "#911403",
                         #shape = "Sex", 
                         fill="grey90", size=3,
                         add = "reg.line", conf.int = TRUE,
                         cor.coef = TRUE, cor.method = "pearson",
                         xlab = "PCV13 Rank", ylab = "Th1",
                         font.label = c(16, "plain")+
                           theme(strip.background = element_blank(), strip.text.y = element_blank(),
                                 legend.position = "none",
                                 axis.text.x = element_text(color="black", size=16 ),
                                 axis.text.y = element_text(color="black", size=16),
                                 axis.title.x = element_text(color="black", size=18 ),
                                 axis.title.y = element_text(color="black", size=18),
                                 strip.text = element_text(face="bold", size=18,lineheight=4.0)))


TH17Rank <- ggscatter(PPSV23, x = "Rank", y = "Th17", color = "#911403", 
                          fill="grey90", size=3,
                          add = "reg.line", conf.int = TRUE,
                          cor.coef = TRUE, cor.method = "pearson",
                          xlab = "PCV13 Rank", ylab = "Th17",
                          font.label = c(16, "plain")+
                            theme(strip.background = element_blank(), strip.text.y = element_blank(),
                                  legend.position = "none",
                                  axis.text.x = element_text(color="black", size=16 ),
                                  axis.text.y = element_text(color="black", size=16),
                                  axis.title.x = element_text(color="black", size=18 ),
                                  axis.title.y = element_text(color="black", size=18),
                                  strip.text = element_text(face="bold", size=18,lineheight=4.0)))

TH2Rank <- ggscatter(PPSV23, x = "Rank", y = "Th2", color = "#911403", 
                         fill="grey90", size=3,
                         add = "reg.line", conf.int = TRUE,
                         cor.coef = TRUE, cor.method = "pearson",
                         xlab = "PCV13 Rank", ylab = "Th2",
                         font.label = c(16, "plain")+
                           theme(strip.background = element_blank(), strip.text.y = element_blank(),
                                 legend.position = "none",
                                 axis.text.x = element_text(color="black", size=16 ),
                                 axis.text.y = element_text(color="black", size=16),
                                 axis.title.x = element_text(color="black", size=18 ),
                                 axis.title.y = element_text(color="black", size=18),
                                 strip.text = element_text(face="bold", size=18,lineheight=4.0)))

ggsave(file="Fig3C_2.pdf", 
       ggarrange(TH1Rank, TH17Rank, TH2Rank,
                 ncol = 3, nrow = 1), height = 4, width=12)
dev.off()