### Figure3 - Vaccine responsiveness rank vs. baseline transcriptional correlates
#Figure3f_1: Expression of CYTOX genes vs PCV13 rank
#Figure3f_2: Expression of CYTOX genes vs PPSV23 rank
#Figure3g:   CYTOX expression in men and women (only PCV13 ohort)
#Figure3h:   Cytox genes vs age

#set the working directory
setwd("$/Figure3/")

#load the required libraries
library(ggplot2)
library(RColorBrewer)
library(readxl)
library(ggpubr)
library(ggsignif)
library(reshape2)

#load the required inputs
PCV13_cytoxgenes <- read_excel(path="Fig3f_input.xls", sheet = "Figure3f_PCV13") %>% 
  as.data.frame
PPSV23_cytoxgenes <- read_excel(path="Fig3f_input.xls", sheet = "Figure3f_PPSV23") %>% 
  as.data.frame

##### Figure 3f. Expression of CYTOX genes vs vaccine responsiveness rank ###
## PCV13 Rank vs CYTOX expression
NCAM1_PCV13 <- ggscatter(PCV13_cytoxgenes, x = "Rank", y = "NCAM1", color = "#002060",
                           #shape = "Sex", 
                           fill="grey90", size=3,
                           add = "reg.line", conf.int = TRUE,
                           cor.coef = TRUE, cor.method = "pearson",
                           xlab = "PCV13 FCRank", ylab = "NCAM1",
                           font.label = c(16, "plain")+
                             theme(strip.background = element_blank(), strip.text.y = element_blank(),
                                   legend.position = "none",
                                   axis.text.x = element_text(color="black", size=16 ),
                                   axis.text.y = element_text(color="black", size=16),
                                   axis.title.x = element_text(color="black", size=18 ),
                                   axis.title.y = element_text(color="black", size=18),
                                   strip.text = element_text(face="bold", size=18,lineheight=4.0)))

GNLY_PCV13 <- ggscatter(PCV13_cytoxgenes, x = "Rank", y = "GNLY", color = "#002060", 
                          fill="grey90", size=3,
                          add = "reg.line", conf.int = TRUE,
                          cor.coef = TRUE, cor.method = "pearson",
                          xlab = "PCV13 FCRank", ylab = "GNLY",
                          font.label = c(16, "plain")+
                            theme(strip.background = element_blank(), strip.text.y = element_blank(),
                                  legend.position = "none",
                                  axis.text.x = element_text(color="black", size=16 ),
                                  axis.text.y = element_text(color="black", size=16),
                                  axis.title.x = element_text(color="black", size=18 ),
                                  axis.title.y = element_text(color="black", size=18),
                                  strip.text = element_text(face="bold", size=18,lineheight=4.0)))

PRF1_PCV13 <- ggscatter(PCV13_cytoxgenes, x = "Rank", y = "PRF1", color = "#002060", 
                          fill="grey90", size=3,
                           add = "reg.line", conf.int = TRUE,
                           cor.coef = TRUE, cor.method = "pearson",
                           xlab = "PCV13 FCRank", ylab = "PRF1",
                          font.label = c(16, "plain")+
                            theme(strip.background = element_blank(), strip.text.y = element_blank(),
                                  legend.position = "none",
                                  axis.text.x = element_text(color="black", size=16 ),
                                  axis.text.y = element_text(color="black", size=16),
                                  axis.title.x = element_text(color="black", size=18 ),
                                  axis.title.y = element_text(color="black", size=18),
                                  strip.text = element_text(face="bold", size=18,lineheight=4.0)))

ggsave(file="Fig3f_1.pdf", 
       ggarrange(NCAM1_PCV13, GNLY_PCV13, PRF1_PCV13,
                 ncol = 3, nrow = 1), height = 4, width=12)
dev.off()


#### PPSV23 Rank vs CYTOX expression

NCAM1_PPSV23 <- ggscatter(PPSV23_cytoxgenes, x = "Rank", y = "NCAM1", color = "#911403",
                           fill="grey90", size=3,
                           add = "reg.line", conf.int = TRUE,
                           cor.coef = TRUE, cor.method = "pearson",
                           xlab = "PPSV23 FCRank", ylab = "NCAM1",
                           font.label = c(16, "plain")+
                             theme(strip.background = element_blank(), strip.text.y = element_blank(),
                                   legend.position = "none",
                                   axis.text.x = element_text(color="black", size=16 ),
                                   axis.text.y = element_text(color="black", size=16),
                                   axis.title.x = element_text(color="black", size=18 ),
                                   axis.title.y = element_text(color="black", size=18),
                                   strip.text = element_text(face="bold", size=18,lineheight=4.0)))

GNLY_PPSV23 <- ggscatter(PPSV23_cytoxgenes, x = "Rank", y = "GNLY", color = "#911403", 
                          fill="grey90", size=3,
                          add = "reg.line", conf.int = TRUE,
                          cor.coef = TRUE, cor.method = "pearson",
                          xlab = "PPSV23 FCRank", ylab = "GNLY", 
                          font.label = c(16, "plain")+
  theme(strip.background = element_blank(), strip.text.y = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(color="black", size=16 ),
        axis.text.y = element_text(color="black", size=16),
        axis.title.x = element_text(color="black", size=18 ),
        axis.title.y = element_text(color="black", size=18),
        strip.text = element_text(face="bold", size=18,lineheight=4.0)))


PRF1_PPSV23 <- ggscatter(PPSV23_cytoxgenes, x = "Rank", y = "PRF1",color = "#911403", 
                          #shape = "Sex", 
                          fill="grey90", size=3,
                          add = "reg.line", conf.int = TRUE,
                          cor.coef = TRUE, cor.method = "pearson",
                          xlab = "PPSV23 FCRank", ylab = "PRF1",
                          font.label = c(16, "plain")+
                            theme(strip.background = element_blank(), strip.text.y = element_blank(),
                                  legend.position = "none",
                                  axis.text.x = element_text(color="black", size=16 ),
                                  axis.text.y = element_text(color="black", size=16),
                                  axis.title.x = element_text(color="black", size=18 ),
                                  axis.title.y = element_text(color="black", size=18),
                                  strip.text = element_text(face="bold", size=18,lineheight=4.0)))


ggsave(file="Figure3f_2.pdf", 
       ggarrange(NCAM1_PPSV23, GNLY_PPSV23, PRF1_PPSV23,
                 ncol = 3, nrow = 1), height = 4, width=12)
dev.off()

####### Figure3g. CYTOX expression in men and women (only PCV13 ohort) ####

Fig3g <- read.csv(file="Fig3g_input.csv", header = T, sep=",")
PCV13_cyto_sex <- melt(Fig3g)

cytox_sex.plot <- ggplot(PCV13_cyto_sex, aes(x=Sex, y=value, fill=Sex)) +
  geom_boxplot() +
  geom_jitter(width=0.1, size=2)+
  scale_fill_manual(values=c("Men"="#5EC3AA", "Women"="#ECC01C"))+
  facet_wrap(~variable, scales = "free", ncol=3) +
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
  ) +
  ylim(3,10)


pdf(file="Fig3g.pdf", height = 5, width=8)
cytox_sex.plot
dev.off()

##### Figure3h. Cytox genes vs age 

Fig3h <- read.csv(file="Fig3h_input.csv", header=TRUE, sep=",", row.names = 1)

GNLY_Age <- ggscatter(Fig3h, x = "Age", y = "GNLY", color = "#002060",
                          fill="grey90", size=2,
                          add = "reg.line", conf.int = TRUE,
                          cor.coef = TRUE, cor.method = "pearson",
                          xlab = "PCV13 Age(in years)", ylab = "GNLY",
                          font.label = c(16, "plain")+
                            theme(strip.background = element_blank(), strip.text.y = element_blank(),
                                  legend.position = "bottom",
                                  axis.text.x = element_text(color="black", size=16 ),
                                  axis.text.y = element_text(color="black", size=16),
                                  axis.title.x = element_text(color="black", size=18 ),
                                  axis.title.y = element_text(color="black", size=18),
                                  strip.text = element_text(face="bold", size=18,lineheight=4.0)))

NCAM1_Age <- ggscatter(Fig3h, x = "Age", y = "NCAM1", color = "#002060",
                           fill="grey90", size=2,
                           add = "reg.line", conf.int = TRUE,
                           cor.coef = TRUE, cor.method = "pearson",
                           xlab = "PCV13 Age(in years)", ylab = "NCAM1",
                           font.label = c(16, "plain")+
                             theme(strip.background = element_blank(), strip.text.y = element_blank(),
                                   legend.position = "bottom",
                                   axis.text.x = element_text(color="black", size=16 ),
                                   axis.text.y = element_text(color="black", size=16),
                                   axis.title.x = element_text(color="black", size=18 ),
                                   axis.title.y = element_text(color="black", size=18),
                                   strip.text = element_text(face="bold", size=18,lineheight=4.0)))


PRF1_Age <- ggscatter(Fig3h, x = "Age", y = "PRF1", color = "#002060",
                          fill="grey90", size=2,
                          add = "reg.line", conf.int = TRUE,
                          cor.coef = TRUE, cor.method = "pearson",
                          xlab = "PCV13 Age(in years)", ylab = "PRF1",
                          font.label = c(16, "plain")+
                            theme(strip.background = element_blank(), strip.text.y = element_blank(),
                                  legend.position = "bottom",
                                  axis.text.x = element_text(color="black", size=16 ),
                                  axis.text.y = element_text(color="black", size=16),
                                  axis.title.x = element_text(color="black", size=18 ),
                                  axis.title.y = element_text(color="black", size=18),
                                  strip.text = element_text(face="bold", size=18,lineheight=4.0)))


ggsave(file="Fig3h_cytoxvsAge.pdf", 
       ggarrange(NCAM1_Age, GNLY_Age,PRF1_Age,
                 ncol = 3, nrow = 1), height = 4, width=10)
dev.off()

###############

