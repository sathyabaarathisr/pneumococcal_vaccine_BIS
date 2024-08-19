### Figure3e - TH1/TH17 ratio vs. age in men and women

#load the required libraries
library(ggplot2)
library(RColorBrewer)
library(ggpubr)
library(ggsignif)
library(reshape2)

#set the working directory
setwd("$/Figure3/")

#load the required input data
TH1Th17 <- read.csv(file="Fig3e_input.csv", header=TRUE, sep=",")
Women_Age <- subset(TH1Th17, Gender=="Women")
Men_Age <- subset(TH1Th17, Gender=="Men")

### Correlation between Age and TH1/TH17 in men 
AgevsTh1.17_men <- ggscatter(Men_Age, x = "Age", y = "Th1.Th17", 
                     color = "black",
                     add.params = list(color="#5EC3AA"),
                       #shape = "Sex", 
                       fill="grey90", size=3,
                       add = "reg.line", conf.int = TRUE,
                       cor.coef = TRUE, cor.method = "pearson",
                       xlab = "Age (in years)", ylab = "Th1Th17",
                       font.label = c(16, "plain")+
                         theme(strip.background = element_blank(), strip.text.y = element_blank(),
                               legend.position = "none",
                               axis.text.x = element_text(color="black", size=16 ),
                               axis.text.y = element_text(color="black", size=16),
                               axis.title.x = element_text(color="black", size=18 ),
                               axis.title.y = element_text(color="black", size=18),
                               strip.text = element_text(face="bold", size=18,lineheight=4.0)))


### Correlation between Age and TH1/TH17 in women
AgevsTh1.17_women <- ggscatter(Women_Age, x = "Age", y = "Th1.Th17", color = "black",
                       add.params = list(color="#ECC01C"),
                       #shape = "Sex", 
                       fill="black", size=3,
                       add = "reg.line", conf.int = TRUE,
                       cor.coef = TRUE, cor.method = "pearson",
                       xlab = "Age (in years)", ylab = "Th1Th17",
                       font.label = c(16, "plain")+
                         theme(strip.background = element_blank(), strip.text.y = element_blank(),
                               legend.position = "none",
                               axis.text.x = element_text(color="black", size=16 ),
                               axis.text.y = element_text(color="black", size=16),
                               axis.title.x = element_text(color="black", size=18 ),
                               axis.title.y = element_text(color="black", size=18),
                               strip.text = element_text(face="bold", size=18,lineheight=4.0))) 


ggsave(file="Figure3e_SexvsAge.pdf", 
       ggarrange(AgevsTh1.17_men, AgevsTh1.17_women, 
                 ncol = 2, nrow = 1), height = 4, width=8)
dev.off()
