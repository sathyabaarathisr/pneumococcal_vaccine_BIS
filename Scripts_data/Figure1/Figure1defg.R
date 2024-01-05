#Rscript to compute:  
#Figure1d - Pre vs post cumulative OPA titer
#Figure1e - Age vs Strength, 
#Figure1f - sex vs. PCV13 responsiveness
#Figure1g - sex vs PPSV23 responsiveness
##########

# required libraries
library(ggplot2)
library(RColorBrewer)
library(ggpubr)
library(ggsignif)
library(reshape2)


##### Figure1d Pre vs Post cumulative OPA titers of PCV13 and PPSV23 

Fig1d <- read_excel(path="Figure1defg.xls", sheet = "Figure1d") %>% 
  as.data.frame
Fig1d$Group <- factor(Fig1d$Group, level=unique(Fig1d$Group))

PCV13_Pre ="#B4C7E7"
PCV13_Post ="#002060"
PPSV23_Pre ="#FFB8B5"
PPSV23_Post ="#911403"

Fig1d.plot <- ggplot(Fig1d, aes(x=Group, y=Sum)) +
  geom_boxplot()+
  geom_line(aes(group=ID, colour = "grey90") ,linetype=1, size=0.25) +
  geom_point(aes(color =Group), size=2)+
  scale_color_manual(values=c("PCV13_Pre"="#B4C7E7",
                             "PCV13_Post"="#002060",
                              "PPSV23_Pre"="#FFB8B5", 
                              "PPSV23_Post"="#911403")) +
  geom_signif(comparisons = list(c("PCV13_Pre","PCV13_Post"),c("PPSV23_Pre", "PPSV23_Post"), 
                                 c("PCV13_Pre", "PPSV23_Pre"), c("PCV13_Post", "PPSV23_Post") ),test="wilcox.test", y_position = c(150, 150, 170, 190))+
  theme_bw()+ ylab("Log 2 cumulative OPA titer") +
  theme(strip.background = element_blank(), strip.text.y = element_blank(),
        legend.position = " ",
        axis.text.x = element_text(color="black", size=16 ),
        axis.text.y = element_text(color="black", size=16),
        axis.title.x = element_text(color="black", size=18 ),
        axis.title.y = element_text(color="black", size=18),
        strip.text = element_text(face="bold", size=18,lineheight=4.0)
  ) +
  coord_cartesian(ylim=c(0, 250))

pdf(file="Fig1d.pdf", height = 5, width=8)
Fig1d.plot
dev.off()

#### Figure 1e Age vs Strength for PCV13 and PPSV23

Fig1e_PCV13 <- read_excel(path="Figure1defg.xls", sheet = "Figure1e_PCV13") %>% 
  as.data.frame

Fig1e_PPSV23 <- read_excel(path="Figure1defg.xls", sheet = "Figure1e_PPSV23") %>% 
  as.data.frame

PCV13 <- ggscatter(Fig1e_PCV13, x = "Age", y = "Strength", 
                   color = "#002060", shape="Sex", size=2,
                   add = "reg.line", 
                   conf.int = TRUE,
                   add.params = list(color = "#002060",
                                     fill = "#002060"),
                   cor.coef = TRUE, cor.method = "pearson",
                   xlab = "Age (in years)", 
                   ylab = "Strenght (fold change)",
                   #ylab = "Prevaccination titer (sum of D0)",
                   #ylab = "Posvaccination titer (sum of D35)",
                   #ylab = "Strength of fold change (sum log2)", 
                   font.label = c(16, "plain"), star.plot = FALSE) +
  #scale_x_continuous ( limits=c(0,10)) +
  #scale_y_continuous ( limits=c(0,20)) +
  theme(strip.background = element_blank(), strip.text.y = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_text(color="black", size=16 ),
        axis.text.y = element_text(color="black", size=16),
        axis.title.x = element_text(color="black", size=18 ),
        axis.title.y = element_text(color="black", size=18),
        strip.text = element_text(face="bold", size=18,lineheight=4.0)
  )+xlim(55,90) + ylim(10,85)


PPSV23 <- ggscatter(Fig1e_PPSV23, x = "Age", y = "Strength", 
                    color = "#911403", shape="Sex", size = 2,
                    add = "reg.line", 
                    conf.int = TRUE,
                    add.params = list(color = "#911403",
                                      fill = "#911403"),
                    cor.coef = TRUE, cor.method = "pearson",
                    xlab = "Age (in years)", 
                    #ylab = "Prevaccination titer (sum of D0)",
                    #ylab = "Strength of fold change (sum log2)", 
                    #ylab = "Posvaccination titer (sum of D35)",
                    ylab = "Strenght (fold change)",
                    font.label = c(16, "plain"), star.plot = FALSE) +
  #scale_x_continuous ( breaks=c(50,60,70, 80, 90), limits=c(50,90)) +
  #scale_y_continuous ( breaks=c(0,25,50,75, 100), limits=c(0,100)) +
  theme(strip.background = element_blank(), strip.text.y = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_text(color="black", size=16 ),
        axis.text.y = element_text(color="black", size=16),
        axis.title.x = element_text(color="black", size=18 ),
        axis.title.y = element_text(color="black", size=18),
        strip.text = element_text(face="bold", size=18,lineheight=4.0)
  ) +xlim(55,90) + ylim(10,85)


ggsave(file="Figure1E.pdf", 
       ggarrange(PCV13, PPSV23,
                 ncol = 2, nrow = 1), height = 5, width=8)
dev.off()

### Figure1f - Sex vs PCV13 vaccine responsiveness 
PCV13<- read_excel(path="Figure1defg.xls", sheet = "Figure1f") %>% 
  as.data.frame
PCV13$Label <- factor(PCV13$Label, levels=unique(PCV13$Label))

p1 <- ggplot(PCV13, aes(x=Sex, y=Score)) + 
  geom_boxplot(aes(color =Sex)) +
  geom_jitter(aes(color =Sex), width = 0.1)+
  scale_color_manual(values=c("#002060","#002060")) +
  facet_wrap(. ~ Label, scales = "free",  nrow = 1, ncol = 4)+
  geom_signif(comparisons = list(c("Women", "Men"), c("Men", "Women"), c("Men", "Women"), c("Men", "Women")),  test = "wilcox.test") + #wilcox test
  ggtitle("Sex difference in PCV13 vaccine responsiveness scores") +
  theme_bw()+ #ylab("#OPA titer (log 2)") + 
  xlab(" ") +
  theme(plot.title = element_text(hjust = 0.5),
        strip.background = element_blank(),  
        legend.position = "",
        axis.text.x = element_text(color="black", size=14, face = ("plain")),
        axis.text.y = element_text(color="black", size=14, face = ("plain")),
        axis.title.x = element_text(color="black", size=14, , face = ("bold"))
  )

pdf("PCV13_sexvsstrength.pdf", height = 5, width = 7)
p1
dev.off()


### Figure1g - Sex vs PPSV23 vaccine responsiveness ###
PPSV23<- read_excel(path="Figure1defg.xls", sheet = "Figure1g") %>% 
  as.data.frame
PPSV23$Label <- factor(PPSV23$Label, levels=unique(PPSV23$Label))

p2 <- ggplot(PPSV23, aes(x=Sex, y=Score)) + 
  geom_boxplot(aes(color =Sex)) +
  geom_jitter(aes(color =Sex), width = 0.1)+
  scale_color_manual(values=c("#911403","#911403")) +
  facet_wrap(. ~ Label, scales = "free", nrow = 1, ncol = 4) +
  geom_signif(comparisons = list(c("Men", "Women"), c("Men", "Women"), c("Men", "Women"), c("Men", "Women")),  test = "wilcox.test") + #wilcox test
  ggtitle("Sex difference in PPSV23 vaccine responsiveness scores") +
  theme_bw()+ #ylab("#OPA titer (log 2)") + 
  xlab(" ") +
  theme(plot.title = element_text(hjust = 0.5),
        strip.background = element_blank(),  
        legend.position = "",
        axis.text.x = element_text(color="black", size=14, face = ("plain")),
        axis.text.y = element_text(color="black", size=14, face = ("plain")),
        axis.title.x = element_text(color="black", size=14, face="bold")
  )

pdf("PPSV23_sex_effect.pdf", height = 5, width = 7)
p2
dev.off()

############################
