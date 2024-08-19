# Figure1b - PCV13 vaccine responsiveness metrics

setwd("$/Figure1/")

#loading libraries
library(psych)
library(gridExtra)
library(dplyr)
library(readxl)
library(scales)
library(ggpubr)
library(ggplot2)
library(reshape2)
library(paletteer)
library(matrixStats)
library(fmsb)
library(paletteer)
library(titer)

# Function to replace zero values to one
List_Zero_1 <- function(Data) {
  Data[Data == 0] <- 1
  return(Data)
}


# to compute maxRBA based vaccine responsiveness rank
PCV13 <- read_excel("Figure1b.xls", sheet = "PCV13_data")
PCV13_titer <- FormatTiters(PCV13)
PCV13_endpoints <- Calculate_maxRBA(PCV13_titer, method = "exp", scoreFun = "sum")
PCV13_maxRBA_rank <- PCV13_endpoints$maxRBA %>% dense_rank


# Strength, Extent and Rank calculation

# load the data
Data_pre <- read_excel(path="Figure1b.xls", sheet = "PCV13_Pre") %>% 
  as.data.frame
Data_post <- read_excel(path="Figure1b.xls", sheet = "PCV13_Post") %>% 
  as.data.frame

rownames(Data_pre) <- Data_pre$DonorID
rownames(Data_post) <- Data_post$DonorID
Data_post <- Data_post[,-1]
Data_pre <- Data_pre[,-1]


#Fold change calcualtion
Data_post_processed <- List_Zero_1(Data_post)
Data_pre_processed <- List_Zero_1(Data_pre)
Data_fc <- Data_post_processed/Data_pre_processed

#rank the data
Data_post.rank <- apply(Data_post, 2, dense_rank) %>% rowSums %>% dense_rank
Data_pre.rank <- apply(Data_pre, 2, dense_rank) %>% rowSums %>% dense_rank
Data_fc.rank <- apply(Data_fc, 2, dense_rank) %>% rowSums %>% dense_rank
Data_fc.extent <- rowSums(Data_fc >= 8)

#sum
Data_fc.sum <- Data_fc %>% log2 %>% rowSums()
Data_post.sum <- Data_post_processed %>% log2 %>% rowSums()
Data_pre.sum <- Data_pre_processed %>% log2 %>% rowSums()
Data_pre.sum <- Data_pre_processed %>% log2 %>% rowSums()


#load the meta data
meta <- read_excel(path = "Figure1b.xls", sheet = "PCV13_Meta") %>% 
  as.data.frame

new.meta <- cbind(meta, 
                  D0_rank = Data_pre.rank, 
                  FC_Rank = Data_fc.rank, 
                  D35_rank = Data_post.rank ,
                  FC_extent = Data_fc.extent, 
                  D0_sum = Data_pre.sum , 
                  FC_sum = Data_fc.sum, 
                  D35_sum = Data_post.sum,
                  sumRBA = PCV13_maxRBA_rank)


write.csv(new.meta, "PCV13_Meta_antibody_ranked_old.csv")
serotypes <- colnames(Data_post_processed)[1:length(Data_post_processed)]


# Bubble plot showing fold difference in antibody titers (post vs pre immunization)
# for each serotype and for each individual.

pre.color <- "grey1"
pst.color <- "navyblue"

colnames(Data_fc) <- serotypes
df.bubble <- log2(Data_fc[rev(order(Data_fc.rank)),])  %>% 
  mutate(Patient = factor(rownames(.), levels= rev(rownames(.)))) %>% melt

bubble_plot <- ggplot(df.bubble, aes(x = variable, y = Patient, size = value, color = ifelse(value < 3, "Non-responsive", "Responsive"))) +
  geom_point(alpha = 1) + 
  xlab("") + ylab("") + 
  #theme_minimal(base_size = 12) +
  scale_x_discrete(position = "top") + 
  scale_y_discrete(position = "right") +
  theme_bw(base_size = 10)+
  scale_size(range = c(0,10)) + 
  labs(color = expression(log2(FC) > 3), size = expression(Response (log2))) +
  scale_color_manual(values = c("grey70", pst.color)) + 
  #"#a1a1a1"
  theme(#legend.title = element_blank(), 
    #legend.position = "none", 
    strip.text = element_blank(),
    plot.title = element_text(hjust = 1),
    axis.text.x = element_text( vjust = 0, hjust=0),
    axis.text=element_text(size=16, colour = "black"),
    axis.title=element_text(size=16))

pdf(file = "Figure1b_Bubble_plot.pdf", width = 10, height = 10)
bubble_plot
dev.off()


ranked.list <- new.meta[rev(order(Data_fc.rank)),]
ranked.list$DonorID <- factor(ranked.list$DonorID, levels = rev(ranked.list$DonorID))

#### Bubble plot for Strength, Rank, Extent and sumRBA

Sex_Mark <- ggplot(ranked.list,
                   aes(x="Sex", y = DonorID, color = Sex, label = sprintf(Sex))) + 
  #geom_point(size = 10) + 
  geom_text(color = "black", size = 8) + 
  theme_void() +
  theme(legend.position = "none") +
  theme(legend.title = element_blank())

Age_Mark <- ggplot(ranked.list,
                   aes(x="Age", y = DonorID, color = Age, label = sprintf("%.0f",Age))) + 
  #geom_point(size = 12) + 
  geom_text(color = "black", size = 8) + 
  theme_void() +
  theme(legend.position = "none") +
  theme(legend.title = element_blank())

sumRBA_rank <- ggplot(ranked.list,
                      aes(x="sumRBA", y = DonorID, color = sumRBA, label = sprintf("%.0f",sumRBA))) + 
  geom_point(size = 16) + 
  geom_text(color = "black", size = 8) + 
  theme_void() +
  theme(legend.position = "none") +
  theme(legend.title = element_blank()) + 
  scale_color_gradient(low = "grey70", high = "#2ca25f")


#FC_sum
FC_sum <- ggplot(ranked.list,
                 aes(x="FC_sum", y = DonorID, color = FC_sum, label = sprintf("%.0f",FC_sum))) + 
  geom_point(size = 16) + 
  geom_text(color = "black", size = 8) + 
  theme_void() +
  theme(legend.position = "none") +
  theme(legend.title = element_blank()) + 
  scale_color_gradient(low = "grey70", high = "#2ca25f")


#FC_rank
FC_Rank <- ggplot(ranked.list,
                  aes(x="FC RANK", y = DonorID, color = FC_Rank, label = sprintf("%.0f",FC_Rank))) + 
  geom_point(size = 16) + 
  geom_text(color = "black", size = 8) + 
  theme_void() +
  theme(legend.position = "none") +
  theme(legend.title = element_blank()) + 
  scale_color_gradient(low = "grey70", high = "#2ca25f")


#FC extent
FC_extent <- ggplot(ranked.list,
                    aes(x="EXTENT", y = DonorID, color = FC_extent, label = sprintf("%.0f",FC_extent))) + 
  geom_point(size = 16) + 
  geom_text(color = "black", size = 8) + 
  theme_void() +
  theme(legend.position = "none") +
  theme(legend.title = element_blank()) + 
  scale_color_gradient(low = "grey70", high = "#2ca25f")


standalone_scores <- ggarrange(FC_sum,   FC_extent, FC_Rank, sumRBA_rank,Sex_Mark, Age_Mark,
                               labels = c( "Strength", "Extent", "Rank", "sumRBA", "Sex", "Age"),
                               ncol = 6, nrow = 1,
                               font.label = list(size=12,color="black", face ="plain"))

pdf(file = "Figure1b_Ranking_scores.pdf", width = 10, height = 12)
standalone_scores
dev.off()

