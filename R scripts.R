##Figure scripts for "Insect galls from the Serra da Bandeira (Barreiras, Western Bahia, Brazil)"
#June 21, 2023

##Library
library(ggplot2)
library(tidyr)
library(dplyr)


# BARPLOT 1
df <- data.frame(
  dose=c("Fabaceae", "Malpighiaceae","Euphorbiaceae",
         "Combretaceae","Anacardiaceae","Myrtaceae","Malvaceae",
         "Ochnaceae","Bignoniaceae","Bignoniaceae",
         "Caryocaraceae","Erythroxylaceae","Verbenaceae"),  
  len=c(26,5,3,3,3,2,1,1,1,1,1,1,1))

p<-ggplot(df, aes(x= reorder(dose, -len), y=len, fill=dose)) +
  geom_bar(stat="identity")+theme_minimal()+theme(axis.text=element_text(size=12),
                                                  axis.title=element_text(size=14))+
  #scale_fill_viridis_d()
  scale_fill_brewer(palette="RdBu") # RdBu #http://rstudio-pubs-static.s3.amazonaws.com/5312_98fc1aba2d5740dd849a5ab797cc2c8d.html

p + 
  ggtitle("") + #title
  xlab("Host plant families") + ylab("Number of gall morphotypes") + 
  labs(fill = "") + #legend title
  theme(legend.title = element_text(size=18)) +
  theme(legend.text = element_text(size=15)) +
  theme(axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15))


# BARPLOT 1 with more than one morphotype 
df <- data.frame(
  dose=c("Fabaceae", "Malpighiaceae","Euphorbiaceae",
         "Combretaceae","Anacardiaceae","Myrtaceae"),  
  len=c(26,5,3,3,3,2))

p<-ggplot(df, aes(x= reorder(dose, -len), y=len, fill=dose)) +
  geom_bar(stat="identity")+theme_minimal()+theme(axis.text=element_text(size=12),
                                                  axis.title=element_text(size=14))+
  #scale_fill_viridis_d()
  scale_fill_brewer(palette="RdBu") # RdBu #http://rstudio-pubs-static.s3.amazonaws.com/5312_98fc1aba2d5740dd849a5ab797cc2c8d.html

p + 
  ggtitle("") + #title
  xlab("Host plant families") + ylab("Number of gall morphotypes") + 
  labs(fill = "") + #legend title
  theme(legend.title = element_text(size=18)) +
  theme(legend.text = element_text(size=15)) +
  theme(axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15))



# BARPLOT 2
library(ggplot2)
require(ggrepel)
library(tidyverse)


Leaf <- (35*100/48)
Twig <- (9*100/48)
Flower <- (2*100/48)
Inflorescence <- (1*100/48)
Fruit <- (1*100/48)


df <- data.frame(
  group=c("Leaf", "Twig","Flower","Inflorescence","Fruit"),  
  value=c(73,19,4,2,2))


# Get the positions
df2 <- df %>% 
  mutate(csum = rev(cumsum(rev(value))), 
         pos = value/2 + lead(csum, 1),
         pos = if_else(is.na(pos), value/2, pos))

ggplot(df, aes(x = "" , y = value, fill = fct_inorder(group))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "RdBu") +
  geom_label_repel(data = df2,
                   aes(y = pos, label = paste0(value, "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "Plant organ")) +
  theme_void()+
  theme(legend.title = element_text(size=18)) +
  theme(legend.text = element_text(size=15))



# BARPLOT 3

Globoid <- (12*100/48)
Cylindrical <- (12*100/48)
Discoid <- (8*100/48)
Conical <- (6*100/48)
Parenchymatical <- (6*100/48)
Fusiform <- (3*100/48)
MarginalLeafFold <- (1*100/48)


df <- data.frame(
  group=c("Globoid", "Cylindrical","Discoid","Conical",
          "Parenchymatical", "Fusiform", "Marginal Leaf Fold"),  
  value=c(25,25,17,12,12,6,2))


# Get the positions
df2 <- df %>% 
  mutate(csum = rev(cumsum(rev(value))), 
         pos = value/2 + lead(csum, 1),
         pos = if_else(is.na(pos), value/2, pos))

ggplot(df, aes(x = "" , y = value, fill = fct_inorder(group))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "RdBu") +
  geom_label_repel(data = df2,
                   aes(y = pos, label = paste0(value, "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "Gall morphotypes")) +
  theme_void() +
  theme(legend.title = element_text(size=18)) +
  theme(legend.text = element_text(size=15))



