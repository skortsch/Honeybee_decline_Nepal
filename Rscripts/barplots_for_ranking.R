#reasons for the decline

setwd("C:/LocalData/susakort/field work Nepal/Honeybee_decline_Nepal")


#Figure dir and pixels
ppi<-300 #pixels per inches
dirF<-"Figures/"

#load library
library(tidyverse)
library(tidytext)
library(ggpubr)
library(ggpmisc)
library(rstatix)

#reasons for the decline
rd<-read.csv("data/reasons_decline.csv", sep=";",check.names=FALSE)
colnames(rd)
rd

rdd2<-round(apply(rd[,c(4:dim(rd)[2])], 2, sum, na.rm=T)/116*100)
barplot(rdd2)

ggplot(rd[,c(4:dim(rd)[2])]) + geom_bar()

rdd<-rd[,c(4:dim(rd)[2])]


causes<-c(table(rd$changing_climate),table(rd$less_flowers),table(rd$insecticides),table(rd$bee_disease),
          table(rd$more_hornets),table(rd$herbicides),table(rd$overgrazing),table(rd$poor_beehive_managment), table(rd$other),
          table(rd$more_pine_martens),table(rd$dont_know), table(rd$`changes in flowering_time`),table(rd$economic_cost))

names(causes)<-c("changing climate", "less flowers", "insecticides", "bee disease", "more hornets", "herbicides", "overgrazing", 
                 "poor managment","other", "more pine martens",   "dont know", "flowering time", "economic cost")

percentage<-round(causes/116*100)

#names<-c("changing_climate", "less_flowers", "insecticides", "bee_disease", "more_hornets", "herbicides", "overgrazing", "poor_beehive_managment", "more_pine_martens", "flowering_time", "economic_cost")
causes2<-as.data.frame(cbind(names(causes), causes, percentage))
colnames(causes2)<-c("reasons", "number", "percentage")

#ggp <- ggplot(causes2, aes(names, causes)) +    # Create vertical barplot in ggplot2
#  geom_bar(stat = "identity")+ coord_flip()
#ggp 
#decline_rank<- ggplot(causes2, aes(x=names, y=causes)) + 
#  geom_bar(stat = "identity")+theme_bw()+ coord_flip()+
#  theme(text = element_text(size = 16))+
#  ylab("percentage")+ xlab("percieved reasons for decline ranked 1")

#causes2$perc <- as.numeric(causes2$causes)/116*100


ggplot(causes2, aes(x=fct_rev(fct_inorder(reasons)), y=as.numeric(percentage))) + 
  geom_bar(aes(fill = fct_inorder(reasons)), position="dodge", stat = "identity", color="black",)+
  scale_fill_grey(start = 0.1, end = .9)+
  theme_minimal()+
  coord_flip()+
  geom_text(aes(label=number), vjust= 0.5, hjust = -0.1, size=4.5)+
  labs(x = bquote(~ "causes for the decline"), y = "percentage of beekeepers") +
  #labs(x = bquote(~ bold("causes for the decline"), y = "% beekeepers"))+
  theme(legend.position = "none")+
  theme(axis.title = element_text(size = 16))+  # Adjust both x and y-axis title properties
  theme(axis.text = element_text( size = 14))  # Adjust both x and y-axis title properties
  theme(legend.position = "none")


ggsave(paste0(dirF, "Figure_4_main_text.png"),width=6.5, height=5, dpi=500, bg="white" ) 



crop_honeybee_plot <- ggplot(crop_data, aes(x = perc_A.cerana_visits, y = reorder(eng_name, perc_A.cerana_visits), fill = perc_farmers_growing)) +
  geom_bar(stat = "identity", color="black") +
  scale_fill_gradient(name = "% farmers growing this crop", low = "white", high = "black", limits = c(0, 100)) +
  labs(x = bquote(~ bold("% flower visits made by" ~ italic("A. cerana"))), y = "Crop") +
  theme_minimal()+
  theme(axis.title = element_text(face = "bold", size = 14))+  # Adjust both x and y-axis title properties
  theme(axis.text = element_text( size = 12))  # Adjust both x and y-axis title properties



#climate change how?
#reasons for the decline
cc<-read.csv("data/climate change.csv", sep=";",check.names=FALSE)
colnames(cc)
cc<-cc[,c(3:dim(cc)[2]-1)]

str(cc)

cc$climatic_dont_know<- as.integer(cc$climatic_dont_know)
cc$climatic_changes_no_snow_winter<- as.integer(cc$climatic_changes_no_snow_winter)
cc<-as.data.frame(cc)
str(cc)

number.cc<-round(apply(cc[,c(3:dim(cc)[2])], 2, sum, na.rm=T))
percentage.cc<-round(apply(cc[,c(3:dim(cc)[2])], 2, sum, na.rm=T)/116*100)

names.cc<- c("wetter", "warmer", "drier", "more unpredictable", "earlier monsoon", "later monsoon", "heavier monsoon",   
             "drier monsoon", "timing of seasons", "winters less cold",  "winters more cold",  "no snow in winter",
             "late frost", "other", "dont know")

cc2<-as.data.frame(cbind(names.cc, as.numeric(number.cc), as.numeric(percentage.cc))) 
colnames(cc2)<- c("names", "number", "perc")

cc3<-cc2[order(as.numeric(cc2$number), decreasing = T), ]

ggplot(cc3, aes(x=fct_rev(fct_inorder(names)), y=as.numeric(perc))) + 
  geom_bar(aes(fill =fct_inorder(names)),position="dodge", stat = "identity")+theme_bw()+
  scale_fill_grey(start = 0.1, end = .9)+
  theme(text = element_text(size = 16))+
  ylab("percentage of beekeepers (%)")+ xlab("climatic change characteristics")+
  geom_text(aes(label=number), vjust= 0.5, hjust = -0.1, size=4)+
  theme(legend.position = "none")+
  ggtitle("")+
  theme( panel.border = element_blank(),
         axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 16),
        axis.text.x = element_text(size = 16))+
    coord_flip()

ggsave(paste0(dirF, "changes in climate.png"),width=8, height = 10, units="in", dpi=600 ) 



#reasons for fewer flowers
lf<-read.csv("data/reasons_less_flowers.csv", sep=";",check.names=FALSE)
colnames(lf)
lf 

lf2<-lf[,c(2:dim(lf)[2])]
head(lf2)

number.lf<-round(apply(lf2[,c(2:dim(lf2)[2])], 2, sum, na.rm=T))
percentage.lf<-round(apply(lf2[,c(2:dim(lf2)[2])], 2, sum, na.rm=T)/116*100)

names.lf<-colnames(lf2)[-1]

lf3<-as.data.frame(cbind(names.lf, as.numeric(number.lf), as.numeric(percentage.lf))) 
colnames(lf3)<- c("names", "number", "perc")

lf4<-lf3[order(as.numeric(lf3$number), decreasing = T), ]

ggplot(lf4, aes(x=fct_rev(fct_inorder(names)), y=as.numeric(perc))) + 
  geom_bar(aes(fill =fct_inorder(names)), stat = "identity")+theme_bw()+ coord_flip()+
  scale_fill_grey(start = 0.1, end = .9)+
  theme(text = element_text(size = 16))+
  ylab("percentage of beekeepers (%)")+ xlab("less flower availability causes")+
  geom_text(aes(label=number), vjust= 0.5, hjust = -0.1, size=4)+
  theme(legend.position = "none")+
  ggtitle("")+
  theme(panel.border = element_blank(),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 16),
        axis.text.x = element_text(size = 16))

ggsave(paste0(dirF, "reasons_less_flowers.png"),width=8, height = 10, units="in", dpi=600 ) 

