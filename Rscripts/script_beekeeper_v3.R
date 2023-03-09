#this is the correct wd path
#C:\LocalData\susakort\field work Nepal\Rscripts
setwd("C:/LocalData/susakort/field work Nepal/Honeybee_decline_Nepal/Rscripts")


#Figure dir and pixels
ppi<-300 #pixels per inches
dirF<-"../Figures/"

#load library
#
library(tidyverse)
library(tidytext)
library(ggpubr)
library(ggpmisc)
library(rstatix)

#data
dt<-read.csv("../data/beehive_data_all.csv")
#honey yield change data file
hy<-read.csv("../data/honey_yield_change.csv", sep=";",check.names=FALSE)
colnames(hy)
head(hy)
#beehive change
bc<-read.csv("../data/beehive_change.csv", sep=";",check.names=FALSE)
bc
head(bc)
colnames(bc)
#colnames(bc)[(4:8)]<-c("2009", "2011", "2017", "2019", "2021")
#bkc<-read.csv("../data/beekeeper_change.csv", check.names=FALSE)
#livestock change
lsc<-read.csv("../data/farmer_livestock.csv", sep=";",check.names=FALSE)
colnames(lsc)
lsc  

#apple change
apl<-read.csv("../data/apple_change.csv", sep=";",check.names=FALSE)
colnames(apl)
apl 

#reasons for the decline
rd<-read.csv("../data/reasons_decline.csv", sep=";",check.names=FALSE)
colnames(rd)
rd 


####### HONEY YIELD CHANGE PER HIVE DECLINE #########
#boxplot honey yield decline 
boxplot(hy[, c(5:9)], ylab="kg honey per hive", xlab="years")#check data

#percentage decrease
length(which(hy$honey_yield_change=="decrease"))/116*100

#convert table honey yield
#hy_t <- pivot_longer(hy[,-2],cols = c("2011.y.before", "2011y", "2017y","2019y","2021y"), names_to = "year")
hy_t <- pivot_longer(hy[,],cols = c("2009", "2012", "2017", "2019", "2021"), names_to = "year")

#boxplots and histograms
ggplot(hy_t) + geom_boxplot(aes(year, value), na.rm = FALSE)#boxplot
ggplot(hy_t, aes(value)) + geom_histogram(binwidth = 0.2)#histogram
ggplot(hy_t, aes(value)) + geom_histogram(binwidth = 0.2)+ scale_x_log10()#hist log scale
ggplot(hy_t, aes(value)) + facet_wrap(~year, scales = 'free_x') + geom_histogram(binwidth = 0.2)+ scale_x_log10()#hist log scale per year


#ANOVA PLOT
#initial visualization to determine if lm is appropriate
hy_plot <- ggplot(data=hy_t, aes(x=year, y=value)) + geom_jitter(shape=16, position=position_jitter(0.1), alpha=0.3) + 
  stat_summary(geom = "point", fun = "mean",col = "black",size = 4,shape = 21,fill = "red")+
  theme_light()+stat_compare_means(method = "anova", label.y = 22)+ ylab("Honey yield per hive")
  #theme(axis.text = element_text(size = 12))+ theme(axis.title = element_text(size = 12)) + 
  #theme(axis.title.y = element_text(margin = margin(r = 1)))+
  #theme(strip.text.x = element_text(size = 14, color = "black")) +
hy_plot

#plot means with confidence intervals
ggplot(hy_t,aes(year,value))+stat_summary(fun.data=mean_cl_normal) +
  geom_smooth(method='lm',formula=log(value+1) ~ as.numeric(year))+
  theme_light()+stat_compare_means(method = "anova", label.y = 7)+ ylab("Honey yield per hive")

####Linear regression plots
library(ggpmisc)
year.labs<-c("<2010", "2011", "2017", "2019", "2021")

#linear model
mod.lm<-lm(log(value+1) ~ as.numeric(year), data=hy_t)
mod.lm
summary(mod.lm)

mod1<-glm(value ~year, data=hy_t,  na.action = na.exclude, 
          family = gaussian(link = "identity"))

#assumptions
plot(mod.lm, which=1)
plot(mod.lm, which=2)

hy_plot<- hy_t %>% ggplot(aes(x = as.numeric(year),y = log(value+1))) + geom_jitter(shape=16, position=position_jitter(0.1), alpha=0.3)+
  theme_classic()+
  stat_compare_means(method = "anova", label.y = 7)+ ylab("Kg honey yield per hive")+xlab("Year")+
  stat_poly_line() +  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  geom_label(aes(x = 2018, y = 3.2), hjust = 0, size=5, label = paste("Adj R2 = ",signif(summary(mod.lm)$adj.r.squared, 5)," \nP =",signif(summary(mod.lm)$coef[2,4], 5))) +
  geom_smooth(method = "lm", color="black", fill="lightgrey")+
  theme(axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        panel.background = element_rect(colour = "black", size=1))
hy_plot
ggsave(paste0(dirF, "honey_yield_all.png"),width=8, height = 10, units="in", dpi=600 ) 

  #hy_t %>% ggplot(aes(x = as.numeric(year),y = log(value+1))) + geom_jitter(shape=16, position=position_jitter(0.1), alpha=0.3)+ geom_smooth(method = "lm")+
  #theme_light()+stat_compare_means(method = "anova", label.y = 7)+ ylab("Honey yield per hive")+xlab("year")+
  #stat_poly_line() +  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  #stat_poly_eq(aes(label = paste(after_stat(eq.label), sep = "*\", \"*")))+xlab("year")
  #annotate("text", x=2011, y=3, label=" P =",signif(summary(mod.lm)$coef[2,4], 5))
  #+theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) #change angles of labs
  #annotate("text", x=2011, y=3, label="p-value: <0.001")+


###
#need to fix x axes labels
 hy_plot_vill<- hy_t %>% ggplot(aes(x = as.numeric(year),y = log(value+1))) + geom_point(col="black", alpha=0.3) + geom_smooth(method = "lm")+
  stat_cor(label.y = c(3.2,3.2,3.2,3.2,3.2,3.2,3.2,3.2,3.2,3.2), col="black")+
  facet_wrap(~study_village, scales = "fixed") +  theme_bw()+
  xlim(2009,2021)+
  theme_light()+stat_compare_means(method = "anova", label.y = 7)+ ylab("Honey yield per hive")+ xlab("year")+
  geom_smooth(method = "lm", color="black", fill="lightgrey")+
  theme(axis.text.y = element_text(size = 14))+ theme(axis.title = element_text(size = 14)) + 
  theme(axis.text.x = element_text(size = 14, angle=90))+ theme(axis.title = element_text(size = 14))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        strip.background = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(strip.text.x = element_text(size = 14, color = "black"))+
  theme(strip.background = element_rect(color="grey0", fill="grey95", linetype="solid"))
 hy_plot_vill
 #+stat_poly_line() + stat_poly_eq(aes(label = paste(after_stat(eq.label), sep = "*\", \"*")))

 ggsave(paste0(dirF, "honey_yield_per_village.png"),width=8, height = 10, units="in", dpi=600 ) 


#BEEHIVE DECLINE
#convert table
#bh_t <- pivot_longer(bc[,-c(2,3)],cols = c("2011.y.before", "2011y", "2017y","2019y","2022y"), names_to = "year")
bh_t <- pivot_longer(bc[,],cols = c("2009", "2012", "2017","2019","2021", "2022"), names_to = "year")

#boxplots
ggplot(bh_t) + geom_boxplot(aes(year, value), na.rm = FALSE)
ggplot(bh_t, aes(value)) + geom_histogram(binwidth = 0.2)
ggplot(bh_t, aes(value)) + facet_wrap(~year, scales = 'free_x') + geom_histogram(binwidth = 0.2)

#initial visualization to determine if lm is appropriate
hy_plot <- ggplot(data=hy_t, aes(x=year, y=value)) + geom_jitter(shape=16, position=position_jitter(0.1), alpha=0.3) + 
  stat_summary(geom = "point", fun = "mean",col = "black",size = 4,shape = 21,fill = "red")+
  theme_light()+stat_compare_means(method = "anova", label.y = 22)+ ylab("Honey yield per hive")
#theme(axis.text = element_text(size = 12))+ theme(axis.title = element_text(size = 12)) + 
#theme(axis.title.y = element_text(margin = margin(r = 1)))+
#theme(strip.text.x = element_text(size = 14, color = "black")) +
hy_plot
ggsave(paste0(dirF, "honey_yield_decline.png"),width=8, height = 10, units="in", dpi=600 ) 


mod.lm<-lm(log(value+1) ~ as.numeric(year), data=bh_t)
mod.lm
summary(mod.lm)

mod1<-glm(value ~year, data=hy_t,  na.action = na.exclude, 
          family = gaussian(link = "identity"))

#+theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) #change angles of labs
# annotate("text", x=2011, y=3
bh_t %>% ggplot(aes(x = as.numeric(year),y = log(value+1))) + geom_jitter(shape=16, position=position_jitter(0.1), alpha=0.3)+ geom_smooth(method = "lm")+
  theme_light()+stat_compare_means(method = "anova", label.y = 7)+ ylab("no. of beehives")+xlab("year")+
  stat_poly_line() + stat_poly_eq(aes(label = paste(after_stat(eq.label), sep = "*\", \"*")))+xlab("year")

bh_plot<- bh_t %>% ggplot(aes(x = as.numeric(year),y = log(value+1))) + geom_jitter(shape=16, position=position_jitter(0.1), alpha=0.3)+
  theme_classic()+
  stat_compare_means(method = "anova", label.y = 7)+ ylab("# Beehives")+xlab("Year")+
  stat_poly_line() +  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  geom_label(aes(x = 2018, y = 4.2), hjust = 0, size=5, label = paste("Adj R2 = ",signif(summary(mod.lm)$adj.r.squared, 5)," \nP =",signif(summary(mod.lm)$coef[2,4], 5))) +
  geom_smooth(method = "lm", color="black", fill="lightgrey")+
  theme(axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        panel.background = element_rect(colour = "black", size=1))
bh_plot
ggsave(paste0(dirF, "beehive_decline.png"),width=8, height = 10, units="in", dpi=600 ) 

#+scale_x_continuous(breaks = scales::pretty_breaks(n = 5)), label="p-value: <0.001")

bh_vill_plot<-bh_t %>% ggplot(aes(x = as.numeric(year),y = log(value+1), colour=study_village, fill=study_village)) + geom_point(col="black", alpha=0.3) + 
  geom_smooth(method = "lm", color="black", fill="lightgrey")+ stat_cor(label.y = c(4.2,4.2,4.2,4.2,4.2,4.2,4.2,4.2,4.2,4.2), col="black")+
  facet_wrap(~study_village, scales = "fixed") +
  theme_light()+ ylab("# Beehives")+xlab("Year")+
    theme(axis.text.y = element_text(size = 14))+ theme(axis.title = element_text(size = 14)) + 
    theme(axis.text.x = element_text(size = 14, angle=90))+ theme(axis.title = element_text(size = 14))+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
          strip.background = element_blank(),panel.border = element_rect(colour = "black"))+
    theme(strip.text.x = element_text(size = 14, color = "black"))+
    theme(strip.background = element_rect(color="grey0", fill="grey95", linetype="solid"))+
    theme(legend.position = "none")
bh_vill_plot
ggsave(paste0(dirF, "beehive_decline_vill.png"),width=8, height = 10, units="in", dpi=600 ) 

  #stat_poly_eq(aes(label = paste(after_stat(eq.label), sep = "*\", \"*")))
  #geom_label(aes(x = 2018, y = 4.2), hjust = 0, size=2, label = paste("Adj R2 = ",signif(summary(mod.lm)$adj.r.squared, 5)," \nP =",signif(summary(mod.lm)$coef[2,4], 5)))

######

bee_import<-read.table("../data/honeybee_importance_percrop_pervillage.txt", header=T)
head(bee_import)
dim(bee_import)
id.gadi<-which(bee_import[,2]=="GADI")
id.pat<-which(bee_import[,2]=="PATM")
bee_import[id.pat,]


#arrange data by village and descending importance of plants for honeybees
bi_reorder<-bee_import %>% arrange(Village, desc(Importance)) %>% group_by(Village) %>%  mutate(rank = dense_rank(desc(Importance)))
bi_reorder_keep <-bi_reorder %>% filter(rank<6)
#a<-bi_reorder_keep %>% group_by(Crop, rank) %>% summarize(rank = n()) 
t<-table(bi_reorder_keep[,c(1,7)])
t2<-t[order(t[,1], decreasing=TRUE),] 

tab_rank<-as.data.frame(t2)
tab_rank2<-tab_rank[order(tab_rank$rank, decreasing=FALSE),]
tab_rank2 %>% group_by(rank) %>% mutate(order = dense_rank(asc(Freq)))

# make heatmap
#library(viridis)
#ggplot(tab_rank2, aes(rank, Crop, fill= Freq)) + 
#  geom_tile() +
#  scale_fill_viridis(discrete=FALSE)
#ggsave(paste0(dirF, "heatmap_importance.png"),width=8, height = 10, units="in", dpi=600 ) 

tab_rank2$n_nas <- ifelse(tab_rank2$Freq==0, NA, tab_rank2$Freq)

ggplot(tab_rank2, aes(x=rank, y=Crop, fill=n_nas)) +
  geom_tile(color="white", size = 0.25) +
  geom_text(aes(label = Freq)) + 
  scale_fill_gradient(low="gold", high="darkorchid", na.value="white")
ggsave(paste0(dirF, "heatmap_importance.png"),width=8, height = 10, units="in", dpi=600 ) 


ggplot(new_dat, aes(x=Importance, y=Crop)) +
  facet_wrap(~Village)+
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

#### Apple decline question
#apple change
apl<-read.csv("../data/apple_change.csv", sep=";",check.names=FALSE)
colnames(apl)
apl 

apl$apple_yield_change

apl[which(apl$apple_yield_change =="decrease"), 4]<-1
apl[which(apl$apple_yield_change =="increase"), 4]<-2
apl[which(apl$apple_yield_change=="no_change"), 4]<-3
apl[which(apl$apple_yield_change=="dont_know"), 4]<-4
apl[which(is.na(apl$apple_yield_change)), 4]<-5

bp_labs<-c('decrease', 'increase', 'no change', 'dont know', "NAs")

ap_quan_plot<-ggplot(apl, aes(x = as.numeric(apple_yield_change) )) +  
  geom_bar(aes(y = (..count..)/sum(..count..)), fill=c("grey20", "grey40", "grey60", "grey70", "grey80"))+ theme_bw() + 
  scale_y_continuous(labels=scales::percent) +
  geom_text(aes(label = scales::percent(..prop..), y= ..prop.. ), stat= "count", vjust = -.5, size=6)+
  theme(axis.text.x=element_text(size=14))+
  theme(text = element_text(size = 16)) +
  ylab("percentage")+ xlab("percieved apple yield change")
 # +
 # scale_x_discrete(labels=bp_labs)

ggsave("../Figures/barplot_apple_yield_change.png")

#apple quality plot perceived change, barplot, percentage

apl[which(apl$apple_qual_change =="decrease"), 6]<-1
apl[which(apl$apple_qual_change =="increase"), 6]<-2
apl[which(apl$apple_qual_change=="no_change"), 6]<-3
apl[which(apl$apple_qual_change=="dont_know"), 6]<-4
apl[which(is.na(apl$apple_qual_change)), 6]<-5

ap_qual_plot<-ggplot(apl, aes(x = as.numeric(apple_qual_change))) +  
  geom_bar(aes(y = (..count..)/sum(..count..)), fill=c("grey20", "grey40", "grey60", "grey70", "grey80"))+ theme_bw() + 
  scale_y_continuous(labels=scales::percent) +
  ylab("")+ xlab("percieved apple quality change")+
  geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), stat= "count", vjust = -.5, size=6)+
  theme(axis.text.x=element_text(size=14))+
  theme(text = element_text(size = 16))  

Fig_apple<-ggarrange(ap_quan_plot, ap_qual_plot, widths = c( 6, 6), labels = c("a", "b"), font.label = list(size = 16, color = "black"), ncol = 2)
annotate_figure(Fig_apple)
ggsave(paste0(dirF, "Fig_apples.png"),width=8, height =8, units="in", dpi=600 ) 


###Total Honey Yield


bc2<-bc[, c(7:11)]
hy2<-hy[, c(5:9)]

Tot_hon<-bc2*hy2
Tot_hon2<-cbind(hy[,c(1,2)], Tot_hon)

tot_hon_t <- pivot_longer(Tot_hon2[,],cols = c("2009", "2012", "2017","2019","2021"), names_to = "year")

#boxplots and histograms
ggplot(tot_hon_t) + geom_boxplot(aes(year, value), na.rm = FALSE)#boxplot

tot_hon_plot<- tot_hon_t %>% ggplot(aes(x = as.numeric(year),y = log(value+1))) + geom_jitter(shape=16, position=position_jitter(0.1), alpha=0.3)+
  theme_classic()+
  stat_compare_means(method = "anova", label.y = 7)+ ylab("total honey kg yield per beekeeper")+xlab("Year")+
  stat_poly_line() +  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  geom_label(aes(x = 2018, y = 6.5), hjust = 0, size=3, label = paste("Adj R2 = ",signif(summary(mod.lm)$adj.r.squared, 5)," \nP =",signif(summary(mod.lm)$coef[2,4], 5))) +
  geom_smooth(method = "lm", color="black", fill="lightgrey")+
  theme(axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        panel.background = element_rect(colour = "black", size=1))
tot_hon_plot
ggsave(paste0(dirF, "tot_honey_decline.png"),width=8, height = 10, units="in", dpi=600 ) 

#+scale_x_continuous(breaks = scales::pretty_breaks(n = 5)), label="p-value: <0.001


#reasons for the decline

rdd2<-round(apply(rd[,c(4:dim(rd)[2])], 2, sum, na.rm=T)/116*100)
barplot(rdd2)

ggplot(rd[,c(4:dim(rd)[2])]) + geom_bar()

rdd<-rd[,c(4:dim(rd)[2])]


causes<-c(table(rd$changing_climate),table(rd$less_flowers),table(rd$insecticides),table(rd$bee_disease),
          table(rd$more_hornets),table(rd$herbicides),table(rd$overgrazing),table(rd$poor_beehive_managment), table(rd$other),
          table(rd$more_pine_martens),table(rd$dont_know), table(rd$`changes in flowering_time`),table(rd$economic_cost))

names(causes)<-c("changing climate", "less flowers", "insecticides", "bee disease", "more hornets", "herbicides", "overgrazing", 
              "poor beehive managment","other", "more pine martens",   "dont know", "flowering time", "economic cost")

#names<-c("changing_climate", "less_flowers", "insecticides", "bee_disease", "more_hornets", "herbicides", "overgrazing", "poor_beehive_managment", "more_pine_martens", "flowering_time", "economic_cost")
causes2<-as.data.frame(cbind(names(causes), causes))
colnames(causes2)<-c("reasons", "percentage")

#ggp <- ggplot(causes2, aes(names, causes)) +    # Create vertical barplot in ggplot2
#  geom_bar(stat = "identity")+ coord_flip()
#ggp 
#decline_rank<- ggplot(causes2, aes(x=names, y=causes)) + 
#  geom_bar(stat = "identity")+theme_bw()+ coord_flip()+
#  theme(text = element_text(size = 16))+
#  ylab("percentage")+ xlab("percieved reasons for decline ranked 1")

#causes2$perc <- as.numeric(causes2$causes)/116*100


ggplot(causes2, aes(x=fct_inorder(reasons), y=as.numeric(percentage))) + 
  geom_bar(aes(fill = fct_inorder(reasons)), position="dodge", stat = "identity")+
  scale_fill_grey(start = 0.1, end = .9)+
  theme_bw()+ coord_flip()+
  geom_text(aes(label=causes), vjust= 1.5, hjust = -0.3, size=4)+
  theme(text = element_text(size = 16))+
  ylab("percentage")+ xlab("")+ ggtitle("reasons for the decline")+
  theme(legend.position = "none")+
  theme(axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 16),
        axis.text.x = element_text(size = 16))
ggsave(paste0(dirF, "causes for decline.png"),width=8, height = 10, units="in", dpi=600 ) 


#climate change how?
#reasons for the decline
cc<-read.csv("../data/climate change.csv", sep=";",check.names=FALSE)
colnames(cc)
cc

climate_causes<-c(2,3,1,29,59,1,1,59,2,1,2,2,0,4,2)
namesCC<-c("colder", "wetter", "warmer", "drier", "more_unpredictable","earlier_monsoon", "later_monsoon", "heavier_monsoon", "drier_monsoon", "timing_of_seasons",
           "winters_less_cold", "winters_more_cold", "no_snow_winter", "late_frost","other")
climate_causes2<-as.data.frame(cbind(namesCC, climate_causes))
climate_causes2$perc <- as.numeric(climate_causes2$climate_causes)/116*100
ggplot(climate_causes2, aes(x=namesCC, y=perc)) + 
  geom_bar(stat = "identity")+theme_bw()+ coord_flip()+
  theme(text = element_text(size = 16))+
  ylab("percentage")+ xlab("climate change causes")
ggsave("../Figures/climate changes causes.png")

#######################
#### Livestock

which(colnames(lsc)=="sheep_number")
which(colnames(lsc)=="goat_number")

goat_sheep<-lsc[,c(4,13, 14)]
ggplot(goat_sheep) + geom_boxplot(aes(study_village, goat_number), na.rm = FALSE)#boxplot 
ggplot(goat_sheep) + geom_boxplot(aes(study_village, sheep_number), na.rm = FALSE)#boxplot 

id<-which(hy[,2]=="GADI")
hy[id,5]


#make sum of goats and sheeps per village and compare to slopes of honey decline  
  
#loop over study village then extract the slopes  
#linear model
no.vill<-length(unique(hy_t$study_village))
honey.slopes.vill<-c()
for (i in 1:no.vill){
  name.vill<-unique(hy_t$study_village)
  ids<-hy_t$study_village==name.vill[i]
  x<-hy_t$value[ids]
  y<-hy_t$year[ids]
  mod.lm<-lm(log(x+1) ~ as.numeric(y), data=hy_t)
  summary(mod.lm)
  honey.slopes.vill[i]<-signif(mod.lm$coef[[2]])
}  

goat_sheep[is.na(goat_sheep)] <- 0
goat_sheep$total<-cbind(apply(goat_sheep[,c(2,3)], 1, sum))


tot.mean.gs<-goat_sheep %>% group_by(study_village) %>% 
  summarise(tot=sum(total), mean.ls=mean(total))

tot.g<-goat_sheep %>% group_by(study_village) %>% summarise(tot=sum(goat_number))
tot.s<-goat_sheep %>% group_by(study_village) %>% summarise(tot=sum(sheep_number))


mean.ls<-goat_sheep %>% group_by(study_village) %>% 
  summarise(mean.s=mean(sheep_number), mean.g=mean(goat_number))

tot.ls<-goat_sheep %>% group_by(study_village) %>% 
  summarise(tot.s=sum(sheep_number), tot.g=sum(goat_number))


tot.gs<-goat_sheep %>% group_by(study_village) %>% 
  mutate(goat_number = ifelse(is.na(goat_number), 0, goat_number), sheep_number = ifelse(is.na(sheep_number), 0, sheep_number))

plot(honey.slopes.vill~tot.gs$tot)
text(honey.slopes.vill~tot.gs$tot, labels=name.vill, cex=0.5)
plot(honey.slopes.vill~tot.g$tot)
plot(honey.slopes.vill~tot.s$tot)

hy_2021<-hy[,c(2,5)]

name.vill<-unique(hy_t$study_village)
tot.mean.gs[,1]<-name.vill

dat_2021<-merge(hy_2021,tot.mean.gs,by="study_village")

mean.ls[,1]<-name.vill
dat_2021<-merge(hy_2021, mean.ls, by="study_village")

mod2<-lm(dat_2021$`2021`~dat_2021$mean.s*dat_2021$mean.g)
summary(mod2)

tot.ls[,1]<-name.vill
dat_2021<-merge(hy_2021, tot.ls, by="study_village")

mod3<-lm(dat_2021$`2021`~dat_2021$tot.s*dat_2021$tot.g)
summary(mod3)

plot(dat_2021$`2021`, dat_2021$tot.s)

#slopes
#signif(mod.lm$coef[[2]])  

#make label on graphs  
#label = paste("Adj R2 = ",signif(summary(fit1)$adj.r.squared, 5),
#              "\nIntercept =",signif(fit1$coef[[1]],5 ),
#              " \nSlope =",signif(fit1$coef[[2]], 5),
#              " \nP =",signif(summary(fit1)$coef[2,4], 5)))
  
  
  
  
  
################################
#beekeeper change
boxplot(bkc[, c(4:8)], ylab="number of beekeepers", xlab="years")

#barplot honey yield
barplot(hy[,c(3:7)])

#beekeeper data v1
bk_data<-read.csv("../data/all_data_fieldwork.csv")
bk_data

#clean data, column beekeeper change
bk_data[which(bk_data$beekeeper_change=="0"), 7]<-3#replace 0 (no change) with 3
bk_data[which(bk_data$beekeeper_change=="98"), 7]<-4#replace 98 (don't know) with 4
##clean data, columns apple change
bk_data[which(bk_data$apple_yield_change =="0"), 24]<-3
bk_data[which(bk_data$apple_yield_change =="98"), 24]<-4
bk_data[which(is.na(bk_data$apple_yield_change)), 24]<-5
bk_data[which(bk_data$apple_qual_change=="0"), 26]<-3
bk_data[which(bk_data$apple_qual_change=="98"), 26]<-4
bk_data[which(is.na(bk_data$apple_qual_change)), 26]<-5


#average beekeeper per village
116/length(unique(bk_data$study_village))

#filter study village
bk_data %>% filter(study_village=="PATM")

#percentage plot
#bk_data[bk_data$beehive_change=="2",4]<-"decrease"
#bk_data[bk_data$beehive_change=="1",4]<-"increase"
#bk_data[bk_data$beehive_change=="3",4]<-"don't KNOW"

#bk_data2<- bk_data %>% count(beehive_change) %>% mutate(perc = n / length(bk_data$beehive_change)) 
#bk_data2$beehive_change<-c("increase", "decrease", "don't know")
         
#plot perceived beehive change, barplot, percentage
ggplot(bk_data, aes(x = beehive_change)) +  
  geom_bar(aes(y = (..count..)/sum(..count..)), fill="grey70")+ theme_bw() + 
  scale_y_continuous(labels=scales::percent) +
  ylab("percentage")+ xlab("percieved beehive change")+
  geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), stat= "count", vjust = -.5, size=6)+
  theme(axis.text.x=element_text(size=0))+
  theme(text = element_text(size = 16))   
ggsave("../Figures/barplot_beehive_change.png")

#plot perceived beepop change, barplot, percentage
ggplot(bk_data, aes(x = population_change)) +  
  geom_bar(aes(y = (..count..)/sum(..count..)), fill="grey70")+ theme_bw() + 
  scale_y_continuous(labels=scales::percent) +
  ylab("percentage")+ xlab("percieved bee population change")+
  geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), stat= "count", vjust = -.5, size=6)+
  theme(axis.text.x=element_text(size=0))+
  theme(text = element_text(size = 16))   
ggsave("../Figures/barplot_beepop_change.png")


decline_rank<- bk_data %>% count(decline_rank1) %>% mutate(perc = n / length(bk_data$decline_rank1)*100) 
ggplot(decline_rank, aes(x=decline_rank1, y=perc)) + 
  geom_bar(stat = "identity")+theme_bw()+ coord_flip()+
  theme(text = element_text(size = 16))+
  ylab("percentage")+ xlab("percieved reasons for decline ranked 1")
ggsave("../Figures/barplot_decline_rank1.png")
 
ggplot(bk_data, aes(x = decline_rank1)) +  
  geom_bar(aes(y = (..count..)/sum(..count..)), fill="grey70")+ theme_bw() + 
  scale_y_continuous(labels=scales::percent) +
  ylab("percentage")+ xlab("percieved bee population change")+
  geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), stat= "count", vjust = -.5, size=6)+
  theme(axis.text.x=element_text(size=0))+
  theme(text = element_text(size = 16))  


#plot perceived honey yield change, barplot, percentage
ggplot(bk_data, aes(x = honey_yield_change)) +  
  geom_bar(aes(y = (..count..)/sum(..count..)), fill="grey70")+ theme_bw() + 
  scale_y_continuous(labels=scales::percent) +
  ylab("percentage")+ xlab("percieved honey yield change")+
  geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), stat= "count", vjust = -.2, size=6)+
  theme(axis.text.x=element_text(size=0))+
  theme(text = element_text(size = 16))   
ggsave("../Figures/barplot_honey_yield_change_change.png")

#plot perceived beekeeper change, barplot, percentage
ggplot(bk_data, aes(x = beekeeper_change )) +  
  geom_bar(aes(y = (..count..)/sum(..count..)), fill="grey70")+ theme_bw() + 
  scale_y_continuous(labels=scales::percent) +
  ylab("percentage")+ xlab("percieved beekeeper change")+
  geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), stat= "count", vjust = -.5, size=6)+
  theme(axis.text.x=element_text(size=0))+
  theme(text = element_text(size = 16))   
ggsave("../Figures/barplot_beekeeper_change.png")


#apple yield plot perceived change, barplot, percentage
ggplot(bk_data, aes(x = apple_yield_change )) +  
  geom_bar(aes(y = (..count..)/sum(..count..)), fill="grey70")+ theme_bw() + 
  scale_y_continuous(labels=scales::percent) +
  ylab("percentage")+ xlab("percieved apple yield change")+
  geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), stat= "count", vjust = -.5, size=6)+
  theme(axis.text.x=element_text(size=0))+
  theme(text = element_text(size = 16))   
ggsave("../Figures/barplot_apple_yield_change.png")

#apple quality plot perceived change, barplot, percentage
ggplot(bk_data, aes(x = apple_qual_change )) +  
  geom_bar(aes(y = (..count..)/sum(..count..)), fill="grey70")+ theme_bw() + 
  scale_y_continuous(labels=scales::percent) +
  ylab("percentage")+ xlab("percieved apple quality change")+
  geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), stat= "count", vjust = -.5, size=6)+
  theme(axis.text.x=element_text(size=0))+
  theme(text = element_text(size = 16))   
ggsave("../Figures/barplot_apple_quality_change.png")


barplot(table(bk_data$apple_yield_years))

causes<-c(table(bk_data$changing_climate),table(bk_data$less_flowers),table(bk_data$insecticides),table(bk_data$bee_disease),table(bk_data$more_hornets),table(bk_data$herbicides),
table(bk_data$overgrazing),table(bk_data$poor_beehive_managment),table(bk_data$more_pine_martens),table(bk_data$changes.in.flowering_time),table(bk_data$economic_cost))
names(causes)<-c("changing_climate", "less_flowers", "insecticides", "bee_disease", "more_hornets", "herbicides", "overgrazing", "poor_beehive_managment", "more_pine_martens", "flowering_time", "economic_cost")

par(mar = c(1, 1, 1, 1))
barplot(causes, las=2, horiz=TRUE, xlim=c(0,70))
png("../Figures/causes_change.png")

 

names<-c("changing_climate", "less_flowers", "insecticides", "bee_disease", "more_hornets", "herbicides", "overgrazing", "poor_beehive_managment", "more_pine_martens", "flowering_time", "economic_cost")
causes2<-as.data.frame(cbind(names, causes))
ggp <- ggplot(causes2, aes(names, causes)) +    # Create vertical barplot in ggplot2
  geom_bar(stat = "identity")+ coord_flip()
ggp 
#decline_rank<- ggplot(causes2, aes(x=names, y=causes)) + 
#  geom_bar(stat = "identity")+theme_bw()+ coord_flip()+
#  theme(text = element_text(size = 16))+
#  ylab("percentage")+ xlab("percieved reasons for decline ranked 1")

causes2$perc <- as.numeric(causes2$causes)/116*100
ggplot(causes2, aes(x=names, y=perc)) + 
  geom_bar(stat = "identity")+theme_bw()+ coord_flip()+
  theme(text = element_text(size = 16))+
  ylab("percentage")+ xlab("causes for decline")
ggsave("../Figures/causes for decline.png")

climate_causes<-c(2,3,1,29,59,1,1,59,2,1,2,2,0,4,2)
namesCC<-c("colder", "wetter", "warmer", "drier", "more_unpredictable","earlier_monsoon", "later_monsoon", "heavier_monsoon", "drier_monsoon", "timing_of_seasons",
"winters_less_cold", "winters_more_cold", "no_snow_winter", "late_frost","other")
climate_causes2<-as.data.frame(cbind(namesCC, climate_causes))
climate_causes2$perc <- as.numeric(climate_causes2$climate_causes)/116*100
ggplot(climate_causes2, aes(x=namesCC, y=perc)) + 
  geom_bar(stat = "identity")+theme_bw()+ coord_flip()+
  theme(text = element_text(size = 16))+
  ylab("percentage")+ xlab("climate change causes")
ggsave("../Figures/climate changes causes.png")

####################################################################

library(dplyr)
d2 <- d %>% 
  group_by(groupchange, Symscore3) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count))

#####################################################################################

ggplot(bk_data, aes(x = no_village, y=beekeeping_years))+
  geom_bar(stat="identity", na.rm = TRUE)+
  geom_col()+
  facet_wrap(~study_village)

#BOXPLOT of the villages
p <- ggplot(bk_data, aes(x = study_village, y=beekeeping_years))
p + geom_boxplot()+
  geom_jitter(width = 0.2)+
  theme_minimal()

#Filter hives over time


hives_time<-bk_data %>% select(study_village, no_village, occupied_hives, hives_3years, hives_5years, hives_10years, hives_before10years) 
hives_time<-rename(hives_time,a_now=occupied_hives, b_yr3=hives_3years, c_yr5=hives_5years, d_yr10=hives_10years, e_more10yr=hives_before10years)
ht<-pivot_longer(hives_time, cols=c(a_now, b_yr3, c_yr5, d_yr10, e_more10yr), names_to = "Time", values_to = "Number")


#hive change over time
ggplot(ht, aes(x = Time, y = Number)) +
  geom_point() + facet_wrap(~study_village) 
#+
 # stat_summary(fun.data=mean_cl_normal) + 
  #geom_smooth(method='lm')
  





