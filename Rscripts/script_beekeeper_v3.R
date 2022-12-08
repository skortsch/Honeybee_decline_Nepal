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


#data
dt<-read.csv("../data/beehive_data_all.csv")
#honey yield change data file
hy<-read.csv("../data/honey_yield_change.csv", sep=";",check.names=FALSE)
colnames(hy)
#beehive change
bc<-read.csv("../data/beehive_change.csv", sep=";",check.names=FALSE)
bc
#colnames(bc)[(4:8)]<-c("2009", "2011", "2017", "2019", "2021")
#bkc<-read.csv("../data/beekeeper_change.csv", check.names=FALSE)


####### HONEY YIELD CHANGE PER HIVE DECLINE #########
#boxplot honey yield decline 
boxplot(hy[, c(5:9)], ylab="kg honey per hive", xlab="years")#check data

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

hy_t %>% ggplot(aes(x = as.numeric(year),y = log(value+1))) + geom_jitter(shape=16, position=position_jitter(0.1), alpha=0.3)+ geom_smooth(method = "lm")+
  theme_light()+stat_compare_means(method = "anova", label.y = 7)+ ylab("Honey yield per hive")+xlab("year")+
  stat_poly_line() +  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  geom_label(aes(x = 2009, y = 3.2), hjust = 0, label = paste("Adj R2 = ",signif(summary(mod.lm)$adj.r.squared, 5)," \nP =",signif(summary(mod.lm)$coef[2,4], 5)))
ggsave(paste0(dirF, "honey_yield_all.png"),width=8, height = 10, units="in", dpi=600 ) 

  #hy_t %>% ggplot(aes(x = as.numeric(year),y = log(value+1))) + geom_jitter(shape=16, position=position_jitter(0.1), alpha=0.3)+ geom_smooth(method = "lm")+
  #theme_light()+stat_compare_means(method = "anova", label.y = 7)+ ylab("Honey yield per hive")+xlab("year")+
  #stat_poly_line() +  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  #stat_poly_eq(aes(label = paste(after_stat(eq.label), sep = "*\", \"*")))+xlab("year")
  #annotate("text", x=2011, y=3, label=" P =",signif(summary(mod.lm)$coef[2,4], 5))
  #+theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) #change angles of labs
  #annotate("text", x=2011, y=3, label="p-value: <0.001")+

hy_t %>% ggplot(aes(x = as.numeric(year),y = log(value+1))) + geom_point() + geom_smooth(method = "lm")+
  facet_wrap(~study_village, scales = "free_y") +
  theme_light()+stat_compare_means(method = "anova", label.y = 7)+ ylab("Honey yield per hive")+ xlab("year")
 #+stat_poly_line() + stat_poly_eq(aes(label = paste(after_stat(eq.label), sep = "*\", \"*")))
ggsave(paste0(dirF, "honey_yield_per_village.png"),width=8, height = 10, units="in", dpi=600 ) 


#BEEHIVE DECLINE
#convert table
bh_t <- pivot_longer(bc[,-c(2,3)],cols = c("2011.y.before", "2011y", "2017y","2019y","2022y"), names_to = "year")
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

bh_t %>% ggplot(aes(x = as.numeric(year),y = log(value+1))) + geom_jitter(shape=16, position=position_jitter(0.1), alpha=0.3)+ geom_smooth(method = "lm")+
  theme_light()+stat_compare_means(method = "anova", label.y = 7)+ ylab("no. of beehives")+xlab("year")+
  stat_poly_line() + stat_poly_eq(aes(label = paste(after_stat(eq.label), sep = "*\", \"*")))+xlab("year")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))
#+theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) #change angles of labs
# annotate("text", x=2011, y=3, label="p-value: <0.001")+

bh_t %>% ggplot(aes(x = as.numeric(year),y = log(value+1))) + geom_point() + geom_smooth(method = "lm")+
  facet_wrap(~study_village, scales = "free_y") +
  theme_light()+stat_compare_means(method = "anova", label.y = 7)+ ylab("no.of beehives")+ xlab("year")
  stat_poly_line() + stat_poly_eq(aes(label = paste(after_stat(eq.label), sep = "*\", \"*")))

  
#######################
#### Livestock
  
#livestock change
lsc<-read.csv("../data/farmer_livestock.csv", sep=";",check.names=FALSE)
colnames(lsc)
lsc  

which(colnames(lsc)=="sheep_number")
which(colnames(lsc)=="goat_number")

goat_sheep<-lsc[,c(4,13, 14)]
ggplot(goat_sheep) + geom_boxplot(aes(study_village, goat_number), na.rm = FALSE)#boxplot 
  
  
#make sum of goats and sheeps per village and compare to slopes of honey decline  
  
  
#loop over study village then extract the slopes  
#linear model
mod.lm<-lm(log(value+1) ~ as.numeric(year), data=hy_t)
mod.lm
summary(mod.lm)
  
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
  





