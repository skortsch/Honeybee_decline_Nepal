# R script for the paper:
# Decline of honeybees and its consequences for beekeepers and crop pollination in western Nepal
# by Susanne Kortsch

#load libraries
library(tidyverse)
library(tidytext)
library(ggpubr)

#Figure dir and pixels
ppi<-300 #pixels per inches
dirF<-"Figures/"

# --- Figure 1 main text ---

### HONEY YIELD CHANGE PER HIVE DECLINE 
#honey yield change data file
hy<-read.csv("data/beekeeper_survey_honey_yield_change.csv", sep=",",check.names=FALSE)
colnames(hy)

#Count number of respondents
hy1<-1*(hy[,c(5:9)]>-0.1)
hy1_sum<-apply(hy1, 2, sum, na.rm=TRUE)
hy1_sum_na<-colSums(is.na(hy1))
tot_sum<-apply(data_hy, 2, sum)
data_hy<-rbind(hy1_sum, hy1_sum_na, tot_sum)
rownames(data_hy)<-c("No. of responses", "No. of NA responses", "No. of beekeepers")


#boxplot honey yield decline 
boxplot(hy[, c(5:9)], ylab="kg honey per hive", xlab="years")#check data

#convert table honey yield
#hy_t <- pivot_longer(hy[,-2],cols = c("2011.y.before", "2011y", "2017y","2019y","2021y"), names_to = "year")
hy_t <- pivot_longer(hy[,],cols = c("2009", "2012", "2017", "2019", "2021"), names_to = "year")

#boxplots and histograms
#ggplot(hy_t, aes(value)) + geom_histogram(binwidth = 0.2)#histogram
#ggplot(hy_t, aes(value)) + geom_histogram(binwidth = 0.2)+ scale_x_log10()#hist log scale

###GLM WITH PREDICTION LINES
y = hy_t$value+1
x = as.numeric(hy_t$year)

#GLM model
hy_glm<-glm(y~x,  na.action = na.exclude, family = gaussian(link = "log"))

###Summary output for the glm log link regression
summary(hy_glm)

#pred lines for the original scales
pred_x = c(2009:2030)
pred_lines = data.frame(x=pred_x, y=predict(hy_glm, data.frame(x=pred_x)))
#pred_lines = data.frame(x=pred_x, y=predict(hy_lm, data.frame(x=pred_x), interval="confidence"))
var<-c("Obs","Pred")
pred_lines$obs_Or_Pred<-rep(var, times=c(13, 9))

lt<-c("solid","dashed")

hy_plot<-  ggplot()+geom_line(data = pred_lines, aes(x, exp(y),  size= I(1.4), colour = obs_Or_Pred, linetype = obs_Or_Pred)) +
  geom_jitter(dat = hy_t, aes(as.numeric(year), value), size = 2, position = position_jitter(0.1), alpha = 0.2) +
  scale_linetype_manual(values = lt) +
  scale_color_manual(values = c('black', 'black')) +
  ylab("Honey yield per beehive (kg)") + xlab("Year") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme_bw() +
  theme(legend.title = element_blank(), legend.key.size = unit(1, 'cm'), legend.key.width= unit(1.5, 'cm'),legend.text = element_text(size = 14)) +
  theme(axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title = element_text(size = 14))

hy_plot
#ggsave(paste0(dirF, "honey_yield_with_pred_line_org_scale.png"),width=7, height = 8, units="in", dpi=600 ) 

### BEEHIVE DECLINE 
bc<-read.csv("data/beehive_change.csv", sep=";",check.names=FALSE)
bc[,5]<-as.integer(bc[,5]) # i don't know this variable suddenly was character
str(bc)

#convert table
bh_t <- pivot_longer(bc[,],cols = c("2009", "2012", "2017","2019","2021", "2022"), names_to = "year")

#remove empty rows
bh_t<-bh_t[-which(bh_t$study_village==""),]

#boxplots
#ggplot(bh_t) + geom_boxplot(aes(year, value), na.rm = FALSE)
#ggplot(bh_t, aes(value)) + geom_histogram(binwidth = 0.2)

###GLM WITH PREDICTION LINES
y2 = bh_t$value+1
x2 = as.numeric(bh_t$year)

bh_glm<-glm(y2~x2,  na.action = na.exclude, family = poisson(link = "log"))

#Summary output for the glm log link regression
summary(bh_glm)

#pred lines for the original scales
pred_x2 = c(2009:2030)
pred_lines2 = data.frame(x2=pred_x2, y2=predict(bh_glm, data.frame(x2=pred_x2)))
pred_lines2$se <- predict(bh_glm, data.frame(x2=pred_x2), type="response", se.fit = TRUE)$se
#rep(var, times=c(14, 5))
var<-c("Obs","Pred")
pred_lines2$obs_Or_Pred<-rep(var, times=c(14, 8))
pred_lines2$lower <- pred_lines2$y - 1.96 * pred_lines2$se
pred_lines2$upper <- pred_lines2$y + 1.96 * pred_lines2$se
#colnames(pred_lines2)-c("x2", "y", "obs_Or_Pred")

bh_plot<-ggplot()+ geom_line(data=pred_lines2, aes(x2,exp(y2), size=I(1.4), colour=obs_Or_Pred, linetype=obs_Or_Pred))+
  geom_jitter(dat=bh_t, aes(as.numeric(year), value), size=2, position=position_jitter(0.1), alpha=0.2)+
  #geom_ribbon(aes(ymin= lower, ymax=upper), linetype = 0, alpha=0.1, data=pred_lines2)
  #geom_line(data=pred_lines2, aes(x2, exp(y2), colour=obs_Or_Pred, linetype=obs_Or_Pred))+
  scale_linetype_manual(values=lt)+
  scale_color_manual(values=c('black', 'black'))+
  scale_linetype_manual(values=lt)+
  ylab("Beehives per beekeeper")+xlab("Year")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  theme_bw()+
  theme(legend.title=element_blank(), legend.text = element_text(size = 14), legend.key.size = unit(1, 'cm'), legend.key.width= unit(2, 'cm'))+
  theme(axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title = element_text(size = 14))

bh_plot

Fig_hy_bh<-ggarrange(hy_plot, bh_plot, labels = c("a", "b"), 
                     font.label = list(size = 16, color = "black"), ncol = 2, common.legend = TRUE, legend="top")
annotate_figure(Fig_hy_bh)

#ggsave(paste0(dirF, "Figure_1_main_text.png"),width=8, height = 6, units="in", dpi=600 ) 


# ---- Figures 2 and 3 ----
# Per village changes

#Fig 2
#Honey yield decline per village 
vill.id<-which(hy_t$study_village=="CHAU")

list.predlines<-list()
p_values<-c()
for (i in 1:length(unique(hy_t$study_village))){
  #i<-2
  vill.id.list<-unique(hy_t$study_village)
  vill.id<-which(hy_t$study_village==vill.id.list[i])
  
  y = hy_t$value[vill.id]+1
  x = as.numeric(hy_t$year)[vill.id]
  
  hy_glm<-glm(y~x,  na.action = na.exclude, family = gaussian(link = "log"))
  
  ###Summary output for the glm log link regression
  summary(hy_glm)
  #coef(summary(hy_glm))[,4]
  
  x1pValue <- broom::tidy(hy_glm)$p.value[2]
  p_values[i]<-x1pValue
  #Calculate percentage change per year
  #slope_hy<-coefficients(hy_glm)[2]
  #(exp(slope_hy)-1)*100
  
  
  #pred lines for the original scales
  #pred_x = c(2009, 2012, 2017, 2019, 2021)
  pred_x = c(2009:2021)
  pred_lines = data.frame(x=pred_x, y=predict(hy_glm, data.frame(x=pred_x)), vill.name=unique(hy_t$study_village)[i])
  #pred_lines = data.frame(x=pred_x, y=predict(hy_lm, data.frame(x=pred_x), interval="confidence"))
  list.predlines[[i]]<- pred_lines
}

pavlues<-as.data.frame(cbind(village=unique(bh_t$study_village),p_values))
write.csv(pavlues, "data/pavlues_honey_yield.csv")

pred.vill<-as.data.frame(list_rbind(list.predlines))
pred.vill[2]<-exp(pred.vill$y)
pred.vill$type<-"lines"

dat.org<-hy_t[,c( 5,6, 2)] 
colnames(dat.org)<-c( "x", "y", "vill.name")
dat.org$type<-"points"

new.dat<-as.data.frame(rbind(dat.org, pred.vill))


hy_vill_plot<-ggplot(new.dat, aes(x = x,y = y), color = factor(type), shape = factor(type), linetype = factor(type))+
  facet_wrap(~vill.name, scales = "free_y")+
  geom_line(data = filter(new.dat, type == "lines"), aes(group = vill.name), size=1.2)+
  geom_point(data = filter(new.dat, type == "points"), size=3, position=position_jitter(0.1), alpha=0.3)+
  theme_light()+ ylab("honey yield per hive")+ xlab("year")+
  theme(axis.text.y = element_text(size = 14))+ theme(axis.title = element_text(size = 16)) + 
  theme(axis.text.x = element_text(size = 14, angle=90))+ theme(axis.title = element_text(size = 16))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        strip.background = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(strip.text.x = element_text(size = 14, color = "black"))+
  theme(strip.background = element_rect(color="grey0", fill="grey95", linetype="solid"))+
  scale_x_discrete(breaks = scales::pretty_breaks(n = 5))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
hy_vill_plot

ggsave(paste0(dirF, "Figure_2_main_text.png"),width=8, height = 8, units="in", dpi=600 ) 

#Fig 3
###Beehive decline per village

#loop for prediction lines and p values
list.predlines<-list()
p_values<-c()
for (i in 1:length(unique(bh_t$study_village))){
  #i<-2
  vill.id.list<-unique(bh_t$study_village)
  vill.id<-which(bh_t$study_village==vill.id.list[i])

  y = bh_t$value[vill.id]+1
  x = as.numeric(bh_t$year)[vill.id]

  bh_glm<-glm(y~x,  na.action = na.exclude, family = poisson(link = "log"))

###Summary output for the glm log link regression
  summary(bh_glm)
  #coef(summary(hy_glm))[,4]

  x1pValue <- broom::tidy(bh_glm)$p.value[2]
  p_values[i]<-x1pValue
#Calculate percentage change per year
  #slope_hy<-coefficients(hy_glm)[2]
  #(exp(slope_hy)-1)*100


#pred lines for the original scales
  #pred_x = c(2009, 2012, 2017, 2019, 2021)
  pred_x = c(2009:2022)
  pred_lines = data.frame(x=pred_x, y=predict(bh_glm, data.frame(x=pred_x)), vill.name=unique(hy_t$study_village)[i])
  #pred_lines = data.frame(x=pred_x, y=predict(hy_lm, data.frame(x=pred_x), interval="confidence"))
  list.predlines[[i]]<- pred_lines
}

pvalues<-as.data.frame(cbind(village=unique(bh_t$study_village),p_values))
#write.csv(pavlues, "data/pavlues_beehives.csv")

pred.vill<-as.data.frame(list_rbind(list.predlines))
pred.vill[2]<-exp(pred.vill$y)
pred.vill$type<-"lines"

dat.org<-bh_t[,c( 6,7, 2)] 
colnames(dat.org)<-c( "x", "y", "vill.name")
dat.org$type<-"points"

new.dat<-as.data.frame(rbind(dat.org, pred.vill))
 
        
hy_vill_plot<-ggplot(new.dat, aes(x = x,y = y), color = factor(type), shape = factor(type), linetype = factor(type))+
          facet_wrap(~vill.name, scales = "free_y")+
          geom_line(data = filter(new.dat, type == "lines"), aes(group = vill.name), size=1.2)+
          geom_point(data = filter(new.dat, type == "points"), size=3, position=position_jitter(0.1), alpha=0.3)+
          theme_light()+ ylab("beehive per beekeeper")+ xlab("year")+
          theme(axis.text.y = element_text(size = 14))+ theme(axis.title = element_text(size = 16)) + 
          theme(axis.text.x = element_text(size = 14, angle=90))+ theme(axis.title = element_text(size = 16))+
          theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
          strip.background = element_blank(),panel.border = element_rect(colour = "black"))+
          theme(strip.text.x = element_text(size = 14, color = "black"))+
          theme(strip.background = element_rect(color="grey0", fill="grey95", linetype="solid"))+
          scale_x_discrete(breaks = scales::pretty_breaks(n = 5))+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
hy_vill_plot

ggsave(paste0(dirF, "Figure_3_main_text.png"),width=8, height = 8, units="in", dpi=600 ) 



#Figure 4
#causes for the decline

#reasons for the decline
rd<-read.csv("data/reasons_decline.csv", sep=";",check.names=FALSE)
colnames(rd)

causes<-c(table(rd$changing_climate),table(rd$less_flowers),table(rd$insecticides),table(rd$bee_disease),
          table(rd$more_hornets),table(rd$herbicides),table(rd$overgrazing),table(rd$poor_beehive_managment), table(rd$other),
          table(rd$more_pine_martens),table(rd$dont_know), table(rd$`changes in flowering_time`),table(rd$economic_cost))

names(causes)<-c("changing climate", "less flowers", "insecticides", "bee disease", "more hornets", "herbicides", "overgrazing", 
                 "poor managment","other", "more pine martens",   "dont know", "flowering time", "economic cost")

percentage<-round(causes/116*100)

#names<-c("changing_climate", "less_flowers", "insecticides", "bee_disease", "more_hornets", "herbicides", "overgrazing", "poor_beehive_managment", "more_pine_martens", "flowering_time", "economic_cost")
causes2<-as.data.frame(cbind(names(causes), causes, percentage))
colnames(causes2)<-c("reasons", "number", "percentage")

#barplot
ggplot(causes2, aes(x=fct_rev(fct_inorder(reasons)), y=as.numeric(percentage))) + 
  geom_bar(aes(fill = fct_inorder(reasons)), position="dodge", stat = "identity", color="black",)+
  scale_fill_grey(start = 0.1, end = .9)+
  theme_minimal()+
  coord_flip()+
  geom_text(aes(label=number), vjust= 0.5, hjust = -0.1, size=4.5)+
  labs(x = bquote(~ "Causes for the decline"), y = "% beekeepers") +
  #labs(x = bquote(~ bold("causes for the decline"), y = "% beekeepers"))+
  theme(legend.position = "none")+
  theme(axis.title = element_text(size = 16))+  # Adjust both x and y-axis title properties
  theme(axis.text = element_text( size = 14))  # Adjust both x and y-axis title properties
theme(legend.position = "none")

ggsave(paste0(dirF, "Figure_4_main_text_revised.png"),width=7, height=5, dpi=500, bg="white" ) 



#Figure 5
##Import crop data
crop_data <- read.csv("Data/Jumla crops.csv") 

str(crop_data)

crop_honeybee_plot <- ggplot(crop_data, aes(x = perc_A.cerana_visits, y = reorder(eng_name, perc_A.cerana_visits), fill = perc_farmers_growing)) +
  geom_bar(stat = "identity", color="black") +
  scale_fill_gradient(name = "% farmers growing this crop", low = "white", high = "black", limits = c(0, 100)) +
  labs(x = bquote(~ "% flower visits made by" ~ italic("A. cerana cerana")), y = "Crop") +
  theme_minimal()+
  theme(axis.title = element_text(size = 16))+  # Adjust both x and y-axis title properties
  theme(axis.text = element_text( size = 14))  # Adjust both x and y-axis title properties

crop_honeybee_plot 

ggsave(paste0(dirF, "Figure_5_main_text.png"),width=9, height=5, dpi=500, bg="white" ) 
ggsave("C:/LocalData/susakort/field work Nepal/Honeybee_decline_Nepal/Figures/Figure_5_main_text_NEW.png",width=9, height=5, dpi=500)
################ SUPPORTING INFROMATION #############################

#A3
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

ggsave(paste0(dirF, "SI_FIG_A_3.png"),width=8, height = 10, units="in", dpi=600 ) 

#Fig A4
#reasons for fewer flowers
lf<-read.csv("data/reasons_less_flowers.csv", sep=";",check.names=FALSE)
colnames(lf)
lf 

lf2<-lf[,c(2:dim(lf)[2])]
head(lf2)

number.lf<-round(apply(lf2[,c(2:dim(lf2)[2])], 2, sum, na.rm=T))
percentage.lf<-round(apply(lf2[,c(2:dim(lf2)[2])], 2, sum, na.rm=T)/116*100)

names.lf<-colnames(lf2)[-1]
names.lf<-gsub("_"," ",names.lf)

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

ggsave(paste0(dirF, "SI_FIG_A4.png"),width=8, height = 10, units="in", dpi=600 ) 

#Fig A5 (NEED TO ADJUST THE FIGURE A5 #

apple<-read.csv("data/apple_change.csv", sep=";",check.names=FALSE)
colnames(apple)

unique(apple$apple_yield_change)
names_yield<-unique(apple$apple_yield_change)
table(as.character(apple$apple_yield_change))
percentage<-round((causes_counts/sum(causes_counts))*100)
percentage$`crop quantity decrease` <-percentage$`crop quantity decrease`+1
sum(percentage)

causes2<-rbind(causes_counts, as.vector(percentage))
causes2<-as.data.frame(t(causes2))
colnames(causes2)<-c("number", "percentage")
causes2$reasons<-colnames(causes_counts)
#change order
causes2 <- causes2[order(as.numeric(causes2$percentage),decreasing=TRUE),] #order rows


#apple yield plot perceived change, barplot, percentage
ggplot(apple, aes(x = apple_yield_change )) +  
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



#Fig A6
#livlihood change
ld<-read.csv("data/livelihood_change.csv", sep=";",check.names=FALSE)

names(causes_counts)<-c("income", "consumption", "beewax", "crop quality decrease", "crop quantity decrease")
reasons<-c("income", "consumption", "beewax", "crop quality decrease", "crop quantity decrease")
percentage<-round((causes_counts/sum(causes_counts))*100)
percentage$`crop quantity decrease` <-percentage$`crop quantity decrease`+1
sum(percentage)

causes2<-rbind(causes_counts, as.vector(percentage))
causes2<-as.data.frame(t(causes2))
colnames(causes2)<-c("number", "percentage")
causes2$reasons<-colnames(causes_counts)
#change order
causes2 <- causes2[order(as.numeric(causes2$percentage),decreasing=TRUE),] #order rows

#barplot 
ggplot(causes2, aes(x=fct_inorder(reasons), y=as.numeric(percentage))) + 
  geom_bar(aes(fill = fct_inorder(reasons)), position="dodge", stat = "identity")+
  scale_fill_grey(start = 0.1, end = .9)+
  theme_bw()+ coord_flip()+
  geom_text(aes(label=percentage), vjust= 1, hjust = -0.1, size=4)+
  theme(text = element_text(size = 16))+
  ylab("percentage")+ xlab("")+ ggtitle("livelihood affected")+
  theme(legend.position = "none")+
  theme(axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 16),
        axis.text.x = element_text(size = 16))

ggsave(paste0(dirF, "SI_FIG_A6.png"),width=8, height = 10, units="in", dpi=600 ) 

