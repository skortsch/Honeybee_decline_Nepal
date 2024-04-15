#### R script honey yield and beehive decline (data is log transformed) ####

#load library
library(tidyverse)
library(tidytext)

library(mdscore)#wald test
dirF<-"Figures/"


####### HONEY YIELD CHANGE PER HIVE DECLINE #########

#honey yield change data file
hy<-read.csv("data/honey_yield_change.csv", sep=";",check.names=FALSE)
colnames(hy)

apply(hy[,c(5:9)], 2, mean, na.rm=TRUE)

#boxplot honey yield decline 
#boxplot(hy[, c(5:9)], ylab="kg honey per hive", xlab="years")#check data
#percentage decrease
#length(which(hy$honey_yield_change=="decrease"))/116*100

#convert table honey yield
#hy_t <- pivot_longer(hy[,-2],cols = c("2011.y.before", "2011y", "2017y","2019y","2021y"), names_to = "year")
hy_t <- pivot_longer(hy[,],cols = c("2009", "2012", "2017", "2019", "2021"), names_to = "year")

#boxplots and histograms
#ggplot(hy_t, aes(value)) + geom_histogram(binwidth = 0.2)#histogram
#ggplot(hy_t, aes(value)) + geom_histogram(binwidth = 0.2)+ scale_x_log10()#hist log scale


###PLOT WITH PREDICTION LINES
y = hy_t$value+1
x = as.numeric(hy_t$year)

hy_glm<-glm(y~x,  na.action = na.exclude, family = gaussian(link = "log"))

###Summary output for the glm log link regression
summary(hy_glm)

#Calculate percentage change per year
slope_hy<-coefficients(hy_glm)[2]
(exp(slope_hy)-1)*100

new <- data.frame(x=c(2021, 2030))
val_2030<-predict(hy_glm, new)
exp(val_2030)

#pred lines for the original scales
pred_x = c(2009:2030)
pred_lines = data.frame(x=pred_x, y=predict(hy_glm, data.frame(x=pred_x)))
#pred_lines = data.frame(x=pred_x, y=predict(hy_lm, data.frame(x=pred_x), interval="confidence"))
var<-c("Obs","Pred")
pred_lines$obs_Or_Pred<-rep(var, times=c(13, 9))

lt<-c("solid","dashed")

hy_plot<-ggplot()+ geom_line(data=pred_lines, aes(x,exp(y), size=I(1.4), colour=obs_Or_Pred, linetype=obs_Or_Pred))+
  geom_jitter(dat=hy_t, aes(as.numeric(year), value), size=2, position=position_jitter(0.1), alpha=0.2)+
  #geom_ribbon(aes(ymin= lower, ymax=upper), linetype = 0, alpha=0.1, data=pred_lines2)
  #geom_line(data=pred_lines2, aes(x2, exp(y2), colour=obs_Or_Pred, linetype=obs_Or_Pred))+
  scale_linetype_manual(values=lt)+
  scale_color_manual(values=c('grey', 'black'))+
  scale_linetype_manual(values=lt)+
  ylab("Honey yield per beehive (kg)")+xlab("Year")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  theme_bw()+
  theme(legend.title=element_blank(), legend.key.size = unit(1, 'cm'), legend.key.width= unit(2, 'cm'), legend.text = element_text(size = 14))+
  theme(axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title = element_text(size = 14))


hy_plot<-  ggplot()+geom_line(data = pred_lines, aes(x, exp(y),  size= I(1.4), colour = obs_Or_Pred, linetype = obs_Or_Pred)) +
  geom_jitter(dat = hy_t, aes(as.numeric(year), value), size = 2, position = position_jitter(0.1), alpha = 0.2) +
  scale_linetype_manual(values = lt) +
  scale_color_manual(values = c('grey', 'black')) +
  ylab("Honey yield per beehive (kg)") + xlab("Year") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme_bw() +
  theme(legend.title = element_blank(), legend.key.size = unit(1, 'cm'), legend.key.width= unit(1.5, 'cm'),legend.text = element_text(size = 14)) +
  theme(axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title = element_text(size = 14))

hy_plot


#ggsave(paste0(dirF, "honey_yield_with_pred_line_org_scale.png"),width=7, height = 8, units="in", dpi=600 ) 


#BEEHIVE DECLINE 

bc<-read.csv("data/beehive_change.csv", sep=";",check.names=FALSE)
bc

#convert table
bh_t <- pivot_longer(bc[,],cols = c("2009", "2012", "2017","2019","2021", "2022"), names_to = "year")

#boxplots
#ggplot(bh_t) + geom_boxplot(aes(year, value), na.rm = FALSE)
#ggplot(bh_t, aes(value)) + geom_histogram(binwidth = 0.2)

###PLOT WITH PREDICTION LINES
y2 = bh_t$value+1
x2 = as.numeric(bh_t$year)

bh_glm<-glm(y2~x2,  na.action = na.exclude, family = gaussian(link = "log"))

###Summary output for the glm log link regression
summary(bh_glm)
1-44984/47335 #=0.04966727 the model accounts for 5% of the deviance in the data
#Wald test
wald.test(model = bh_glm, terms=1)

#Calculate percentage change per year
slope_bh<-coefficients(bh_glm)[2]
(exp(slope_bh)-1)*100

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
  scale_color_manual(values=c('grey', 'black'))+
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


#ggsave(paste0(dirF, "hy_bh_decline_org_scale_org_scale_glm_version.png"),width=8, height = 6, units="in", dpi=600 ) 



#per village change

#need to fix x axes labels

###Beehive decline per village
#vill.id<-which(bh_t$study_village=="CHAU")

list.predlines<-list()
p_values<-c()
for (i in 1:length(unique(bh_t$study_village))){
  #i<-2
  vill.id.list<-unique(bh_t$study_village)
  vill.id<-which(bh_t$study_village==vill.id.list[i])

  y = bh_t$value[vill.id]+1
  x = as.numeric(bh_t$year)[vill.id]

  bh_glm<-glm(y~x,  na.action = na.exclude, family = gaussian(link = "log"))

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
  pred_x = c(2009:2021)
  pred_lines = data.frame(x=pred_x, y=predict(bh_glm, data.frame(x=pred_x)), vill.name=unique(hy_t$study_village)[i])
  #pred_lines = data.frame(x=pred_x, y=predict(hy_lm, data.frame(x=pred_x), interval="confidence"))
  list.predlines[[i]]<- pred_lines
}

pavlues<-as.data.frame(cbind(village=unique(bh_t$study_village),p_values))
write.csv(pavlues, "data/pavlues_beehives.csv")

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

ggsave(paste0(dirF, "beehive_per_village_glm_loop.png"),width=8, height = 8, units="in", dpi=600 ) 



###Honey yield decline per village 

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

ggsave(paste0(dirF, "honey_yield_per_village.png"),width=8, height = 8, units="in", dpi=600 ) 


       


bh_vill_plot<-bh_t %>% ggplot(aes(x = as.numeric(year),y = value+1, colour=study_village, fill=study_village)) + geom_point(col="black", alpha=0.3) + 
  geom_smooth(method = "glm", method.args=list(family=gaussian(link = "log")), color="black", fill="lightgrey")+ 
  stat_cor(label.y = c(4.2,4.2,4.2,4.2,4.2,4.2,4.2,4.2,4.2,4.2), col="black")+
  facet_wrap(~study_village, scales = "free") +
  theme_light()+ ylab("# Beehives")+xlab("Year")+
  theme(axis.text.y = element_text(size = 14))+ theme(axis.title = element_text(size = 14)) + 
  theme(axis.text.x = element_text(size = 14, angle=90))+ theme(axis.title = element_text(size = 14))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        strip.background = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(strip.text.x = element_text(size = 14, color = "black"))+
  theme(strip.background = element_rect(color="grey0", fill="grey95", linetype="solid"))+
  theme(legend.position = "none")
bh_vill_plot





