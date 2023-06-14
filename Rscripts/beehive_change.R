###Beehive change

#Figure dir and pixels
ppi<-300 #pixels per inches
dirF<-"Figures/"

#load library
library(tidyverse)
library(tidytext)
library(ggpubr)
library(ggpmisc)
library(rstatix)

#beehive change
bc<-read.csv("data/beehive_change.csv", sep=";",check.names=FALSE)
bc
head(bc)
colnames(bc)

bh_t <- pivot_longer(bc[,],cols = c("2009", "2012", "2017","2019","2021", "2022"), names_to = "year")

#historic beehive change
bhh<-read.csv("data/hives_historic.csv", sep=";",check.names=FALSE)
head(bhh)

#remove columns with only zeros
bhh<-bhh[-which(apply(bhh[,3:5], 1, sum, na.rm = TRUE)==0),]
unique(hhy$Village)

#BEEHIVE DECLINE
#convert table
bhh_t <- pivot_longer(bhh[,],cols = c("1995", "1996", "1997"), names_to = "year")

#boxplots
ggplot(bhh_t) + geom_boxplot(aes(year, value), na.rm = FALSE)
ggplot(bhh_t, aes(value)) + geom_histogram(binwidth = 0.2)
ggplot(bhh_t, aes(value)) + facet_wrap(~year, scales = 'free_x') + geom_histogram(binwidth = 0.2)

#initial visualization to determine if lm is appropriate
bhh_plot <- ggplot(data=bhh_t, aes(x=year, y=value)) + geom_jitter(shape=16, position=position_jitter(0.1), alpha=0.3) + 
  stat_summary(geom = "point", fun = "mean",col = "black",size = 4,shape = 21,fill = "red")+
  theme_light()+stat_compare_means(method = "anova", label.y = 22)+ ylab("Honey yield per hive")
#theme(axis.text = element_text(size = 12))+ theme(axis.title = element_text(size = 12)) + 
#theme(axis.title.y = element_text(margin = margin(r = 1)))+
#theme(strip.text.x = element_text(size = 14, color = "black")) +
bhh_plot
#ggsave(paste0(dirF, "honey_yield_decline.png"),width=8, height = 10, units="in", dpi=600 ) 

#make a linear regression of only the historic data
mod.lm<-lm(log(value+1) ~ as.numeric(year), data=bhh_t)
mod.lm
summary(mod.lm)

mod1<-glm(value ~year, data=bhh_t,  na.action = na.exclude, 
          family = gaussian(link = "identity"))

#+theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) #change angles of labs
# annotate("text", x=2011, y=3
bhh_t %>% ggplot(aes(x = as.numeric(year),y = log(value+1))) + geom_jitter(shape=16, position=position_jitter(0.1), alpha=0.3)+ geom_smooth(method = "lm")+
  theme_light()+stat_compare_means(method = "anova", label.y = 7)+ ylab("no. of beehives")+xlab("year")+
  stat_poly_line() + stat_poly_eq(aes(label = paste(after_stat(eq.label), sep = "*\", \"*")))+xlab("year")

bh_plot<- bhh_t %>% ggplot(aes(x = as.numeric(year),y = log(value+1))) + geom_jitter(shape=16, position=position_jitter(0.1), alpha=0.3)+
  theme_classic()+
  stat_compare_means(method = "anova", label.y = 7)+ ylab("# Beehives")+xlab("Year")+
  stat_poly_line() +  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  geom_label(aes(x = 1996, y = 4.2), hjust = 0, size=5, label = paste("Adj R2 = ",signif(summary(mod.lm)$adj.r.squared, 5)," \nP =",signif(summary(mod.lm)$coef[2,4], 5))) +
  geom_smooth(method = "lm", color="black", fill="lightgrey")+
  theme(axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        panel.background = element_rect(colour = "black", size=1))
bh_plot
ggsave(paste0(dirF, "beehive_decline_historic.png"),width=8, height = 10, units="in", dpi=600 ) 

#filter of study villages 
villages<-c("chum", "urthu", "patmara", "patrasi", "gadigaun", "p_gadigaun", "Lorpa","tirkhu", "luma", "s.gadigaun", "rini","chaura", 
            "pere", "Tirku")
bhh2<-bhh %>% filter(study_village%in%villages)
bhh2_t <- pivot_longer(bhh2[,],cols = c("1995", "1996", "1997"), names_to = "year")

df1<-bhh2_t %>% select(study_village, year, value)

unique(df1$study_village)
df2<-bh_t %>% select(study_village, year, value)
df3<-bind_rows(df1, df2)

###PLOT WITH PREDICTION LINES
y2 = log(df3$value+1)
x2 = as.numeric(df3$year)

bh_lm <- lm(y2~x2)
summary(bh_lm)

pred_x2 = c(min(x2),rep(max(x2),2),max(x2)+5)
pred_lines2 = data.frame(x2=pred_x2,
                         y2=predict(bh_lm, data.frame(x2=pred_x2)),
                         obs_Or_Pred=rep(c("Obs","Pred"), each=2))

bh_plot_hist<-ggplot(pred_lines2, aes(x2, y2, colour=obs_Or_Pred, shape=obs_Or_Pred, linetype=obs_Or_Pred)) +
  geom_jitter(data=data.frame(x2,y2, obs_Or_Pred="Obs"), size=3, position=position_jitter(0.1), alpha=0.3) +
  geom_line(size=1) +scale_color_manual(values=c('grey50', 'black'))+
  scale_shape_manual(values=c(16,NA)) +
  ylab("Beehives per beekeeper")+xlab("Year")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  theme_bw()+
  theme(legend.title=element_blank())+
  theme(axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title = element_text(size = 14))+
  geom_label(aes(x = 2015, y = 4.6), hjust = 0, size=4,
             label = paste("Adj R2 = ",signif(summary(bh_lm)$adj.r.squared, 2)," \nP =",signif(summary(bh_lm)$coef[2,4], 2)), show.legend = FALSE)
bh_plot_hist

ggsave(paste0(dirF, "beehive_decline_historic.png"),width=8, height = 6, units="in", dpi=600 ) 

Fig_hy_bh_hist<-ggarrange(hy_plot_hist, bh_plot_hist, labels = c("a", "b"), 
                     font.label = list(size = 16, color = "black"), ncol = 2, common.legend = TRUE, legend="top")
annotate_figure(Fig_hy_bh_hist)
ggsave(paste0(dirF, "hy_bh_decline_historic.png"),width=8, height = 6, units="in", dpi=600 ) 


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
