#### R script honey yield and beehive decline (data is logtransformed) ####


####### HONEY YIELD CHANGE PER HIVE DECLINE #########

#honey yield change data file
hy<-read.csv("data/honey_yield_change.csv", sep=";",check.names=FALSE)
colnames(hy)

#boxplot honey yield decline 
#boxplot(hy[, c(5:9)], ylab="kg honey per hive", xlab="years")#check data
#percentage decrease
#length(which(hy$honey_yield_change=="decrease"))/116*100

#conve#rt table honey yield
hy_t <- pivot_longer(hy[,],cols = c("2009", "2012", "2017", "2019", "2021"), names_to = "year")

#boxplots and histograms
#ggplot(hy_t, aes(value)) + geom_histogram(binwidth = 0.2)#histogram
#ggplot(hy_t, aes(value)) + geom_histogram(binwidth = 0.2)+ scale_x_log10()#hist log scale


###PLOT WITH PREDICTION LINES
y = log(hy_t$value+1)
x = as.numeric(hy_t$year)

hy_lm <- lm(y~x)
#Summary of the regression
summary(hy_lm)

#Calculate percentage change per year
slope_hy<-coefficients(hy_lm)[2]
(exp(slope_hy)-1)*100

#pred lines for the original scales
pred_x = c(2009:2030)
pred_lines = data.frame(x=pred_x, y=predict(hy_lm, data.frame(x=pred_x)))
#pred_lines = data.frame(x=pred_x, y=predict(hy_lm, data.frame(x=pred_x), interval="confidence"))
var<-c("Obs","Pred")
pred_lines$obs_Or_Pred<-rep(var, times=c(13, 9))

hy_plot<-ggplot(pred_lines, aes(x, exp(y), colour=obs_Or_Pred, shape=obs_Or_Pred, linetype=obs_Or_Pred)) +
  geom_jitter(data=data.frame(x,y, obs_Or_Pred="Obs"), size=3, position=position_jitter(0.1), alpha=0.3) +
  geom_line(linewidth=1) +scale_color_manual(values=c('grey50', 'black'))+
  scale_shape_manual(values=c(16,NA)) +
  ylab("Kg log honey yield per hive")+xlab("Year")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  theme_bw()+
  theme(legend.title=element_blank(), legend.text = element_text(size = 12))+
  theme(axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title = element_text(size = 14)
  )+
  geom_label(aes(x = 2009, y = 26), hjust = 0, size=4,
             label = paste("Adj R2 = ",signif(summary(hy_lm)$adj.r.squared, 2)," \nP =",signif(summary(hy_lm)$coef[2,4], 2)), show.legend = FALSE)

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
y2 = log(bh_t$value+1)
x2 = as.numeric(bh_t$year)

bh_lm <- lm(y2~x2)
#Summary of the regression
summary(bh_lm)

#Calculate percentage change per year
slope_bh<-coefficients(bh_lm)[2]
(exp(slope_bh)-1)*100

#pred lines for the original scales
pred_x2 = c(2009:2030)
pred_lines2 = data.frame(x2=pred_x2, y2=predict(bh_lm, data.frame(x2=pred_x2)))
#rep(var, times=c(14, 5))
var<-c("Obs","Pred")
pred_lines2$obs_Or_Pred<-rep(var, times=c(14, 8))
#colnames(pred_lines2)-c("x2", "y", "obs_Or_Pred")

bh_plot<-ggplot(pred_lines2, aes(x2, exp(y2), colour=obs_Or_Pred, shape=obs_Or_Pred, linetype=obs_Or_Pred)) +
  geom_jitter(data=data.frame(x2,y2, obs_Or_Pred="Obs"), size=3, position=position_jitter(0.1), alpha=0.3) +
  geom_line(linewidth=1) +scale_color_manual(values=c('grey50', 'black'))+
  scale_shape_manual(values=c(16,NA)) +
  ylab("Beehives per beekeeper")+xlab("Year")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  theme_bw()+
  theme(legend.title=element_blank(), legend.text = element_text(size = 12))+
  theme(axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title = element_text(size = 14))+
  geom_label(aes(x = 2009, y = 94), hjust = 0, size=4,
             label = paste("Adj R2 = ",signif(summary(bh_lm)$adj.r.squared, 2)," \nP =",signif(summary(bh_lm)$coef[2,4], 2)), show.legend = FALSE)
bh_plot

Fig_hy_bh<-ggarrange(hy_plot, bh_plot, labels = c("a", "b"), 
                     font.label = list(size = 16, color = "black"), ncol = 2, common.legend = TRUE, legend="top")
annotate_figure(Fig_hy_bh)


#ggsave(paste0(dirF, "hy_bh_decline_org_scale_org_scale.png"),width=8, height = 6, units="in", dpi=600 ) 

