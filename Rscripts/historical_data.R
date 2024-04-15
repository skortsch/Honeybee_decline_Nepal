#Historical data set


#Figure dir and pixels
ppi<-300 #pixels per inches
dirF<-"Figures/"

#load library
library(tidyverse)
library(tidytext)
library(ggpubr)
library(ggpmisc)
library(rstatix)


#honey yield change data file
hy<-read.csv("data/honey_yield_change.csv", sep=";",check.names=FALSE)
colnames(hy)
head(hy)

#beehive change
bc<-read.csv("data/beehive_change.csv", sep=";",check.names=FALSE)
bc
head(bc)
colnames(bc)

#historic honeybee data
hhy<-read.csv("data/honey_yield_historic.csv", sep=";",check.names=FALSE)
hhy

#total honey yield
keep<-c("2009", "2012", "2017", "2019", "2021")
head(bc)
head(hy)
th<-data.frame(hy[, keep]*bc[, keep])

rm.rows<-unique(c(which(th[,1]>100),which(th[,2]>100),which(th[,3]>100),which(th[,4]>100),which(th[,5]>100)))

th1<-th[-rm.rows,]
boxplot(th1, ylab="kg honey total", xlab="years")#check dat
boxplot(th, ylab="kg honey total", xlab="years")#check dat
boxplot(th,ylab="kg honey total", xlab="years", outline=FALSE)
ggsave(paste0(dirF, "total_honey_yield_without_outliers2.png"),width=7, height = 8, units="in", dpi=600 ) 

boxplot(hhy[,c(3:5)],ylab="kg honey total", xlab="years")
boxplot(hhy[,c(3:5)],ylab="kg honey total", xlab="years", outline=FALSE)
ggsave(paste0(dirF, "total_honey_historic_without_outliers.png"),width=7, height = 8, units="in", dpi=600 ) 

#convert table honey yield
hy_t <- pivot_longer(hy[,],cols = c("2009", "2012", "2017", "2019", "2021"), names_to = "year")
year.labs<-c("<2010", "2011", "2017", "2019", "2021")

#select only mathcing study villages
hhy<-hhy[-which(apply(hhy[,3:5], 1, sum, na.rm = TRUE)==0),]
unique(hhy$study_village)

villages<-c("chum", "urthu", "patmara", "patrasi", "gadigaun", "p_gadigaun", "Lorpa","tirkhu", "luma", "chaur", "s.gadigaun", "rini","chaura", "pere", "Tirku")
hhy2<-hhy %>% filter(study_village%in%villages)

#change structure of data
#subset of study villages
hhy2_t<-pivot_longer(hhy2[,], cols = c("1995", "1997", "1996"), names_to = "year")#,values_transform = as.numeric
df1<-hhy2_t %>% select(study_village, year, value)

#all villages
hhy_t<-pivot_longer(hhy[,], cols = c("1995", "1997", "1996"), names_to = "year")#,values_transform = as.numeric
df1<-hhy_t %>% select(study_village, year, value)

#unique(df1$study_village)
df2<-hy_t %>% select(study_village, year, value)
df3<-bind_rows(df1, df2)

###PLOT WITH PREDICTION LINES

#90s
y=log(df1$value+1)
x=as.numeric(df1$year)
mod.lm.hy1<-lm(y~x)
#assumptions
#plot(mod.lm.hy1)
#summary
#summary(mod.lm.hy1)

#2010s
y=log(df2$value+1)
x=as.numeric(df2$year)
mod.lm.hy2<-lm(y~x)
#assumptions
#plot(mod.lm.hy2)
#summary
#summary(mod.lm.hy2)

#All data
y = log(df3$value+1)
x = as.numeric(df3$year)
mod.lm.hy3<-lm(y~x)
#assumptions
#plot(mod.lm.hy3)
#summary
#summary(mod.lm.hy3)

#predict decline in 2026
#new_data<-data.frame(year=c(2023, 2024, 2025, 2026, 2027))
#predict(mod.lm.hy3, new_data, interval = 'confidence')

#pred_x = c(min(x),rep(max(x),2),max(x)+5)
pred_x3 = c(1995:2026)
#pred_lines = data.frame(x=pred_x,y=predict(hy_lm, data.frame(x=pred_x)),
 #                       obs_Or_Pred=rep(c("All data","Pred"), each=2))

pred_lines = data.frame(x=pred_x3, y=predict(mod.lm.hy3, data.frame(x=pred_x3)))
var<-c("Obs","Pred")
pred_lines$obs_Or_Pred<-rep(var, times=c(27, 5))
#pred lines for the original scales

#90s predlines
pred_x2 = c(1995:1997)
pred_lines_2 = data.frame(x=pred_x2, y=predict(mod.lm.hy2, data.frame(x=pred_x2)))
pred_lines_2$obs_Or_Pred<-c("90s")

#2010s predlines
pred_x2 = c(2009:2021)
pred_lines_3 = data.frame(x=pred_x2, y=predict(mod.lm.hy2, data.frame(x=pred_x2)))
pred_lines_3$obs_Or_Pred<-c("2010s")

#df1.lines<-data.frame(x=xs, y=ys, obs_Or_Pred=rep(c("90s","90s")))
#df2.lines<-data.frame(x=xs2, y=ys2, obs_Or_Pred=rep(c("2010s","2010s")))

pred_lines_al<-rbind(pred_lines, pred_lines_2, pred_lines_3)

lt<-c("solid","solid", "solid", "dashed", "dashed")
cols<-c("red", "blue", "black", "black", "black")
hy_plot_hist<-ggplot(pred_lines_al, aes(x, exp(y), colour=factor(obs_Or_Pred), shape=factor(obs_Or_Pred), linetype=factor(obs_Or_Pred))) +
  geom_jitter(data=data.frame(x,y, obs_Or_Pred="Obs"), size=3, position=position_jitter(0.1), alpha=0.3, colour="black", shape=16) +
  geom_line(size=1) +
  ylab("Kg honey yield per hive")+xlab("Year")+
  scale_linetype_manual(values=lt)+
  scale_color_manual(values = cols)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  theme_bw()+
  theme(axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title = element_text(size = 14)
  )+
  geom_label(aes(x = 2018, y = 45), hjust = 0, size=4,
             label = paste("Adj R2 = ",signif(summary(hy_lm)$adj.r.squared, 2)," \nP =",signif(summary(hy_lm)$coef[2,4], 2)), show.legend = FALSE, col="black")+
  theme(legend.title=element_blank(), legend.text = element_text(color = "black", size=14), legend.position="top")
  #geom_segment(aes(x = xs[1], xend = xs[2], y = ys[1], yend = ys[2]),colour = "red", size=1)+
  #geom_segment(aes(x = xs2[1], xend = xs2[2], y = ys2[1], yend = ys2[2]),colour = "blue",size=1)
hy_plot_hist

ggsave(paste0(dirF, "Historic_honey_yield_with_pred_line_org_scale_all.png"),width=7, height = 8, units="in", dpi=600 ) 





#get intercept and slope value for fd1
coeff<-coefficients(mod.lm.hy1)          
intercept1<-coeff[1]
slope1<- coeff[2]

xs = c(1995, 1997)
beta = c(intercept1, slope1)
ys = cbind(1, xs) %*% beta

#get intercept and slope value for fd1
coeff2<-coefficients(mod.lm.hy2)          
intercept2<-coeff2[1]
slope2<- coeff2[2]

xs2 = c(2009, 2021)
beta2 = c(intercept2, slope2)
ys2 = cbind(1, xs2) %*% beta2
