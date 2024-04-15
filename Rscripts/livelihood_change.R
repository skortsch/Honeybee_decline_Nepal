#load 
library(tidyverse)

#livlihood change
ld<-read.csv("data/livelihood_change.csv", sep=";",check.names=FALSE)

perc.ld<-ld %>% select(livelihood_affected) %>% count(livelihood_affected) %>% mutate(freq = n / sum(n)*100)
  
ld_causes_perc<-round(apply(ld[,c(4:9)], 2, sum, na.rm=T)/116*100)
#barplot(ld2)

ld2<-ld[,c(4:9)]

causes_counts<-ld2 %>% summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) 


names(causes_counts)<-c("income", "consumption", "beewax", "crop quality decrease", "crop quantity decrease")
reasons<-c("income", "consumption", "beewax", "crop quality decrease", "crop quantity decrease")
percentage<-round((causes_counts/sum(causes_counts))*100)
percentage$`crop quantity decrease` <-percentage$`crop quantity decrease`+1
sum(percentage)
#sum(ld$not_affected_other_income, na.rm=T)

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

#piechart

bp<- ggplot(causes2, aes(x=reasons, y=percentage, fill=reasons))+
  geom_bar(width = 1, stat = "identity")
bp

  bp+ coord_polar("y", start=0)+ theme_void()

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

ggsave(paste0(dirF, "livelihood_affected_pie_chart.png"),width=8, height = 10, units="in", dpi=600 ) 


pie_colors <- c("palegreen2","orangered2","orchid3",
                "palevioletred3","paleturquoise")

ggplot(causes2, aes("", percentage, fill = reasons)) +
  geom_bar(width = 1, size = 1, color = "white", 
           stat = "identity") +
  coord_polar("y") +
  geom_text(aes(label = paste0(percentage, "%")), 
            position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "Livlihood affected") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(values = pie_colors) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, 
                                  color = "black"))
