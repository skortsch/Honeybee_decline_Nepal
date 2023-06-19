#livlihood change
ld<-read.csv("data/livelihood_change.csv", sep=";",check.names=FALSE)

perc.ld<-ld %>% select(livelihood_affected) %>% count(livelihood_affected) %>% mutate(freq = n / sum(n)*100)
  
ld_causes_perc<-round(apply(ld[,c(4:9)], 2, sum, na.rm=T)/116*100)
#barplot(ld2)

ld2<-ld[,c(4:9)]

causes_counts<-ld2 %>% summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) 


names(causes_counts)<-c("income", "consumption", "beewax", "crop quality decrease", "crop quantity decrease")
reasons<-c("income", "consumption", "beewax", "crop quality decrease", "crop quantity decrease")
percentage<-round(causes_counts/116*100)

causes2<-rbind(names(causes_counts), causes_counts, as.vector(percentage))
causes2<-as.data.frame(t(causes2))
#colnames(causes2)<-c("reasons", "number", "percentage")

causes2 <- causes2[order(as.numeric(causes2$percentage),decreasing=TRUE),] #order rows

#plot
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

ggsave(paste0(dirF, "livelihood_affected.png"),width=8, height = 10, units="in", dpi=600 ) 


