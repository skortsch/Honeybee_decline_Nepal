##Install required packages
library(ggplot2)

##Import crop data
crop_data <- read.csv("C:/LocalData/susakort/field work Nepal/Honeybee_decline_Nepal/Data/Jumla crops.csv") 

str(crop_data)

crop_honeybee_plot <- ggplot(crop_data, aes(x = perc_A.cerana_visits, y = reorder(eng_name, perc_A.cerana_visits), fill = perc_farmers_growing)) +
  geom_bar(stat = "identity", color="black") +
  scale_fill_gradient(name = "% farmers growing this crop", low = "white", high = "black", limits = c(0, 100)) +
  labs(x = bquote(~ "% flower visits made by" ~ italic("A. cerana")), y = "Crop") +
  theme_minimal()+
  theme(axis.title = element_text(size = 16))+  # Adjust both x and y-axis title properties
  theme(axis.text = element_text( size = 14))  # Adjust both x and y-axis title properties
  
crop_honeybee_plot 


ggsave(paste0(dirF, "Figure_5_main_text.png"),width=8, height=5, dpi=500, bg="white" ) 

