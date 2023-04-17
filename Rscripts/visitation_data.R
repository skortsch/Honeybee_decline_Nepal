

#this is the correct wd path
#C:\LocalData\susakort\field work Nepal\Rscripts
setwd("C:/LocalData/susakort/field work Nepal/Honeybee_decline_Nepal/Rscripts")


#Figure dir and pixels
ppi<-300 #pixels per inches
dirF<-"../Figures/"

#load library
library(tidyverse)
library(tidytext)
library(ggpubr)
library(ggpmisc)
library(rstatix)

#pollinator visitation data

pv<-read.csv("../data/pollinator_visitation_data.csv", sep=";",check.names=FALSE)
colnames(pv)
head(pv)

#Check unique honeybees
honeybees <- pv %>% select(village_code, pollinator_taxa, insect_OTU, plant_sci_name, plot_altitude, plant_habitat) %>% 
  filter(pollinator_taxa=="Honeybee")

unique(honeybees$insect_OTU)

#number of visits of bees to plants
pv %>% select(survey_date, village_code, pollinator_taxa, insect_OTU, plant_sci_name, plot_altitude, plant_habitat) %>% 
  group_by(survey_date, village_code, insect_OTU, plant_sci_name) %>% summarize(number_visits = n()) 

#number of visits of bees to plants
no_vis<-pv %>% select(survey_date, village_code, pollinator_taxa, insect_OTU, plant_sci_name, plot_altitude, plant_habitat) %>% 
  group_by(village_code, pollinator_taxa, insect_OTU, plant_sci_name) %>% summarize(number_visits = n()) 

pv %>% select(village_code, pollinator_taxa, insect_OTU, plant_sci_name, plot_altitude, plant_habitat) %>% 
  filter(insect_OTU=="Apis_cerana")


no_vis_poll_tax<-pv %>% select(survey_date, village_code, pollinator_taxa, insect_OTU, plant_sci_name, plot_altitude, plant_habitat) %>% 
  group_by(village_code, pollinator_taxa, plant_sci_name) %>% summarize(number_visits = n()) 


pd<-read.csv("../data/poll_dependence.csv", sep=";",check.names=FALSE)
head(pd)
pl.sel<-pd %>% filter(grepl('herb_spice|fruit_nut|oilseed|pulse', plant_category))
pl.sel$sci_name
pd$sci_name <- sub(" ", "_", pd$sci_name)
pd$plant_category

pd$sci_name
barplot(pd$poll_dependence)

pd$eng_name
