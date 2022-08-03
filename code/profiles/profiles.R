#Load in packages
library(tidyverse) #includes ggplot and dplyr
library(lubridate) #helps convert numbers/characters to dates

#Read in the data (you might have to modify depending on how you created your R project and what folder structure you have)
profiles<- read_csv("data/ancillary/profiles.csv")

#Convert Date from a character to a Date using the lubridate package and dplyr format
profiles<- profiles%>%
  mutate(Date = as.Date(Date, "%y-%m-%d")) #Originally "22-06-01" and now "2022-06-01"

#Temperature Plot
ggplot(profiles)+
  geom_point(aes(x = Temp_C, y = Depth, color = as.factor(Date)))+ #you can change x axis 
  geom_path(aes(x = Temp_C, y = Depth, color = as.factor(Date)))+ #you can change x axis 
  scale_y_reverse()+
  facet_wrap(~Site, scales = "free")+
  xlab("Temperature (C°)")+  #you can edit to change x axis label
  ylab("Depth (m)")+
  theme_bw()

#Conductivity Plot
ggplot(profiles)+
  geom_point(aes(x = Conductivity, y = Depth, color = as.factor(Date)))+
  geom_path(aes(x = Conductivity, y = Depth, color = as.factor(Date)))+
  scale_y_reverse()+
  facet_wrap(~Site, scales = "free")+
  xlab(expression(paste('Conductivity (',mu,'S)')))+ #another way to write an axis label if there's a special symbol
  ylab("Depth (m)")+
  theme_bw()

#Dissolved Oxygen Saturation (%)

#pH

#Chlorophyll

