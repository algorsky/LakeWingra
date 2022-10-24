#Load in packages
library(tidyverse)
library(lubridate) 

#Read in the data 
chloride<- read_csv("data/chloride/chloride.csv")

ggplot(chloride)+
  geom_jitter(aes(x = date, y = chloride, color = as.factor(site)))+
  theme_bw()

#Read in conductivity profiles
profiles<- read_csv("data/ancillary/profiles.csv")

#Read in historical profiles
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/2/34/3f740d0b77b3caf6930a8ce9cca4306a" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
wingra_chem<- read_csv(infile1)
wingra_chem = wingra_chem %>% filter(lakeid %in% c('WI'))%>%
  mutate(season = case_when(month(sampledate) <= 3 ~ 'winter',
                            month(sampledate) > 3 ~ 'summer'))

wingra_cl<- wingra_chem%>%
  filter(cl < 200 & cl > 10)

ggplot(wingra_cl)+
  geom_point(aes(x = sampledate, y = cl))
  
