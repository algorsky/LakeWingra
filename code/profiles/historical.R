#Load in packages
library(tidyverse) #includes ggplot and dplyr
library(lubridate) #helps convert numbers/characters to dates


# Package ID: knb-lter-ntl.29.30 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: Physical Limnology of Primary Study Lakes 1981 - current.

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/29/30/03e232a1b362900e0f059859abe8eb97" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))

# Package ID: knb-lter-ntl.1.52 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: Chemical Limnology of Primary Study Lakes: Nutrients, pH and Carbon 1981 - current.
inUrl2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/1/52/802d63a4c35050b09ef6d1e7da3efd3f" 
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl"))

wingra = read_csv(infile1) 
str(wingra) # Check Structure
#Filter for Wingra and add season
wingra = wingra %>% filter(lakeid %in% c('WI'))%>%
  mutate(season = case_when(month(sampledate) <= 3 ~ 'winter',
                          month(sampledate) > 3 ~ 'summer'))

wingra_chem<- read_csv(infile2)
wingra_chem = wingra_chem %>% filter(lakeid %in% c('WI'))%>%
  mutate(season = case_when(month(sampledate) <= 3 ~ 'winter',
                            month(sampledate) > 3 ~ 'summer'))


ggplot(wingra)+
  geom_point(aes(x = o2sat, y = depth, color = as.factor(month(sampledate))))+
  geom_path(aes(x = o2sat, y = depth, group = sampledate, color = as.factor(month(sampledate))))+
  scale_y_reverse()+
  facet_wrap(~year4, scales = "free")+
  theme_bw()
