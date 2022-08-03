#### Calculate flux using data from LGR
##### Instructions #####
  #1 set working directory under SETUP. Folder should be a folder specific to date and location as files will also be written there.
  #2 Load File and enter pressure under LOAD FILE and INFO
  #3 Run PLOT
  #4 Using plots, adjust the start and stop time of each trial under CHOOSE START STOP TIMES TRIAL _.
  #5 Then run all three of these sections
  #6 Run GET TIMES, CO2 FLUX, and CH4 FLUX


##### SETUP #####

rm(list=xxls())

library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggpubr)
library(cowplot)
library(stringi)
O<- .23 #Height (m)
M<- .30 #Height (m)
list.files("data/lgr/2022-07-13") #shows you all of the files inside the LGR Files folder

##### LOAD FILE and INFO #####

######replace the inside of the quotation marks with the file you need and the pressure
start_stop_finder <- read_csv("data/lgr/2022-07-13/gga_2022-07-13_f0000.txt", skip = 1)

pressure<-as.numeric("736.34") #mmHG
site<-as.character("Site5") # AL#
date<-"2022-07-13" #YYYY-MM-DD
date1<-"2022/07/13" #YYYY/MM/DD
chamber<-O #either O or M for open or macrophyte chambers



##### PLOT #####
start_stop_finder$Time <- mdy_hms(start_stop_finder$Time)
#plots a graph, you can use times as a reference
plot(start_stop_finder$Time, start_stop_finder$`[CH4]_ppm`, type = "l", main = "CH4 v Time", xlab = "Time", ylab = "CH4") 
plot(start_stop_finder$Time, start_stop_finder$`[CO2]_ppm`, type = "l", main = "CO2 v Time", xlab = "Time", ylab = "CO2") 




####CHOOSE START STOP TIMES TRIAL 1#####
#subset based on plot/data/recorded info for trials-
start_stop_finder2 <- start_stop_finder[c(370:475),]
#275-400
plot(start_stop_finder2$Time, start_stop_finder2$`[CH4]_ppm`, type = "l", main = "CH4 v Time", xlab = "Time", ylab = "CH4") 
plot(start_stop_finder2$Time, start_stop_finder2$`[CO2]_ppm`, type = "l", main = "CO2 v Time", xlab = "Time", ylab = "CO2")

####CHOOSE START STOP TIMES TRIAL 2#####
#subset based on plot/data/recorded info for trials-
start_stop_finder3 <- start_stop_finder[c(500: 600),]

plot(start_stop_finder3$Time, start_stop_finder3$`[CH4]_ppm`, type = "l", main = "CH4 v Time", xlab = "Time", ylab = "CH4") 
plot(start_stop_finder3$Time, start_stop_finder3$`[CO2]_ppm`, type = "l", main = "CO2 v Time", xlab = "Time", ylab = "CO2")

####CHOOSE START STOP TIMES TRIAL 3#####
#subset based on plot/data/recorded info for trials-
start_stop_finder4 <- start_stop_finder[c(640:800),]

plot(start_stop_finder4$Time, start_stop_finder4$`[CH4]_ppm`, type = "l", main = "CH4 v Time", xlab = "Time", ylab = "CH4") 
plot(start_stop_finder4$Time, start_stop_finder4$`[CO2]_ppm`, type = "l", main = "CO2 v Time", xlab = "Time", ylab = "CO2")






##### CO2 FLUX ####

#mol/m2/hour
CO2flux <-  function(trial,pressure){
  data <- trial
  data$Time <- as.numeric(data$Time)
  data$Time <- data$Time-data$Time[1]
  model <- lm(data$`[CO2]_ppm`~data$Time)
  SlopeAverage <- unname(model$coefficients[2][1])
  AirTemp <-  273.15+mean(data$GasT_C,na.rm = TRUE);#Kelvin
  Pressure <-  0.00131579*pressure#atmospheres
  GasConstant <-  44.617516*Pressure; #corrected gas constant in mol/m3
  ChamberHt <-  as.numeric(chamber)
  flux <-  (GasConstant/(273.15/AirTemp)*SlopeAverage*0.000001*ChamberHt)*60*60
  return(flux)}

#calculates flux for each trial
site_fluxes_CO2 <- data.frame(matrix(nrow = 1,ncol =6))
colnames(site_fluxes_CO2) <- c("Date", "Site", "co2 trial 1","co2 trial 2","co2 trial 3","co2_mean")

site_fluxes_CO2[1, "Date"]<-date1
site_fluxes_CO2[1, "Site"]<-site
site_fluxes_CO2[1, "co2 trial 1"]<-trial1fluxCO2<-CO2flux(start_stop_finder2, pressure)
site_fluxes_CO2[1, "co2 trial 2"]<-trial2fluxCO2<-CO2flux(start_stop_finder3, pressure)
site_fluxes_CO2[1, "co2 trial 3"]<-trial3fluxCO2<-CO2flux(start_stop_finder4, pressure)
site_fluxes_CO2[1,"co2_mean"] <- (trial1fluxCO2 + trial2fluxCO2 + trial3fluxCO2)/3 #Fix Mean function

nameCO2<-(as.character(c("data/lgr/clean/" ,date,site,"CO2.csv")))
nameCO2<- stri_paste(nameCO2, collapse='')
write_csv(site_fluxes_CO2,nameCO2)



##### CH4 CLUX #####

#mol/m2/hour
CH4flux <-  function(trial,pressure){
  data <- trial
  data$Time <- as.numeric(data$Time)
  data$Time <- data$Time-data$Time[1]
  model <- lm(data$`[CH4]_ppm`~data$Time)
  SlopeAverage <- unname(model$coefficients[2][1])
  AirTemp <-  273.15+mean(data$GasT_C,na.rm = TRUE);#Kelvin
  Pressure <-  0.00131579*pressure#atmospheres
  GasConstant <-  44.617516*Pressure; #corrected gas constant in mol/m3
  ChamberHt <-  as.numeric(chamber)
  flux <-  (GasConstant/(273.15/AirTemp)*SlopeAverage*0.000001*ChamberHt)*60*60
  return(flux)}


#calculates flux for each trial
site_fluxes_CH4 <- data.frame(matrix(nrow = 1,ncol =6))
colnames(site_fluxes_CH4) <- c("Date", "Site", "ch4 trial 1","ch4 trial 2","ch4 trial 3","ch4_mean")

site_fluxes_CH4[1, "Date"]<-date1
site_fluxes_CH4[1, "Site"]<-site
site_fluxes_CH4[1, "ch4 trial 1"]<-trial1fluxCH4<-CH4flux(start_stop_finder2, pressure)
site_fluxes_CH4[1, "ch4 trial 2"]<-trial2fluxCH4<-CH4flux(start_stop_finder3, pressure)
site_fluxes_CH4[1, "ch4 trial 3"]<-trial3fluxCH4<-CH4flux(start_stop_finder4, pressure)
site_fluxes_CH4[1,"ch4_mean"] <- (trial1fluxCH4 + trial2fluxCH4 + trial3fluxCH4)/3 # fix mean


nameCH4<-(as.character(c("data/lgr/clean/",date, site, "CH4.csv")))
      nameCH4<- stri_paste(nameCH4, collapse='')
write_csv(site_fluxes_CH4, paste(nameCH4))


##### Merge LGR Files #####

#Merges the clean files into a single spreadsheet with all dates and sites into LGRClean.csv in ALCreekMeth/Data

#read in ch4 files and bind together
ch4fluxfilelist<-as.character(list.files(path="Data/LGRCleanFiles/",pattern = 'CH4'))
ch4fluxlist<-lapply(paste("Data/LGRCleanFiles/",ch4fluxfilelist, sep = ""), read_csv)
ch4flux<-bind_rows(ch4fluxlist)

#read in co2 files and bind together
co2fluxfilelist<-as.character(list.files(path="Data/LGRCleanFiles/", pattern = 'CO2'))
co2fluxlist<-lapply(paste("Data/LGRCleanFiles/",co2fluxfilelist, sep = ""), read_csv)
co2flux<-bind_rows(co2fluxlist)

#join flux tables
df<-left_join(co2flux, ch4flux )
df<-df %>% #add depth 0 to bind
  mutate(Depth =0)

write_csv(df, "Data/LGRClean.csv")


