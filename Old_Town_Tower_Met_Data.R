#This script reads, filters, and plots data collected from meteorological towers installed in Old Town, ME
#Variables include air temperature, relative humidity, soil temperature, moisture, specific conductance, and snow depth
#Senors are a combination of "research grade" instrumentation from Campbell Scientific and "open source" instrumentation
#controlled by an Arduino Uno logger
#The towers also monitor vegetation phenology with a phenocam, but these data are not included here

#There are two towers, one in a hardwood site, "the hardwood neighborhood", situated within a mixed
#deciduous and coniferous forest and one in a softwood site, in a "softwood neighborhood"
#Data are collected by Ed Lindsey and the Collaborative Research class, with the NSF MSB Vernal Windows project 
#and the NSF INSPIRES projects playing a supporting role

#Code written by Alix Contosta (alix.contosta@unh.edu)

#####################################
#Initiatl Set Up
#####################################

#call libraries
library(data.table)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggpmisc)
library(pracma)
library(zoo)

########################################
#read in data and do data preprocessing#
########################################

#set working directory
setwd("C:\\Users\\alixc\\OneDrive - USNH\\UNH\\Projects\\Vernal Windows\\Tower_Data")

#read manual OT snow depth data
msno = read.table("manual_snow.csv", head = T, sep = ",")

#read tables. The SOIL_MET.dat files are dynamic and continuously updated
hw = read.table("New_Tower_Data\\\\OTHSHW_SOIL_MET.dat", head = F, sep = ",", skip = 4, na.strings = c("NaN", "NAN"))
sw = read.table("New_Tower_Data\\\\OTHSSW_SOIL_MET.dat", head = F, sep = ",", skip = 4, na.strings = c("NaN", "NAN"))

#the backup files are static and contain data from before open-source, Arduino sensors were added
hw_old = read.table("OTHSHW_SOIL_MET.dat.backup", head = F, sep = ",", skip = 4, na.strings = c("NaN", "NAN"))
sw_old = read.table("OTHSSW_SOIL_MET.dat.backup", head = F, sep = ",", skip = 4, na.strings = c("NaN", "NAN"))

#add ghost columns to hw_old and sw_old from before the Arduino data were added
hw_ghost = data.frame(matrix(vector(), nrow = nrow(hw_old), ncol = 5))
sw_ghost = data.frame(matrix(vector(), nrow = nrow(sw_old), ncol = 5))

#convert data to numeric
hw_ghost = as.data.frame(sapply(hw_ghost, as.numeric))
sw_ghost = as.data.frame(sapply(sw_ghost, as.numeric))

#combine ghost columns with hw_old and sw_old
hw_old.1 = cbind(hw_old, hw_ghost)
sw_old.1 = cbind(sw_old, sw_ghost)

#add names to dataframes
#first read lines with column names, and split and format strings 
h = readLines("New_Tower_Data\\\\OTHSHW_SOIL_MET.dat")[2]
h2 = as.factor(unlist(strsplit(h, ",")) )
h3 = gsub('"', "", h2)

s = readLines("New_Tower_Data\\\\OTHSSW_SOIL_MET.dat")[2]
s2 = as.factor(unlist(strsplit(s, ",")) )
s3 = gsub('"', "", s2)

#add names to hw and sw
names(hw) = h3
names(sw) = s3

#add names to hw_old.1 and sw_old.2
names(hw_old.1) = h3
names(sw_old.1) = s3

#combine hw and hw_old.1 and sw and sw.old.1
hw.1 = rbind(hw_old.1, hw)
sw.1 = rbind(sw_old.1, sw)

#add column for identifying site
hw.1$Site = "HW"
sw.1$Site = "SW"

#combine into a single file
ot = rbind(hw.1, sw.1)

#convert date to posix compliant object and extract Year, Month, and DOY
ot$DATETIME = as.POSIXct(strptime(ot$TIMESTAMP, "%Y-%m-%d %H:%M:%S", tz="EST"))
ot$Year = as.numeric(strftime(ot$DATETIME, format = "%Y", tz = "EST", usetz = FALSE))
ot$Month = as.numeric(strftime(ot$DATETIME, format = "%m", tz = "EST", usetz = FALSE))
ot$DOY = as.numeric(strftime(ot$DATETIME, format = "%j", tz = "EST", usetz = FALSE))
ot$hour = as.numeric(strftime(ot$DATETIME, format = "%H", tz = "EST", usetz = FALSE))

msno$DATE = as.Date(strptime(msno$Sample.Date, "%m/%d/%Y %H:%M", tz="EST"))
msno$Year = as.numeric(strftime(msno$DATE, format = "%Y", tz = "EST", usetz = FALSE))
msno$DOY = as.numeric(strftime(msno$DATE, format = "%j", tz = "EST", usetz = FALSE))

#############################################
#Run QAQC protocols to flag data are outside
#measurement range or are of poor quality
###########################################

###########################################
#Power reaching datalogger (batt_volt_Min)#
###########################################

#Flag values below 12V, which would indicate that the system did not have enough power to operate
#"A" = acceptable, "I" = outside of range
ot$batt_Flag = ifelse(ot$batt_volt_Min < 12, "I", "A")

#####################################
#Air Temperature (AirT and ARD_AirT)#
#####################################

#Flag values outside of EE181 instrument range (-40 to 60 deg C)
#"A" indicates acceptable; "I" indicates outside of measurement range; "M" indicates missing
ot$AirT_Flag = ifelse(is.na(ot$AirT) == T, "M", ifelse(ot$AirT < -40 | ot$AirT > 60, "I", "A"))

#Flag values outside of AM2315 Air Temp and RH probe (-40 to 125 deg C)
#"A" indicates acceptable; "I" indicates outside of measurement range; "M" indicates missing
ot$ARD_AirT_Flag = ifelse(is.na(ot$ARD_AirT) == T, "M", ifelse(ot$ARD_AirT < -40 | ot$ARD_AirT > 125, "I", "A"))

###################################
#Relative Humidity (RH and ARD_RH)#
###################################

#Flag values outside of EE181 instrument range (0-100%)
#"A" indicates acceptable; "I" indicates outside of measurement range; "M" indicates missing
ot$RH_Flag = ifelse(is.na(ot$RH) == T, "M", ifelse(ot$RH < 0 | ot$RH > 100, "I", "A"))

#Flag values outside of AM2315 Air Temp and RH probe (0-100%)
#"A" indicates acceptable; "I" indicates outside of measurement range; "M" indicates missing
ot$ARD_RH_Flag = ifelse(is.na(ot$ARD_RH) == T, "M", ifelse(ot$ARD_RH < 0 | ot$ARD_RH > 100, "I", "A"))

##############################
#Snow Depth (TCDT and ARD_DT)#
##############################

#Flag values outside of SR50A instrument range (0.5-10 m)
#"A" indicates acceptable; "I" indicates outside of measurement range; "M" indicates missing
ot$TCDT_Flag = ifelse(is.na(ot$TCDT) == T, "M", ifelse(ot$TCDT < 0.5 | ot$TCDT > 10, "I", "A"))

#Flag values outside of Max Botix HRXL instrument range (50 to 1940 cm)
#"A" indicates acceptable; "I" indicates outside of measurement range; "M" indicates missing
ot$ARD_DT_Flag = ifelse(is.na(ot$ARD_DT) == T, "M", ifelse(ot$ARD_DT < 50 | ot$ARD_DT > 1940, "I", "A"))

#Flag measurements for SR50A of poor quality, where Q < 152 or > 210
#"A" indicates acceptable; "I" indicates outside of data quality range; "M" indicates missing
ot$Q_Flag = ifelse(is.na(ot$Q) == T, "M", ifelse(ot$Q < 152 | ot$Q > 210, "I", "A"))

#################################################
#Soil Temperature (TSoil_5, TSoil_25, ARD_SoilT)#
#################################################

#Flag values outside of measurement range for CS650-33 (-50 to 70 deg C)
#"A" indicates acceptable; "I" indicates outside of measurement range; "M" indicates missing
ot$TSoil_5_Flag = ifelse(is.na(ot$TSoil_5) == T, "M", ifelse(ot$TSoil_5 < -50 | ot$TSoil_5 > 70, "I", "A"))
ot$TSoil_25_Flag = ifelse(is.na(ot$TSoil_25) == T, "M", ifelse(ot$TSoil_25 < -50 | ot$TSoil_25 > 70, "I", "A"))

#Flag values outside of measurement range for SHT10 (-40 to 123.8 deg C)
#"A" indicates acceptable; "I" indicates outside of measurement range; "M" indicates missing
ot$ARD_SoilT_Flag = ifelse(is.na(ot$ARD_SoilT) == T, "M", ifelse(ot$ARD_SoilT < -40 | ot$ARD_SoilT > 123.8, "I", "A"))

#######################################
#Soil Moisture(VWC_5, VWC_25, ARD_VWC)#
#######################################

#Flag values outside of measurement range for CS650-33 (0 to 100%)
#"A" indicates acceptable; "I" indicates outside of measurement range; "M" indicates missing
ot$VWC_5_Flag = ifelse(is.na(ot$VWC_5) == T, "M", ifelse(ot$VWC_5 < 0 | ot$VWC_5 > 100, "I", "A"))
ot$VWC_25_Flag = ifelse(is.na(ot$VWC_25) == T, "M", ifelse(ot$VWC_25 < 0 | ot$VWC_25 > 100, "I", "A"))

#Flag values outside of measurement range for SHT10 (0 to 100%)
#"A" indicates acceptable; "I" indicates outside of measurement range; "M" indicates missing
ot$ARD_VWC_Flag = ifelse(is.na(ot$ARD_VWC_5) == T, "M", ifelse(ot$ARD_VWC_5 < 0 | ot$ARD_VWC_5 > 100, "I", "A"))

###########################################
#Soil Electrical Conductivity(EC_5, EC_25)#
###########################################

#Flag values outside of measurement range for CS650-33 (0 to 3 dS / m)
#"A" indicates acceptable; "I" indicates outside of measurement range; "M" indicates missing
ot$EC_5_Flag = ifelse(is.na(ot$EC_5) == T, "M", ifelse(ot$EC_5 < 0 | ot$EC_5 > 3, "I", "A"))
ot$EC_25_Flag = ifelse(is.na(ot$EC_25) == T, "M", ifelse(ot$EC_25 < 0 | ot$EC_25 > 3, "I", "A"))

#############################################
#Create new columns that filter flagged data#
#############################################

ot$AirT_corr = ifelse(ot$AirT_Flag == "I", NA, ot$AirT)
ot$RH_corr = ifelse(ot$RH_Flag == "I", NA, ot$RH)
ot$TCDT_corr = ifelse(ot$TCDT_Flag == "I" | ot$Q_Flag == "I", NA, ot$TCDT)
ot$TSoil_5_corr = ifelse(ot$TSoil_5_Flag == "I", NA, ot$TSoil_5)
ot$TSoil_25_corr = ifelse(ot$TSoil_25_Flag == "I", NA, ot$TSoil_25)
ot$VWC_5_corr = ifelse(ot$VWC_5_Flag == "I", NA, ot$VWC_5)
ot$VWC_25_corr = ifelse(ot$VWC_25_Flag == "I", NA, ot$VWC_25)
ot$EC_5_corr = ifelse(ot$EC_5_Flag == "I", NA, ot$EC_5)
ot$EC_25_corr = ifelse(ot$EC_25_Flag == "I", NA, ot$EC_25)

ot$ARD_AirT_corr = ifelse(ot$ARD_AirT_Flag == "I", NA, ot$ARD_AirT)
ot$ARD_RH_corr = ifelse(ot$ARD_RH_Flag == "I", NA, ot$ARD_RH)
ot$ARD_DT_corr = ifelse(ot$ARD_DT_Flag == "I", NA, ot$ARD_DT)
ot$ARD_SoilT_corr = ifelse(ot$ARD_SoilT_Flag == "I", NA, ot$ARD_SoilT)
ot$ARD_VWC_corr = ifelse(ot$ARD_VWC_Flag == "I", NA, ot$ARD_VWC_5)

##############################################################
#subtract snow sensor height from TCDT_corr to get snow depth#
#height varies over time as boom was adjusted
#multiply by 100 to get units in cm
##############################################################
ot$SD = ifelse(ot$DATETIME < as.POSIXct("2019-11-15 00:00:00") &
                 ot$Site == "HW",
               (1.81 - ot$TCDT_corr) * 100,
               ifelse(ot$DATETIME < as.POSIXct("2020-04-01 00:00:00") &
                        ot$Site == "HW",
                      (1.80 - ot$TCDT_corr) * 100,
                      ifelse(ot$DATETIME >= as.POSIXct("2020-04-01 00:00:00") &
                               ot$Site == "HW",
                             (1.84 - ot$TCDT_corr) * 100,
                             ifelse(ot$DATETIME < as.POSIXct("2020-04-01 00:00:00") &
                                      ot$Site == "SW",
                                    (1.84 - ot$TCDT_corr) * 100,
                                    (1.88 - ot$TCDT_corr) * 100))))

#make values that were below zero or outside of period of snowpack
#occurrence (determined by Ed Lindsey and OTHS students
#see link: https://docs.google.com/spreadsheets/d/1A-dZsYg5leZ4hKduwx23xjWYmg7BKPQFQ0pEczrk3Rs/edit#gid=1196535705)
ot$SnowDepth = ifelse(ot$DATETIME > as.POSIXct("2019-04-16 00:00:00") & ot$DATETIME < as.POSIXct("2019-11-13 00:00:00")
                      & ot$Site == "HW", 0,
                      ifelse(ot$DATETIME > as.POSIXct("2020-04-15 00:00:00") & ot$DATETIME < as.POSIXct("2020-12-07 00:00:00")
                             & ot$Site == "HW", 0,
                             ifelse(ot$DATETIME > as.POSIXct("2021-03-28 00:00:00") & ot$DATETIME < as.POSIXct("2021-11-28 00:00:00")
                                    & ot$Site == "HW", 0,
                                    ifelse(ot$DATETIME > as.POSIXct("2022-03-30 00:00:00") & ot$DATETIME < as.POSIXct("2022-12-20 00:00:00") 
                                           & ot$Site == "HW", 0,
                                           ifelse(ot$DATETIME > as.POSIXct("2023-04-07 00:00:00")
                                                  & ot$Site == "HW", 0,
                                                  ifelse(ot$DATETIME > as.POSIXct("2019-04-17 00:00:00") & ot$DATETIME < as.POSIXct("2019-11-13 00:00:00")
                                                         & ot$Site == "SW", 0,
                                                         ifelse(ot$DATETIME > as.POSIXct("2020-04-15 00:00:00") & ot$DATETIME < as.POSIXct("2020-12-07 00:00:00")
                                                                & ot$Site == "SW", 0,
                                                                ifelse(ot$DATETIME > as.POSIXct("2021-03-22 00:00:00") & ot$DATETIME < as.POSIXct("2021-11-28 00:00:00")
                                                                       & ot$Site == "SW", 0,
                                                                       ifelse(ot$DATETIME > as.POSIXct("2022-03-30 00:00:00") & ot$DATETIME < as.POSIXct("2022-12-20 00:00:00")
                                                                              & ot$Site == "SW", 0,
                                                                              ifelse(ot$DATETIME > as.POSIXct("2023-04-03 00:00:00")
                                                                                     & ot$Site == "SW", 0,
                                                                                     ifelse(ot$SD < 0, 0, ot$SD)))))))))))


##############################################################
#subtract snow sensor height from ARD_DT_corr to get snow depth#
#height is 1920 in HW and 1937 in SW
#divide by 10 to get units in cm
##############################################################
ot$ARD_SD = ifelse(ot$Site == "HW", (1920 - ot$ARD_DT_corr) /10,
                   (1937 - ot$ARD_DT_corr) /10)

#make values that were below zero or outside of period of snowpack
#occurrence (determined by Ed Lindsey and OTHS students
#see link: https://docs.google.com/spreadsheets/d/1A-dZsYg5leZ4hKduwx23xjWYmg7BKPQFQ0pEczrk3Rs/edit#gid=1196535705)
ot$ARD_SnowDepth = ifelse(ot$DATETIME > as.POSIXct("2021-03-22 00:00:00") & 
                            ot$DATETIME < as.POSIXct("2021-11-28 00:00:00"), 0, 
                          ifelse(ot$DATETIME > as.POSIXct("2022-03-30 00:00:00") & ot$DATETIME < as.POSIXct("2022-12-20 00:00:00") 
                                 & ot$Site == "HW", 0,
                                 ifelse(ot$DATETIME > as.POSIXct("2023-04-07 00:00:00")
                                        & ot$Site == "HW", 0,
                                        ifelse(ot$DATETIME > as.POSIXct("2022-03-30 00:00:00") & ot$DATETIME < as.POSIXct("2022-12-20 00:00:00")
                                               & ot$Site == "SW", 0,
                                               ifelse(ot$DATETIME > as.POSIXct("2023-04-03 00:00:00")
                                                      & ot$Site == "SW", 0,
                                                      ifelse(ot$ARD_SD < 0, 0,
                                                             ifelse(ot$ARD_SD > quantile(ot$SD, 0.99, na.rm = T), NA,
                                                                    ot$ARD_SD))))))) 

######################################################
#correct electrical conductivity measurements for 
#temperature, which will provide specific conductance
######################################################
ot$SpeCon_5 = ot$EC_5_corr / (1 + 0.02 * (ot$TSoil_5_corr - 25))
ot$SpeCon_25 = ot$EC_25_corr / (1 + 0.02 * (ot$TSoil_25_corr - 25))

###############################################
#select corrected data columns for file export#
###############################################
ot_sub = ot[ , c("Site", "DATETIME", "Year", "Month", "DOY",
                 "hour", "AirT_corr", "RH_corr", "TSoil_5_corr", 
                 "TSoil_25_corr", "VWC_5_corr", "VWC_25_corr", 
                 "ARD_AirT_corr",  "ARD_RH_corr", "ARD_SoilT_corr", 
                 "ARD_VWC_corr", "SnowDepth", "SD", "ARD_SnowDepth",
                 "ARD_SD", "SpeCon_5", "SpeCon_25")]
names(ot_sub) = c("Site", "DATETIME", "Year", "Month", "DOY",
                  "hour", "AirT", "RH", "TSoil_5", 
                  "TSoil_25", "VWC_5", "VWC_25", 
                  "ARD_AirT",  "ARD_RH", "ARD_SoilT", 
                  "ARD_VWC", "SnowDepth", "SD_0", "ARD_SnowDepth",
                  "ARD_SD_0", "SpeCon_5", "SpeCon_25")

#######################
#write table to folder#
#######################

write.table(ot_sub, "btow_QA.csv", col.names = T, row.names = F, sep = ",")

#############################
#make figures for manuscript#
#############################

#do data transformations to prepare for making figures

#rename variables for plotting
ot_sub$Cover = factor(ot_sub$Site, levels = c("HW", "SW"), 
                     labels = c("Mixed Forest", "Coniferous Forest"))



#calculate daily summary values
#make ydoy column for data reduction
ot_sub$ydoy = paste(ot_sub$Year, ot_sub$DOY)
ot_sub$ID = paste(ot_sub$ydoy, ot_sub$Cover)

#make ot_sub into a data table
ot_sub = data.table(ot_sub)

#calculate daily mean, min, and max snow depth, air temperature, soil temperature
#and soil moisture. This will return some warnings due to missing values.

ot_sum = ot_sub[ , list(Year = unique(Year), DOY = unique(DOY), 
                        ydoy = unique(ydoy), Cover = unique(Cover),
                        SnowDepth.avg = mean(SnowDepth, na.rm = T), 
                        SnowDepth.min = min(SnowDepth, na.rm = T),
                        SnowDepth.max = max(SnowDepth, na.rm = T),
                        AirT.avg = mean(AirT, na.rm = T), 
                        AirT.min = min(AirT, na.rm = T),
                        AirT.max = max(AirT, na.rm = T),
                        TSoil_5.avg = mean(TSoil_5, na.rm = T), 
                        TSoil_5.min = min(TSoil_5, na.rm = T),
                        TSoil_5.max = max(TSoil_5, na.rm = T),
                        VWC_5.avg = mean(VWC_5, na.rm = T), 
                        VWC_5.min = min(VWC_5, na.rm = T),
                        VWC_5.max = max(VWC_5, na.rm = T)), by = ID]


#make ydoy into a date object
ot_sum$DATE = as.Date(ot_sum$ydoy, "%Y %j")

################################################
#merge ot_sum with manual snow observations (msno) 
#to gap fill missing snow sensor measurements
################################################

#make new msno column for snow cover
msno$Cover = ifelse(msno$Neighborhood == "HW", "Mixed Forest", "Coniferous Forest")

#make ID columns for merging
ot_sum$cydoy = paste0(ot_sum$Cover, ot_sum$Year, ot_sum$DOY)
msno$cydoy = paste0(msno$Cover, msno$Year, msno$DOY)

#merge
ot_sum = merge(ot_sum, msno, by = "cydoy", all.x = T, all.y = T)

#convert manual measurements into cm
ot_sum$SnowDepth.cm = ot_sum$Snow.Depth / 10

#calculate diurnal range in air and soil temperature
ot_sum$AirT.range = ot_sum$AirT.max - ot_sum$AirT.min
ot_sum$TSoil_5.range = ot_sum$TSoil_5.max - ot_sum$TSoil_5.min

############################################################
#plot relationship between manual and automated measurements
############################################################

#write linear formula to include on figure
my.formula = y ~ x

snocomp = ggplot(ot_sum, aes(x = SnowDepth.cm, y = SnowDepth.avg, col = Cover.x, shape = Cover.x))+
  geom_abline(slope = 1, intercept = 0, color = "red")+
  geom_smooth(method='lm', fill = "grey80", formula= y~x)+
  stat_poly_eq(formula = my.formula,
               label.x = "left", label.y = "top",
               aes(label = paste(..p.value.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +   
  geom_point(size = 4)+
  coord_cartesian(xlim = c(min(c(ot_sum$SnowDepth.cm, ot_sum$SnowDepth.avg), na.rm = T),
                           max(c(ot_sum$SnowDepth.cm, ot_sum$SnowDepth.avg), na.rm = T)), 
                  ylim = c(min(c(ot_sum$SnowDepth.cm, ot_sum$SnowDepth.avg), na.rm = T),
                           max(c(ot_sum$SnowDepth.cm, ot_sum$SnowDepth.avg), na.rm = T)))+
  scale_colour_manual(values = c("#26854b", "#457dbd"))+
  labs(x = "Manual Snow Depth (cm)", y = "Automated Snow Depth (cm)",
       col = "Cover", shape = "Cover")+
  theme_bw()+
  theme(legend.position = c(0.85, 0.25))

#run regressions to obtain parameters for gapfilling
lm.hw = lm(SnowDepth.avg ~ SnowDepth.cm, data = ot_sum, 
           subset = ot_sum$Cover.x == "Mixed Forest")
lm.sw = lm(SnowDepth.avg ~ SnowDepth.cm, data = ot_sum, 
           subset = ot_sum$Cover.x == "Coniferous Forest")

#################################################################
#fill missing automated values with manual values (when present)
#adjusting for the difference between measurements with 
#regression parameters
#################################################################

ot_sum$snofill.1 = ifelse(is.na(ot_sum$SnowDepth.avg) == T & ot_sum$Cover.y == "Mixed Forest",
                          (ot_sum$SnowDepth.cm *lm.hw$coefficients[2]) + lm.hw$coefficients[1],
                          ifelse(is.na(ot_sum$SnowDepth.avg) == T & ot_sum$Cover.y == "Coniferous Forest",
                                 (ot_sum$SnowDepth.cm *lm.sw$coefficients[2]) + lm.sw$coefficients[1],
                                 ot_sum$SnowDepth.avg))
                                 
#create master columns for Cover, Year, DOY for ordering observations for plotting
ot_sum$Cover = ifelse(is.na(ot_sum$Cover.x) == T, ot_sum$Cover.y, 
                      as.character(ot_sum$Cover.x))
ot_sum$Year = ifelse(is.na(ot_sum$Year.x) == T, ot_sum$Year.y, 
                      ot_sum$Year.x)
ot_sum$DOY = ifelse(is.na(ot_sum$DOY.x) == T, ot_sum$DOY.y, 
                     ot_sum$DOY.x)
ot_sum$DATE = as.Date(ifelse(is.na(ot_sum$DATE.x) == T, as.Date(ot_sum$DATE.y), 
                      ot_sum$DATE.x))

#set snow depth to 0 for 11-14-2020 in the coniferous stand to manually adjust spurious values
#that passed intial QA
ot_sum$snofill.2 = ifelse(ot_sum$Cover == "Coniferous Forest" & 
                            ot_sum$DATE == as.Date("2020-11-14"), 0,
                              ot_sum$snofill.1)


#make a column for water year to keep winters together
ot_sum$wyear = ifelse(ot_sum$DOY < 275, ot_sum$Year, ot_sum$Year + 1)

#then make ID column that combines wyear and cover
ot_sum$ID = paste(ot_sum$wyear, ot_sum$Cover)

#order observations
ot_sum = ot_sum[order(ot_sum$Cover, ot_sum$Year, ot_sum$DOY), ]

#interpolate between NAs
ot_sum$snofill = na.fill(na.approx(ot_sum$snofill.2, na.rm = F), "extend")

################################################################
#generate statistics on max snow depth for each site and winter
################################################################

#calculate max depth
max_snow = ot_sum %>%
  group_by(ID) %>%
  summarise(max_snow = max(snofill, na.rm = T))

#calculate day of year at which max depth occurrred
max_rows <- ot_sum %>%
  group_by(ID) %>%
  slice(which.max(SnowDepth.avg))


#####################################################################################
#calculate average, min, and max snow depth for each DOY and each stand for plotting
#####################################################################################

#create column for forest type and DOY
ot_sum$cdoy = paste(ot_sum$Cover, ot_sum$DOY, sep = "_")

ot_avg = ot_sum[ , list(Cover = unique(Cover), DOY = unique(DOY),
                        avgsnow = mean(snofill),
                        medsnow = median(snofill),
                        minsnow = min(snofill),
                        maxsnow = max(snofill)), by = cdoy]


#####################################################
#plot snow depth across all five years in the dataset
#with rows showing different years and columns showing
#canopy density. This is Fig. S4 in the Pastore,
#Nelson, et al. Ecosphere manuscript
#####################################################

#make new canopy cover column to order layers in figure
ot_sum$c2 = ifelse(ot_sum$Cover == "Coniferous Forest", "B", "A")

#make new DOY to start ordering at DOY 300
#first omit leap years
ot_sum = ot_sum[ot_sum$DOY != 366, ]
ot_sum$DOY2 = ifelse(ot_sum$DOY >= 300, ot_sum$DOY - 299, ot_sum$DOY + 65)

#remove data for non-dormant season (so that the plots don't have a lot of white space
#when there is no snow)
ot_sum2 = ot_sum[ot_sum$DOY2 <= 200, ]

snowp = ggplot(ot_sum2, aes(x = DOY2, y = snofill, color = c2, fill = c2))+
  geom_line()+
  geom_area(position = "identity")+
  scale_color_manual(values = c("#457dbd", "#26854b"), labels = c("Medium", "High"))+
  scale_fill_manual(values = c("#a2bede", "#92c2a5"), labels = c("Medium", "High"))+
  labs(x = "Day of Year", y = "Snow depth (cm)", 
       fill = "Canopy Density",
       color = "Canopy Density")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(legend.position = "none")+
  facet_grid(rows = vars(wyear),
             cols = vars(c2), labeller = as_labeller(c("A" = "Medium Canopy Density",
                                              "B" = "High Canopy Density",
                                              "2019" = "2019",
                                              "2020" = "2020",
                                              "2021" = "2021",
                                              "2022" = "2022",
                                              "2023" = "2023")))
#write snowp to folder
#call pdf options
pdf.options(width= 6.5, height= 6.5, paper="letter", pointsize=10)

pdf("OT_All_Snow.pdf")

snowp

dev.off()

#####################################################
#plot median and range of snow depth by stand 
#This is Fig. 4a in Pastore, Nelson, et al. Ecosphere 
#manuscript
#####################################################

#make new canopy cover column to order layers in figure
ot_avg$c2 = ifelse(ot_avg$Cover == "Coniferous Forest", "B", "A")

#make new DOY to start ordering at DOY 300
#first omit leap years
ot_avg = ot_avg[ot_avg$DOY != 366, ]
ot_avg$DOY2 = ifelse(ot_avg$DOY >= 300, ot_avg$DOY - 299, ot_avg$DOY + 65)

#remove data for non-dormant season (so that the plots don't have a lot of white space
#when there is no snow)
ot_avg2 = ot_avg[ot_avg$DOY2 <= 200, ]

avgsnop = ggplot(data = ot_avg2, aes(x = DOY2))+
  geom_line(aes(y = medsnow, color = c2), linewidth = 1)+
  #geom_area(position = "identity")+
  scale_color_manual(values = c("#457dbd", "#26854b"), labels = c("Medium", "High"))+
  scale_fill_manual(values = c("#a2bede", "#92c2a5"), labels = c("Medium", "High"))+
  scale_x_continuous(
    breaks = c(0, 50, 100, 150, 200),
    labels = c(300, 350, 35, 85, 135)
  ) +
  labs(x = "Day of Year", y = "Snow depth (cm)", 
       fill = "Canopy Density",
       color = "Canopy Density")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(legend.position = "none")+
  facet_wrap(vars(c2), labeller = as_labeller(c("A" = "Medium Canopy Density",
                                                "B" = "High Canopy Density")))

#write avgsnop to folder
#call pdf options
pdf.options(width= 6.5, height= 4.5, paper="letter", pointsize=10)

pdf("OT_Median_Snow.pdf")

avgsnop

dev.off()

####################################################
#calculate threshold conditions for snow depth
#air temperature, and soil temperature
####################################################

#subset ot_sum to include just the snow-covered season
#list date ranges
date_ranges = list(
  start = c(as.Date("2018-11-21"), as.Date("2019-11-13"), as.Date("2020-12-07"), 
                    as.Date("2021-11-28"), as.Date("2022-12-19")), 
  end = c(as.Date("2019-04-16"), as.Date("2020-04-15"), as.Date("2021-03-28"),
          as.Date("2022-03-30"), as.Date("2023-04-07"))
  )
  
#Initialize an empty logical vector to store the filtering conditions
conditions = logical(nrow(ot_sum))

# Loop through each date range and update conditions
for (i in seq_along(date_ranges$start)) {
  conditions <- conditions | (ot_sum$DATE >= date_ranges$start[i] & ot_sum$DATE <= date_ranges$end[i])
}

# Subset the dataframe based on the conditions
ot_scs = ot_sum[conditions, ]

#flag days with deep snow (>= 15 cm)
ot_scs$deepsno = ifelse(ot_scs$snofill >= 15, 1, 0)

#calculate the range of daily air and soil temperatures
#and number of days that snowpack was deep

airt = ot_scs %>%
  group_by(Cover) %>%
  reframe(airt = quantile(AirT.avg, na.rm = T))

soilt = ot_scs %>%
  group_by(Cover) %>%
  reframe(soilt = quantile(TSoil_5.avg, na.rm = T))

airtmn = ot_scs %>%
  group_by(ID) %>%
  reframe(airtmn = median(AirT.avg, na.rm = T))

soiltmn = ot_scs %>%
  group_by(ID) %>%
  reframe(soiltmn = median(TSoil_5.max, na.rm = T))

daysdsno = ot_scs %>%
  group_by(ID) %>%
  reframe(daysdsno = sum(deepsno, na.rm = T)) 

#calculate the difference between min and max daily temperature for plotting
ot_scs$AirT.range = ot_scs$AirT.max - ot_scs$AirT.min
ot_scs$TSoil_5.range = ot_scs$TSoil_5.max - ot_scs$TSoil_5.min

######################################################################
#make a violin plot showing the distribution of air and soil temperatures
#and the distribution in diel variation in air and soil temperatures
#in the HW and SW sites during winter. This is Fig. S5 in the 
#Pastore, Nelson, et al. Ecosphere manuscript
######################################################################

winat = ggplot(data = ot_scs, aes(x = c2, y= AirT.avg, fill = c2))+
  geom_violin(alpha = 0.7)+
  geom_hline(yintercept = 0, linetype="dashed", color = "gray50")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Mixed", "Coniferous"))+
  scale_fill_manual(values = c("#a2bede", "#92c2a5"))+
  labs(x = "Forest Type", y = expression("Daily Average Air Temp ("*degree~C*")"), 
       fill = "Canopy Density")

winst = ggplot(data = ot_scs, aes(x = c2, y= TSoil_5.avg, fill = c2))+
  geom_violin(alpha = 0.7)+
  geom_hline(yintercept = 0, linetype="dashed", color = "gray50")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Mixed", "Coniferous"))+
  scale_fill_manual(values = c("#a2bede", "#92c2a5"), 
                    labels = c("Medium", "High"))+
  labs(x = "Forest Type", y = expression("Daily Average Soil Temp ("*degree~C*")"), 
       fill = "Canopy Density")

winatr = ggplot(data = ot_scs, aes(x = c2, y= AirT.range, fill = c2))+
  geom_violin(alpha = 0.7)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Mixed", "Coniferous"))+
  scale_fill_manual(values = c("#a2bede", "#92c2a5"))+
  labs(x = "Forest Type", y = expression("Daily Air Temp Range ("*degree~C*")"), 
       fill = "Canopy Density")

winstr = ggplot(data = ot_scs, aes(x = c2, y= TSoil_5.range, fill = c2))+
  geom_violin(alpha = 0.7)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(legend.position = c(0.5, 0.8))+
  theme(legend.text = element_text(size = 8),
        legend.title = element_text(size = 10))+
  guides(color = guide_legend(override.aes = list(size = 6)))+
  scale_x_discrete(labels = c("Mixed", "Coniferous"))+
  scale_fill_manual(values = c("#a2bede", "#92c2a5"), 
                    labels = c("Medium", "High"))+
  labs(x = "Forest Type", y = expression("Daily Soil Temp Range ("*degree~C*")"), 
       fill = "Canopy Density")

  
#arrange in a grid and export

#call pdf options
pdf.options(width= 6.5, height= 6.5, paper="letter", pointsize=10)

#name pdf export file
pdf(file="OT_Temp_Ranges.pdf")

ggarrange(winat, winst, winatr, winstr,#  +rremove("y.text")+ rremove("ylab"), 
          labels = c("a", "b", "c", "d"), #label.x = c(0.15, 0.075), hjust = c(0.1, 0.5),
          #vjust = 2,
          ncol = 2, nrow = 2)

dev.off()

################################################################
#plot soil temperature as a function of air temperature
#to further illustrate how differences in snow depth between
#the mixed and coniferous forests affected the ground 
#thermal regime. This is Fig. S6 in the Pastore, Nelson, et al.
#Ecosphere paper
################################################################

#divide snow depth into six bins to manually control color ramp in legend
ot_scs$SnowDepthBin <- cut(
    ot_scs$SnowDepth.avg,
    breaks = 6,  # 6 bins
    labels = c(0, 10, 20, 30, 40, 50),
  include.lowest = TRUE)

#remove NA values
ot_scs$SnowDepthBin <- droplevels(ot_scs$SnowDepthBin)

#plot soil temperature as a function of air temperature
winatst = ggplot(data = ot_scs, aes(x = AirT.avg, y= TSoil_5.avg, col = SnowDepthBin))+
  geom_point(size = 2, alpha = 1)+
  geom_hline(yintercept = 0, linetype="dashed", color = "gray50")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5, nrow = 1))+
  theme(legend.position = "bottom", legend.text = element_text(size = 10),
          legend.title = element_text(size = 10),
        legend.direction = "horizontal")+
  labs(color = "Snow Depth (cm)")+
  #scale_x_discrete(labels = c("Mixed", "Coniferous"))+
  scale_color_viridis_d(option = "D", na.translate = FALSE) +
  labs(x = expression("Daily Average Air Temp ("*degree~C*")"), 
       y = expression("Daily Average Soil Temp ("*degree~C*")"))+
facet_wrap(vars(c2), labeller = as_labeller(c("A" = "Medium Canopy Cover",
                                              "B" = "High Canopy Cover")))

#write winatst to folder
#call pdf options
pdf.options(width= 6.5, height= 4.5, paper="letter", pointsize=10)

pdf("OT_Soil_Air_Temp.pdf")

winatst

dev.off()


