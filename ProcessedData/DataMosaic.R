library(readxl)
library(tidyverse)
library(ggplot2)


#Update path to where files are located
setwd("C:/Users/popovm2/Downloads/")
gcm<-read_excel("gcm_data_72h_prepared.xlsx")
sleepData<-read.csv("./../rawData/Sleep-2016-08-05--19.27-05.52-vitals.csv")
sleepData$timestamp<-as.POSIXct(sleepData$timestamp, origin='1970-01-01')

activity<- read_excel("fitbit_export_20160829_phase1_prepared.xlsx", sheet = "Activity")
sleepFitbit<- read_excel("fitbit_export_20160829_phase1_prepared.xlsx", sheet = "Sleep")
diet<- read_excel("fitbit_export_20160829_phase1_prepared.xlsx", sheet = "Diet")

gp<-ggplot() + geom_line(data=gcm, aes(x=Timestamp, y=GlucoseLevel), color="blue")
gp

gp1<-ggplot()+geom_line(data=sleepData, aes(x=timestamp,y=hr), color="red")
gp1<-gp1+geom_line(data=sleepData, aes(x=timestamp,y=rr), color="blue")
gp1<-gp1+geom_line(data=sleepData, aes(x=timestamp,y=act), color="green")
gp1
