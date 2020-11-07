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



#### Extract preprocessed data
######################################

gcm<-read_excel("./DataMosicAllData.xlsx", sheet = "GCM")
activity <- read_excel("./DataMosicAllData.xlsx", sheet = "Activity")
diet <- read_excel("./DataMosicAllData.xlsx", sheet = "Diet")
injections <- read_excel("./DataMosicAllData.xlsx", sheet = "Injections")
weather <- read_excel("./DataMosicAllData.xlsx", sheet = "Weather")

#### Add time stamp as feature
######################################

add_timestamp <- function(dt){
  dt$Date <- format(as.POSIXct(dt$Date,format='%Y-%m-%d %H:%M:%S'),format='%Y-%m-%d')
  dt$Time <- format(as.POSIXct(dt$Time,format='%Y-%m-%d %H:%M:%S'),format='%H:%M')
  dt$timestamp <- format(as.POSIXct(paste(dt$Date, dt$Time)), "%Y-%m-%d %H:%M:%S")
  return(dt)
}

weather <- add_timestamp(weather)
injections <- add_timestamp(injections)
diet <- add_timestamp(diet)
activity <- add_timestamp(activity)

gcm$timestamp <- as.POSIXct(as.character(gcm$Timestamp),format='%Y-%m-%d %H:%M:%S')


### Data formation
#############################

tm <- unique(c(as.character(gcm$timestamp), as.character(weather$timestamp),
               as.character(injections$timestamp), as.character(diet$timestamp)))
data_set <- data.frame(timestamp= tm, row.names = tm, GlucoseLevel = NA, injections_Units =NA,  Weather = NA,
                       CaloriesProt = NA, CaloriesCarbs = NA, CaloriesFat =NA,CaloriesTot= NA,
                       CaloriesBurnt=NA, Steps=NA, Distance=NA,Floors=NA,MinutesSitting=NA,MinutesLightActivity=NA,
                       MinutesRelativeHighActivity=NA,MinutesVeryHighActivity=NA,ActivityCalories=NA)

data_set$GlucoseLevel[match(as.character(gcm$Timestamp), tm)] <- gcm$GlucoseLevel
data_set$Weather[match(as.character(weather$timestamp), tm)] <- weather$Weather
data_set$injections_Units[match(as.character(injections$timestamp), tm)] <- injections$Units
#data_set$CaloriesProt[match(as.character(diet$timestamp), tm)] <- diet$CaloriesProt
data_set[match(as.character(diet$timestamp), tm), c("CaloriesProt","CaloriesCarbs","CaloriesFat","CaloriesTot")] <-
  diet[,c("CaloriesProt","CaloriesCarbs","CaloriesFat","CaloriesTot")]
data_set[match(as.character(activity$timestamp), tm),c("CaloriesBurnt","Steps","Distance","Floors",
                                                   "MinutesSitting","MinutesLightActivity",
                                                   "MinutesRelativeHighActivity","MinutesVeryHighActivity",
                                                   "ActivityCalories")] <-
  activity[c("CaloriesBurnt","Steps","Distance","Floors","MinutesSitting","MinutesLightActivity",
         "MinutesRelativeHighActivity","MinutesVeryHighActivity","ActivityCalories")]


### Data extrapolation
#############################

extrapolat_data <- function(vect){
  new_vect <- vect
  current_value <- vect[which(!is.na(vect))[1]]
  for(i in 1:length(vect)){
    if(!is.na(vect[i])){current_value <- vect[i]}
    new_vect[i] <- current_value
  }
  return(new_vect)
}

data_set$timestamp <- NULL
colnames(data_set)
for(i in colnames(data_set)){
  data_set[,i] <- extrapolat_data(data_set[,i])
}
data_set <- apply(data_set, 2, extrapolat_data)

write.csv(data_set, "./naive_extrapolated_data.csv")

### GLM modeling
#####################

glm_model <-glm(GlucoseLevel ~ .,data = data_set, family = gaussian)

### manually prepared text data
#data_set <- data_set[800:nrow(data_set),]
#write.csv(data_set[nrow(data_set),-1], "template_question_data.csv")


### prediction
#####################

question_data <- read.csv("question_data.csv", header = T, row.names = 1)
predict <- predict(glm_model, question_data, type = 'response')
write.csv(data.frame(GlucoseLevel = predict), "./predicted_data.csv")
