#LIBRARIES
####################################################################################################
####################################################################################################
library(dplyr)
library(lubridate) #time and dates
library(ggpubr)
library(cowplot) #combine plots
library(praznik)
library(psych)
library(FactoMineR) #MCA
library(factoextra) #MCA
library(caret) #training and testing
library(randomForest)
library(tidyverse)    # data manipulation and visualization
library(kernlab)      # SVM methodology
library(e1071)        # SVM methodology
library(fastDummies)
library(dummies) #knn
library(class) #knn
library(psych)
library(naniar)
library(rpart)
library(rpart.plot)
require(caTools)
library(ipred) 
library(rattle)
library(DMwR) #smote
library(unbalanced) #unbalanced data
library(vcd) #correlation categorical variables
library(gmodels)
library(descr)
library(GoodmanKruskal)
library(greybox)
library(lsr)
library(DataExplorer)
library(mlbench)     # for PimaIndiansDiabetes2 dataset
library(dplyr)       # for data manipulation (dplyr) 
library(broom)       # for making model summary tidy
library(visreg)      # for potting logodds and probability 
library(margins)     # to calculate Average Marginal Effects
library(rcompanion)  # to calculate pseudo R2
library(ROCR)        # to compute and plot Reciever Opering Curve
library(FSelector)   # feature selection using chi squared
library(corrplot)    # correlation plot
library(Boruta)      # feature selection
library(caret)
library(rpart)
library(RRF)
library(glmnet)      # LASSO
library(DMwR)        # SMOTE
library(car)
library(ISLR)
library(caret)

#IMPORT UK ROAD ACCIDENTS DATA SET
####################################################################################################
####################################################################################################
#Import raw dataset 
accidents_2019 <- read.csv("Road Safety Data - Accidents 2019.csv")


#INITIAL EXPLORATORY ANALYSIS
####################################################################################################
####################################################################################################
#check dimensiones of data sets
dim(accidents_2019)
#check dataset structure
str(accidents_2019)
#check dataset structure - plot
DataExplorer::plot_str(accidents_2019)
#summary
summary(accidents_2019)
#metrics about variables
DataExplorer::plot_intro(accidents_2019)


#reducing the number of variables, as some variables are not relevant
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
accidents <- accidents_2019[-c(2,3,4,5,6,8,9,13,14,16,21,22,31,32)]
#checking structure of new dataset
str(accidents)
DataExplorer::plot_str(accidents)
#check missing data only from selected variables
DataExplorer::plot_intro(accidents)
accidents[!complete.cases(accidents), ] #no missing rows in selected variablles



#INITIAL DATA TRANSFORMATION
####################################################################################################
####################################################################################################
#ACCIDENT SEVERITY
#check how many records are from each of the categories of the targeted variable "accident severity"
accidents %>% 
  group_by(Accident_Severity)%>%
  summarize(count = n()) #the data set is clearly unbalanced
#changing severity from 1, 2 and 3 to 1 and 2, in order to have only two classes
#this way the number of records in each of the classes is more balanced
accidents$Accident_Severity[accidents$Accident_Severity == 1] <- 1
accidents$Accident_Severity[accidents$Accident_Severity == 2] <- 1
accidents$Accident_Severity[accidents$Accident_Severity == 3] <- 0
#check again
accidents %>% 
  group_by(Accident_Severity)%>%
  summarize(count = n()) #the classes are still unbalanced
#changing to factor
accidents$Accident_Severity <- factor(accidents$Accident_Severity, levels = c(0,1))
#renaming the variable
accidents <- accidents %>% dplyr::rename("accidentseverity" = "Accident_Severity")



#TIME OF THE DAY
#create new variable hour from the existing Time variable
accidents$hour <- as.numeric(gsub("\\:.*$", "", accidents$Time))
#change the time into morning, afternoon, evening and noon
accidents <- accidents %>%
  add_column(timeday = ifelse (accidents$hour >= 6 & accidents$hour <= 12,"Morning",
                               ifelse (accidents$hour >12 & accidents$hour <=17,"Afternoon",
                                       ifelse (accidents$hour >18 & accidents$hour <24,"Evening", "Noon"))))
#change class and add levels
accidents$timeday <- factor(accidents$timeday, levels = c("Morning", "Afternoon", "Evening", "Noon"))
#count number of n/a
sum(is.na(accidents$timeday)) #64 na
#remove na, which are only 64 values
accidents <- subset(accidents,!is.na(accidents$timeday))


#MONTH
#transform date as month
accidents$Date <- as.Date(accidents$Date, "%d/%m/%Y")
accidents$month <- months(accidents$Date)
#change class and add levels
accidents$month <- factor(accidents$month, levels = c("January", "February","March","April","May","June","July",
                                                      "August","September","October","November","December"))
#check if there are any nas
sum(is.na(accidents$month))# there is no nas



#FIRST ROAD CLASS
#note: the 2nd road contains different information than the one mentioned in the uk goc website, maybe is a mistake in the data set. That;s why was not included
#change 1st road class
accidents <- accidents %>%
  add_column(road1class = ifelse (accidents$X1st_Road_Class == 1 | accidents$X1st_Road_Class == 2,"B",
                                  ifelse (accidents$X1st_Road_Class == 3 |  accidents$X1st_Road_Class == "4","M",
                                          ifelse (accidents$X1st_Road_Class == 5 ,"S","U"))))
#change to factor and add levels
accidents$road1class <- factor(accidents$road1class, levels = c("B","M","S","U"))
#check to see if there are any nas
sum(is.na(accidents$road1class))



#DAY OF THE WEEK
#change to factor and add levels
accidents$weekday <- factor(accidents$Day_of_Week, levels = c(1,2,3,4,5,6,7))
#check to see if there are any nas
sum(is.na(accidents$weekday))



#ROAD TYPE
#reduce the number of categories and rename them
accidents <- accidents %>%
  add_column(roadtype = ifelse (accidents$Road_Type == 1, "roundabout",
                                ifelse(accidents$Road_Type == 2, "onewaystreet",
                                       ifelse(accidents$Road_Type == 3,"dualcarriage",
                                              ifelse(accidents$Road_Type == 6, "singlecarriage",     
                                                     ifelse (accidents$Road_Type == 9 |  accidents$Road_Type == -1,"unknown","sliproad"))))))
#change class to factors                               
accidents$roadtype <- factor(accidents$roadtype)
#check to see if there are any nas
sum(is.na(accidents$weekday))


#SPEED LIMIT
#change to factor and add the levels
accidents$speedlimit <- factor(accidents$Speed_limit, levels = c(20,30,40,50,60,70,-1))



#JUNCTION DETAIL
#reduce the number of categories and rename them
accidents <- accidents %>%
  add_column(junctiondetail = ifelse (accidents$Junction_Detail == 0, "notjunction",
                                      ifelse(accidents$Junction_Detail == 1 ,"roundabout",
                                             ifelse(accidents$Junction_Detail == 2,"miniroundabout",
                                                    ifelse (accidents$Junction_Detail == 3,"torstaggeredjunction",
                                                            ifelse (accidents$Junction_Detail == 5,"sliproad",
                                                                    ifelse (accidents$Junction_Detail == 6,"crossroads",
                                                                            ifelse (accidents$Junction_Detail == 7, "morethan4arms_notroundabout",
                                                                                    ifelse(accidents$Junction_Detail == 8,"privatedrive",
                                                                                           ifelse(accidents$Junction_Detail == 9,"otherjunction","unknown"))))))))))
#changed to factor
accidents$junctiondetail <- factor(accidents$junctiondetail)

#JUNCTION CONTROL
#reduce the number of categories and rename them
accidents <- accidents %>%
  add_column(junctioncontrol = ifelse (accidents$Junction_Control == 0, "notjunction",
                                       ifelse(accidents$Junction_Control == 1 ,"authorisedperson",
                                              ifelse(accidents$Junction_Control == 2,"autotrafficsignal",
                                                     ifelse (accidents$Junction_Control == 3,"stopsign",
                                                             ifelse (accidents$Junction_Control == 4,"giveawayoruncontrolled","unknown"))))))
#changed to factor
accidents$junctioncontrol <- factor(accidents$junctioncontrol)


#PEDESTRIAN CROSSING HUMAN CONTROL
#reduce the number of categories and rename them
accidents <- accidents %>%
  add_column(pedestrian = ifelse (accidents$Pedestrian_Crossing.Human_Control == 0, "none50m",
                                  ifelse(accidents$Pedestrian_Crossing.Human_Control == 1 ,"schoolpatrol",
                                         ifelse(accidents$Pedestrian_Crossing.Human_Control == 2,"authorisedperson","unknown"))))
#changed to factor
accidents$pedestrian <- factor(accidents$pedestrian)


#PEDRESTIAN CROSSING PHYSICAL FACILITIES
#reduce the number of categories and rename them
accidents <- accidents %>%
  add_column(crossing = ifelse (accidents$Pedestrian_Crossing.Physical_Facilities == 0, "notcrossing50m",
                                ifelse(accidents$Pedestrian_Crossing.Physical_Facilities == 1 ,"zebra",
                                       ifelse(accidents$Pedestrian_Crossing.Physical_Facilities == 4,"lightcrossing",
                                              ifelse (accidents$Pedestrian_Crossing.Physical_Facilities == 5,"trafficjunction",
                                                      ifelse (accidents$Pedestrian_Crossing.Physical_Facilities == 7,"subway",
                                                              ifelse (accidents$Pedestrian_Crossing.Physical_Facilities == 8,"centralrefuge","unknown")))))))
#changed to factor
accidents$crossing <- factor(accidents$crossing)



#LIGHT CONDITIONS
#reduce the number of categories and rename them
accidents <- accidents %>%
  add_column(light = ifelse (accidents$Light_Conditions == 1, "day",
                             ifelse(accidents$Light_Conditions == 4 ,"darklightlit",
                                    ifelse(accidents$Light_Conditions == 5,"darklightunits",
                                           ifelse (accidents$Light_Conditions == 6,"darknolight",
                                                   ifelse (accidents$Light_Conditions == 7,"darklightunknown","unknown"))))))
#changed to factor
accidents$light <- factor(accidents$light)



#WEATHER CONDITIONS
#reduce the number of categories and rename them
accidents <- accidents %>%
  add_column(weather = ifelse (accidents$Weather_Conditions == 1, "ok",
                               ifelse (accidents$Weather_Conditions == 2 ,"rain",
                                       ifelse (accidents$Weather_Conditions == 3 ,"snow",
                                               ifelse (accidents$Weather_Conditions == 4 ,"wind",
                                                       ifelse (accidents$Weather_Conditions == 5 ,"rainwind",
                                                               ifelse (accidents$Weather_Conditions == 6 ,"snowwind",
                                                                       ifelse (accidents$Weather_Conditions == 7 ,"frostmist",
                                                                               ifelse (accidents$Weather_Conditions == 8 ,"other","unknown")))))))))

#changed to factor
accidents$weather <- factor(accidents$weather)



#ROADSURFACE CONDITIONS
#reduce the number of categories and rename them
accidents <- accidents %>%
  add_column(road = ifelse (accidents$Road_Surface_Conditions == 1, "dry",
                            ifelse (accidents$Road_Surface_Conditions == 2 ,"wet",
                                    ifelse (accidents$Road_Surface_Conditions == 3 ,"snow",
                                            ifelse (accidents$Road_Surface_Conditions == 4 ,"ice",
                                                    ifelse (accidents$Road_Surface_Conditions == 5 ,"flood",
                                                            ifelse (accidents$Road_Surface_Conditions == 6 ,"oil",
                                                                    ifelse (accidents$Road_Surface_Conditions == 7 ,"muld","unknown"))))))))

#changed to factor
accidents$road <- factor(accidents$road)



#SPECIAL CONDITIONS AT SITE
#reduce the number of categories and rename them
accidents <- accidents %>%
  add_column(special = ifelse (accidents$Special_Conditions_at_Site == 0, "none",
                               ifelse (accidents$Special_Conditions_at_Site == 1,"signalout",
                                       ifelse (accidents$Special_Conditions_at_Site == 2 | accidents$Special_Conditions_at_Site == 3,"defectivesignal",
                                               ifelse (accidents$Special_Conditions_at_Site == 4 ,"roadworks",
                                                       ifelse (accidents$Special_Conditions_at_Site == 5 ,"defectiveroad",
                                                               ifelse (accidents$Special_Conditions_at_Site == 6 ,"oil",
                                                                       ifelse (accidents$Special_Conditions_at_Site == 7 ,"muld","unknown"))))))))

#changed to factor
accidents$special <- factor(accidents$special)




#CARRIAGE HAZARDS
#reduce the number of categories and rename them
accidents <- accidents %>%
  add_column(carriagehazards = ifelse (accidents$Carriageway_Hazards == 0, "none",
                                       ifelse (accidents$Carriageway_Hazards == 1,"vehicleloan",
                                               ifelse (accidents$Carriageway_Hazards == 2 ,"object",
                                                       ifelse (accidents$Carriageway_Hazards == 3 ,"previousaccident",
                                                               ifelse (accidents$Carriageway_Hazards == 4 ,"dog",
                                                                       ifelse (accidents$Carriageway_Hazards == 5 | accidents$Carriageway_Hazards == 7 ,"otheranimal",
                                                                               ifelse (accidents$Carriageway_Hazards == 6 ,"pedestrian","unknown"))))))))

#changed to factor
accidents$carriagehazards <- factor(accidents$carriagehazards)




#URBAN OR RUAL AREA
#reduce the number of categories and rename them
accidents <- accidents %>%
  add_column(urbanrural = ifelse (accidents$Urban_or_Rural_Area == 1, "urban",
                                  ifelse (accidents$Urban_or_Rural_Area == 2,"rural","unknown")))

#changed to factor
accidents$urbanrural <- factor(accidents$urbanrural)



#DROP THE OLD VARIABLES
#drop date, time, hour
accidents <- accidents[-c(1,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)]
#doblecheck the variables remaining are the needed ones
dim(accidents)
str(accidents)
summary(accidents)



#EXPLORATORY ANALYSIS
####################################################################################################
####################################################################################################

#grouping
dayweek <- accidents %>%
  group_by(accidentseverity, weekday)%>%
  summarize(n = n())
#creating plot
dayweekplot <- ggplot(dayweek, aes(x=weekday, y = n, fill = accidentseverity)) + 
  labs(title = "Accident Severity by Day of the Week", x = "Day of the Week", y = "Count")+
  geom_bar(position="stack", stat="identity", alpha = 0.6) +
  scale_fill_manual("legend", values = c("1" = "#990000", "0" = "#e98417"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size=6, angle=45))


#grouping
time <- accidents %>%
  group_by(accidentseverity, timeday)%>%
  summarize(n = n())
#creating plot
timeplot <- ggplot(time, aes(x=timeday, y = n, fill = accidentseverity)) + 
  labs(title = "Accident Severity by Time of the Day", x = "Time of the Day", y = "Count")+
  geom_bar(position="stack", stat="identity", alpha = 0.6) +
  scale_fill_manual("legend", values = c("1" = "#990000", "0" = "#e98417"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size=6, angle=45))


#grouping
roadclass <- accidents %>%
  group_by(accidentseverity, road1class)%>%
  summarize(n = n())
#creating plot
roadclassplot <- ggplot(roadclass, aes(x=road1class, y = n, fill = accidentseverity)) + 
  labs(title = "Accident Severity by Class of the Road 1", x = "Road 1 Class", y = "Count")+
  geom_bar(position="stack", stat="identity", alpha = 0.6) +
  scale_fill_manual("legend", values = c("1" = "#990000", "0" = "#e98417"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size=6, angle=45))

#grouping
month <- accidents %>%
  group_by(accidentseverity, month)%>%
  summarize(n = n())
#creating plot
monthplot <- ggplot(month, aes(x=month, y = n, fill = accidentseverity)) + 
  labs(title = "Accident Severity by Month", x = "Month", y = "Count")+
  geom_bar(position="stack", stat="identity", alpha = 0.6) +
  scale_fill_manual("legend", values = c("1" = "#990000", "0" = "#e98417"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size=6, angle=45))

#grouping
roadtype <- accidents %>%
  group_by(accidentseverity, roadtype)%>%
  summarize(n = n())
#creating plot
roadtypeplot <- ggplot(roadtype, aes(x=roadtype, y = n, fill = accidentseverity)) + 
  labs(title = "Accident Severity by Road Type", x = "Road Type", y = "Count")+
  geom_bar(position="stack", stat="identity", alpha = 0.6) +
  scale_fill_manual("legend", values = c("1" = "#990000", "0" = "#e98417"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size=6, angle=45))

#grouping
speedlimit <- accidents %>%
  group_by(accidentseverity, speedlimit)%>%
  summarize(n = n())
#creating plot
speedlimitplot <- ggplot(speedlimit, aes(x=speedlimit, y = n, fill = accidentseverity)) + 
  labs(title = "Accident Severity by Speed Limit (MpH)", x = "Speed Limit (MpH)", y = "Count")+
  geom_bar(position="stack", stat="identity", alpha = 0.6) +
  scale_fill_manual("legend", values = c("1" = "#990000", "0" = "#e98417"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size=6, angle=45))

#grouping
junctiondetail <- accidents %>%
  group_by(accidentseverity, junctiondetail)%>%
  summarize(n = n())
#creating plot
junctiondetailplot <- ggplot(junctiondetail, aes(x=junctiondetail, y = n, fill = accidentseverity)) + 
  labs(title = "Accident Severity by Junction Detail", x = "Junction Detail", y = "Count")+
  geom_bar(position="stack", stat="identity", alpha = 0.6) +
  scale_fill_manual("legend", values = c("1" = "#990000", "0" = "#e98417"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size=6, angle=45))


#grouping
junctioncontrol <- accidents %>%
  group_by(accidentseverity, junctioncontrol)%>%
  summarize(n = n())
#creating plot
junctioncontrolplot <- ggplot(junctioncontrol, aes(x=junctioncontrol, y = n, fill = accidentseverity)) + 
  labs(title = "Accident Severity by Junction Control", x = "Junction Control", y = "Count")+
  geom_bar(position="stack", stat="identity", alpha = 0.6) +
  scale_fill_manual("legend", values = c("1" = "#990000", "0" = "#e98417"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size=6, angle=45))

#grouping
pedestrian <- accidents %>%
  group_by(accidentseverity, pedestrian)%>%
  summarize(n = n())
#creating plot
pedestrianplot <- ggplot(pedestrian, aes(x=pedestrian, y = n, fill = accidentseverity)) + 
  labs(title = "Accident Severity by Pedestrian Crossing Human Control", x = "Pedestrian Crossing Human Control", y = "Count")+
  geom_bar(position="stack", stat="identity", alpha = 0.6) +
  scale_fill_manual("legend", values = c("1" = "#990000", "0" = "#e98417"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size=6, angle=45))

#grouping
crossing <- accidents %>%
  group_by(accidentseverity, crossing)%>%
  summarize(n = n())
#creating plot
crossingplot <- ggplot(crossing, aes(x=crossing, y = n, fill = accidentseverity)) + 
  labs(title = "Accident Severity by Pedestrian Crossing Physical Facilities", x = "Pedestrian Crossing Physical Facilities", y = "Count")+
  geom_bar(position="stack", stat="identity", alpha = 0.6) +
  scale_fill_manual("legend", values = c("1" = "#990000", "0" = "#e98417"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size=6, angle=45))

#grouping
light <- accidents %>%
  group_by(accidentseverity, light)%>%
  summarize(n = n())
#creating plot
lightplot <- ggplot(light, aes(x=light, y = n, fill = accidentseverity)) + 
  labs(title = "Accident Severity by Light Conditions", x = "Light Conditions", y = "Count")+
  geom_bar(position="stack", stat="identity", alpha = 0.6) +
  scale_fill_manual("legend", values = c("1" = "#990000", "0" = "#e98417"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size=6, angle=45))

#grouping
weather <- accidents %>%
  group_by(accidentseverity, weather)%>%
  summarize(n = n())
#creating plot
weatherplot <- ggplot(weather, aes(x=weather, y = n, fill = accidentseverity)) + 
  labs(title = "Accident Severity by Weather Conditions", x = "Weather Conditions", y = "Count")+
  geom_bar(position="stack", stat="identity", alpha = 0.6) +
  scale_fill_manual("legend", values = c("1" = "#990000", "0" = "#e98417"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size=6, angle=45))

#grouping
road <- accidents %>%
  group_by(accidentseverity, road)%>%
  summarize(n = n())
#creating plot
roadplot <- ggplot(road, aes(x=road, y = n, fill = accidentseverity)) + 
  labs(title = "Accident Severity by Road Surface Conditions", x = "Road Surface Conditions", y = "Count")+
  geom_bar(position="stack", stat="identity", alpha = 0.6) +
  scale_fill_manual("legend", values = c("1" = "#990000", "0" = "#e98417"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size=6, angle=45))

#grouping
special <- accidents %>%
  group_by(accidentseverity, special)%>%
  summarize(n = n())
#creating plot
specialplot <- ggplot(special, aes(x=special, y = n, fill = accidentseverity)) + 
  labs(title = "Accident Severity by Special Conditions at Site", x = "Special Conditions at Site", y = "Count")+
  geom_bar(position="stack", stat="identity", alpha = 0.6) +
  scale_fill_manual("legend", values = c("1" = "#990000", "0" = "#e98417"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size=6, angle=45))

#grouping
urbanrural <- accidents %>%
  group_by(accidentseverity, urbanrural)%>%
  summarize(n = n())
#creating plot
urbanruralplot <- ggplot(urbanrural, aes(x=urbanrural, y = n, fill = accidentseverity)) + 
  labs(title = "Accident Severity by Urban - Rural", x = "Urban - Rural", y = "Count")+
  geom_bar(position="stack", stat="identity", alpha = 0.6) +
  scale_fill_manual("legend", values = c("1" = "#990000", "0" = "#e98417"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size=6, angle=45))

#grouping
carriagehazards <- accidents %>%
  group_by(accidentseverity, carriagehazards)%>%
  summarize(n = n())
#creating plot
carriagehazardsplot <- ggplot(carriagehazards, aes(x=carriagehazards, y = n, fill = accidentseverity)) + 
  labs(title = "Accident Severity by Carriageway Hazards", x = "Carriageway Hazards", y = "Count")+
  geom_bar(position="stack", stat="identity", alpha = 0.6) +
  scale_fill_manual("legend", values = c("1" = "#990000", "0" = "#e98417"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size=6, angle=45))



#plotting all the previous plotts
plot_grid(dayweekplot, timeplot, roadclassplot,
          monthplot,nrow = 2)

plot_grid(roadtypeplot,speedlimitplot,junctiondetailplot,
          junctioncontrolplot,nrow = 2)

plot_grid(pedestrianplot,crossingplot, roadplot,
          specialplot,nrow = 2)

plot_grid(urbanruralplot, nrow = 2, ncol =2)




#STRATIFIED SAMPLING
####################################################################################################
####################################################################################################
#the following code was extracted from https://towardsdatascience.com/stratified-sampling-and-how-to-perform-it-in-r-8b753efde1ef
library(sqldf)

n_sample <- 30000

dimensions = names(accidents)

generated = head(accidents,0)
while (nrow(generated) < n_sample) {
  # For debug purposes
  cat(nrow(generated),"\n")
  flush.console()
  
  tmp = accidents
  
  # Calculate the histogram for each dimension
  # and select one value at a time, slicing the
  # original dataset according to its histogram
  for (i in 1:length(dimensions)) {
    
    colname = dimensions[i]
    
    # Categorical variable
    
    # Histogram for the selected dimension
    aggr = as.data.frame(table(tmp[,colname],useNA="ifany"))
    names(aggr) = c("dim","count")
    
    # Generate a value according to the histogram
    generated_value = sample(aggr$dim,prob=aggr$count,1)
    
    # Slice the actual multivariate histogram in order to
    # take only records with the selected value on the
    # selected dimension
    if (!is.na(generated_value)) {
      tmp = tmp[tmp[[colname]] == generated_value & !is.na(tmp[[colname]]),]
    }
    else {
      tmp = tmp[is.na(tmp[[colname]]),]
    }
  }
  
  # Once the procedure finishes, we get a bulk of records
  # with the same values of each dimension. Let's take
  # one of these records uniformly
  random_index = sample(1:nrow(tmp),1)
  new_record = tmp[random_index,]
  
  # Let's remove duplicates
  inserted_record = sqldf("select * from new_record except select * from generated")
  
  # Insert in the "generated" data frame and repeat until desired sample size is reached
  generated = rbind(generated,inserted_record)
}


generated %>% 
  group_by(accidentseverity)%>%
  summarize(count = n())



#This code has been used to upload the stratified sample
write.csv(generated, "generated.csv")
#import file with reduced number of rows by using the stratified sampling
#this code was created to avoid running the previous script each time when wokring on the next steps
accidents_reduced <- read.csv("generated.csv", stringsAsFactors = TRUE)
#removing first column with row id
accidents_reduced <- accidents_reduced[-c(1)]
#structure
str(accidents_reduced) #the columns are not factorts anymore, needs to be converted again
#summary
summary(accidents_reduced)





#TRANSFORMATION TO FACTORS AGAIN 
####################################################################################################
####################################################################################################
#after importing the generated.csv file, the data needs to be converted into factors again
#this step is not needed if the data is not imported, and instead the code is run all at once

accidents_reduced$accidentseverity <- factor(accidents_reduced$accidentseverity, levels = c(0,1))

accidents_reduced$speedlimit <- gsub('-1', 'other', accidents_reduced$speedlimit)
accidents_reduced$speedlimit <- factor(accidents_reduced$speedlimit, levels = c(20,30,40,50,60,70,"other"))

accidents_reduced$weekday <- factor(accidents_reduced$weekday, levels = c(1,2,3,4,5,6,7))

#checking the resulting variables
str(accidents_reduced)
summary(accidents_reduced)




#EXPLORATORY ANALYSIS
#The exploratory analysis is done once more to ensure that the sample has the same distribution as the raw dataset
####################################################################################################
####################################################################################################


dayweekr <- accidents_reduced %>%
  group_by(accidentseverity, weekday)%>%
  summarize(n = n())



dayweekplotr <- ggplot(dayweekr, aes(x=weekday, y = n, fill = accidentseverity)) + 
  labs(title = "Accident Severity by Day of the Week", x = "Day of the Week", y = "Count")+
  geom_bar(position="stack", stat="identity", alpha = 0.6) +
  scale_fill_manual("legend", values = c("1" = "#990000", "0" = "#e98417"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size=6, angle=45))




timer <- accidents_reduced %>%
  group_by(accidentseverity, timeday)%>%
  summarize(n = n())


timeplotr <- ggplot(timer, aes(x=timeday, y = n, fill = accidentseverity)) + 
  labs(title = "Accident Severity by Time of the Day", x = "Time of the Day", y = "Count")+
  geom_bar(position="stack", stat="identity", alpha = 0.6) +
  scale_fill_manual("legend", values = c("1" = "#990000", "0" = "#e98417"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size=6, angle=45))



roadclassr <- accidents_reduced %>%
  group_by(accidentseverity, road1class)%>%
  summarize(n = n())


roadclassplotr <- ggplot(roadclassr, aes(x=road1class, y = n, fill = accidentseverity)) + 
  labs(title = "Accident Severity by Class of the Road 1", x = "Road 1 Class", y = "Count")+
  geom_bar(position="stack", stat="identity", alpha = 0.6) +
  scale_fill_manual("legend", values = c("1" = "#990000", "0" = "#e98417"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size=6, angle=45))




monthr <- accidents_reduced %>%
  group_by(accidentseverity, month)%>%
  summarize(n = n())


monthplotr <- ggplot(monthr, aes(x=month, y = n, fill = accidentseverity)) + 
  labs(title = "Accident Severity by Month", x = "Month", y = "Count")+
  geom_bar(position="stack", stat="identity", alpha = 0.6) +
  scale_fill_manual("legend", values = c("1" = "#990000", "0" = "#e98417"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size=6, angle=45))




roadtyper <- accidents_reduced %>%
  group_by(accidentseverity, roadtype)%>%
  summarize(n = n())


roadtypeplotr <- ggplot(roadtyper, aes(x=roadtype, y = n, fill = accidentseverity)) + 
  labs(title = "Accident Severity by Road Type", x = "Road Type", y = "Count")+
  geom_bar(position="stack", stat="identity", alpha = 0.6) +
  scale_fill_manual("legend", values = c("1" = "#990000", "0" = "#e98417"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size=6, angle=45))




speedlimitr <- accidents_reduced %>%
  group_by(accidentseverity, speedlimit)%>%
  summarize(n = n())


speedlimitplotr <- ggplot(speedlimitr, aes(x=speedlimit, y = n, fill = accidentseverity)) + 
  labs(title = "Accident Severity by Speed Limit (MpH)", x = "Speed Limit (MpH)", y = "Count")+
  geom_bar(position="stack", stat="identity", alpha = 0.6) +
  scale_fill_manual("legend", values = c("1" = "#990000", "0" = "#e98417"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size=6, angle=45))



junctiondetailr <- accidents_reduced %>%
  group_by(accidentseverity, junctiondetail)%>%
  summarize(n = n())


junctiondetailplotr <- ggplot(junctiondetailr, aes(x=junctiondetail, y = n, fill = accidentseverity)) + 
  labs(title = "Accident Severity by Junction Detail", x = "Junction Detail", y = "Count")+
  geom_bar(position="stack", stat="identity", alpha = 0.6) +
  scale_fill_manual("legend", values = c("1" = "#990000", "0" = "#e98417"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size=6, angle=45))



junctioncontrolr <- accidents_reduced %>%
  group_by(accidentseverity, junctioncontrol)%>%
  summarize(n = n())


junctioncontrolplotr <- ggplot(junctioncontrolr, aes(x=junctioncontrol, y = n, fill = accidentseverity)) + 
  labs(title = "Accident Severity by Junction Control", x = "Junction Control", y = "Count")+
  geom_bar(position="stack", stat="identity", alpha = 0.6) +
  scale_fill_manual("legend", values = c("1" = "#990000", "0" = "#e98417"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size=6, angle=45))



pedestrianr <- accidents_reduced %>%
  group_by(accidentseverity, pedestrian)%>%
  summarize(n = n())


pedestrianplotr <- ggplot(pedestrianr, aes(x=pedestrian, y = n, fill = accidentseverity)) + 
  labs(title = "Accident Severity by Pedestrian Crossing Human Control", x = "Pedestrian Crossing Human Control", y = "Count")+
  geom_bar(position="stack", stat="identity", alpha = 0.6) +
  scale_fill_manual("legend", values = c("1" = "#990000", "0" = "#e98417"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size=6, angle=45))



crossingr <- accidents_reduced %>%
  group_by(accidentseverity, crossing)%>%
  summarize(n = n())


crossingplotr <- ggplot(crossingr, aes(x=crossing, y = n, fill = accidentseverity)) + 
  labs(title = "Accident Severity by Pedestrian Crossing Physical Facilities", x = "Pedestrian Crossing Physical Facilities", y = "Count")+
  geom_bar(position="stack", stat="identity", alpha = 0.6) +
  scale_fill_manual("legend", values = c("1" = "#990000", "0" = "#e98417"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size=6, angle=45))




lightr <- accidents_reduced %>%
  group_by(accidentseverity, light)%>%
  summarize(n = n())


lightplotr <- ggplot(lightr, aes(x=light, y = n, fill = accidentseverity)) + 
  labs(title = "Accident Severity by Light Conditions", x = "Light Conditions", y = "Count")+
  geom_bar(position="stack", stat="identity", alpha = 0.6) +
  scale_fill_manual("legend", values = c("1" = "#990000", "0" = "#e98417"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size=6, angle=45))




weatherr <- accidents_reduced %>%
  group_by(accidentseverity, weather)%>%
  summarize(n = n())


weatherplotr <- ggplot(weatherr, aes(x=weather, y = n, fill = accidentseverity)) + 
  labs(title = "Accident Severity by Weather Conditions", x = "Weather Conditions", y = "Count")+
  geom_bar(position="stack", stat="identity", alpha = 0.6) +
  scale_fill_manual("legend", values = c("1" = "#990000", "0" = "#e98417"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size=6, angle=45))



roadr <- accidents_reduced %>%
  group_by(accidentseverity, road)%>%
  summarize(n = n())


roadplotr <- ggplot(roadr, aes(x=road, y = n, fill = accidentseverity)) + 
  labs(title = "Accident Severity by Road Surface Conditions", x = "Road Surface Conditions", y = "Count")+
  geom_bar(position="stack", stat="identity", alpha = 0.6) +
  scale_fill_manual("legend", values = c("1" = "#990000", "0" = "#e98417"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size=6, angle=45))



specialr <- accidents_reduced %>%
  group_by(accidentseverity, special)%>%
  summarize(n = n())


specialplotr <- ggplot(specialr, aes(x=special, y = n, fill = accidentseverity)) + 
  labs(title = "Accident Severity by Special Conditions at Site", x = "Special Conditions at Site", y = "Count")+
  geom_bar(position="stack", stat="identity", alpha = 0.6) +
  scale_fill_manual("legend", values = c("1" = "#990000", "0" = "#e98417"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size=6, angle=45))



urbanruralr <- accidents_reduced %>%
  group_by(accidentseverity, urbanrural)%>%
  summarize(n = n())


urbanruralplotr <- ggplot(urbanruralr, aes(x=urbanrural, y = n, fill = accidentseverity)) + 
  labs(title = "Accident Severity by Urban - Rural", x = "Urban - Rural", y = "Count")+
  geom_bar(position="stack", stat="identity", alpha = 0.6) +
  scale_fill_manual("legend", values = c("1" = "#990000", "0" = "#e98417"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size=6, angle=45))


carriagehazardsr <- accidents_reduced %>%
  group_by(accidentseverity, carriagehazards)%>%
  summarize(n = n())


carriagehazardsplotr <- ggplot(carriagehazardsr, aes(x=carriagehazards, y = n, fill = accidentseverity)) + 
  labs(title = "Accident Severity by Carriageway Hazards", x = "Carriageway Hazards", y = "Count")+
  geom_bar(position="stack", stat="identity", alpha = 0.6) +
  scale_fill_manual("legend", values = c("1" = "#990000", "0" = "#e98417"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size=6, angle=45))





plot_grid(dayweekplotr, timeplotr,
          roadclassplotr,
          monthplotr,nrow = 2)

plot_grid(roadtypeplotr,speedlimitplotr,
          junctiondetailplotr,junctioncontrolplotr,nrow = 2)

plot_grid(pedestrianplotr,
          crossingplotr, roadplotr,specialplotr,nrow = 2)

plot_grid(urbanruralplotr, nrow = 2, ncol = 2)



#ASSOCIATIONS / DEPENDANCE
####################################################################################################
####################################################################################################
# #http://cran.irsn.fr/web/packages/vcd/vcd.pdf
#https://cran.r-project.org/web/packages/GoodmanKruskal/vignettes/GoodmanKruskal.html
#cramer's test using goodmankruskal
GK <- GKtauDataframe(accidents_reduced)
plot(GK,diagSize = 0.8)
plot

#https://cran.r-project.org/web/packages/greybox/vignettes/maUsingGreybox.html
#Cramer's test using greybox package
assoc(accidents_reduced)




#CHI-SQUARED TEST
#accidentseverity
mapply(function(x, y) chisq.test(x, y)$p.value, accidents_reduced[, -1], MoreArgs=list(accidents_reduced[,1]))
#timeday
mapply(function(x, y) chisq.test(x, y)$p.value, accidents_reduced[, -2], MoreArgs=list(accidents_reduced[,2]))
#month
mapply(function(x, y) chisq.test(x, y)$p.value, accidents_reduced[, -3], MoreArgs=list(accidents_reduced[,3]))
#road1class
mapply(function(x, y) chisq.test(x, y)$p.value, accidents_reduced[, -4], MoreArgs=list(accidents_reduced[,4]))
#weekday
mapply(function(x, y) chisq.test(x, y)$p.value, accidents_reduced[, -5], MoreArgs=list(accidents_reduced[,5]))
#roadtype
mapply(function(x, y) chisq.test(x, y)$p.value, accidents_reduced[, -6], MoreArgs=list(accidents_reduced[,6]))
#speedlimit
mapply(function(x, y) chisq.test(x, y)$p.value, accidents_reduced[, -7], MoreArgs=list(accidents_reduced[,7]))
#junctiondetail
mapply(function(x, y) chisq.test(x, y)$p.value, accidents_reduced[, -8], MoreArgs=list(accidents_reduced[,8]))
#junctioncontrol
mapply(function(x, y) chisq.test(x, y)$p.value, accidents_reduced[, -9], MoreArgs=list(accidents_reduced[,9]))
#pedestrian
mapply(function(x, y) chisq.test(x, y)$p.value, accidents_reduced[, -10], MoreArgs=list(accidents_reduced[,10]))
#crossing
mapply(function(x, y) chisq.test(x, y)$p.value, accidents_reduced[, -11], MoreArgs=list(accidents_reduced[,11]))
#light
mapply(function(x, y) chisq.test(x, y)$p.value, accidents_reduced[, -12], MoreArgs=list(accidents_reduced[,12]))
#weather
mapply(function(x, y) chisq.test(x, y)$p.value, accidents_reduced[, -13], MoreArgs=list(accidents_reduced[,13]))
#road
mapply(function(x, y) chisq.test(x, y)$p.value, accidents_reduced[, -14], MoreArgs=list(accidents_reduced[,14]))
#special
mapply(function(x, y) chisq.test(x, y)$p.value, accidents_reduced[, -15], MoreArgs=list(accidents_reduced[,15]))
#carriagehazards
mapply(function(x, y) chisq.test(x, y)$p.value, accidents_reduced[, -16], MoreArgs=list(accidents_reduced[,16]))
#urbanrural
mapply(function(x, y) chisq.test(x, y)$p.value, accidents_reduced[, -17], MoreArgs=list(accidents_reduced[,17]))




#DUMMY VARIABLES
####################################################################################################
####################################################################################################
#https://quantdev.ssri.psu.edu/sites/qdev/files/kNN_tutorial.html
#creating dummy variables
timeday <- as.data.frame(dummy.code(accidents_reduced$timeday))
colnames(timeday) <- paste("timeday", colnames(timeday), sep = "_")
month <- as.data.frame(dummy.code(accidents_reduced$month))
colnames(month) <- paste("month", colnames(month), sep = "_")
road1class <- as.data.frame(dummy.code(accidents_reduced$road1class))
colnames(road1class) <- paste("road1class", colnames(road1class), sep = "_")
weekday <- as.data.frame(dummy.code(accidents_reduced$weekday))
colnames(weekday) <- paste("weekday", colnames(weekday), sep = "_")
roadtype <- as.data.frame(dummy.code(accidents_reduced$roadtype))
colnames(roadtype) <- paste("roadtype", colnames(roadtype), sep = "_")
speedlimit <- as.data.frame(dummy.code(accidents_reduced$speedlimit))
colnames(speedlimit) <- paste("speedlimit", colnames(speedlimit), sep = "_")
junctiondetail <- as.data.frame(dummy.code(accidents_reduced$junctiondetail))
colnames(junctiondetail) <- paste("junctiondetail", colnames(junctiondetail), sep = "_")
junctioncontrol <- as.data.frame(dummy.code(accidents_reduced$junctioncontrol))
colnames(junctioncontrol) <- paste("junctioncontrol", colnames(junctioncontrol), sep = "_")
pedestrian <- as.data.frame(dummy.code(accidents_reduced$pedestrian))
colnames(pedestrian) <- paste("pedestrian", colnames(pedestrian), sep = "_")
crossing <- as.data.frame(dummy.code(accidents_reduced$crossing))
colnames(crossing) <- paste("crossing", colnames(crossing), sep = "_")
light <- as.data.frame(dummy.code(accidents_reduced$light))
colnames(light) <- paste("light", colnames(light), sep = "_")
weather <- as.data.frame(dummy.code(accidents_reduced$weather))
colnames(weather) <- paste("weather", colnames(weather), sep = "_")
road <- as.data.frame(dummy.code(accidents_reduced$road))
colnames(road) <- paste("road", colnames(road), sep = "_")
special <- as.data.frame(dummy.code(accidents_reduced$special))
colnames(special) <- paste("special", colnames(special), sep = "_")
carriagehazards <- as.data.frame(dummy.code(accidents_reduced$carriagehazards))
colnames(carriagehazards) <- paste("carriagehazards", colnames(carriagehazards), sep = "_")
urbanrural <- as.data.frame(dummy.code(accidents_reduced$urbanrural))
colnames(urbanrural) <- paste("urbanrural", colnames(urbanrural), sep = "_")


accidents_reduced_dummy <- accidents_reduced%>%
  select(-one_of(c("timeday","month", "road1class","weekday","roadtype","speedlimit","junctiondetail","junctioncontrol",
                   "pedestrian","crossing","light","weather","road","special","carriagehazards","urbanrural")))


accidents_reduced_dummy <- cbind(accidents_reduced_dummy,timeday,month, road1class,weekday,roadtype,speedlimit,junctiondetail,
                                 junctioncontrol,pedestrian,crossing,light,weather,road,special,carriagehazards,urbanrural)




#CORRELATION
####################################################################################################
####################################################################################################
#structure
str(accidents_reduced_dummy)
#creating a new dataset to perform the changes needed to analize the correlations
accidents_reduced_dummy_2 <- accidents_reduced_dummy
#chagnig accident severity from factor to integer to be able to perform the correlations
accidents_reduced_dummy_2$accidentseverity <- as.integer(accidents_reduced_dummy_2$accidentseverity)
#structure
str(accidents_reduced_dummy_2)

#library(corrplot)
#checking correlations
correlations <- cor(accidents_reduced_dummy_2)
#plot
corrplot(correlations, method="circle",cl.cex=0.8, tl.cex=0.5)

#checking correlations numbers as it is hard to see in the plot
corr <- as.data.frame(correlations)
corr




#MULTICOLINEARITY
####################################################################################################
####################################################################################################
#when perfomring the multiple regression with all the variables appears to be perfect multicolinearity
#to avoid perfect multicolinearity we remove some variables

#removing junctioncontrol (as has correlation with junctiondettail),road1class (as has correlation with speedlimit), 
#roadtype (as has correlation with junctiondetail)
str(accidents_reduced_dummy_2)

#tested with and whiout those categories, always appears as perfect multicolinearity
accidents_reduced_dummy_3 <- accidents_reduced_dummy_2[-c(5,17,19,28,33,41,50,53,59,66,74,77,86,91,99,107)]

str(accidents_reduced_dummy_3)




#FEATURE SELECTION
####################################################################################################
####################################################################################################
#https://www.imsbio.co.jp/RGM/R_rdfile?f=FSelector/man/chi.squared.Rd&d=R_CC
#library(FSelector)
#Fslector package using Chi-Squared to understand the most important variables
set.seed(333)
weights <- chi.squared(accidentseverity~., accidents_reduced)
weights <- weights %>%
  arrange(desc(attr_importance))

weights


#https://www.machinelearningplus.com/machine-learning/feature-selection/
#boruta
#library(Boruta)
#Boruta package to understand the most important variables
set.seed(333)
boruta_output <- Boruta(accidentseverity ~ ., data=na.omit(accidents_reduced), doTrace=0)  

#importance scores
imps <- attStats(roughFixMod)
imps

#plot variables importance
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")  



#https://www.machinelearningplus.com/machine-learning/feature-selection/
#LASSO
######################################
#library(glmnet)
#LASSO regression to understand the most important variables
x <- data.matrix(accidents_reduced[,-1]) # all X vars
y <- as.matrix(accidents_reduced[, 1]) # Only Class

# Fit the LASSO model (Lasso: Alpha = 1)
#https://stackoverflow.com/questions/48179423/error-error-in-lognetx-is-sparse-ix-jx-y-weights-offset-alpha-nobs
#https://cran.r-project.org/web/packages/glmnet/glmnet.pdf
#alpha 1 is the LASSO penalty
#type.measure="auc" is for two-class logistic regression only and gives area under the ROC curve
#cv.glmnet perform cross-validation
#“auc” (for two-class logistic regression ONLY) gives area under the ROC curve
set.seed(333)
cv.lasso <- cv.glmnet(x, y, family='binomial', alpha=1, parallel=TRUE, standardize=TRUE, type.measure='auc')

# Results
plot(cv.lasso)

# plot(cv.lasso$glmnet.fit, xvar="lambda", label=TRUE)
cat('Min Lambda: ', cv.lasso$lambda.min, '\n 1Sd Lambda: ', cv.lasso$lambda.1se)

#lambda.min return the min mean cross-validated error
df_coef <- round(as.matrix(coef(cv.lasso, s=cv.lasso$lambda.min)), 2)

# See all contributing variables
df_coef[df_coef[, 1] != 0, ]



#SMOTE
####################################################################################################
####################################################################################################
#use of smote technique to deal with unbalanced dataset
#library(DMwR)
#count the number of records before smote
count(subset(accidents_reduced,accidents_reduced$accidentseverity == "1")) #6,917
count(subset(accidents_reduced,accidents_reduced$accidentseverity == "0")) #23,083
(count(subset(accidents_reduced,accidents_reduced$accidentseverity == "1")))/30000 #23.06%
(count(subset(accidents_reduced,accidents_reduced$accidentseverity == "0")))/30000 #76.94%

#https://rikunert.com/SMOTE_explained
#dup_size = 0 uses the optimal number of minority class resuses in order to reach parity with the majority class
#apply smote
set.seed(333)
accidents_smote  <- SMOTE(accidentseverity ~ ., accidents_reduced, perc.over = 175, dup_size = 0) #, perc.under=100

#count the number of records after smote
dim(accidents_smote) #27,668
count(subset(accidents_reduced,accidents_smote$accidentseverity == "1")) #13,834
count(subset(accidents_reduced,accidents_smote$accidentseverity == "0")) #16,166
prop.table(table(accidents_smote$accidentseverity)) # aprox. 0.5/0.5

#save the new dataset
write.csv(accidents_smote, "accidents_smote.csv")
#code to import the dataset once the smote technique was to avoid running the previous code
accidents_smote <- read.csv("accidents_smote.csv", stringsAsFactors = TRUE)
dim(accidents_smote)
accidents_smote <- accidents_smote[-c(1)]

str(accidents_smote)

#TRANSFORMATION TO FACTORS AGAIN 
####################################################################################################
####################################################################################################
#as mentioned before, once the data has been imported the variables needs to be converted into factors
#this step is not needed if the new dataset instead of imported, is used by running all the code at once
#ACCIDENT SEVERITY
accidents_smote$accidentseverity <- factor(accidents_smote$accidentseverity, levels = c(0,1))


accidents_smote$speedlimit <- gsub('-1', 'other', accidents_smote$speedlimit)
accidents_smote$speedlimit <- factor(accidents_smote$speedlimit, levels = c(20,30,40,50,60,70,"other"))

#DAY OF THE WEEK
accidents_smote$weekday <- factor(accidents_smote$weekday, levels = c(1,2,3,4,5,6,7))

str(accidents_smote)

summary(accidents_smote)


#MCA - Multiple Correspondance Analysis
####################################################################################################
####################################################################################################
#http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/114-mca-multiple-correspondence-analysis-in-r-essentials/
#http://vxy10.github.io/2016/06/24/si-mca/
#https://rstudio-pubs-static.s3.amazonaws.com/2120_cfbb7161b23e494ea47707f5a12032f1.html
#remove target variable
accidents_mca <- accidents_smote[-c(1)]

res.mca <- MCA(accidents_mca, graph = FALSE)
summary(res.mca)

dimdesc(res.mca)

plot(res.mca, invisible = c("ind", "quali.sup"),autoLab = "y",cex = 0.5)


var <- get_mca_var(res.mca)
var

res.mca

#eigenvalues
eig.val <- get_eigenvalue(res.mca)
eig.val


#scree plot
fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 45))


#biplot
fviz_mca_biplot(res.mca)


# fviz_mca_biplot(res.mca,
#                 repel = TRUE, # Avoid text overlapping (slow if many point)
#                 ggtheme = theme_minimal())


#check the variables that contribute the most
#http://www.sthda.com/english/wiki/fviz-cos2-quick-visualization-of-the-quality-of-representation-of-rows-columns-r-software-and-data-mining
fviz_cos2(res.mca, choice = "var", axes = 1:2)

fviz_cos2(res.mca, choice="var", axes = 1)
fviz_cos2(res.mca, choice="var", axes = 2)

#http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/
fviz_pca_var(res.mca, col.var = "black")

var$coord

#http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/
library("corrplot")
corrplot(var$cos2, is.corr=FALSE)


fviz_pca_var(res.mca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)



#contribution of the variables
var$contrib
corrplot(var$contrib, is.corr=FALSE)




fviz_pca_ind(res.mca,
             label = "none", # hide individual labels
             habillage = accidents_mca$timeday, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07","#559400"),
             addEllipses = TRUE # Concentration ellipses
)




#VARIABLE GROUPS
####################################################################################################
####################################################################################################
str(accidents_smote)


#Group 1: roadtype, speedlimit, junctiondetail, junctioncontrol, pedestrian, crossing, light, urbanrural
accidents_smote_1 <- accidents_smote[-c(2,3,4,5,13,14,15,16)]
str(accidents_smote_1)

#Group 2: roadtype, speedlimit, junctiondetail, pedestrian, crossing, light, weather, urbanrural
accidents_smote_2 <- accidents_smote[-c(2,3,4,5,9,14,15,16)]
str(accidents_smote_2)

#Group 3: roadtype, speedlimit, junctiondetail, crossing, urbanrural
accidents_smote_3 <- accidents_smote[-c(2,3,4,5,9,10,12,13,14,15,16)]
str(accidents_smote_3)




#VARIABLE GROUPS - DUMMY
####################################################################################################
####################################################################################################
#SMOTE DUMMY
timeday1 <- as.data.frame(dummy.code(accidents_smote$timeday))
colnames(timeday1) <- paste("timeday", colnames(timeday1), sep = "_")
month1 <- as.data.frame(dummy.code(accidents_smote$month))
colnames(month1) <- paste("month", colnames(month1), sep = "_")
road1class1 <- as.data.frame(dummy.code(accidents_smote$road1class))
colnames(road1class1) <- paste("road1class", colnames(road1class1), sep = "_")
weekday1 <- as.data.frame(dummy.code(accidents_smote$weekday))
colnames(weekday1) <- paste("weekday", colnames(weekday1), sep = "_")
roadtype1 <- as.data.frame(dummy.code(accidents_smote$roadtype))
colnames(roadtype1) <- paste("roadtype", colnames(roadtype1), sep = "_")
speedlimit1 <- as.data.frame(dummy.code(accidents_smote$speedlimit))
colnames(speedlimit1) <- paste("speedlimit", colnames(speedlimit1), sep = "_")
junctiondetail1 <- as.data.frame(dummy.code(accidents_smote$junctiondetail))
colnames(junctiondetail1) <- paste("junctiondetail", colnames(junctiondetail1), sep = "_")
junctioncontrol1 <- as.data.frame(dummy.code(accidents_smote$junctioncontrol))
colnames(junctioncontrol1) <- paste("junctioncontrol", colnames(junctioncontrol1), sep = "_")
pedestrian1 <- as.data.frame(dummy.code(accidents_smote$pedestrian))
colnames(pedestrian1) <- paste("pedestrian", colnames(pedestrian1), sep = "_")
crossing1 <- as.data.frame(dummy.code(accidents_smote$crossing))
colnames(crossing1) <- paste("crossing", colnames(crossing1), sep = "_")
light1 <- as.data.frame(dummy.code(accidents_smote$light))
colnames(light1) <- paste("light", colnames(light1), sep = "_")
weather1 <- as.data.frame(dummy.code(accidents_smote$weather))
colnames(weather1) <- paste("weather", colnames(weather1), sep = "_")
road1 <- as.data.frame(dummy.code(accidents_smote$road))
colnames(road1) <- paste("road", colnames(road1), sep = "_")
special1 <- as.data.frame(dummy.code(accidents_smote$special))
colnames(special1) <- paste("special", colnames(special1), sep = "_")
carriagehazards1 <- as.data.frame(dummy.code(accidents_smote$carriagehazards))
colnames(carriagehazards1) <- paste("carriagehazards", colnames(carriagehazards1), sep = "_")
urbanrural1 <- as.data.frame(dummy.code(accidents_smote$urbanrural))
colnames(urbanrural1) <- paste("urbanrural", colnames(urbanrural1), sep = "_")


accidents_smote_dummy <- accidents_smote%>%
  select(-one_of(c("timeday","month", "road1class","weekday","roadtype","speedlimit","junctiondetail","junctioncontrol",
                   "pedestrian","crossing","light","weather","road","special","carriagehazards","urbanrural")))


accidents_smote_dummy_0 <- cbind(accidents_smote_dummy,timeday1,month1, road1class1,weekday1,roadtype1,speedlimit1,junctiondetail1,
                                 junctioncontrol1,pedestrian1,crossing1,light1,weather1,road1,special1,carriagehazards1,urbanrural1)


#SMOTE DUMMY 1
#Group 1: roadtype, speedlimit, junctiondetail, junctioncontrol, pedestrian, crossing, light, urbanrural
accidents_smote_dummy_1 <- cbind(accidents_smote_dummy,roadtype1,speedlimit1,junctiondetail1,
                               junctioncontrol1,pedestrian1,crossing1,light1,urbanrural1)

#SMOTE DUMMY 2
#Group 2: roadtype, speedlimit, junctiondetail, pedestrian, crossing, light, weather, urbanrural
accidents_smote_dummy_2 <- cbind(accidents_smote_dummy,roadtype1,speedlimit1,junctiondetail1,
                               pedestrian1,crossing1,light1,weather1,urbanrural1)

#SMOTE DUMMY 3
#Group 3: roadtype, speedlimit, junctiondetail, crossing, urbanrural
accidents_smote_dummy_3 <- cbind(accidents_smote_dummy,roadtype1,speedlimit1,junctiondetail1,
                               crossing1,urbanrural1)








#MODELS GROUP 0 
###########################################################################################################################################
###########################################################################################################################################
###########################################################################################################################################
#DATA SPLIT TRAINING / VALIDATION 
####################################################################################################
#Setting the seed 
set.seed(333)
#Create data partition
validation_index_0 <- createDataPartition(accidents_smote$accidentseverity, p=0.80, list=FALSE)

# 80% of data for training purposes
training_0.1 <- accidents_smote[validation_index_0,]

# 20% of the data for validation
validation_0.1 <- accidents_smote[-validation_index_0,]

#Checking the number of 0 and 1 in each of them
length(grep("1",training_0.1$accidentseverity))
length(grep("1",validation_0.1$accidentseverity ))
length(grep("0",training_0.1$accidentseverity))
length(grep("0",validation_0.1$accidentseverity))

####################################################################################################

# 80% of data for training purposes
training_0.2 <- accidents_smote[validation_index_0,-1]
# 20% of the data for validation
validation_0.2 <- accidents_smote[-validation_index_0,-1]

#Extract output label
training_label_0.2 <- accidents_smote[validation_index_0,1] 
#Extract output label
validation_label_0.2 <- accidents_smote[-validation_index_0,1]


#Checking the number of 0 and 1 in each of them
length(grep("1",training_label_0.2))
length(grep("1",validation_label_0.2))
length(grep("0",training_label_0.2))
length(grep("0",validation_label_0.2))


####################################################################################################
#Setting the seed again to confirm
set.seed(333)
#Create data partition 
validation_index_dummy_0 <- createDataPartition(accidents_smote_dummy_0$accidentseverity, p=0.80, list=FALSE)

# 80% of data for training purposes
training_dummy_0.1 <- accidents_smote_dummy_0[validation_index_dummy_0,]

# 20% of the data for validation
validation_dummy_0.1 <- accidents_smote_dummy_0[-validation_index_dummy_0,]

#Checking the number of 0 and 1 in each of them
length(grep("1",training_dummy_0.1$accidentseverity))
length(grep("1",validation_dummy_0.1$accidentseverity ))
length(grep("0",training_dummy_0.1$accidentseverity))
length(grep("0",validation_dummy_0.1$accidentseverity))

####################################################################################################

# 80% of data for training purposes
training_dummy_0.2 <- accidents_smote_dummy_0[validation_index_dummy_0,-1]
# 20% of the data for validation
validation_dummy_0.2 <- accidents_smote_dummy_0[-validation_index_dummy_0,-1]

#Extract output label
training_label_dummy_0.2 <- accidents_smote_dummy_0[validation_index_dummy_0,1] 
#Extract output label
validation_label_dummy_0.2 <- accidents_smote_dummy_0[-validation_index_dummy_0,1]


#Checking the number of 0 and 1 in each of them
length(grep("1",training_label_dummy_0.2))
length(grep("1",validation_label_dummy_0.2))
length(grep("0",training_label_dummy_0.2))
length(grep("0",validation_label_dummy_0.2))


#DECISION TREE
####################################################################################################
####################################################################################################
#https://mlr.mlr-org.com/articles/tutorial/cost_sensitive_classif.html
#https://stats.stackexchange.com/questions/96081/how-do-i-specify-a-loss-matrix-in-rpart
#training_tree$accidentseverity <- as.factor(training_tree$accidentseverity)
#training_tree <- SMOTE(accidentseverity ~ ., training_1, perc.over = 100, perc.under=200)
#training_tree$accidentseverity <- as.numeric(training_tree$accidentseverity)

prop.table(table(training_0.1$accidentseverity))


#Creating the decision tree model
set.seed(333)
accidents_tree_0  <- rpart(accidentseverity ~ .,
                         data = training_0.1,
                         control = rpart.control(xval=10),
                         method = "class")

#Checking the decision tree
accidents_tree_0

#Visualizing the decision tree
fancyRpartPlot(accidents_tree_0)

#Using the decision tree to predict the class of the validation segment
predicted_0 = predict(accidents_tree_0, validation_0.1, type = "class")

#Confusion matrix decision tree
confusionMatrix(predicted_0, validation_0.1$accidentseverity,positive = "1")


#RANDOM FOREST
####################################################################################################
####################################################################################################
#The partition used will be the same as the decision tree
#Creating the ramdom forest model
set.seed(333)
random_forest_0 <- randomForest(accidentseverity~.,data = training_0.1, importance = TRUE)
class(random_forest_0)
#Using the random forest to predict the class of the validation segment
predicted_random_forest_0 <- predict(random_forest_0, validation_0.1)

#Confusion matrix random forest
confusionMatrix(predicted_random_forest_0, validation_0.1$accidentseverity,positive = "1")


#KNN
####################################################################################################
####################################################################################################
#knn caret package
#https://rpubs.com/njvijay/16444
#https://www.listendata.com/2017/12/k-nearest-neighbor-step-by-step-tutorial.html
#check variables with zero variance
#https://stackoverflow.com/questions/34835096/quickest-way-to-exclude-variables-with-zero-variance-in-r

#knn
#library(ISLR)
#library(caret)
set.seed(333)
str(training_dummy_0.1)

#before aplying knn the categories with variance zero are removed, as other wise the model have issues
nearZeroVar(training_dummy_0.1) #removing variables with little information https://campus.datacamp.com/courses/machine-learning-with-caret-in-r/preprocessing-your-data?ex=13

training_knn_0 <- training_dummy_0.1[-c(21,33,34,40,41,46,47,48,49,50,51,56,57,60,61,67,68,72,73,74,78,79,80,81,82,
                                      83,87,88,89,92,93,94,95,96,97,100,101,102,103,104,107)]

validation_knn_0 <- validation_dummy_0.1[-c(21,33,34,40,41,46,47,48,49,50,51,56,57,60,61,67,68,72,73,74,78,79,80,81,82,
                                          83,87,88,89,92,93,94,95,96,97,100,101,102,103,104,107)]


training_knn_0_label <- training_knn_0[1]
training_knn_0_data <- training_knn_0[-1]
validation_knn_0_label <- validation_knn_0[1]
validation_knn_0_data <- validation_knn_0[-1]

dim(training_knn_0_label)
class(training_knn_0_label)
training_label_0 <- training_knn_0_label[,1] #transform as vector
dim(training_knn_0_data)
class(training_knn_0_data)
dim(validation_knn_0_label)
validation_label_0 <- validation_knn_0_label[,1] #transform as vector
dim(validation_knn_0_data)
#OPTIMAL K
set.seed(333)
i = 1                          # declaration to initiate for loop

k.optm = 1                     # declaration to initiate for loop

for (i in 1:50){ 
  knn.mod <-  knn(train=training_knn_0_data, test=validation_knn_0_data, cl=training_label_0, k=i)
  k.optm[i] <- 100 * sum(validation_label_0 == knn.mod)/NROW(validation_label_0)
  k=i 
  cat(k,'=',k.optm[i],'\n')      #the optimal value is on the value 19 with 65.18438 
}

k.optm 

#Accuracy plot
set.seed(333)
plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level", main = "Accuracy Plot", col = "#008081")
knn_prediction_0 <- knn(training_knn_0_data, validation_knn_0_data, cl= training_label_0, k = 19)
confusionMatrix(knn_prediction_0, validation_label_0,positive = "1")



#NAIVE BAYES
####################################################################################################
####################################################################################################
set.seed(333)
naive_model_0 = naiveBayes(accidentseverity~., data = training_0.1)
naive_predict_0 = predict(naive_model_0,validation_0.1)
naive_matrix_0 <- table(validation_0.1$accidentseverity, naive_predict_0)

confusionMatrix(naive_predict_0, validation_0.1$accidentseverity,positive = "1")

#LOGISTIC REGRESSION
####################################################################################################
####################################################################################################
#https://www.datacamp.com/community/tutorials/logistic-regression-R
#https://blog.datasciencedojo.com/logistic-regression-in-r-tutorial/
#https://onezero.blog/modelling-binary-logistic-regression-using-r-research-oriented-modelling-and-interpretation/
#ISLR package
#https://discuss.analyticsvidhya.com/t/warning-message-glm-fit-algorithm-did-not-converge/5299/2
#https://www.researchgate.net/post/Help_with_Logistic_Regression_In_rglmfit_fitted_probabilities_numerically_0_or_1_occurred_glmfit_algorithm_did_not_converge
#https://medium.com/analytics-vidhya/a-guide-to-machine-learning-in-r-for-beginners-part-5-4c00f2366b90
#https://rpubs.com/ryankelly/ml_logistic
glm.fit_0 <- glm(accidentseverity ~ ., data = training_knn_0, family = binomial)#(link = "logit")

summary(glm.fit_0)

#ods ratio
#https://www.youtube.com/watch?v=vq-_4kWmzTo&feature=youtu.be
#https://s4be.cochrane.org/blog/2013/08/13/a-beginners-guide-to-interpreting-odds-ratios-confidence-intervals-and-p-values-the-nuts-and-bolts-20-minute-tutorial/
#if confidence interval crosses 1 then the variable makes no difference to the model
#glm.fit: fitted probabilities numerically 0 or 1 occurred,model overfitting causing extreme link scores
#https://www.r-bloggers.com/2010/11/learn-logistic-regression-and-beyond/
exp(coef(glm.fit_0))

exp(cbind(OR = coef(glm.fit_0), confint(glm.fit_0)))

log.predictions_0 <- predict(glm.fit_0,validation_knn_0, type = "response")


log.prediction.rd_0 <- ifelse(log.predictions_0 > 0.5, 1, 0)

table(log.prediction.rd_0 , validation_knn_0$accidentseverity)

log.prediction.rd_0 <- as.factor(log.prediction.rd_0)

confusionMatrix(validation_knn_0$accidentseverity,log.prediction.rd_0, positive = "1")



#SVM
####################################################################################################
####################################################################################################
#https://rpubs.com/arpitr/svm
#https://data-flair.training/blogs/e1071-in-r/
#USING LIBRARY library(e1071)
set.seed(333)
svm_fit_radial_0 <- svm(accidentseverity ~ ., kernel="radial", data = training_0.1)
predictions_radial_0 <-  predict(svm_fit_radial_0, validation_0.1[-1])
confusionMatrix(predictions_radial_0, validation_0.1$accidentseverity,positive = "1")

set.seed(333)
svm_fit_poly_0 <- svm(accidentseverity ~ ., kernel="polynomial",data = training_0.1)
predictions_poly_0 <-  predict(svm_fit_poly_0, validation_0.1[-1])
confusionMatrix(predictions_poly_0, validation_0.1$accidentseverity,positive = "1")

set.seed(333)
svm_fit_sigm_0 <- svm(accidentseverity ~ ., kernel="sigmoid",data = training_0.1)
predictions_sigm_0 <-  predict(svm_fit_sigm_0, validation_0.1[-1])
confusionMatrix(predictions_sigm_0, validation_0.1$accidentseverity,positive = "1")

set.seed(333)
svm_fit_linear_0 <- svm(accidentseverity ~ ., kernel="linear", data = training_0.1, scale = F)
predictions_linear_0 <-  predict(svm_fit_linear_0, validation_0.1[-1])
confusionMatrix(predictions_linear_0, validation_0.1$accidentseverity,positive = "1")













#MODELS GROUP 1
###########################################################################################################################################
###########################################################################################################################################
###########################################################################################################################################
#DATA SPLIT TRAINING / VALIDATION 
####################################################################################################
#Setting the seed again to confirm
set.seed(333)
#Create data partition
validation_index_1 <- createDataPartition(accidents_smote_1$accidentseverity, p=0.80, list=FALSE)

# 80% of data for training purposes
training_1.1 <- accidents_smote_1[validation_index_1,]

# 20% of the data for validation
validation_1.1 <- accidents_smote_1[-validation_index_1,]

#Checking the number of 0 and 1 in each of them
length(grep("1",training_1.1$accidentseverity))
length(grep("1",validation_1.1$accidentseverity ))
length(grep("0",training_1.1$accidentseverity))
length(grep("0",validation_1.1$accidentseverity))

####################################################################################################

# 80% of data for training purposes
training_1.2 <- accidents_smote_1[validation_index_1,-1]
# 20% of the data for validation
validation_1.2 <- accidents_smote_1[-validation_index_1,-1]

#Extract output label
training_label_1.2 <- accidents_smote_1[validation_index_1,1] 
#Extract output label
validation_label_1.2 <- accidents_smote_1[-validation_index_1,1]


#Checking the number of 0 and 1 in each of them
length(grep("1",training_label_1.2))
length(grep("1",validation_label_1.2))
length(grep("0",training_label_1.2))
length(grep("0",validation_label_1.2))


####################################################################################################
#Setting the seed again to confirm
set.seed(333)
#Create data partitio
validation_index_dummy_1 <- createDataPartition(accidents_smote_dummy_1$accidentseverity, p=0.80, list=FALSE)

# 80% of data for training purposes
training_dummy_1.1 <- accidents_smote_dummy_1[validation_index_dummy_1,]

# 20% of the data for validation
validation_dummy_1.1 <- accidents_smote_dummy_1[-validation_index_dummy_1,]

#Checking the number of 0 and 1 in each of them
length(grep("1",training_dummy_1.1$accidentseverity))
length(grep("1",validation_dummy_1.1$accidentseverity ))
length(grep("0",training_dummy_1.1$accidentseverity))
length(grep("0",validation_dummy_1.1$accidentseverity))

####################################################################################################

# 80% of data for training purposes
training_dummy_1.2 <- accidents_smote_dummy_1[validation_index_dummy_1,-1]
# 20% of the data for validation
validation_dummy_1.2 <- accidents_smote_dummy_1[-validation_index_dummy_1,-1]

#Extract output label
training_label_dummy_1.2 <- accidents_smote_dummy_1[validation_index_dummy_1,1] 
#Extract output label
validation_label_dummy_1.2 <- accidents_smote_dummy_1[-validation_index_dummy_1,1]


#Checking the number of 0 and 1 in each of them
length(grep("1",training_label_dummy_1.2))
length(grep("1",validation_label_dummy_1.2))
length(grep("0",training_label_dummy_1.2))
length(grep("0",validation_label_dummy_1.2))


#DECISION TREE
####################################################################################################
####################################################################################################
#training_tree$accidentseverity <- as.factor(training_tree$accidentseverity)
#training_tree <- SMOTE(accidentseverity ~ ., training_1, perc.over = 100, perc.under=200)
#training_tree$accidentseverity <- as.numeric(training_tree$accidentseverity)

prop.table(table(training_1.1$accidentseverity))


#Creating the decision tree model
set.seed(333)
accidents_tree_1  <- rpart(accidentseverity ~ .,
                           data = training_1.1,
                           method = "class")

#Checking the decision tree
accidents_tree_1

#Visualizing the decision tree
fancyRpartPlot(accidents_tree_1)

#Using the decision tree to predict the class of the validation segment
predicted_1 = predict(accidents_tree_1, validation_1.1, type = "class")

#Confusion matrix decision tree
confusionMatrix(predicted_1, validation_1.1$accidentseverity,positive = "1")



#RANDOM FOREST
####################################################################################################
####################################################################################################
#The partition used will be the same as the decision tree
#Creating the ramdom forest model
set.seed(333)
random_forest_1 <- randomForest(accidentseverity~.,data = training_1.1, importance = TRUE)
class(random_forest_1)
#Using the random forest to predict the class of the validation segment
predicted_random_forest_1 <- predict(random_forest_1, validation_1.1)

#Confusion matrix random forest
confusionMatrix(predicted_random_forest_1, validation_1.1$accidentseverity,positive = "1")

#KNN
####################################################################################################
####################################################################################################
#knn caret package
#https://rpubs.com/njvijay/16444
#https://www.listendata.com/2017/12/k-nearest-neighbor-step-by-step-tutorial.html
#check variables with zero variance
#https://stackoverflow.com/questions/34835096/quickest-way-to-exclude-variables-with-zero-variance-in-r

#knn
#library(ISLR)
#library(caret)
set.seed(333)
str(training_dummy_1.1)

#before aplying knn the categories with variance zero are removed, as other wise the model have issues
nearZeroVar(training_dummy_1.1)

training_knn_1 <- training_dummy_1.1[-c(6,7,13,14,19,20,21,22,23,24,28,29,30,33,34,40,41,45,46,49)]

validation_knn_1 <- validation_dummy_1.1[-c(6,7,13,14,19,20,21,22,23,24,28,29,30,33,34,40,41,45,46,49)]


training_knn_1_label <- training_knn_1[1]
training_knn_1_data <- training_knn_1[-1]
validation_knn_1_label <- validation_knn_1[1]
validation_knn_1_data <- validation_knn_1[-1]

dim(training_knn_1_label)
class(training_knn_1_label)
training_label_1 <- training_knn_1_label[,1] #transform as vector
dim(training_knn_1_data)
class(training_knn_1_data)
dim(validation_knn_1_label)
validation_label_1 <- validation_knn_1_label[,1] #transform as vector
dim(validation_knn_1_data)
#OPTIMAL K
set.seed(333)
i = 1                          # declaration to initiate for loop

k.optm = 1                     # declaration to initiate for loop

for (i in 1:50){ 
  knn.mod <-  knn(train=training_knn_1_data, test=validation_knn_1_data, cl=training_label_1, k=i, use.all = FALSE) #the parameter use.all is set as FALSE but still appears too many ties
  k.optm[i] <- 111 * sum(validation_label_1 == knn.mod)/NROW(validation_label_1)
  k=i 
  cat(k,'=',k.optm[i],'\n')      #too many ties appearong after the number reached is 7. The best performing until then is 1
}

#as a solution for too many ties I have used k=1 as per https://stackoverflow.com/questions/33344872/what-is-the-fundamental-solution-for-error-in-knn-too-many-ties-in-knn

#Accuracy plot
#plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level", main = "Accuracy Plot", col = "#118181")
set.seed(333)
knn_prediction_1 <- knn(training_knn_1_data, validation_knn_1_data, cl= training_label_1, k = 1)
confusionMatrix(knn_prediction_1, validation_label_1,positive = "1")






#NAIVE BAYES
####################################################################################################
####################################################################################################
set.seed(333)
naive_model_1 = naiveBayes(accidentseverity~., data = training_1.1)
naive_predict_1 = predict(naive_model_1,validation_1.1)
naive_matrix_1 <- table(validation_1.1$accidentseverity, naive_predict_1)

confusionMatrix(naive_predict_1, validation_1.1$accidentseverity,positive = "1")






#LOGISTIC REGRESSION
####################################################################################################
####################################################################################################
#https://www.datacamp.com/community/tutorials/logistic-regression-R
#https://blog.datasciencedojo.com/logistic-regression-in-r-tutorial/
#https://onezero.blog/modelling-binary-logistic-regression-using-r-research-oriented-modelling-and-interpretation/
#ISLR package
#https://discuss.analyticsvidhya.com/t/warning-message-glm-fit-algorithm-did-not-converge/5299/2
#https://www.researchgate.net/post/Help_with_Logistic_Regression_In_rglmfit_fitted_probabilities_numerically_0_or_1_occurred_glmfit_algorithm_did_not_converge
#https://medium.com/analytics-vidhya/a-guide-to-machine-learning-in-r-for-beginners-part-5-4c00f2366b90
#https://rpubs.com/ryankelly/ml_logistic
glm.fit_1 <- glm(accidentseverity ~ ., data = training_knn_1, family = binomial)#(link = "logit")

summary(glm.fit_1)

#ods ratio
#https://www.youtube.com/watch?v=vq-_4kWmzTo&feature=youtu.be
#https://s4be.cochrane.org/blog/2013/08/13/a-beginners-guide-to-interpreting-odds-ratios-confidence-intervals-and-p-values-the-nuts-and-bolts-20-minute-tutorial/
#if confidence interval crosses 1 then the variable makes no difference to the model
#glm.fit: fitted probabilities numerically 0 or 1 occurred,model overfitting causing extreme link scores
#https://www.r-bloggers.com/2010/11/learn-logistic-regression-and-beyond/
exp(coef(glm.fit_1))

exp(cbind(OR = coef(glm.fit_1), confint(glm.fit_1)))

log.predictions_1 <- predict(glm.fit_1,validation_knn_1, type = "response")


log.prediction.rd_1 <- ifelse(log.predictions_1 > 0.5, 1, 0)

table(log.prediction.rd_1 , validation_knn_1$accidentseverity)

log.prediction.rd_1 <- as.factor(log.prediction.rd_1)

confusionMatrix(validation_knn_1$accidentseverity,log.prediction.rd_1, positive = "1")






#SVM
####################################################################################################
####################################################################################################
#https://rpubs.com/arpitr/svm
#https://data-flair.training/blogs/e1071-in-r/
#USING LIBRARY library(e1071)
set.seed(333)
svm_fit_radial_1 <- svm(accidentseverity ~ ., kernel="radial", data = training_1.1)
predictions_radial_1 <-  predict(svm_fit_radial_1, validation_1.1[-1])
confusionMatrix(predictions_radial_1, validation_1.1$accidentseverity,positive = "1")

set.seed(333)
svm_fit_poly_1 <- svm(accidentseverity ~ ., kernel="polynomial",data = training_1.1)
predictions_poly_1 <-  predict(svm_fit_poly_1, validation_1.1[-1])
confusionMatrix(predictions_poly_1, validation_1.1$accidentseverity,positive = "1")

set.seed(333)
svm_fit_sigm_1 <- svm(accidentseverity ~ ., kernel="sigmoid",data = training_1.1)
predictions_sigm_1 <-  predict(svm_fit_sigm_1, validation_1.1[-1])
confusionMatrix(predictions_sigm_1, validation_1.1$accidentseverity,positive = "1")

set.seed(333)
svm_fit_linear_1 <- svm(accidentseverity ~ ., kernel="linear", data = training_1.1, scale = F)
predictions_linear_1 <-  predict(svm_fit_linear_1, validation_1.1[-1])
confusionMatrix(predictions_linear_1, validation_1.1$accidentseverity,positive = "1")















#MODELS GROUP 2
###########################################################################################################################################
###########################################################################################################################################
###########################################################################################################################################
#DATA SPLIT TRAINING / VALIDATION 
####################################################################################################
#Setting the seed again to confirm
set.seed(333)
#Create data partition
validation_index_2 <- createDataPartition(accidents_smote_2$accidentseverity, p=0.80, list=FALSE)

# 80% of data for training purposes
training_2.1 <- accidents_smote_2[validation_index_2,]

# 20% of the data for validation
validation_2.1 <- accidents_smote_2[-validation_index_2,]

#Checking the number of 0 and 1 in each of them
length(grep("1",training_2.1$accidentseverity))
length(grep("1",validation_2.1$accidentseverity ))
length(grep("0",training_2.1$accidentseverity))
length(grep("0",validation_2.1$accidentseverity))

####################################################################################################

# 80% of data for training purposes
training_2.2 <- accidents_smote_2[validation_index_2,-1]
# 20% of the data for validation
validation_2.2 <- accidents_smote_2[-validation_index_2,-1]

#Extract output label
training_label_2.2 <- accidents_smote_2[validation_index_2,1] 
#Extract output label
validation_label_2.2 <- accidents_smote_2[-validation_index_2,1]


#Checking the number of 0 and 1 in each of them
length(grep("1",training_label_2.2))
length(grep("1",validation_label_2.2))
length(grep("0",training_label_2.2))
length(grep("0",validation_label_2.2))


####################################################################################################
#Setting the seed again to confirm
set.seed(333)
#Create data partition
validation_index_dummy_2 <- createDataPartition(accidents_smote_dummy_2$accidentseverity, p=0.80, list=FALSE)

# 80% of data for training purposes
training_dummy_2.1 <- accidents_smote_dummy_2[validation_index_dummy_2,]

# 20% of the data for validation
validation_dummy_2.1 <- accidents_smote_dummy_2[-validation_index_dummy_2,]

#Checking the number of 0 and 1 in each of them
length(grep("1",training_dummy_2.1$accidentseverity))
length(grep("1",validation_dummy_2.1$accidentseverity ))
length(grep("0",training_dummy_2.1$accidentseverity))
length(grep("0",validation_dummy_2.1$accidentseverity))

####################################################################################################

# 80% of data for training purposes
training_dummy_2.2 <- accidents_smote_dummy_2[validation_index_dummy_2,-1]
# 20% of the data for validation
validation_dummy_2.2 <- accidents_smote_dummy_2[-validation_index_dummy_2,-1]

#Extract output label
training_label_dummy_2.2 <- accidents_smote_dummy_2[validation_index_dummy_2,1] 
#Extract output label
validation_label_dummy_2.2 <- accidents_smote_dummy_2[-validation_index_dummy_2,1]


#Checking the number of 0 and 1 in each of them
length(grep("1",training_label_dummy_2.2))
length(grep("1",validation_label_dummy_2.2))
length(grep("0",training_label_dummy_2.2))
length(grep("0",validation_label_dummy_2.2))

#DECISION TREE
####################################################################################################
####################################################################################################
#training_tree$accidentseverity <- as.factor(training_tree$accidentseverity)
#training_tree <- SMOTE(accidentseverity ~ ., training_1, perc.over = 100, perc.under=200)
#training_tree$accidentseverity <- as.numeric(training_tree$accidentseverity)

prop.table(table(training_2.1$accidentseverity))


#Creating the decision tree model
set.seed(333)
accidents_tree_2  <- rpart(accidentseverity ~ .,
                           data = training_2.1,
                           method = "class")

#Checking the decision tree
accidents_tree_2


#Visualizing the decision tree
fancyRpartPlot(accidents_tree_2)

#Using the decision tree to predict the class of the validation segment
predicted_2 = predict(accidents_tree_2, validation_2.1, type = "class")

#Confusion matrix decision tree
confusionMatrix(predicted_2, validation_2.1$accidentseverity,positive = "1")



#RANDOM FOREST
####################################################################################################
####################################################################################################
#The partition used will be the same as the decision tree
#Creating the ramdom forest model
set.seed(333)
random_forest_1 <- randomForest(accidentseverity~.,data = training_2.1, importance = TRUE)
class(random_forest_1)
#Using the random forest to predict the class of the validation segment
predicted_random_forest_1 <- predict(random_forest_1, validation_2.1)

#Confusion matrix random forest
confusionMatrix(predicted_random_forest_1, validation_2.1$accidentseverity,positive = "1")


#KNN
####################################################################################################
####################################################################################################
#knn caret package
#https://rpubs.com/njvijay/16444
#https://www.listendata.com/2017/12/k-nearest-neighbor-step-by-step-tutorial.html
#check variables with zero variance
#https://stackoverflow.com/questions/34835096/quickest-way-to-exclude-variables-with-zero-variance-in-r

#knn
#library(ISLR)
#library(caret)
set.seed(333)
str(training_dummy_2.1)

#before aplying knn the categories with variance zero are removed, as other wise the model have issues
nearZeroVar(training_dummy_2.1)

training_knn_2 <- training_dummy_2.1[-c(6,7,13,14,19,20,21,22,23,24,27,28,34,35,39,40,44,45,46,47,48,49,52)]

validation_knn_2 <- validation_dummy_2.1[-c(6,7,13,14,19,20,21,22,23,24,27,28,34,35,39,40,44,45,46,47,48,49,52)]


training_knn_2_label <- training_knn_2[1]
training_knn_2_data <- training_knn_2[-1]
validation_knn_2_label <- validation_knn_2[1]
validation_knn_2_data <- validation_knn_2[-1]

dim(training_knn_2_label)
class(training_knn_2_label)
training_label_2 <- training_knn_2_label[,1] #transform as vector
dim(training_knn_2_data)
class(training_knn_2_data)
dim(validation_knn_2_label)
validation_label_2 <- validation_knn_2_label[,1] #transform as vector
dim(validation_knn_2_data)
#OPTIMAL K
set.seed(333)
i = 1                          # declaration to initiate for loop

k.optm = 1                     # declaration to initiate for loop

for (i in 1:52){ 
  knn.mod <-  knn(train=training_knn_2_data, test=validation_knn_2_data, cl=training_label_2, k=i, use.all = FALSE)
  k.optm[i] <- 122 * sum(validation_label_2 == knn.mod)/NROW(validation_label_2)
 k=i 
  cat(k,'=',k.optm[i],'\n')      #the optimal value is on the value 19 with 65.18438 
} #after 19 iteration appears too many ties. the best until then is 1

#as a solution for too many ties I have used k=1 as per https://stackoverflow.com/questions/33344872/what-is-the-fundamental-solution-for-error-in-knn-too-many-ties-in-knn


#Accuracy plot
#plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level", main = "Accuracy Plot", col = "#228281")
set.seed(333)
knn_prediction_2 <- knn(training_knn_2_data, validation_knn_2_data, cl= training_label_2, k = 1)
confusionMatrix(knn_prediction_2, validation_label_2,positive = "1")


#https://www.rdocumentation.org/packages/kknn/versions/1.3.1/topics/kknn
#http://rstudio-pubs-static.s3.amazonaws.com/24844_335efcfc09954ad99c4e05d9548ed2ad.html




#NAIVE BAYES
####################################################################################################
####################################################################################################
set.seed(333)
naive_model_2 = naiveBayes(accidentseverity~., data = training_2.1)
naive_predict_2 = predict(naive_model_2,validation_2.1)
naive_matrix_2 <- table(validation_2.1$accidentseverity, naive_predict_2)

confusionMatrix(naive_predict_2, validation_2.1$accidentseverity,positive = "1")



#LOGISTIC REGRESSION
####################################################################################################
####################################################################################################
#https://www.datacamp.com/community/tutorials/logistic-regression-R
#https://blog.datasciencedojo.com/logistic-regression-in-r-tutorial/
#https://onezero.blog/modelling-binary-logistic-regression-using-r-research-oriented-modelling-and-interpretation/
#ISLR package
#https://discuss.analyticsvidhya.com/t/warning-message-glm-fit-algorithm-did-not-converge/5299/2
#https://www.researchgate.net/post/Help_with_Logistic_Regression_In_rglmfit_fitted_probabilities_numerically_0_or_1_occurred_glmfit_algorithm_did_not_converge
#https://medium.com/analytics-vidhya/a-guide-to-machine-learning-in-r-for-beginners-part-5-4c00f2366b90
#https://rpubs.com/ryankelly/ml_logistic
glm.fit_2 <- glm(accidentseverity ~ ., data = training_knn_2, family = binomial)#(link = "logit")

summary(glm.fit_2)

#ods ratio
#https://www.youtube.com/watch?v=vq-_4kWmzTo&feature=youtu.be
#https://s4be.cochrane.org/blog/2013/08/13/a-beginners-guide-to-interpreting-odds-ratios-confidence-intervals-and-p-values-the-nuts-and-bolts-20-minute-tutorial/
#if confidence interval crosses 1 then the variable makes no difference to the model
#glm.fit: fitted probabilities numerically 0 or 1 occurred,model overfitting causing extreme link scores
#https://www.r-bloggers.com/2010/11/learn-logistic-regression-and-beyond/
exp(coef(glm.fit_2))

exp(cbind(OR = coef(glm.fit_2), confint(glm.fit_2)))

log.predictions_2 <- predict(glm.fit_2,validation_knn_2, type = "response")


log.prediction.rd_2 <- ifelse(log.predictions_2 > 0.5, 1, 0)

table(log.prediction.rd_2 , validation_knn_2$accidentseverity)

log.prediction.rd_2 <- as.factor(log.prediction.rd_2)

confusionMatrix(validation_knn_2$accidentseverity,log.prediction.rd_2, positive = "1")

#SVM
####################################################################################################
####################################################################################################
#https://rpubs.com/arpitr/svm
#https://data-flair.training/blogs/e1071-in-r/
#USING LIBRARY library(e1071)
set.seed(333)
svm_fit_radial_2 <- svm(accidentseverity ~ ., kernel="radial", data = training_2.1)
predictions_radial_2 <-  predict(svm_fit_radial_2, validation_2.1[-1])
confusionMatrix(predictions_radial_2, validation_2.1$accidentseverity,positive = "1")

set.seed(333)
svm_fit_poly_2 <- svm(accidentseverity ~ ., kernel="polynomial",data = training_2.1)
predictions_poly_2 <-  predict(svm_fit_poly_2, validation_2.1[-1])
confusionMatrix(predictions_poly_2, validation_2.1$accidentseverity,positive = "1")

set.seed(333)
svm_fit_sigm_2 <- svm(accidentseverity ~ ., kernel="sigmoid",data = training_2.1)
predictions_sigm_2 <-  predict(svm_fit_sigm_2, validation_2.1[-1])
confusionMatrix(predictions_sigm_2, validation_2.1$accidentseverity,positive = "1")

set.seed(333)
svm_fit_linear_2 <- svm(accidentseverity ~ ., kernel="linear", data = training_2.1, scale = F)
predictions_linear_2 <-  predict(svm_fit_linear_2, validation_2.1[-1])
confusionMatrix(predictions_linear_2, validation_2.1$accidentseverity,positive = "1")





















#MODELS GROUP 3
###########################################################################################################################################
###########################################################################################################################################
###########################################################################################################################################
#DATA SPLIT TRAINING / VALIDATION 
####################################################################################################
#Setting the seed again to confirm
set.seed(333)
#Create data partition
validation_index_3 <- createDataPartition(accidents_smote_3$accidentseverity, p=0.80, list=FALSE)

# 80% of data for training purposes
training_3.1 <- accidents_smote_3[validation_index_3,]

# 20% of the data for validation
validation_3.1 <- accidents_smote_3[-validation_index_3,]

#Checking the number of 0 and 1 in each of them
length(grep("1",training_3.1$accidentseverity))
length(grep("1",validation_3.1$accidentseverity ))
length(grep("0",training_3.1$accidentseverity))
length(grep("0",validation_3.1$accidentseverity))

####################################################################################################

# 80% of data for training purposes
training_3.2 <- accidents_smote_3[validation_index_3,-1]
# 20% of the data for validation
validation_3.2 <- accidents_smote_3[-validation_index_3,-1]

#Extract output label
training_label_3.2 <- accidents_smote_3[validation_index_3,1] 
#Extract output label
validation_label_3.2 <- accidents_smote_3[-validation_index_3,1]


#Checking the number of 0 and 1 in each of them
length(grep("1",training_label_3.2))
length(grep("1",validation_label_3.2))
length(grep("0",training_label_3.2))
length(grep("0",validation_label_3.2))


####################################################################################################
#Setting the seed again to confirm
set.seed(333)
#Create data partition
validation_index_dummy_3 <- createDataPartition(accidents_smote_dummy_3$accidentseverity, p=0.80, list=FALSE)

# 80% of data for training purposes
training_dummy_3.1 <- accidents_smote_dummy_3[validation_index_dummy_3,]

# 20% of the data for validation
validation_dummy_3.1 <- accidents_smote_dummy_3[-validation_index_dummy_3,]

#Checking the number of 0 and 1 in each of them
length(grep("1",training_dummy_3.1$accidentseverity))
length(grep("1",validation_dummy_3.1$accidentseverity ))
length(grep("0",training_dummy_3.1$accidentseverity))
length(grep("0",validation_dummy_3.1$accidentseverity))

####################################################################################################

# 80% of data for training purposes
training_dummy_3.2 <- accidents_smote_dummy_3[validation_index_dummy_3,-1]
# 20% of the data for validation
validation_dummy_3.2 <- accidents_smote_dummy_3[-validation_index_dummy_3,-1]

#Extract output label
training_label_dummy_3.2 <- accidents_smote_dummy_3[validation_index_dummy_3,1] 
#Extract output label
validation_label_dummy_3.2 <- accidents_smote_dummy_3[-validation_index_dummy_3,1]


#Checking the number of 0 and 1 in each of them
length(grep("1",training_label_dummy_3.2))
length(grep("1",validation_label_dummy_3.2))
length(grep("0",training_label_dummy_3.2))
length(grep("0",validation_label_dummy_3.2))

#DECISION TREE
####################################################################################################
####################################################################################################
#training_tree$accidentseverity <- as.factor(training_tree$accidentseverity)
#training_tree <- SMOTE(accidentseverity ~ ., training_1, perc.over = 100, perc.under=200)
#training_tree$accidentseverity <- as.numeric(training_tree$accidentseverity)

prop.table(table(training_3.1$accidentseverity))


#Creating the decision tree model
set.seed(333)
accidents_tree_3  <- rpart(accidentseverity ~ .,
                           data = training_3.1,
                           method = "class")

#Checking the decision tree
accidents_tree_3


#Visualizing the decision tree
fancyRpartPlot(accidents_tree_3)

#Using the decision tree to predict the class of the validation segment
predicted_3 = predict(accidents_tree_3, validation_3.1, type = "class")

#Confusion matrix decision tree
confusionMatrix(predicted_3, validation_3.1$accidentseverity,positive = "1")



#RANDOM FOREST
####################################################################################################
####################################################################################################
#The partition used will be the same as the decision tree
#Creating the ramdom forest model
set.seed(333)
random_forest_3 <- randomForest(accidentseverity~.,data = training_3.1, importance = TRUE)
class(random_forest_3)
#Using the random forest to predict the class of the validation segment
predicted_random_forest_3 <- predict(random_forest_3, validation_3.1)

#Confusion matrix random forest
confusionMatrix(predicted_random_forest_3, validation_3.1$accidentseverity,positive = "1")

#KNN
####################################################################################################
####################################################################################################
#knn caret package
#https://rpubs.com/njvijay/16444
#https://www.listendata.com/2317/12/k-nearest-neighbor-step-by-step-tutorial.html
#check variables with zero variance
#https://stackoverflow.com/questions/34835396/quickest-way-to-exclude-variables-with-zero-variance-in-r

#knn
#library(ISLR)
#library(caret)
set.seed(333)
str(training_dummy_3.1)

#before aplying knn the categories with variance zero are removed, as other wise the model have issues
nearZeroVar(training_dummy_3.1)

training_knn_3 <- training_dummy_3.1[-c(6,7,13,14,19,20,21,22,23,24,30,31,34)]

validation_knn_3 <- validation_dummy_3.1[-c(6,7,13,14,19,20,21,22,23,24,30,31,34)]


training_knn_3_label <- training_knn_3[1]
training_knn_3_data <- training_knn_3[-1]
validation_knn_3_label <- validation_knn_3[1]
validation_knn_3_data <- validation_knn_3[-1]

dim(training_knn_3_label)
class(training_knn_3_label)
training_label_3 <- training_knn_3_label[,1] #transform as vector
dim(training_knn_3_data)
class(training_knn_3_data)
dim(validation_knn_3_label)
validation_label_3 <- validation_knn_3_label[,1] #transform as vector
dim(validation_knn_3_data)
#OPTIMAL K
set.seed(333)
i = 1                          # declaration to initiate for loop

k.optm = 1                     # declaration to initiate for loop

n = c(1,3,5,7,9,11,13)

for (i in n){ 
  knn.mod <-  knn(train=training_knn_3_data, test=validation_knn_3_data, cl=training_label_3, k=i)
  k.optm[i] <- 133 * sum(validation_label_3 == knn.mod)/NROW(validation_label_3)
  k=i 
  cat(k,'=',k.optm[i],'\n')      #the optimal value is on the value 19 with 65.18438 
}

#as a solution for too many ties I have used k=1 as per https://stackoverflow.com/questions/33344872/what-is-the-fundamental-solution-for-error-in-knn-too-many-ties-in-knn


#Accuracy plot
#plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level", main = "Accuracy Plot", col = "#338381")
set.seed(333)
knn_prediction_3 <- knn(training_knn_3_data, validation_knn_3_data, cl= training_label_3, k = 1, use.all = FALSE)
confusionMatrix(knn_prediction_3, validation_label_3,positive = "1")

#https://www.rdocumentation.org/packages/kknn/versions/1.3.1/topics/kknn
#http://rstudio-pubs-static.s3.amazonaws.com/24844_335efcfc09954ad99c4e05d9548ed2ad.html


#too many ties https://stackoverflow.com/questions/33344872/what-is-the-fundamental-solution-for-error-in-knn-too-many-ties-in-knn
#From an algorithmic standpoint, this likely means that there are too many neighbors equidistant to the target point, such that the algorithm cannot choose only k of them. 


#NAIVE BAYES
####################################################################################################
####################################################################################################
set.seed(333)
naive_model_3 = naiveBayes(accidentseverity~., data = training_3.1)
naive_predict_3 = predict(naive_model_3,validation_3.1)
naive_matrix_3 <- table(validation_3.1$accidentseverity, naive_predict_3)

confusionMatrix(naive_predict_3, validation_3.1$accidentseverity,positive = "1")





#LOGISTIC REGRESSION
####################################################################################################
####################################################################################################
#https://www.datacamp.com/community/tutorials/logistic-regression-R
#https://blog.datasciencedojo.com/logistic-regression-in-r-tutorial/
#https://onezero.blog/modelling-binary-logistic-regression-using-r-research-oriented-modelling-and-interpretation/
#ISLR package
#https://discuss.analyticsvidhya.com/t/warning-message-glm-fit-algorithm-did-not-converge/5299/2
#https://www.researchgate.net/post/Help_with_Logistic_Regression_In_rglmfit_fitted_probabilities_numerically_0_or_1_occurred_glmfit_algorithm_did_not_converge
#https://medium.com/analytics-vidhya/a-guide-to-machine-learning-in-r-for-beginners-part-5-4c00f2366b90
#https://rpubs.com/ryankelly/ml_logistic
glm.fit_3 <- glm(accidentseverity ~ ., data = training_knn_3, family = binomial)#(link = "logit")

summary(glm.fit_3)

#ods ratio
#https://www.youtube.com/watch?v=vq-_4kWmzTo&feature=youtu.be
#https://s4be.cochrane.org/blog/3013/08/13/a-beginners-guide-to-interpreting-odds-ratios-confidence-intervals-and-p-values-the-nuts-and-bolts-30-minute-tutorial/
#if confidence interval crosses 1 then the variable makes no difference to the model
#glm.fit: fitted probabilities numerically 0 or 1 occurred,model overfitting causing extreme link scores
#https://www.r-bloggers.com/3010/11/learn-logistic-regression-and-beyond/
exp(coef(glm.fit_3))

exp(cbind(OR = coef(glm.fit_3), confint(glm.fit_3)))

log.predictions_3 <- predict(glm.fit_3,validation_knn_3, type = "response")


log.prediction.rd_3 <- ifelse(log.predictions_3 > 0.5, 1, 0)

table(log.prediction.rd_3 , validation_knn_3$accidentseverity)

log.prediction.rd_3 <- as.factor(log.prediction.rd_3)

confusionMatrix(validation_knn_3$accidentseverity,log.prediction.rd_3, positive = "1")


#testing on read data set

str(accidents_reduced_dummy_test)

str(validation_knn_3)
accidents_reduced_dummy_test_knn <- accidents_reduced_dummy_test[c(1,2,36)]

log.predictions_4 <- predict(glm.fit_3,accidents, type = "response")


log.prediction.rd_3 <- ifelse(log.predictions_3 > 0.5, 1, 0)

table(log.prediction.rd_3 , validation_knn_3$accidentseverity)

log.prediction.rd_3 <- as.factor(log.prediction.rd_3)

confusionMatrix(validation_knn_3$accidentseverity,log.prediction.rd_3, positive = "1")


#SVM
####################################################################################################
####################################################################################################
#https://rpubs.com/arpitr/svm
#https://data-flair.training/blogs/e1071-in-r/
#USING LIBRARY library(e1071)
set.seed(333)
svm_fit_radial_3 <- svm(accidentseverity ~ ., kernel="radial", data = training_3.1)
predictions_radial_3 <-  predict(svm_fit_radial_3, validation_3.1[-1])
confusionMatrix(predictions_radial_3, validation_3.1$accidentseverity,positive = "1")

set.seed(333)
svm_fit_poly_3 <- svm(accidentseverity ~ ., kernel="polynomial",data = training_3.1)
svm_fit_poly_3
predictions_poly_3 <-  predict(svm_fit_poly_3, validation_3.1[-1])
confusionMatrix(predictions_poly_3, validation_3.1$accidentseverity,positive = "1")

summary(svm_fit_poly_3)



set.seed(333)
svm_fit_sigm_3 <- svm(accidentseverity ~ ., kernel="sigmoid",data = training_3.1)
predictions_sigm_3 <-  predict(svm_fit_sigm_3, validation_3.1[-1])
confusionMatrix(predictions_sigm_3, validation_3.1$accidentseverity,positive = "1")

set.seed(333)
svm_fit_linear_3 <- svm(accidentseverity ~ ., kernel="linear", data = training_3.1, scale = F)
predictions_linear_3 <-  predict(svm_fit_linear_3, validation_3.1[-1])
confusionMatrix(predictions_linear_3, validation_3.1$accidentseverity,positive = "1")



#############
#roadtype, speedlimit,junctiondetail, crossing, urbanrural
str(accidents_smote_3)

#Setting the seed again to confirm
set.seed(333)
#Create data partition to be able to train the model as kNN is supervised
validation_index_3 <- createDataPartition(accidents_smote_3$accidentseverity, p=0.80, list=FALSE)

# 80% of data for training purposes
training_3.1 <- accidents_smote_3[validation_index_3,]

# 20% of the data for validation
validation_3.1 <- accidents_smote_3[-validation_index_3,]

#speedlimit, junctiondetail, crossing, urbanrural
accidents_smote_3_1 <- accidents_smote_3[-c(2)]
#Setting the seed again to confirm
set.seed(333)
#Create data partition to be able to train the model as kNN is supervised
validation_index_3_1 <- createDataPartition(accidents_smote_3_1$accidentseverity, p=0.80, list=FALSE)

# 80% of data for training purposes
training_3_1.1 <- accidents_smote_3_1[validation_index_3_1,]

# 20% of the data for validation
validation_3_1.1 <- accidents_smote_3_1[-validation_index_3_1,]


#roadtype, junctiondetail, crossing, urbanrural
accidents_smote_3_2 <- accidents_smote_3[-c(3)]
#Setting the seed again to confirm
set.seed(333)
#Create data partition to be able to train the model as kNN is supervised
validation_index_3_2 <- createDataPartition(accidents_smote_3_2$accidentseverity, p=0.80, list=FALSE)

# 80% of data for training purposes
training_3_2.1 <- accidents_smote_3_2[validation_index_3_2,]

# 20% of the data for validation
validation_3_2.1 <- accidents_smote_3_2[-validation_index_3_2,]


#roadtype, speedlimit, crossing, urbanrural
accidents_smote_3_3 <- accidents_smote_3[-c(4)]
#Setting the seed again to confirm
set.seed(333)
#Create data partition to be able to train the model as kNN is supervised
validation_index_3_3 <- createDataPartition(accidents_smote_3_3$accidentseverity, p=0.80, list=FALSE)

# 80% of data for training purposes
training_3_3.1 <- accidents_smote_3_3[validation_index_3_3,]

# 20% of the data for validation
validation_3_3.1 <- accidents_smote_3_3[-validation_index_3_3,]


#roadtype, speedlimit,junctiondetail, urbanrural
accidents_smote_3_4 <- accidents_smote_3[-c(5)]
#Setting the seed again to confirm
set.seed(333)
#Create data partition to be able to train the model as kNN is supervised
validation_index_3_4 <- createDataPartition(accidents_smote_3_4$accidentseverity, p=0.80, list=FALSE)

# 80% of data for training purposes
training_3_4.1 <- accidents_smote_3_4[validation_index_3_4,]

# 20% of the data for validation
validation_3_4.1 <- accidents_smote_3_4[-validation_index_3_4,]


#roadtype, speedlimit,junctiondetail, crossing
accidents_smote_3_5 <- accidents_smote_3[-c(6)]
#Setting the seed again to confirm
set.seed(333)
#Create data partition to be able to train the model as kNN is supervised
validation_index_3_5 <- createDataPartition(accidents_smote_3_5$accidentseverity, p=0.80, list=FALSE)

# 80% of data for training purposes
training_3_5.1 <- accidents_smote_3_5[validation_index_3_5,]

# 20% of the data for validation
validation_3_5.1 <- accidents_smote_3_5[-validation_index_3_5,]





set.seed(333)
svm_fit_poly_3 <- svm(accidentseverity ~ ., kernel="polynomial",data = training_3.1)
svm_fit_poly_3
predictions_poly_3 <-  predict(svm_fit_poly_3, validation_3.1[-1])
confusionMatrix(predictions_poly_3, validation_3.1$accidentseverity,positive = "1")


set.seed(333)
svm_fit_poly_3_1 <- svm(accidentseverity ~ ., kernel="polynomial",data = training_3_1.1)
svm_fit_poly_3_1
predictions_poly_3_1 <-  predict(svm_fit_poly_3_1, validation_3_1.1[-1])
confusionMatrix(predictions_poly_3_1, validation_3_1.1$accidentseverity,positive = "1")

set.seed(333)
svm_fit_poly_3_2 <- svm(accidentseverity ~ ., kernel="polynomial",data = training_3_2.1)
svm_fit_poly_3_2
predictions_poly_3_2 <-  predict(svm_fit_poly_3_2, validation_3_2.1[-1])
confusionMatrix(predictions_poly_3_2, validation_3_2.1$accidentseverity,positive = "1")

set.seed(333)
svm_fit_poly_3_3 <- svm(accidentseverity ~ ., kernel="polynomial",data = training_3_3.1)
svm_fit_poly_3_3
predictions_poly_3_3 <-  predict(svm_fit_poly_3_3, validation_3_3.1[-1])
confusionMatrix(predictions_poly_3_3, validation_3_3.1$accidentseverity,positive = "1")

set.seed(333)
svm_fit_poly_3_4 <- svm(accidentseverity ~ ., kernel="polynomial",data = training_3_4.1)
svm_fit_poly_3_4
predictions_poly_3_4 <-  predict(svm_fit_poly_3_4, validation_3_4.1[-1])
confusionMatrix(predictions_poly_3_4, validation_3_4.1$accidentseverity,positive = "1")

set.seed(333)
svm_fit_poly_3_5 <- svm(accidentseverity ~ ., kernel="polynomial",data = training_3_5.1)
svm_fit_poly_3_5
predictions_poly_3_5 <-  predict(svm_fit_poly_3_5, validation_3_5.1[-1])
confusionMatrix(predictions_poly_3_5, validation_3_5.1$accidentseverity,positive = "1")



