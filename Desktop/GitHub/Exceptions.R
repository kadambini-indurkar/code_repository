
#============================= Loading necessary libraries =====================================================================#
library(forecast)
library(dplyr)
library(reshape2)
library(lubridate)
library(caret)
library(mondate)
library(Metrics)
library(Matrix)
library(gtools)
library(tsintermittent)
#================================================================================================================================#


#============================== Reading Input Data ==============================================================================#
#Rename columns
colnames(ActualsData)[which(names(ActualsData) == "Item.Planning Item")] <- "SKU"
#colnames(ActualsData)[which(names(ActualsData) == "Sales Domain.[Depot]")] <- "Depot"
colnames(ActualsData)[which(names(ActualsData) == "Time.Planning Month")] <- "Time"
colnames(ActualsData)[which(names(ActualsData) == "Outlier Corrected Actuals SKU")] <- "Actual"
colnames(ActualsData)[which(names(ActualsData) == "Outlier Corrected Actuals LY")] <- "LY_Actual"
colnames(ActualsData)[which(names(ActualsData) == "Outlier Corrected Actuals LLY")] <- "LLY_Actual"

#colnames(Forecastable)[which(names(Forecastable) == "Item.Item")] <- "SKU"

# One Year Actuals Data
colnames(OneYearActualsData)[which(names(OneYearActualsData) == "Item.Planning Item")] <- "SKU"
#colnames(ActualsData)[which(names(ActualsData) == "Sales Domain.[Depot]")] <- "Depot"
colnames(OneYearActualsData)[which(names(OneYearActualsData) == "Time.Planning Month")] <- "Time"
colnames(OneYearActualsData)[which(names(OneYearActualsData) == "Outlier Corrected Actuals SKU")] <- "Actuals"
OneYearActualsData$Year <- substr(OneYearActualsData$Time,5,8)
#OneYearActualsData$Year1 <- paste("20",OneYearActualsData$Year,sep = "")
OneYearActualsData$Month <- substr(OneYearActualsData$Time,1,3)
OneYearActualsData$Week<-paste(OneYearActualsData$Year,"-",match(OneYearActualsData$Month,month.abb),"-","01")
OneYearActualsData$Week<-as.Date(OneYearActualsData$Week, format = "%Y - %m - %d")

# Reading Forecast Data
colnames(Forecast)[which(names(Forecast) == "Item.Planning Item")] <- "SKU"
colnames(Forecast)[which(names(Forecast) == "Time.Planning Month")] <- "Time"
colnames(Forecast)[which(names(Forecast) == "Best Fit SKU")] <- "Lag1_Forecast"


# Forward Forecast data
colnames(OneYearForecastData)[which(names(OneYearForecastData) == "Item.Planning Item")] <- "SKU"
#colnames(ActualsData)[which(names(ActualsData) == "Sales Domain.[Depot]")] <- "Depot"
colnames(OneYearForecastData)[which(names(OneYearForecastData) == "Time.Planning Month")] <- "Time"
colnames(OneYearForecastData)[which(names(OneYearForecastData) == "Best Fit SKU")] <- "FForecast"
OneYearForecastData$Year <- substr(OneYearForecastData$Time,5,8)
#OneYearForecastData$Year1 <- paste("20",OneYearForecastData$Year,sep = "")
OneYearForecastData$Month <- substr(OneYearForecastData$Time,1,3)
OneYearForecastData$Week<-paste(OneYearForecastData$Year,"-",match(OneYearForecastData$Month,month.abb),"-","01")
OneYearForecastData$Week<-as.Date(OneYearForecastData$Week, format = "%Y - %m - %d")


# Current month
CM <- format(as.Date(CurrentDate$DPCurrentDate),"%Y-%m-01")
CM <- as.Date(CM)
Current_lag1 <- as.Date(CM) %m+% months(1)
month <- format(as.Date(Current_lag1),"%b")
#================================================================================================================================#


#============================================= Average Deviations =====================================================#
BaseData <- left_join(ActualsData, Forecast, by = "SKU")

#BaseData$CV <- rowMeans(BaseData[,c("Actual","LY_Actual","LLY_Actual")], na.rm = T)
BaseData$Deviation_wrt_Recent <- ifelse(BaseData$Actual == 0,1, (BaseData$Lag1_Forecast - BaseData$Actual)/BaseData$Actual)

BaseData$AvgActuals <- rowMeans(BaseData[,c("Actual","LY_Actual","LLY_Actual")], na.rm = T)
BaseData$Deviation_wrt_Avg <- ifelse(BaseData$AvgActuals == 0,1, (BaseData$Lag1_Forecast - BaseData$AvgActuals)/BaseData$AvgActuals)
#BaseData$Growth <- (sum((BaseData$Actual - BaseData$LY_Actual)/BaseData$LY_Actual,(BaseData$LY_Actual - BaseData$LLY_Actual)/BaseData$LLY_Actual))/2
BaseData$d <- ifelse(BaseData$LY_Actual==0,1,((BaseData$Actual/BaseData$LY_Actual) - 1))
BaseData$f <- ifelse(BaseData$LLY_Actual==0,1,((BaseData$LY_Actual/BaseData$LLY_Actual) - 1))


BaseData$sum <- BaseData$d + BaseData$f
BaseData$Growth <- as.numeric(BaseData$sum)/as.numeric(2)

BaseData$Growth <- ifelse(is.na(BaseData$Growth), 0, BaseData$Growth)
BaseData$Growth <- ifelse(is.infinite(BaseData$Growth), 0, BaseData$Growth)
BaseData$Growth <- ifelse(is.nan(BaseData$Growth), 0, BaseData$Growth)

# Yearly Growth -
TotalActuals$Month <- substr(TotalActuals$`Time.Planning Month`,1,3)
TotalActuals$Time <- as.Date(paste(substr(TotalActuals$`Time.Planning Month`,5,8),"-",match(TotalActuals$Month,month.abb),"-","01"), format = "%Y - %m - %d")
ForecastPeriodDates <- OneYearForecastData$Week
ActualYearCategory1 <- as.Date(ForecastPeriodDates) %m-% months(12)
ActualYearCategory2 <- as.Date(ForecastPeriodDates) %m-% months(24)
ActualYearCategory3 <- as.Date(ForecastPeriodDates) %m-% months(36)
TotalActuals$YearFlagNum <- ifelse(as.Date(TotalActuals$Time) %in% (ActualYearCategory1),1,"NA")
TotalActuals$YearFlagNum <- ifelse(as.Date(TotalActuals$Time) %in% (ActualYearCategory2),2,TotalActuals$YearFlagNum)
TotalActuals$YearFlagNum <- ifelse(as.Date(TotalActuals$Time) %in% (ActualYearCategory3),3,TotalActuals$YearFlagNum)

TotalActuals <- TotalActuals %>% group_by(`Item.Planning Item`,`YearFlagNum`) %>% summarise(YearlySum = sum(`Outlier Corrected Actuals SKU`, na.rm = T))
TotalActuals <- reshape2::dcast(TotalActuals, `Item.Planning Item` ~ YearFlagNum, value.var = "YearlySum")
TotalActuals <- rename(TotalActuals, "RecentYear"="1", "LY"="2","LLY"="3")
TotalActuals$d <- ifelse(TotalActuals$LY==0,1,((TotalActuals$RecentYear - TotalActuals$LY)/TotalActuals$LY))
TotalActuals$f <- ifelse(TotalActuals$LLY==0,1,((TotalActuals$LY - TotalActuals$LLY)/TotalActuals$LLY))
TotalActuals$sum <- TotalActuals$d + TotalActuals$f
#TotalActuals$YearlyGrowth <- ifelse(TotalActuals$sum1==0,0,TotalActuals$sum/2)
TotalActuals$YearlyGrowth <- TotalActuals$sum/2
TotalActuals$sum1 <- TotalActuals$RecentYear+TotalActuals$LLY+TotalActuals$LY
TotalActuals <- as.data.frame(TotalActuals)
TotalActuals$YearlyGrowth <- ifelse(is.na(TotalActuals$YearlyGrowth), 0, TotalActuals$YearlyGrowth)
TotalActuals$YearlyGrowth <- ifelse(is.infinite(TotalActuals$YearlyGrowth), 0, TotalActuals$YearlyGrowth)
TotalActuals$YearlyGrowth <- ifelse(is.nan(TotalActuals$YearlyGrowth), 0, TotalActuals$YearlyGrowth)

#BaseData$Growth1 <- mean((BaseData$Actual)/BaseData$LY_Actual,(BaseData$LY_Actual)/BaseData$LLY_Actual)

BaseData <- left_join(BaseData, TotalActuals, by = c("SKU"="Item.Planning Item"))

BaseData <- rename(BaseData,"Time.[Planning Month]"="Time.x","Item.[Planning Item]"="SKU","Measure.[Deviation wrt to Recent Actual]"="Deviation_wrt_Recent","Measure.[Deviation wrt AvgActuals]"="Deviation_wrt_Avg","Measure.[Average YoY Growth]"="Growth","Measure.[Average Yearly Growth]"="YearlyGrowth")
BaseData <- BaseData[,c("Item.[Planning Item]","Time.[Planning Month]","Measure.[Deviation wrt to Recent Actual]","Measure.[Deviation wrt AvgActuals]","Measure.[Average YoY Growth]","Measure.[Average Yearly Growth]")]
#BaseData$`AverageYoY Growth` <- BaseData$Growth
BaseData$`Time.[Planning Month]` <- ifelse(BaseData$`Time.[Planning Month]`==paste(format(as.Date(Current_lag1),"%b"),year(as.Date(Current_lag1)),sep = "-"),BaseData$`Time.[Planning Month]`,paste(format(as.Date(Current_lag1),"%b"),year(as.Date(Current_lag1)),sep = "-"))
#=======================================================================================================================================================#



#============================================= Six and Twelve Month A&F Deviations =====================================================#
Actuals <- OneYearActualsData %>% group_by(SKU) %>% summarise( MeanActuals = mean(Actuals, na.rm =T))
FForecast <- OneYearForecastData %>% group_by(SKU) %>% summarise( MeanForecast = mean(FForecast, na.rm =T))

#Maxdate to filter data
maxdateA <- max(OneYearActualsData$Week, na.rm = T)
maxdateA <- as.Date(maxdateA) %m-% months(6)
maxdateF <- max(OneYearForecastData$Week, na.rm = T)
maxdateF <- as.Date(maxdateF) %m-% months(6)
Actuals6M <- subset(OneYearActualsData, OneYearActualsData$Week > maxdateA )
Forecast6M <- subset(OneYearForecastData, OneYearForecastData$Week <= maxdateF)

# Group by to calculate average
Actuals6M <- Actuals6M %>% group_by(SKU) %>% summarise( MeanActuals = mean(Actuals, na.rm =T))
Forecast6M <- Forecast6M %>% group_by(SKU) %>% summarise( MeanForecast = mean(FForecast, na.rm =T))

Deviation <- left_join(Actuals,FForecast, by = "SKU")
Deviation6M <- left_join(Actuals6M,Forecast6M, by = "SKU")
Deviation[is.na(Deviation)] <- 0
Deviation$Deviation12M <- ifelse(Deviation$MeanActuals == 0, 1,(Deviation$MeanForecast - Deviation$MeanActuals)/Deviation$MeanActuals)
Deviation6M$deviation <- ifelse(Deviation6M$SKU == 0, 1, (Deviation6M$MeanForecast - Deviation6M$MeanActuals)/Deviation6M$MeanActuals)

Deviation <- rename(Deviation, "Item.[Planning Item]"="SKU","Measure.[Twelve Month Deviation]"="Deviation12M")
Deviation <- Deviation[,c("Item.[Planning Item]","Measure.[Twelve Month Deviation]")]
Deviation6M <- rename(Deviation6M, "Item.[Planning Item]"="SKU","Measure.[Six Month Deviation]"="deviation")
Deviation6M <- Deviation6M[,c("Item.[Planning Item]","Measure.[Six Month Deviation]")]

#=======================================================================================================================================================#


#============================================= LC Based Exceptions =====================================================#

LaggedForecast$MonthName <- substr(LaggedForecast$`Time.Planning Month`,1,3)
LaggedForecast <- subset(LaggedForecast, LaggedForecast$MonthName==month)
LaggedForecast <- subset(LaggedForecast, LaggedForecast$Lag.Lag==2)
if(nrow(LaggedForecast)>0){
  #ActualsData$MonthName = substr(ActualsData$Time, 1,3)
  LCBased <- left_join(LaggedForecast,Forecast, by = c("Item.Planning Item"="SKU","Time.Planning Month"="Time"))
  LCBased$Exception <- abs(LCBased$Lag1_Forecast - LCBased$`Lagged Forecast`)/LCBased$`Lagged Forecast`
  LCBased <- LCBased[,c("Time.Planning Month","Item.Planning Item","Exception")]
  LCBased <- rename(LCBased, "Time.[Planning Month]"="Time.Planning Month","Item.[Planning Item]"="Item.Planning Item","Measure.[LC Deviation]"="Exception")
}
#=======================================================================================================================================================#
FinalException <- left_join(Deviation,Deviation6M)
FinalException <- left_join(FinalException,BaseData)
FinalException <- rename(FinalException,"Measure.[Twelve Month Deviation1]"="Measure.[Twelve Month Deviation]","Measure.[Six Month Deviation1]"="Measure.[Six Month Deviation]")
FinalException$`Time.[Planning Month]` <- ifelse(is.na(FinalException$`Time.[Planning Month]`),paste(format(as.Date(Current_lag1),"%b"),year(as.Date(Current_lag1)),sep = "-"),FinalException$`Time.[Planning Month]`)
FinalException$`Time.[Planning Month]` <- ifelse(FinalException$`Time.[Planning Month]`==paste(format(as.Date(Current_lag1),"%b"),year(as.Date(Current_lag1)),sep = "-"),FinalException$`Time.[Planning Month]`,paste(format(as.Date(Current_lag1),"%b"),year(as.Date(Current_lag1)),sep = "-"))
#FinalException <- FinalException[complete.cases(FinalException),]
#=======================================================================================================================================================#


#============================================= Priority Marking to Exceptions =====================================================#

#Taking Volume Group

Priority <- left_join(FinalException,VolumeData, by = c("Item.[Planning Item]"="Item.Planning Item"))
Priority$`Volume Group` <- ifelse(is.na(Priority$`Volume Group`), "C", Priority$`Volume Group`)
Priority <- subset(Priority, !(Priority$`Volume Group`=="D"))
# Assigning the priority between high, medium and low

UpperThreshold <- Thresholds$`Upper Threshold`
LowerThreshold <- Thresholds$`Lower Threshold`
UpperThreshold <- ifelse(is.na(UpperThreshold),1.2,UpperThreshold)
LowerThreshold <- ifelse(is.na(LowerThreshold),0.7,LowerThreshold)

# Weight variable to decide weightage to be given to lag deviations between recent and avg deviations
LagDeviation_Weights <- 0.75
# Weight variable to to decide weightage to be given to runrate between 6M and 12M
# Greater weights will give more weightage to 12M deviation and less weightage to ^M deviation and vice versa
FF_Weights <- 0.75
#Priority$LagDeviation <- LagDeviation_Weights * (Priority$`Measure.[Deviation wrt to Recent Actual]`) + (1 - LagDeviation_Weights) * Priority$`Measure.[Deviation wrt AvgActuals]`
#Priority$FFDeviation <- FF_Weights * (Priority$`Measure.[Twelve Month Deviation1]`) + (1 - FF_Weights) * Priority$`Measure.[Six Month Deviation1]`
Priority$LagDeviation <- LagDeviation_Weights * (Priority$`Measure.[Deviation wrt to Recent Actual]` - Priority$`Measure.[Average YoY Growth]`) + (1 - LagDeviation_Weights) * Priority$`Measure.[Deviation wrt AvgActuals]`
Priority$FFDeviation <- FF_Weights * (Priority$`Measure.[Twelve Month Deviation1]` - Priority$`Measure.[Average Yearly Growth]`) + (1 - FF_Weights) * Priority$`Measure.[Six Month Deviation1]`


# Priority if else block
# if(Priority$`Forecast Rule` %in% c(1,2) & Priority$LagDeviation > UpperThreshold & Priority$FFDeviation > UpperThreshold){
#   Priority$priority <- "High - Stat Override Recommended!"
# } else if(Priority$`Forecast Rule` %in% c(1,2) & Priority$LagDeviation > UpperThreshold | Priority$FFDeviation > UpperThreshold){
#   
# } else{
#   
# }


# Medium Priority
Priority$priority <- ifelse(Priority$`Volume Group` %in% c("A","B") & (abs(Priority$LagDeviation) > UpperThreshold | abs(Priority$FFDeviation) > UpperThreshold), "Medium - Stat Review Recommended","NA")
Priority$priority <- ifelse(Priority$`Volume Group` %in% c("A","B") & (abs(Priority$LagDeviation) < UpperThreshold & abs(Priority$FFDeviation) < UpperThreshold & abs(Priority$LagDeviation) > LowerThreshold & abs(Priority$FFDeviation) > LowerThreshold), "Medium - Stat Review Recommended" ,Priority$priority)
# Low Volume and both the deviations are greater than upper threshold
Priority$priority <- ifelse(Priority$`Volume Group` %in% c("C") & (abs(Priority$LagDeviation) > UpperThreshold & abs(Priority$FFDeviation) > UpperThreshold), "Medium - Stat Review Recommended",Priority$priority)


#Priority$priority <- ifelse(Priority$`Volume Group` %in% c("A","B") & Priority$LagDeviation > UpperThreshold & Priority$FFDeviation > UpperThreshold, "High - Stat Override Recommended",Priority$priority)
# Low Volume
# Same condition in 5th line wrt to this line
#Priority$priority <- ifelse(Priority$`Volume Group` %in% c("A","B") & (Priority$LagDeviation > LowerThreshold | Priority$FFDeviation > LowerThreshold), "Low - Stat Review Optional",Priority$priority)
Priority$priority <- ifelse(Priority$`Volume Group` %in% c("C") & (abs(Priority$LagDeviation) > UpperThreshold | abs(Priority$FFDeviation) > UpperThreshold), "Low - Stat Review Optional",Priority$priority)

#Priority$priority <- ifelse(Priority$`Volume Group` %in% c("A","B") & (Priority$LagDeviation > UpperThreshold | Priority$FFDeviation > UpperThreshold), "Low - Stat Review Optional",Priority$priority)



# High Volume, Any of the deviations are above lower threshold
Priority$priority <- ifelse(Priority$`Volume Group` %in% c("A","B") & ((abs(Priority$LagDeviation) < UpperThreshold & abs(Priority$FFDeviation) < UpperThreshold) & (abs(Priority$LagDeviation) > LowerThreshold | abs(Priority$FFDeviation) > LowerThreshold)), "Low - Stat Review Optional" ,Priority$priority)

# High Priority
Priority$priority <- ifelse(Priority$`Volume Group` %in% c("A","B") & (abs(Priority$LagDeviation) > UpperThreshold & abs(Priority$FFDeviation) > UpperThreshold), "High - Stat Override Recommended",Priority$priority)


# If volume group is NA, For Putty and Non Segmentable ones
Priority$priority <- ifelse(is.na(Priority$`Volume Group`) & Priority$`Forecast Rule` == 2 & (abs(Priority$LagDeviation) > UpperThreshold | abs(Priority$FFDeviation) > UpperThreshold), "Low - Stat Review Optional",Priority$priority)

# No Exception
Priority$priority <- ifelse(Priority$priority == "NA" | is.na(Priority$priority), "No Exception",Priority$priority)
Priority <- rename(Priority, "Measure.[Seasonal Lag Deviation]"="LagDeviation", "Measure.[Long Term Deviation]"="FFDeviation","Measure.[Exception Priority]"="priority")
Priority <- Priority[,c("Item.[Planning Item]","Time.[Planning Month]","Measure.[Seasonal Lag Deviation]","Measure.[Long Term Deviation]","Measure.[Exception Priority]")]
Priority$`Time.[Planning Month]` <- ifelse(Priority$`Time.[Planning Month]`==paste(format(as.Date(Current_lag1),"%b"),year(as.Date(Current_lag1)),sep = "-"), Priority$`Time.[Planning Month]`,paste(format(as.Date(Current_lag1),"%b"),year(as.Date(Current_lag1)),sep = "-"))
#Priority <- Priority[complete.cases(Priority),]

# Final Exception to be populated
FinalException <- FinalException[,c("Item.[Planning Item]","Time.[Planning Month]","Measure.[Twelve Month Deviation1]","Measure.[Six Month Deviation1]","Measure.[Deviation wrt to Recent Actual]","Measure.[Deviation wrt AvgActuals]")]

if(nrow(LaggedForecast)>0){
  FinalException <- left_join(FinalException,LCBased,by="Item.[Planning Item]")
  FinalException <- FinalException[,c("Item.[Planning Item]","Time.[Planning Month].y","Measure.[Twelve Month Deviation1]","Measure.[Six Month Deviation1]","Measure.[Deviation wrt to Recent Actual]","Measure.[Deviation wrt AvgActuals]","Measure.[LC Deviation]")]
  FinalException <- rename(FinalException, "Time.[Planning Month]"="Time.[Planning Month].y")
} else {
  FinalException$`Measure.[LC Deviation]` <- NA
}
FinalException$`Time.[Planning Month]` <- ifelse(is.na(FinalException$`Time.[Planning Month]`),paste(format(as.Date(Current_lag1),"%b"),year(as.Date(Current_lag1)),sep = "-"),FinalException$`Time.[Planning Month]`)
FinalException$`Time.[Planning Month]` <- ifelse(FinalException$`Time.[Planning Month]`==paste(format(as.Date(Current_lag1),"%b"),year(as.Date(Current_lag1)),sep = "-"),FinalException$`Time.[Planning Month]`,paste(format(as.Date(Current_lag1),"%b"),year(as.Date(Current_lag1)),sep = "-"))

#=======================================================================================================================================================#
#=======================================================================================================================================================#

