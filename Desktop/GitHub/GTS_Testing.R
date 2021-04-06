## HTS for GSK ###

#install.packages('hts', dependencies = TRUE)
library(hts)
#print(htseg1)
library(data.table)
library(dplyr)
library(mondate)
library(lubridate)
#================================================================================================================================#

# File Reading
ActualsData <- fread("C:\\Users\\kadambini.indurkar\\GSK\\HTS\\SalesData_1Sep2020 (1) (1).csv")
# names(ActualsData)
# [1] "Version.[Version Name]"        "Sales Domain.[Channel]"       
# [3] "Sales Domain.[Customer Group]" "Item.[L5]"                    
# [5] "Item.[L4]"                     "Item.[Transition Item]"       
# [7] "o9 Grouping"                   "Sales Domain.[Ship To]"       
# [9] "Time.[Planning Month]"         "Actual"                       
# [11] "Sell Out Actual"
#================================================================================================================================#


#Rename columns
colnames(ActualsData)[which(names(ActualsData) == "Item.[Transition Item]")] <- "Item"
#ActualsData$Item <- as.character(ActualsData$Item)
ActualsData$SKU <- paste(ActualsData$Item, ActualsData$`Sales Domain.[Channel]`,ActualsData$`Item.[L5]`, ActualsData$`Item.[L4]`,ActualsData$`Sales Domain.[Customer Group]`,ActualsData$`o9 Grouping`, sep = "_")
colnames(ActualsData)[which(names(ActualsData) == "Time.[Planning Month]")] <- "Time"
colnames(ActualsData)[which(names(ActualsData) == "TotalActuals")] <- "Shipment"
#ActualsData <- subset(ActualsData, !(ActualsData$`Sales Domain.[Channel]`=="Malaysia"))
ActualsData$Year <- substr(ActualsData$Time,5,8)
ActualsData$Year1 <- paste("20",ActualsData$Year,sep = "")
ActualsData$Month <- substr(ActualsData$Time,1,3)
ActualsData$Week<-paste(ActualsData$Year1,"-",match(ActualsData$Month,month.abb),"-","01")
ActualsData$Week<-as.Date(ActualsData$Week, format = "%Y - %m - %d")


# ActualsData <- subset(ActualsData,ActualsData$`Sales Domain.[Channel]`=="Germany")
# ActualsData <- subset(ActualsData, ActualsData$`Item.[L5]`=="AIFG02M")
unique(ActualsData$`Item.[L5]`)
unique(ActualsData$`Item.[L4]`)

#================================================================================================================================#

ActualsData1<-ActualsData[,c("SKU","Week","Shipment")]

# Storing ActualsData1 to actuals, ActualsData1 is input data backup
actuals1<-ActualsData1
colnames(actuals1)<-c("PRODUCT_NUM_OBS","C445_WK_STRT_DATE","POS_GROSS_SALES_QTY")
#================================================================================================================================#


#===================================== Reformat data column and group by - by SKU and Date ======================================#

# Reformat date column to date type
actuals1$C445_WK_STRT_DATE<-as.Date(actuals1$C445_WK_STRT_DATE)

# Group data by SKU and Date
actuals1<-actuals1%>%group_by(PRODUCT_NUM_OBS,C445_WK_STRT_DATE)%>%summarise(POS_GROSS_SALES_QTY =sum(POS_GROSS_SALES_QTY,na.rm=T))
#================================================================================================================================#


#============================= Calculating last month of Input data =============================================================#

# This will be used to subset data 
maxdate<-max(actuals1$C445_WK_STRT_DATE)

# Current month
#maxdate <- format(as.Date(CurrentDate$DPCurrentDate),"%Y-%m-01")
#maxdate <- "2019-10-01"
maxdate <- as.Date(maxdate)

# data till past month (so taking maxdate to lastmonth)
maxdate<-as.Date(maxdate) %m-% months(1)
#================================================================================================================================#


#============================== Filtering Shipment data of last 36 months =======================================================#

# subset data using maxdate
actuals1<-subset(actuals1,actuals1$C445_WK_STRT_DATE<=maxdate)
#dateprev36<-as.Date(maxdate) %m-% months(36)
#actuals1<-subset(actuals1,actuals1$C445_WK_STRT_DATE>=dateprev36)
#================================================================================================================================#


#=========================== Functions to calculate month difference ==============================================================#

# Add/Subtract "n" months to a Date
add.month=function(date,n=num_months_to_add) as.Date(seq(date, by= paste (n, "months"), length=2)[2])
subtract.month=function(date,n=num_months_to_add) as.Date(seq(date, by= paste (-n, "months"), length=2)[2])
add.week=function(date,n=num_weeks_to_add) as.Date(seq(date, by= paste (n, "weeks"), length=2)[2])
subtract.week=function(date,n=num_weeks_to_add) as.Date(seq(date, by= paste (-n, "weeks"), length=2)[2])
#================================================================================================================================#


#=========================== Handling NULLs and Negatives in data ===============================================================#
colnames(actuals1) <- c("SKU","Date","Qty")
OutlierCorrectedValue <- data.frame()
#actuals1[actuals1$Qty<0] <- 0
actuals1 <- as.data.frame(actuals1)
actuals1[,c(colnames(select_if(actuals1,is.numeric)))<0] <- 0
#,colnames(select_if(forecastoutputfile1,is.numeric))
actuals1[is.na(actuals1)] <- 0
OutlierCorrectedValue <- actuals1
OutlierCorrectedValue$Qty <- as.numeric(OutlierCorrectedValue$Qty)
#================================================================================================================================#


# Dcast Data



data1 <- OutlierCorrectedValue %>% group_by(SKU, Date) %>% summarise(Sales =sum(Qty,na.rm=T))
temp_df <- reshape2::dcast(data1,SKU ~ Date,value.var = "Sales",add.missing = TRUE, fill = NA)
tempdf_melt <- reshape2::melt(temp_df, id.vars = "SKU")

# start and end date
startdate <- data1 %>% group_by(SKU) %>% summarise(Start_Date = min(Date,na.rm = T))
tempdf_melt1 <- left_join(tempdf_melt,startdate,by="SKU")
tempdf_melt1$value1 <- ifelse(is.na(tempdf_melt1$value) & as.Date(tempdf_melt1$variable)>=as.Date(tempdf_melt1$Start_Date),0,tempdf_melt1$value)
tempdf_melt1 <- tempdf_melt1[,c("SKU","variable", "value1")]
colnames(tempdf_melt1) <- c("SKU","Date","Sales")
data1 <- tempdf_melt1
data1 <- data1[complete.cases(data1),]
rm(tempdf_melt1)
rm(tempdf_melt)
rm(temp_df)


# Like a pivot table, where SKU is in row, Week is in column with value as Sales(Shipment)
data1_backup <- data1
data11 <- data1
# Dcast to create series matrix
dcastData <- reshape2::dcast(data1,Date ~ SKU,value.var = "Sales",add.missing = TRUE, fill = 0)

# https://rdrr.io/cran/hts/src/R/hts.R link saying no NA in series.


# What should be done

#1. can be sorted by date. 
#2. Some text mining grouping is needed for aggregations.
#3. characters vector use for proper nomenclature to be used.
#4. Sort by Brand and Sub Brand, Then grouping can be logical/automated.

# Information
#1. aggts() is used to extract the ts aggregated at specified level.
# nodes <- list(5,c(1,1,10,3,4))
# 
# #Train
# htsobject <- hts(dcastData[,2:20],nodes)
# #Test
# 
# 
# tryForecast <- forecast(htsobject,h=12,method = "comb", fmethod = "arima")
# tryForecast <- forecast(htsobject,h=12,method = "comb")
# 
# tryForecast1 <- forecast(htsobject,h=12,method = "comb", fmethod = "arima", keep.fitted = T, keep.resid = T)
# 
# 
# # for this I should have the test data set and train data set
# accuracy.gts(tryForecast,tryForecast)
# #gh <- hts(dcastData)
# plot(tryForecast, levels=1)
#================================================================================================================================#




hh <- strsplit(data11$SKU,"_")
gg <- do.call(rbind,hh)

# To come up with nodes automatically..
data11$Item <- gg[,1]
data11$Country <- gg[,2]
data11$Brand <- gg[,3]
data11$SubBrand <- gg[,4]
data11$CG <- gg[,5]
data11$o9Grouping <- gg[,6]
# Restart

finalForecast <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(finalForecast) <- c("Date","SKU","Forecast","Method")
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------

# GTS groupings

str(data11)
summary(data11)
#gts_data <- ActualsData[,c("Sales Domain.[Channel]","Sales Domain.[Customer Group]","Item.[L5]","Item.[L4]","Item","o9 Grouping","Sales Domain.[Ship To]","Week","Shipment")]

names(data11)
# summary <- data11 %>%
#   group_by(SKU) %>% 
#   summarise_each(funs(n_distinct(.)))

# Like an association
summary_table <- data11 %>%
  group_by(SKU) %>% 
  summarise(Sales = sum(Sales, na.rm = T))


hh <- strsplit(summary_table$SKU,"_")
gg <- do.call(rbind,hh)

# To come up with nodes automatically..
summary_table$Item <- gg[,1]
summary_table$Country <- gg[,2]
summary_table$Brand <- gg[,3]
summary_table$SubBrand <- gg[,4]
summary_table$CG <- gg[,5]
summary_table$o9Grouping <- gg[,6]

# 3 country 9 brands
# Country <- c("Australia","Germany","Poland")
# Brand <- unique(summary$Brand)
# SubBrand <- unique(summary$SubBrand)
# CG <- unique(summary$CG)
# Item <- unique(summary$Item)

# I think the easy way, add and then divide

Country <- summary_table$Country
Brand <- summary_table$Brand
SubBrand <- summary_table$SubBrand
CG <- summary_table$CG
o9Grouping <- summary_table$o9Grouping
Item <- summary_table$Item

# lowest levels se sort karke if distinct then usi level ko we can use for dcast..

# groups matrix

grp <- rbind(Country, CG, Brand, SubBrand, o9Grouping, Item)


# Keep on rbind and iterate
# What is n?


# Data Matrix for for gts
# sort data by sku/lowest level
data11 <- data11 %>% arrange(SKU)
dcastData <- reshape2::dcast(data11,Date ~ SKU,value.var = "Sales",add.missing = TRUE, fill = 0)


# GTS Object

# Train test Data
testperiod <- 6
forecastperiod <- 12
train <- dcastData[1:(nrow(dcastData)-6),]
test <- dcastData[(nrow(dcastData)-6):nrow(dcastData),]

gts_train <- gts(train[,2:length(dcastData)], groups = grp)
gts_test <- gts(test[,2:length(dcastData)], groups = grp)

forecastTrain <- forecast(gts_train, h=12)

gtsobject <- gts(dcastData[,2:length(dcastData)], groups = grp)
#View(gtsobject$bts)
model_test <- forecast(gts_test,h=nrow(test),method = "comb", fmethod = "arima",keep.fitted = TRUE, keep.resid = TRUE)


model_train <- forecast(gts_train,h=7,method = "comb", fmethod = "arima",keep.fitted = TRUE, keep.resid = TRUE)

forecast1 <- accuracy.gts(model_train,gts_test)


model_dcastData <- forecast(gtsobject, h=12,method = "comb")


# Error Metrics

error_metric <- accuracy.gts(model_dcastData,gtsobject)

#Forecast from all the levels
View(allts(model_dcastData))


# Store Forecast
model_dcastData$bts <- as.data.frame(model_dcastData$bts)
model_dcastData$bts$Date <- seq(as.Date(add.month(maxdate,1)), as.Date(add.month(maxdate,forecastperiod)), by="month")
bottomLevelForecast <- reshape2::melt(model_dcastData$bts, id.vars = "Date")

bottomLevelForecast$MonthName <- as.POSIXct(bottomLevelForecast$Date)
bottomLevelForecast$MonthName1 <- format(bottomLevelForecast$MonthName,"%b")
bottomLevelForecast$Year <- format(bottomLevelForecast$MonthName,"%Y")
bottomLevelForecast$Month <- paste(bottomLevelForecast$MonthName1,substr(bottomLevelForecast$Year,3,4),sep = "-")

uu <- strsplit(as.character(bottomLevelForecast$variable), "_")
jj <- do.call(rbind,uu)
bottomLevelForecast$Item <- jj[,1]
bottomLevelForecast$Version <- "CurrentWorkingView"
bottomLevelForecast <- rename(bottomLevelForecast, "Version.[Version Name]"=Version, "Time.[Planning Month]"=Month,"Measure.[GTS Forecast]"=value, "Item.[Transition Item]"=Item)
FinalForecast <- bottomLevelForecast[,c("Version.[Version Name]","Item.[Transition Item]","Time.[Planning Month]","Measure.[GTS Forecast]")]
#fg <- allts(model_dcastData$bts, forecasts = T)


# to count how many ncols in matrix is possible??
# if sku is unique per country then direct sum of any group by is count, else to know the count
# we can map it using some association





# https://stackoverflow.com/questions/31395444/r-hts-package-combinef-and-aggts-not-working-with-gts-object
# must read - https://robjhyndman.com/papers/Foresight-hts-final.pdf
# https://cran.r-project.org/web/packages/hts/hts.pdf



# ToDo
# step 1 - allts se sabka forecasts mil jayega
# step 2 - characters argument ko dekhna hai for proper labelling
# step 3 - melt and then add dates
# Seasonality Jugaad - https://community.rstudio.com/t/hierarchical-time-series-forecast-using-hts-library-with-fourier-as-xreg/46552/2
# Imp - https://stackoverflow.com/questions/33062141/hierarchical-time-series
# https://medium.com/brillio-data-science/forecasting-hierarchical-time-series-using-r-598828dba435
# https://pkg.earo.me/hts/reference/gts-class.html