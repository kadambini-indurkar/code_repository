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
ActualsData$SKU <- paste(ActualsData$Item, ActualsData$`Sales Domain.[Channel]`,ActualsData$`Item.[L5]`, ActualsData$`Item.[L4]`, sep = "_")
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
maxdate <- "2019-09-01"
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




hh <- strsplit(data1$SKU,"_")
gg <- do.call(rbind,hh)

# To come up with nodes automatically..
data11$Item <- gg[,1]
data11$Country <- gg[,2]
data11$Brand <- gg[,3]
data11$SubBrand <- gg[,4]
# Restart

finalForecast <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(finalForecast) <- c("Date","SKU","Forecast","Method")

# Start of iterations
iter <- 1
forecastperiod <- 24
for(B1 in sort(unique(data11$Brand))){
  #B1 <- as.character(B1)
  data1 <- subset(x=data11, data11$Brand==B1)
  
  # Step 1 to sort the data.
  # Without Lowest Level
  data1 <- data1 %>% arrange(Brand,SubBrand)
  
  # for loop here...filter brand loop wise, melt a/ccharacter/somehow rbind
  library(dplyr)
  t <- data1 %>%
    group_by(SubBrand) %>%
    summarise(n_distinct(Item))
  # c(t$`n_distinct(Item)`)
  t1 <- data1 %>%
    group_by(Brand) %>%
    summarise(n_distinct(SubBrand))
  # This is how lowest level nodes we will get
  data1 <- data1 %>% arrange(SubBrand)
  data2 <- data1[,c("SKU","Date","Sales","SubBrand")]
  nodes <- list(t1$`n_distinct(SubBrand)`,c(t$`n_distinct(Item)`))
  # if(B1=="AF3505E"){
  #   # tsobject <- ts(train[2:length(train)],12)
  #   # etsobj <- ets(tsobject)
  #   # forecast <- forecast(etsobj,h=forecastperiod)
  #   next;
  # 
  # }
  data2 <- data2 %>% arrange(SubBrand)
  #data2 <- data2[,c("SKU","Date","Sales")]
  # Dcast to create series matrix
  # Perfect dcast now...
  dcastData <- reshape2::dcast(data2,Date ~ SubBrand+SKU,value.var = "Sales",add.missing = TRUE, fill = 0)
  
  # Jugaad for grouped series
  
  
  # Splitting Data to train and test sets now
  # Defining number to take last n points for test
  testperiod <- 6
  train <- dcastData[1:(nrow(dcastData)-6),]
  test <- dcastData[(nrow(dcastData)-6):nrow(dcastData),]
  
  
  # Creating hts object for train, test and dcastData(for full forward forecast)
  hts_train <- hts(train[2:length(train)],nodes)
  hts_train$labels$`Level 0` <- t1$Brand
  hts_train$labels$`Level 1` <- t$SubBrand
  hts_test <- hts(test[2:length(test)],nodes)
  hts_test$labels$`Level 0` <- t1$Brand
  hts_test$labels$`Level 1` <- t$SubBrand
  htsobject <- hts(dcastData[2:length(dcastData)], nodes)
  htsobject$labels$`Level 0` <- t1$Brand
  htsobject$labels$`Level 1` <- t$SubBrand
  
  #---------------------------------------------------------------------------
  # Different Methods
  # Bottom Up
  # generate forecast at base level and then get forecast at aggregated levels by just simple sums
  #tryForecast <- forecast(htsobject,h=12,method = "bu", fmethod = "arima")
  
  
  model_train <- forecast(hts_train,h=forecastperiod,method="bu",fmethod = "arima")
  model_train <- forecast(hts_train,h=forecastperiod)
  
  model_test1 <- forecast(hts_test,h=nrow(test),method = "bu", fmethod = "arima",keep.fitted = TRUE, keep.resid = TRUE)
  model_test2 <- forecast(hts_test,h=nrow(test),method = "bu",keep.fitted = TRUE, keep.resid = TRUE)
  
  # Accuracy
  a1 <- accuracy.gts(model_test1,hts_test)
  a1 <- as.data.frame(a1)
  a1$Method <- "BU_ARIMA"
  a2 <- accuracy.gts(model_test2,hts_test)
  a2 <- as.data.frame(a2)
  a2$Method <- "BU_Auto"
  a <- rbind(a1,a2)
  model_dcastData <- forecast(htsobject, h=forecastperiod, method="bu",fmethod="arima")
  model_dcastData$bts <- as.data.frame(model_dcastData$bts)
  model_dcastData$bts$Date <- seq(as.Date(add.month(maxdate,1)), as.Date(add.month(maxdate,forecastperiod)), by="month")
  
  bottomLevelForecast <- reshape2::melt(model_dcastData$bts, id.vars = "Date")
  bottomLevelForecast <- rename(bottomLevelForecast, Forecast = value, SKU=variable)
  bottomLevelForecast$Method <- "BU_ARIMA"
  model_dcastData <- forecast(htsobject, h=forecastperiod, method="bu")
  model_dcastData$bts <- as.data.frame(model_dcastData$bts)
  model_dcastData$bts$Date <- seq(as.Date(add.month(maxdate,1)), as.Date(add.month(maxdate,forecastperiod)), by="month")
  bottomLevelForecast1 <- reshape2::melt(model_dcastData$bts, id.vars = "Date")
  bottomLevelForecast1 <- rename(bottomLevelForecast1, Forecast = value, SKU=variable)
  bottomLevelForecast1$Method <- "BU_Auto"
  
  bts_Forecast <- rbind(bottomLevelForecast, bottomLevelForecast1)
  rm(bottomLevelForecast)
  rm(bottomLevelForecast1)
  # BrandForecast
  # SubBrandForecast

  #---------------------------------------------------------------------------
  
  # ME Mean Error
  # RMSE Root Mean Square Error
  # MAE Mean Absolute Error
  # MAPE Mean Absolute Percentage Error
  # MPE Mean Percentage Error
  # MASE Mean Absolute Scaled Error
  
  # for methods combinations
  
  # Iimportant Link - https://pkg.earo.me/hts/reference/forecast.gts.html
  # TOP DOWN
  # generate forecast at higher levels then split using historical proportions to lower levels
  # tdgsa
  model_train <- forecast(hts_train,h=forecastperiod,method="tdgsa",fmethod = "arima")
  model_train <- forecast(hts_train,h=forecastperiod,method="tdgsa")
  
  model_test1 <- forecast(hts_test,h=nrow(test),method = "tdgsa", fmethod = "arima",keep.fitted = TRUE, keep.resid = TRUE)
  model_test2 <- forecast(hts_test,h=nrow(test),method = "tdgsa",keep.fitted = TRUE, keep.resid = TRUE)
  
  # Accuracy
  a1 <- accuracy.gts(model_test1,hts_test)
  a2 <- accuracy.gts(model_test2,hts_test)
  model_dcastData <- forecast(htsobject, h=forecastperiod, method="tdgsa",fmethod="arima")
  # plot(model_dcastData, levels = 0)
  # plot(model_dcastData, levels = 1)
  # Accuracy
  a1 <- accuracy.gts(model_test1,hts_test)
  a1 <- as.data.frame(a1)
  a1$Method <- "TDGSA_ARIMA"
  a2 <- accuracy.gts(model_test2,hts_test)
  a2 <- as.data.frame(a2)
  a2$Method <- "TDGSA_Auto"
  a <- rbind(a1,a2)
  model_dcastData$bts <- as.data.frame(model_dcastData$bts)
  model_dcastData$bts$Date <- seq(as.Date(add.month(maxdate,1)), as.Date(add.month(maxdate,forecastperiod)), by="month")
  
  bottomLevelForecast <- reshape2::melt(model_dcastData$bts, id.vars = "Date")
  bottomLevelForecast <- rename(bottomLevelForecast, Forecast = value, SKU=variable)
  bottomLevelForecast$Method <- "TDGSA_ARIMA"
  model_dcastData <- forecast(htsobject, h=forecastperiod, method="tdgsa")
  model_dcastData$bts <- as.data.frame(model_dcastData$bts)
  model_dcastData$bts$Date <- seq(as.Date(add.month(maxdate,1)), as.Date(add.month(maxdate,forecastperiod)), by="month")
  bottomLevelForecast1 <- reshape2::melt(model_dcastData$bts, id.vars = "Date")
  bottomLevelForecast1 <- rename(bottomLevelForecast1, Forecast = value, SKU=variable)
  bottomLevelForecast1$Method <- "TDGSA_Auto"
  
  bts_Forecast <- rbind(bts_Forecast, bottomLevelForecast)
  bts_Forecast <- rbind(bts_Forecast, bottomLevelForecast1)
  rm(bottomLevelForecast1)
  rm(bottomLevelForecast)
  
  #---------------------------------------------------------------------------
  # tdgsf
  # https://cran.r-project.org/web/packages/hts/hts.pdf
  model_train <- forecast(hts_train,h=forecastperiod,method="tdgsf",fmethod = "arima")
  model_train <- forecast(hts_train,h=forecastperiod,method="tdgsf")
  
  model_test1 <- forecast(hts_test,h=nrow(test),method = "tdgsf", fmethod = "arima",keep.fitted = TRUE, keep.resid = TRUE)
  model_test2 <- forecast(hts_test,h=nrow(test),method = "tdgsf",keep.fitted = TRUE, keep.resid = TRUE)
  
  
  model_dcastData <- forecast(htsobject, h=forecastperiod, method="tdgsf",fmethod="arima")
  plot(model_dcastData, levels = 0)
  plot(model_dcastData, levels = 1)
  
  
  # Accuracy
  a1 <- accuracy.gts(model_test1,hts_test)
  a2 <- accuracy.gts(model_test2,hts_test)
  a1 <- as.data.frame(a1)
  a1$Method <- "TDGSF_ARIMA"
  a2 <- as.data.frame(a2)
  a2$Method <- "TDGSF_Auto"
  a <- rbind(a1,a2)
  a <- rbind(a,a1)
  a <- rbind(a,a2)
  rm(a1)
  rm(a2)
  model_dcastData$bts <- as.data.frame(model_dcastData$bts)
  model_dcastData$bts$Date <- seq(as.Date(add.month(maxdate,1)), as.Date(add.month(maxdate,forecastperiod)), by="month")
  
  bottomLevelForecast <- reshape2::melt(model_dcastData$bts, id.vars = "Date")
  bottomLevelForecast <- rename(bottomLevelForecast, Forecast = value, SKU=variable)
  bottomLevelForecast$Method <- "TDGSF_ARIMA"
  model_dcastData <- forecast(htsobject, h=forecastperiod, method="tdgsf")
  model_dcastData$bts <- as.data.frame(model_dcastData$bts)
  model_dcastData$bts$Date <- seq(as.Date(add.month(maxdate,1)), as.Date(add.month(maxdate,forecastperiod)), by="month")
  bottomLevelForecast1 <- reshape2::melt(model_dcastData$bts, id.vars = "Date")
  bottomLevelForecast1 <- rename(bottomLevelForecast1, Forecast = value, SKU=variable)
  bottomLevelForecast1$Method <- "TDGSF_Auto"
  
  bts_Forecast <- rbind(bts_Forecast, bottomLevelForecast)
  bts_Forecast <- rbind(bts_Forecast, bottomLevelForecast1)
  rm(bottomLevelForecast1)
  rm(bottomLevelForecast)
  
  #---------------------------------------------------------------------------
  #tdfp
  
  #model_train <- forecast(hts_train,h=forecastperiod,method="tdfp",fmethod = "arima")
  #model_train <- forecast(hts_train,h=forecastperiod,method="tdgsf")
  
  
  #model_test1 <- forecast(hts_test,h=nrow(test),method = "tdfp", fmethod = "arima",keep.fitted = TRUE, keep.resid = TRUE)
  #model_test2 <- forecast(hts_test,h=nrow(test),method = "tdfp",keep.fitted = TRUE, keep.resid = TRUE)
  
  # Accuracy
  # a1 <- accuracy.gts(model_test1,hts_test)
  # a2 <- accuracy.gts(model_test2,hts_test)
  #model_dcastData <- forecast(htsobject, h=forecastperiod, method="tdfp",fmethod="arima")
  # plot(model_dcastData, levels = 0)
  # plot(model_dcastData, levels = 2)
  # Accuracy
  # a1 <- accuracy.gts(model_test1,hts_test)
  # a2 <- accuracy.gts(model_test2,hts_test)
  # a1 <- as.data.frame(a1)
  # a1$Method <- "TDFP_ARIMA"
  # a2 <- as.data.frame(a2)
  # a2$Method <- "TDFP_Auto"
  # a <- rbind(a1,a2)
  # a <- rbind(a,a1)
  # a <- rbind(a,a2)
  # rm(a1)
  # rm(a2)
  # model_dcastData$bts <- as.data.frame(model_dcastData$bts)
  # model_dcastData$bts$Date <- seq(as.Date(add.month(maxdate,1)), as.Date(add.month(maxdate,forecastperiod)), by="month")
  # 
  # bottomLevelForecast <- reshape2::melt(model_dcastData$bts, id.vars = "Date")
  # bottomLevelForecast <- rename(bottomLevelForecast, Forecast = value, SKU=variable)
  # bottomLevelForecast$Method <- "TDFP_ARIMA"
  # model_dcastData <- forecast(htsobject, h=forecastperiod, method="tdfp")
  # model_dcastData$bts <- as.data.frame(model_dcastData$bts)
  # model_dcastData$bts$Date <- seq(as.Date(add.month(maxdate,1)), as.Date(add.month(maxdate,forecastperiod)), by="month")
  # bottomLevelForecast1 <- reshape2::melt(model_dcastData$bts, id.vars = "Date")
  # bottomLevelForecast1 <- rename(bottomLevelForecast1, Forecast = value, SKU=variable)
  # bottomLevelForecast1$Method <- "TDFP_Auto"
  # 
  # bts_Forecast <- rbind(bts_Forecast, bottomLevelForecast)
  # bts_Forecast <- rbind(bts_Forecast, bottomLevelForecast1)
  # rm(bottomLevelForecast1)
  # rm(bottomLevelForecast)
  
  #---------------------------------------------------------------------------
  
  
  # Combination/Optimal Reconciliation/Hierarchical Optimal Reconciliation
  
  model_train <- forecast(hts_train,h=forecastperiod,method="comb",fmethod = "arima")
  model_train <- forecast(hts_train,h=forecastperiod,method="comb")
  
  model_test1 <- forecast(hts_test,h=nrow(test),method = "comb", fmethod = "arima",keep.fitted = TRUE, keep.resid = TRUE)
  model_test2 <- forecast(hts_test,h=nrow(test),method = "comb",keep.fitted = TRUE, keep.resid = TRUE)
  
  # Accuracy
  # a1 <- accuracy.gts(model_test1,hts_test)
  # a2 <- accuracy.gts(model_test2,hts_test)
  model_dcastData <- forecast(htsobject, h=forecastperiod, method="comb",fmethod="arima")
  # plot(model_dcastData, levels = 0)
  # plot(model_dcastData, levels = 1)
  # plot(model_dcastData, levels = 2)
  # Accuracy
  a1 <- accuracy.gts(model_test1,hts_test)
  a2 <- accuracy.gts(model_test2,hts_test)
  a1 <- as.data.frame(a1)
  a1$Method <- "OPTCOMB_ARIMA"
  a2 <- as.data.frame(a2)
  a2$Method <- "OPTCOMB_Auto"
  a <- rbind(a1,a2)
  a <- rbind(a,a1)
  a <- rbind(a,a2)
  rm(a1)
  rm(a2)
  model_dcastData$bts <- as.data.frame(model_dcastData$bts)
  model_dcastData$bts$Date <- seq(as.Date(add.month(maxdate,1)), as.Date(add.month(maxdate,forecastperiod)), by="month")
  
  bottomLevelForecast <- reshape2::melt(model_dcastData$bts, id.vars = "Date")
  bottomLevelForecast <- rename(bottomLevelForecast, Forecast = value, SKU=variable)
  bottomLevelForecast$Method <- "OPTCOMB_ARIMA"
  model_dcastData <- forecast(htsobject, h=forecastperiod, method="comb", fmethod = "ets")
  model_dcastData$bts <- as.data.frame(model_dcastData$bts)
  model_dcastData$bts$Date <- seq(as.Date(add.month(maxdate,1)), as.Date(add.month(maxdate,forecastperiod)), by="month")
  bottomLevelForecast1 <- reshape2::melt(model_dcastData$bts, id.vars = "Date")
  bottomLevelForecast1 <- rename(bottomLevelForecast1, Forecast = value, SKU=variable)
  bottomLevelForecast1$Method <- "OPTCOMB_Auto"
  
  bts_Forecast <- rbind(bts_Forecast, bottomLevelForecast)
  bts_Forecast <- rbind(bts_Forecast, bottomLevelForecast1)
  rm(bottomLevelForecast1)
  rm(bottomLevelForecast)

  finalForecast <- rbind(finalForecast, bts_Forecast)

  ets_forecast <- forecast(ets(ts(dcastData[2:length(dcastData)],12)),12)
  finalForecast <- rbind(finalForecast, ets_forecast)
  #accuracy <- rbind(accuracy,a)
  tq <- t1$Brand
  #fwrite(a,paste(tq,".csv", sep = ""))
  #print(B1)
  iter <- iter+1
}



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