
#============================== Loading necessary libraries =====================================================================#
library(forecast)
library(dplyr)
library(data.table)
library(reshape2)
library(lubridate)
library(caret)
library(mondate)
library(Metrics)
library(Matrix)
library(gtools)
library(tsintermittent)
library(thief)
library(MAPA)
library(readxl)
#================================================================================================================================#


#============================== Error function to calculate mape ================================================================#

#mape <- function(actual,predicted){
# out1 <- mean(abs((actual-predicted)/actual) * 100)
#}
#================================================================================================================================#


#============================== Reading Input Data ==============================================================================#
#ActualsData <- read.csv("C:/Users/kadambini.indurkar/GSK/Forecasting-GSK/ActualData-Sell_In.csv", check.names = F)
#ActualsData <- read.csv("C:/Users/kadambini.indurkar/GSK/Forecasting-GSK/Fact.Sell Out.csv", check.names = F)
#ActualsData <- fread("C:\\Users\\kadambini.indurkar\\GSK\\HierarchicalForecast-v1\\Sub-BrandAndSKU.csv")
#ActualsData <- fread("C:\\Users\\kadambini.indurkar\\GSK\\Future Cycles Forecast\\LatestSKU_ActualsData.csv")

#ActualsData <- read_xlsx("C:\\Users\\kadambini.indurkar\\GSK\\o9 grouping Iteration\\SalesData_1Aug2020 (1).xlsx", sheet = "Sheet2")
#ActualsData <- fread("C:\\Users\\kadambini.indurkar\\GSK\\o9 grouping Iteration\\Fact.Actuals60000000112925.csv")
#ActualsData <- fread("C:\\Users\\kadambini.indurkar\\GSK\\Phase II - Regeneration\\SalesData_1Sep2020 (1) (1).csv")
#ActualsData <- fread("C:/Users/kadambini.indurkar/GSK/Phase II - Regeneration/Phase II Brand and Item/Brand Actuals Sellin and Sellout_12Oct.csv")
#ActualsData <- fread("C:/Users/kadambini.indurkar/GSK/Phase II - Regeneration/Phase II Brand and Item/Item Banner Level Actuals Sellin And Sellout_12Oct.csv")

#ActualsData <- fread("C:\\Users\\kadambini.indurkar\\GSK\\Phase II - Regeneration\\Final Files Compilation\\Accuracy Analysis\\APAC Data\\Australia_Data.csv")
#ActualsData <- fread("C:/Users/kadambini.indurkar/GSK/PSF/SellinData.csv")
#ActualsData <- fread("C:\\Users\\kadambini.indurkar\\GSK\\Phase II - Regeneration\\Final Files Compilation\\Accuracy Analysis\\APAC Data\\panadol_Australi.csv")

# ActualsDataM <- subset(ActualsData, ActualsData$`Sales Domain.[Channel]`=="Malaysia")
# ActualsData <- subset(ActualsData, !(ActualsData$`Sales Domain.[Channel]`=="Malaysia"))
# # o9 grouping format
# o9grouping <- read_xlsx("C:\\Users\\kadambini.indurkar\\GSK\\o9 grouping Iteration\\Item Master Data-3007.xlsx", sheet = "Updated")

#Master_Join <- left_join(ActualsData,o9grouping) 
#colnames(ActualsData) <- c("SKU","Time","Shipment")
#colnames(ActualsData)[which(names(ActualsData) == "Item.[Transition Item]")] <- "SKU"

# for sell in
#ActualsData <- subset(ActualsData, (ActualsData$`Sales Domain.[Channel]`=="Malaysia"))
#ActualsData$SKU <- paste(ActualsData$`Item.[L5]`, ActualsData$Country, sep = "_")

# Filter Countries
#ActualsData <- subset(ActualsData, (ActualsData$`Sales Domain.[Channel]` %in% c("Germany", "Poland")))
# ActualsData <- subset(ActualsData, (ActualsData$`Sales Domain.[Channel]`=="Australia"))
# ActualsData <- subset(ActualsData, (ActualsData$`Sales Domain.[Channel]` %in% c("Australia", "Malaysia")))
#ActualsData <- subset(ActualsData, (ActualsData$`Sales Domain.[Channel]` %in% c("Australia", "Germany", "Poland")))
# colnames(ActualsData)[which(names(ActualsData) == "Time.[Planning Month]")] <- "Time"
# #colnames(ActualsData)[which(names(ActualsData) == "Actual")] <- "Shipment"
# colnames(ActualsData)[which(names(ActualsData) == "Actual")] <- "Shipment"
# #ActualsData <- subset(ActualsData,ActualsData$`Item.[Transition Item]` %in% c("526077","522617","530644","514196","515774","531036","532409") )
# #ActualsData <- subset(ActualsData,ActualsData$`Item.[Transition Item]`=="60000000108361")
# #ActualsData$SKU <- paste(ActualsData$`Item.[Transition Item]`,ActualsData$`Sales Domain.[Channel]`, sep = "_")
# ActualsData$SKU <- ActualsData$Item



#================================================================================================================================#


#ActualsData <- fread("C:\\Users\\kadambini.indurkar\\NBTY\\Fact.DownloadResult (1).csv")
ActualsData <- fread("C:\\Users\\kadambini.indurkar\\NBTY\\Data\\ShipmentsAdjusted_0104.csv")

#ActualsData <- fread("C:\\Users\\kadambini.indurkar\\NBTY\\Data\\AggressiveOutlierCorrection.csv")

colnames(ActualsData)[which(names(ActualsData) == "Item.[Planning Item]")] <- "Item"
colnames(ActualsData)[which(names(ActualsData) == "Time.[Month]")] <- "Time"
colnames(ActualsData)[which(names(ActualsData) == "Shipments Adjusted")] <- "QTY"
#colnames(ActualsData)[which(names(ActualsData) == "Actual Median")] <- "QTY"

# ActualsData <- fread("C:\\Users\\kadambini.indurkar\\NBTY\\Final Data\\Zero_Filled_Data_NBTY_1502_Month.csv")
# ActualsData <- rename(ActualsData, Item_Number = SKU)
# Define SKU here
ActualsData$SKU <- ActualsData$`Item.[A2]`

#============================== Formatting date column =========================================================#

# # Berger Specific Time formatting
# ActualsData$Year <- year(mdy(ActualsData$Time))
# ActualsData$Month<-month(mdy(ActualsData$Time))
# ActualsData$Week<-paste(ActualsData$Year,ActualsData$Month,"01",sep="-")
# ActualsData$Week<-as.Date(ActualsData$Week)

ActualsData$Year <- substr(ActualsData$Time,5,8)
ActualsData$Year1 <- paste("20",ActualsData$Year,sep = "")
ActualsData$Month <- substr(ActualsData$Time,1,3)
ActualsData$Week<-paste(ActualsData$Year1,"-",match(ActualsData$Month,month.abb),"-","01")
ActualsData$Month<-as.Date(ActualsData$Week, format = "%Y - %m - %d")

# New dataframe ActualsData1 contains only three columns i.e.,
# SKU, Week and Shipment
ActualsData1<-ActualsData[,c("SKU","Month","QTY")]
# This will be used to subset data 
#maxdate<-max(actuals1$C445_WK_STRT_DATE)
k <- 1
cycles <- 10
initial_date <- "2020-11-01"
for(k in 1:cycles){
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
  
  
  maxdate <- as.Date(initial_date) %m+% months(k)
  maxdate1 <- maxdate # just to write for file, month cycle of forecast
  #maxdate <- "2020-10-01" # 6...7...8
  # Current month
  #maxdate <- format(as.Date(CurrentDate$DPCurrentDate),"%Y-%m-01")
  maxdate <- as.Date(maxdate)
  
  # data till past month (so taking maxdate to lastmonth)
  maxdate<-as.Date(maxdate) %m-% months(1)
  #================================================================================================================================#
  
  # One more thing to add Kadambini is sequence time ka merge karna hai...
  #============================== Filtering Shipment data of last 36 months =======================================================#
  
  # subset data using maxdate
  actuals1<-subset(actuals1,actuals1$C445_WK_STRT_DATE<=maxdate)
  # dateprev36<-as.Date(maxdate) %m-% months(36)
  # actuals1<-subset(actuals1,actuals1$C445_WK_STRT_DATE>=dateprev36)
  #================================================================================================================================#
  
  
  #=========================== Reading Segmentation Input generated from SegmentationPlugin =======================================#
  #seg_analysis_vf <- SegmentationInput
  #seg_analysis_vf <- read.csv("C:/Users/kadambini.indurkar/GSK/Forecasting-GSK/SegmentationFileWithRule.csv", check.names = F)
  
  #seg_analysis_vf <- read.csv("C:\\Users\\kadambini.indurkar\\GSK\\o9 grouping Iteration\\O9Grouping_Segmentation.csv")
  #seg_analysis_vf$`Item.[Planning Item]` <- dput(seg_analysis_vf$`Item.[Planning Item]`)
  # seg_analysis_vf$SKU <- paste(as.character(seg_analysis_vf$`Item.[Planning Item]`),seg_analysis_vf$`Item.[L6]`,seg_analysis_vf$`Sales Domain.[Channel]`,seg_analysis_vf$`Sales Domain.[Ship To]`,sep = "_")
  # colnames(seg_analysis_vf)[which(names(seg_analysis_vf) == "PLC Status")] <- "PLC"
  # colnames(seg_analysis_vf)[which(names(seg_analysis_vf) == "Forecast Rule")] <- "Rule"
  # seg_analysis_vf <- seg_analysis_vf[,c("SKU","Rule","PLC")]
  # #===============================================================================================================================
  
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
  
  
  #============================ Creating Dataframes to store output results =======================================================#
  
  # To store RMSE values
  OutputSKURMSE<-data.frame(matrix(ncol =24, nrow = 0))
  colnames(OutputSKURMSE)<-c("SKU","Average","Moving Average","Snaive","Theta","STLM","SES","DES","TES","DES Damped","TES Damped","ARIMA","TBATS","STLF","Croston","Best Fit","Thief Auto", "Thief Arima","Thief Naive", "Thief stlf", "Thief tbats","TSB","ETS","SMA")
  
  # To store Algorithmic model parameters
  StatParameters<-data.frame(matrix(ncol =23, nrow = 0))
  colnames(StatParameters)<-c("SKU","Average","Moving Average","Snaive","Theta","STLM","SES","DES","TES","DES Damped","TES Damped","ARIMA","TBATS","STLF","Croston","Thief Auto", "Thief Arima","Thief Naive", "Thief stlf", "Thief tbats","TSB","ETS","SMA")
  
  # To store final forecast output
  FinalForecastperSKU1<-data.frame(matrix(ncol = 29, nrow = 0))
  colnames(FinalForecastperSKU1)<-c("SKU","Date","Rule","Algorithms","RuleDesc","Average","Moving Average","Snaive","Theta","STLM","SES","DES","TES","DES Damped","TES Damped","ARIMA","TBATS","STLF","Croston","Best Fit Algorithm","Best Fit","Thief Auto", "Thief Arima","Thief Naive", "Thief stlf", "Thief tbats","TSB","ETS","SMA")
  
  # To store forecast/fitted values of testing period
  FinalForecastperSKUtesting1<-data.frame(matrix(ncol = 29, nrow = 0))
  colnames(FinalForecastperSKUtesting1)<-c("SKU","Date","Rule","Algorithms","RuleDesc","Average","Moving Average","Snaive","Theta","STLM","SES","DES","TES","DES Damped","TES Damped","ARIMA","TBATS","STLF","Croston","Best Fit Algorithm","Best Fit","Thief Auto", "Thief Arima","Thief Naive", "Thief stlf", "Thief tbats","TSB","ETS","SMA")
  
  # Create Container Matrices for forecast and testing period for each SKU inside FOR loop (intermeditate data frames)
  forecastoutputfile1<-data.frame(matrix(ncol =28, nrow = 0))
  colnames(forecastoutputfile1)<-c("SKU","Date","Rule","RuleDesc","Average","Moving Average","Snaive","Theta","STLM","SES","DES","TES","DES Damped","TES Damped","ARIMA","TBATS","STLF","Croston","Best Fit Algorithm","Best Fit", "Thief Auto", "Thief Arima","Thief Naive", "Thief stlf", "Thief tbats","TSB","ETS","SMA")
  
  forecastoutputfiletesting1<-data.frame(matrix(ncol =28, nrow = 0))
  colnames(forecastoutputfiletesting1)<-c("SKU","Date","Rule","RuleDesc","Average","Moving Average","Snaive","Theta","STLM","SES","DES","TES","DES Damped","TES Damped","ARIMA","TBATS","STLF","Croston","Best Fit Algorithm","Best Fit","Thief Auto", "Thief Arima","Thief Naive", "Thief stlf", "Thief tbats","TSB","ETS","SMA")
  
  #================================================================================================================================#
  
  #========================= Casting of Input Data ==============================================================================#
  
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
  #================================================================================================================================#
  
  # #========================= Casting of Input Data ==============================================================================#
  # 
  # data1 <- OutlierCorrectedValue %>% group_by(SKU, Date) %>% summarise(Sales =sum(Qty,na.rm=T))
  # 
  # # Like a pivot table, where SKU is in row, Week is in column with value as Sales(Shipment)
  # data1 <- dcast(data1, SKU ~ Date, value.var="Sales", add.missing=TRUE, fill=0)
  # data1_backup <- data1
  # #================================================================================================================================#
  
  
  #========================= Filter out EOL SKUs, i.e., No forecast for EOL parts ===============================================#
  #EOL <- subset(seg_analysis_vf, seg_analysis_vf$PLC == "EOL")
  #data1 <- subset(data1, !(data1$SKU %in% EOL$SKU) )
  #================================================================================================================================#
  
  
  #========================= Loop to calculate forecasts results for each SKU ===================================================#
  
  # iteration variable to be used in for loop
  iter<-1
  
  # n months forecasting period ahead of last months
  forecastperiod <- 26
  
  # period of seasonality
  frequency <- 12
  
  # initialization of vectors to zero
  vec1 <- 0
  hec1 <- 0
  
  for(FSKU in sort(unique(data1$SKU))) {
    
    # skuonlydata stores, actuals data of a SKU of all dates in columns
    # skuonlydata=subset(x=data1,(data1$SKU==FSKU))
    # skuData = skuonlydata[1,]
    # size<-ncol
    #FSKU <- "AF3505E_Germany"
    #FSKU <- "60000000112925"
    #FSKU <- "60000000118433"
    skuonlydata=subset(x=data1,(data1$SKU==FSKU))
    skuonlydata <- reshape2::dcast(skuonlydata,  SKU ~ Date, value.var = "Sales")
    skuData = skuonlydata[1,]
    size<-ncol(skuData)
    
    
    # skuData= skuonlydata[1,]
    # # actual occurrence
    # actualOccurrence <- melt(skuData, id.vars = "SKU")
    # # actualOccurrence <- actualOccurrence %>%
    # #   arrange(variable) %>%
    # #   dplyr::filter(value!=0) %>%
    # #   top_n(length(actualOccurrence)+1, variable)
    # actualOccurrence <- actualOccurrence %>%
    #   arrange(variable) %>%
    #   filter(row_number() >= min(row_number()[value != 0])) %>%
    #   ungroup()
    # skuData <- dcast(actualOccurrence, SKU ~ variable, value.var = "value", add.missing=T, fill = 0)
    # size<-ncol(skuData)
    
    
    # in sample last 12 months
    trainperiod <- 6 
    trainperiod <- ifelse(trainperiod>=size,size-1,trainperiod)
    #trainperiod <- ifelse()
    #subrul<-subset(seg_analysis_vf,seg_analysis_vf$SKU == FSKU)
    # 
    # # Taking rulenum generated from segmentation
    # rulenum<-as.numeric(subrul$Rule)
    # rulenum <- ifelse(is.null(rulenum),4,rulenum)
    # rulenum <- ifelse(is.na(rulenum),4,rulenum)
    
    # parameter to calculate test period of a SKU
    testcol<-(size - trainperiod) +1
    
    # if testcol is 1, then index is 2 because 1 is position for SKU name
    testcol <- ifelse(testcol==1,2,testcol)
    
    # Whole data points
    org<-ts(as.numeric(t(skuData[1,2:(size)])),f=frequency)
    # Outlier Correction
    #org <- tsclean(org, replace.missing = T)
    if(sum(org)==0){
      next;
    }
    # Testing Period
    test<-(skuData[1,testcol:size])
    
    # Initialize RMSE and parameter Data frame for SKU,
    OutputSKURMSE[iter,"SKU"]<-FSKU
    StatParameters[iter,"SKU"]<-FSKU
    z<-melt(test, id.vars = NULL)
    # if(length(rulenum)==0){
    #   next;
    # }
    
    # small corner case check - if all the data points is zero for a particular SKU 
    # then skip that SKU and proceed to next one
    tempvec<-skuData[1,(2:length(skuData))]
    sumvec<-sum(tempvec)
    if(sumvec==0){
      next;
    }
    
    #================================== Trend Calculation =====================================#
    # data of first 12 months, next 12 months and last 12 months
    Y1<-org[1:12]
    Y2<-org[13:24]
    Y3<-org[25:36]
    # Taking sum of data
    SUM1<-sum(Y1,na.rm = TRUE)
    SUM2<-sum(Y2,na.rm = TRUE)
    SUM3<-sum(Y3,na.rm = TRUE)
    # calculating trend1 and trend 2
    TREND1<-(SUM2-SUM1)/SUM1
    TREND2<-(SUM3 - SUM2)/SUM2
    # capping it to range -0.10 to 0.10
    L = -0.10
    U = 0.10
    TREND1<-ifelse(TREND1 > U,U,TREND1)
    TREND1<-ifelse(TREND1 < L,L,TREND1)
    TREND2<-ifelse(TREND2 > U,U,TREND2)
    TREND2<-ifelse(TREND2 < L,L,TREND2)
    
    forecastoutputfile1[1:forecastperiod,1] <- FSKU
    forecastoutputfiletesting1[1:trainperiod,1] <- FSKU
    
    vec1<-seq(as.Date(add.month(maxdate,1)), as.Date(add.month(maxdate,forecastperiod)), by="month")
    hec1<-seq(as.Date(subtract.month(maxdate, trainperiod-1)), as.Date(maxdate), by="month")
    # kahi aur par initialize karke call karte hain
    forecastoutputfile1$Date<-vec1
    #forecastoutputfiletesting1$Date <-hec1
    # forecastoutputfile1$Rule <- rulenum
    # forecastoutputfiletesting1$Rule <- rulenum
    # 
    if(length(org)> 2 * frequency){
    # For thief Testing
    thiefauto<-thief(org,h=forecastperiod)
    fit <- tail(fitted(thiefauto),trainperiod)
    #forecastoutputfiletesting1$`Thief Auto`<-as.numeric(fit)
    #OutputSKURMSE[iter,"ARIMA"]<-mape(test,fit)
    OutputSKURMSE[iter,"Thief Auto"]<-rmse(z[,2],fit)
    k<-forecast(thiefauto,forecastperiod)
    forecastoutputfile1$`Thief Auto`<-as.numeric(k$mean)
    
    # Thief with Naive
    thief1<-thief(org,h=forecastperiod, usemodel = c("naive"))
    fit <- tail(fitted(thief1),trainperiod)
    #forecastoutputfiletesting1$`Thief Naive`<-as.numeric(fit)
    #OutputSKURMSE[iter,"ARIMA"]<-mape(test,fit)
    OutputSKURMSE[iter,"Thief Naive"]<-rmse(z[,2],fit)
    k<-forecast(thief1,forecastperiod)
    forecastoutputfile1$`Thief Naive`<-as.numeric(k$mean)
    }
    
    #print("Hi")
    #~~~~~~~~~~~~~~~~~~~ DES Damped ~~~~~~~~~~~~~~~~#
    
    modDesdam<-ets(org,model='AAN',damped = TRUE,lower=c(0.0,0.0,0,0.75), upper=c(1,1,1,0.95))
    fit <- tail(fitted(modDesdam), trainperiod)
    #forecastoutputfiletesting1$`DES Damped`<-fit
    #OutputSKURMSE[iter,"DES Damped"]<-mape(test,fit)
    #print("Hi2")
    modDesdam
    if(length(org)==1){
      OutputSKURMSE[iter,"DES Damped"]<-rmse(z[,1],fit)
    } else {
      OutputSKURMSE[iter,"DES Damped"]<-rmse(z[,2],fit)
    }
    k<-forecast(modDesdam,forecastperiod)
    forecastoutputfile1$`DES Damped`<-as.numeric(k$mean)
  
    
    
    # Thief With Arima
    if(length(org)>26) {
      # Thief with Naive
      thief2<-thief(org,h=forecastperiod, usemodel = c("arima"))
      fit <- tail(fitted(thief2),trainperiod)
      #forecastoutputfiletesting1$`Thief Arima`<-as.numeric(fit)
      #OutputSKURMSE[iter,"ARIMA"]<-mape(test,fit)
      OutputSKURMSE[iter,"Thief Arima"]<-rmse(z[,2],fit)
      k<-forecast(thief2,forecastperiod)
      forecastoutputfile1$`Thief Arima`<-as.numeric(k$mean)
    }
    
    # MAPA try with Thief STLF and Thief TBATS column name ;) 
    if(length(org)>24){
      mapaforecast <- mapaest(org)
      inSample <- mapafor(org,mapaforecast,fh=forecastperiod)
      fit <- inSample$infor[,seq(length(inSample$infor)-trainperiod+1,length(inSample$infor))]
      # thief3<-thief(org,h=12,forecastfunction = mystlf)
      # fit <- tail(fitted(thief3),trainperiod)
      #forecastoutputfiletesting1$`Thief stlf`<-ifelse(is.null(as.numeric(fit)),0,as.numeric(fit))
      # #OutputSKURMSE[iter,"ARIMA"]<-mape(test,fit)
      OutputSKURMSE[iter,"Thief stlf"]<-rmse(z[,2],fit)
      # k<-forecast(thief3,forecastperiod)
      forecastoutputfile1$`Thief stlf`<- inSample$outfor
    }
    #-------------- Forecasting Algorithms In Sample Best Fit --------------------#
    
    
    #~~~~~~~~~~~~~~~ ARIMA ~~~~~~~~~~~~~~~~~~~#
    # https://stats.stackexchange.com/questions/178577/how-to-read-p-d-and-q-of-auto-arima
    if(length(org)!=1) {
    modelarima<-auto.arima(org)
    fit <- tail(fitted(modelarima),trainperiod)
    #forecastoutputfiletesting1$ARIMA<-as.numeric(fit)
    #OutputSKURMSE[iter,"ARIMA"]<-mape(test,fit)
    OutputSKURMSE[iter,"ARIMA"]<-rmse(z[,2],fit)
    k<-forecast(modelarima,forecastperiod)
    forecastoutputfile1$ARIMA<-(k$mean)
    
    modDesdam<-ets(org,model='AAN',damped = TRUE,lower=c(0.0,0.0,0,0.85), upper=c(1,1,1,0.95))
    fit <- tail(fitted(modDesdam), trainperiod)
    #forecastoutputfiletesting1$`DES Damped`<-fit
    #OutputSKURMSE[iter,"DES Damped"]<-mape(test,fit)
    if(length(org)==1){
      OutputSKURMSE[iter,"DES Damped"]<-rmse(z[,1],fit)
    } else {
      OutputSKURMSE[iter,"DES Damped"]<-rmse(z[,2],fit)
    }
    k<-forecast(modDesdam,forecastperiod)
    forecastoutputfile1$`DES Damped`<-as.numeric(k$mean)
  
    modTesdam<-ets(org,model='AAA',damped = TRUE,lower=c(0.0,0.0,0,0.85), upper=c(1,1,1,0.95))
    fit <- tail(fitted(modTesdam), trainperiod)
    #forecastoutputfiletesting1$`DES Damped`<-fit
    #OutputSKURMSE[iter,"DES Damped"]<-mape(test,fit)
    if(length(org)==1){
      OutputSKURMSE[iter,"TES Damped"]<-rmse(z[,1],fit)
    } else {
      OutputSKURMSE[iter,"TES Damped"]<-rmse(z[,2],fit)
    }
    k<-forecast(modTesdam,forecastperiod)
    forecastoutputfile1$`TES Damped`<-as.numeric(k$mean)
  
  
    #~~~~~~~~~~~~~~ TES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    modTes<-ets(org,model="AAA")
    if(sum(org!=0)==0) {
      modTes1<-ets(org,model="MAM")
      if(modTes$mse > modTes1$mse){
        modTes <- modTes1
      }
    }
    fit <- tail(fitted(modTes), trainperiod)
    #forecastoutputfiletesting1$TES<-(fit)
    OutputSKURMSE[iter,"TES"]<-rmse(z[,2],fit)
    k<-forecast(modTes,forecastperiod)
    forecastoutputfile1$TES<-(k$mean)
    
    #~~~~~~~~~~~~~~~ ETS ~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    # Auto ETS
    modets<-ets(org)
    
    fit <- tail(fitted(modets), trainperiod)
    #forecastoutputfiletesting1$ETS<-(fit)
    OutputSKURMSE[iter,"ETS"]<-rmse(z[,2],fit)
    k<-forecast(modets,forecastperiod)
    forecastoutputfile1$ETS<-as.numeric(k$mean)
    
    #~~~~~~~~~~~~~~~ STLF ~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    if(length(org)>2*frequency){
      modelstlf<-stlf(org,h=trainperiod)
      fit <- tail(fitted(modelstlf), trainperiod)
      #forecastoutputfiletesting1$STLF <- fit
      #OutputSKURMSE[iter,"STLF"]<-mape(test,fit)
      OutputSKURMSE[iter,"STLF"]<-rmse(z[,2],fit)
      modelstlf<-stlf(org,h=forecastperiod)
      forecastoutputfile1$STLF<-(modelstlf$mean)
    }
  
    
    #~~~~~~~~~~~~~~~~~ TBATS ~~~~~~~~~~~~~~~~~~~~~~~~#
    # let's see
    
    modeltbat<-tbats(org,use.damped.trend = TRUE)
    fit <- tail(fitted(modeltbat), trainperiod)
    #forecastoutputfiletesting1$TBATS<-fit
    #OutputSKURMSE[iter,"TBATS"]<-mape(test,fit)
    OutputSKURMSE[iter,"TBATS"]<-rmse(z[,2],fit)
    
    k<-forecast(modeltbat,forecastperiod)
    forecastoutputfile1$TBATS<-k$mean
  
    
    
    if(length(org)>2) {
      modelma<-sma(org)
      fit <- tail(fitted(modelma,trainperiod))
      #forecastoutputfiletesting1$`Moving Average`<-(fit)
      #OutputSKURMSE[iter,"Moving Average"]<-mape(test,fit)
      OutputSKURMSE[iter,"Moving Average"]<-rmse(tail(z[,2], length(fit)),fit)
      k<-forecast(modelma,forecastperiod)
      forecastoutputfile1$`Moving Average`<-(k$mean)
    }
    
    
    
    
    # Theta
    #~~~~~~~~~~~~~~ Theta ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    modTheta <- thetaf(org,forecastperiod)
    fit <- tail(fitted(modTheta), trainperiod)
    #forecastoutputfiletesting1$Theta<-(fit)
    OutputSKURMSE[iter,"Theta"]<-rmse(z[,2],fit)
    k<-forecast(modTheta,forecastperiod)
    forecastoutputfile1$Theta<-(k$mean)
    }
  
  
    if(length(org)<13) {
    #~~~~~~~~~~~~~~~~~~~~ SnaiveYOY ~~~~~~~~~~~~~~~~~~#
    modsnaive<-snaive(org,h=forecastperiod)
    fit<-fitted(modsnaive)*(1+TREND1)
    fit <- tail(fit,trainperiod)
    #forecastoutputfiletesting1$Snaive<-(fit)
    #OutputSKURMSE[iter,"Snaive"]<-mape(test,fit)
    if(length(org)==1){
      OutputSKURMSE[iter,"Snaive"]<-rmse(z[,1],fit)
    } else {
      OutputSKURMSE[iter,"Snaive"]<-rmse(z[,2],fit)
    }
    #[iter,"Snaive"]<-rmse(z[,2],fit)
    k<-forecast(modsnaive,forecastperiod)
    k<-k$mean
    forecastoutputfile1$Snaive<-(k)
    
    if(length(org)>2) {
      modelma<-sma(org)
      fit <- tail(fitted(modelma,trainperiod))
      forecastoutputfiletesting1[1:trainperiod,"Moving Average"]<-(fit)
      #OutputSKURMSE[iter,"Moving Average"]<-mape(test,fit)
      OutputSKURMSE[iter,"Moving Average"]<-rmse(tail(z[,2], length(fit)),fit)
      k<-forecast(modelma,forecastperiod)
      forecastoutputfile1$`Moving Average`<-(k$mean)
    }
    modses<-ets(org,model = "ANN",lower=c(0.6,0.0,0,0.60), upper=c(1,1,1,0.95))
    fit <- tail(fitted(modses),trainperiod)
    #forecastoutputfiletesting1[1:trainperiod,"SES"]<-(fit)
    #OutputSKURMSE[iter,"SES"]<-mape(test,fit)
    if(length(org)==1){
      OutputSKURMSE[iter,"SES"]<-rmse(z[,1],fit)
    } else {
      OutputSKURMSE[iter,"SES"]<-rmse(z[,2],fit)
    }
    k<-forecast(modses,forecastperiod)
    forecastoutputfile1$SES<-as.numeric(k$mean)
    #~~~~~~~~~~~~~~~~~~~ DES Damped ~~~~~~~~~~~~~~~~#
    
    modDesdam<-ets(org,model='AAN',damped = TRUE,lower=c(0.0,0.0,0,0.60), upper=c(1,1,1,0.95))
    fit <- tail(fitted(modDesdam), trainperiod)
    #forecastoutputfiletesting1$`DES Damped`<-fit
    #OutputSKURMSE[iter,"DES Damped"]<-mape(test,fit)
    if(length(org)==1){
      OutputSKURMSE[iter,"DES Damped"]<-rmse(z[,1],fit)
    } else {
      OutputSKURMSE[iter,"DES Damped"]<-rmse(z[,2],fit)
    }
    k<-forecast(modDesdam,forecastperiod) 
    forecastoutputfile1$`DES Damped`<-as.numeric(k$mean)
    
    }
  
    
    #-------------------------------------------------#
   
      
    # #~~~~~~~~~~~~~~~~~ Croston ~~~~~~~~~~~~~~~~~~~~~~~#
    # modelCroston<-croston(abs(org), alpha = 0.1, h=trainperiod)
    # fit <- tail(fitted(modelCroston), trainperiod)
    # #forecastoutputfiletesting1$Croston<-(fit)
    # #OutputSKURMSE[iter,"Croston"]<-mape(test,fit)
    # OutputSKURMSE[iter,"Croston"]<-rmse(z[,2],fit)
    # k<-forecast(modelCroston,forecastperiod)
    # forecastoutputfile1$Croston<-(k$mean)
    # #~~~~~~~~~~~~~~~~~ Stat Parameters ~~~~~~~~~~~~~~~#
    # rm(k)
    # 
    # rm(cp)
    # #------------------------------------------------#
    
    
    # #~~~~~~~~~~~~~~~~~ TSB - Teunter-Syntetos-Babai ~~~~~~~~~~~~~~~~~~~~~~~#
    # modeltsb <- tsb(org,12)
    # fit <- tail(modeltsb$frc.in,12)
    # #forecastoutputfiletesting1$TSB<-(fit)
    # #OutputSKURMSE[iter,"Croston"]<-mape(test,fit)
    # OutputSKURMSE[iter,"TSB"]<-rmse(z[,2],fit)
    # k<-modeltsb$frc.out
    # forecastoutputfile1$TSB<- k
    
    
    
    # rm(rulenum)
    # print("Yes")
    # print(rulenum,"gjjgj")
    
    #=================================== Selecting Best Fit Algorithm =======================================================#
    OutputSKURMSE[is.na(OutputSKURMSE)]<-100000000
    columnnamesmin <- names(select_if(OutputSKURMSE,is.numeric))[which.min(apply(select_if(OutputSKURMSE[iter,],is.numeric),MARGIN=2,min))] #--
    OutputSKURMSE[iter,"Best Fit"]<-columnnamesmin
    forecastoutputfile1$`Best Fit`<- forecastoutputfile1[,columnnamesmin]
    forecastoutputfiletesting1$`Best Fit`<-forecastoutputfiletesting1[,columnnamesmin]
    forecastoutputfile1$`Best Fit Algorithm`<-columnnamesmin
    forecastoutputfiletesting1$`Best Fit Algorithm`<-columnnamesmin
    forecastoutputfile1[,colnames(select_if(forecastoutputfile1,is.numeric))][forecastoutputfile1[,colnames(select_if(forecastoutputfile1,is.numeric))] < 0] <- 0 #--
    # check what is this...
    #forecastoutputfile1[is.na(forecastoutputfile1)] <- 0
    forecastoutputfiletesting1[,colnames(select_if(forecastoutputfiletesting1,is.numeric))][forecastoutputfiletesting1[,colnames(select_if(forecastoutputfiletesting1,is.numeric))] < 0] <- 0 #--
    #forecastoutputfiletesting1[forecastoutputfiletesting1 < 0] <- 0
    #forecastoutputfiletesting1[is.na(forecastoutputfiletesting1)] <- 0 # not sure, why NAs are filled with 0
    
    zz <- c("Average","Moving Average","Snaive","Theta","STLM","SES","DES","TES","DES Damped","TES Damped","ARIMA","TBATS","STLF","Croston","Best Fit","Thief Auto", "Thief Arima","Thief Naive", "Thief stlf", "Thief tbats","TSB","ETS","SMA")
    forecastoutputfile1[zz] <- lapply(forecastoutputfile1[zz], as.numeric)
    FinalForecastperSKU1<-rbind(FinalForecastperSKU1,forecastoutputfile1)
    #FinalForecastperSKUtesting1[zz] <- lapply(FinalForecastperSKUtesting1[zz], as.numeric)
    forecastoutputfiletesting1[zz] <- lapply(forecastoutputfile1[zz], as.numeric)
    FinalForecastperSKUtesting1<-rbind(FinalForecastperSKUtesting1,forecastoutputfiletesting1)
    forecastoutputfile1[zz] <- NA
    forecastoutputfiletesting1[zz] <- NA
    
    #print(iter);print(FSKU);
    iter<-iter + 1
    #print(rulenum)
    
    rm(trainperiod)
    # Because test period is not same always,
    # forecastoutputfiletesting1 <- forecastoutputfiletesting1[0,]
    
  }

fwrite(FinalForecastperSKU1,paste0(format(maxdate1,"%B"),"CycleCategory_Sellout_0104.csv"))
gc();
#k <- k + 1;

}

#fwrite(FinalForecastperSKU1,"o9Grouping_Sellin_July_Lag2.csv")
#=============================================== Final Output Table ===========================================================#

#=============================================== Fromatting back date according to tenant ===========================================================#

# Forecast Period (horizon)
FinalForecastperSKU1$MonthName <- as.POSIXct(FinalForecastperSKU1$Date)
FinalForecastperSKU1$MonthName1 <- format(FinalForecastperSKU1$MonthName,"%b")
FinalForecastperSKU1$Year <- format(FinalForecastperSKU1$MonthName,"%Y")
FinalForecastperSKU1$Month <- paste(FinalForecastperSKU1$MonthName1,FinalForecastperSKU1$Year,sep = "-")

# Backtest period (horizon)
FinalForecastperSKUtesting1$MonthName <- as.POSIXct(FinalForecastperSKUtesting1$Date)
FinalForecastperSKUtesting1$MonthName1 <- format(FinalForecastperSKUtesting1$MonthName,"%b")
FinalForecastperSKUtesting1$Year <- format(FinalForecastperSKUtesting1$MonthName,"%Y")
FinalForecastperSKUtesting1$Month <- paste(FinalForecastperSKUtesting1$MonthName1,FinalForecastperSKUtesting1$Year,sep = "-")

# Keeping necessary columns
FinalForecastperSKU1 <- FinalForecastperSKU1[,c("SKU","Month","Average","Moving Average","Snaive","Theta","STLM","SES","DES","TES","DES Damped","TES Damped","ARIMA","TBATS", "STLF","Croston","Best Fit Algorithm","Best Fit")]

# Renaming to o9 understandable format
colnames(FinalForecastperSKU1) <- c("Item.[Planning Item]","Time.[Planning Month]","Measure.[Average]","Measure.[Moving Average]","Measure.[Snaive]","Measure.[Theta]","Measure.[STLM]","Measure.[SES]","Measure.[DES]","Measure.[TES]","Measure.[DES Damped]","Measure.[TES Damped]","Measure.[ARIMA]","Measure.[TBATS]", "Measure.[STLF]","Measure.[Croston]","Measure.[Best Fit Algorithm]","Measure.[Best Fit SKU]")

# FinalForecastOutput final data frame name in plugin settings
FinalForecastOutput <- FinalForecastperSKU1

# Keeping necessary columns
FinalForecastperSKUtesting1 <- FinalForecastperSKUtesting1[,c("SKU","Month","Average","Moving Average","Snaive","Theta","STLM","SES","DES","TES","DES Damped","TES Damped","ARIMA","TBATS", "STLF","Croston","Best Fit")]

# Renaming to o9 understandable format
colnames(FinalForecastperSKUtesting1) <- c("Item.[Planning Item]","Time.[Planning Month]","Measure.[Average Backtest]","Measure.[Moving Average Backtest]","Measure.[Snaive Backtest]","Measure.[Theta Backtest]","Measure.[STLM Backtest]","Measure.[SES Backtest]","Measure.[DES Backtest]","Measure.[TES Backtest]","Measure.[DES Damped Backtest]","Measure.[TES Damped Backtest]","Measure.[ARIMA Backtest]","Measure.[TBATS Backtest]", "Measure.[STLF Backtest]","Measure.[Croston Backtest]","Measure.[Best Fit SKU Backtest]")

# BackCastingOutput final data frame name in plugin settings
BackCastingOutput <- FinalForecastperSKUtesting1
#=======================================================================================================================================================#


#=============================================== Returning RMSE values to final data frame ===========================================================#

OutputSKURMSE <- OutputSKURMSE[,c("SKU","Average","Moving Average","Snaive","Theta","STLM","SES","DES","TES","DES Damped","TES Damped","ARIMA","TBATS","STLF","Croston")]

colnames(OutputSKURMSE) <- c("Item.[Planning Item]","Measure.[Average MAPE]","Measure.[Moving Average MAPE]","Measure.[Snaive MAPE]","Measure.[Theta RMSE]","Measure.[STLM RMSE]","Measure.[SES MAPE]","Measure.[DES MAPE]","Measure.[TES MAPE]","Measure.[DES Damped MAPE]","Measure.[TES Damped MAPE]","Measure.[ARIMA MAPE]","Measure.[TBATS MAPE]","Measure.[STLF MAPE]","Measure.[Croston MAPE]")
MAPEOutput <- OutputSKURMSE
MAPEOutput$`Measure.[Best Fit RMSE]` <- apply(MAPEOutput[,2:14],1,FUN = min)
MAPEOutput[MAPEOutput == 100000000] <- NA
#=======================================================================================================================================================#

# Returning Stat Parameters
colnames(StatParameters) <- c("Item.[Planning Item]","Measure.[Average Parameters]","Measure.[Moving Average Parameters]","Measure.[Snaive Parameters]","Measure.[Theta Parameters]","Measure.[STLM Parameters]","Measure.[SES Parameters]","Measure.[DES Parameters]","Measure.[TES Parameters]","Measure.[DES Damped Parameters]","Measure.[TES Damped Parameters]","Measure.[ARIMA Parameters]","Measure.[TBATS Parameters]","Measure.[STLF Parameters]","Measure.[Croston Parameters]")
#=======================================================================================================================================================#
#=======================================================================================================================================================#






# Disaggregate forecast at SKU and SKU Banner level.

#=============================== Hierarchical Forecasting from SKU to SKU Banner =========================#

# Forecast from Higher Levels
OctoberHigher <- fread("octSubbrand.csv")
NovemberHigher <- fread("novsubrand.csv")
DecemberHigher <- fread("decsubbrand.csv")


# # Forecast from Lower levels
# OctoberLower <- fread("C:\\Users\\kadambini.indurkar\\GSK\\SKULevelForecasting\\Forecast From SKU Banner Level\\OctLag2Latest.csv")
# NovemberLower <- read_xlsx("C:\\Users\\kadambini.indurkar\\GSK\\SKULevelForecasting\\Forecast From SKU Banner Level\\NovLag2v1.xlsx", sheet = "NovLag2v1")
# DecemberLower <- fread("C:\\Users\\kadambini.indurkar\\GSK\\SKULevelForecasting\\Forecast From SKU Banner Level\\DEcLag2latest.csv")



# temp1 <- strsplit(NovemberLower$SKU,"_")
# d <- do.call(rbind,temp1)
# OctoberLower$SKU_Country <- paste(d[,1],d[,3], sep = "_")
# 
# temp2 <- strsplit(NovemberLower$SKU,"_")
# d2 <- do.call(rbind,temp2)
# OctoberLower$SKU_Country <- paste(d2[,1],d2[,3], sep = "_")
# 
# temp3 <- strsplit(DecemberLower$SKU,"_")
# d3 <- do.call(rbind,temp3)
# OctoberLower$SKU_Country <- paste(d3[,1],d3[,3], sep = "_")



# New Generated cycles for sell in
# what we want sku_country in lower levels,

OctoberLower1 <- fread("octsku.csv")
NovemberLower1 <- fread("novsku.csv")
DecemberLower1 <- fread("decsku.csv")
colnames(OctoberLower1)[7] <- c("Item")
colnames(NovemberLower1)[5] <- c("Item")
colnames(DecemberLower1)[5] <- c("Item")

OctoberLower1$sub <- paste(OctoberLower1$`Sub Brand`, OctoberLower1$V8, sep = "_")
NovemberLower1$sub <- paste(NovemberLower1$`Sub Brand`, NovemberLower1$V6, sep = "_")
DecemberLower1$sub <- paste(DecemberLower1$`Sub Brand`, NovemberLower1$V6, sep = "_")
OctoberLower1$`Sub Brand` <- OctoberLower1$sub
NovemberLower1$`Sub Brand` <- NovemberLower1$sub
DecemberLower1$`Sub Brand` <- DecemberLower1$sub
# Selecting important columns
LowerLevelForecastOctober <- OctoberLower1 %>% select("SKU","Sub Brand","Date","Best Fit")
LowerLevelForecastNovember <- NovemberLower1 %>% select("SKU","Sub Brand","Date","Best Fit")
LowerLevelForecastDecember <-  DecemberLower1 %>% select("SKU","Sub Brand","Date","Best Fit")

# Best Fit NA anywhere?
LowerLevelForecastOctober$`Best Fit`[is.na(LowerLevelForecastOctober$`Best Fit`)] <- ''
LowerLevelForecastNovember$`Best Fit`[is.na(LowerLevelForecastNovember$`Best Fit`)] <- ''
LowerLevelForecastDecember$`Best Fit`[is.na(LowerLevelForecastDecember$`Best Fit`)] <- ''

# Safe as.numeric

LowerLevelForecastOctober$`Best Fit` <- as.numeric(LowerLevelForecastOctober$`Best Fit`)
LowerLevelForecastNovember$`Best Fit` <- as.numeric(LowerLevelForecastNovember$`Best Fit`)
LowerLevelForecastDecember$`Best Fit` <- as.numeric(LowerLevelForecastDecember$`Best Fit`)

# Group bys
LowerLevelForecastOctober <- LowerLevelForecastOctober %>% group_by(`SKU`,`Sub Brand`,`Date`) %>%
  summarise(`Best Fit` = sum(`Best Fit`, na.rm = T)) %>% ungroup()
LowerLevelForecastNovember <- LowerLevelForecastNovember %>% group_by(`SKU`,`Sub Brand`,`Date`) %>%
  summarise(`Best Fit` = sum(`Best Fit`, na.rm = T)) %>% ungroup()
LowerLevelForecastDecember <- LowerLevelForecastDecember %>% group_by(`SKU`,`Sub Brand`,`Date`) %>%
  summarise(`Best Fit` = sum(`Best Fit`, na.rm = T)) %>% ungroup()

# as.date
LowerLevelForecastOctober$Date <- as.Date(LowerLevelForecastOctober$Date, format = "%d-%m-%Y")
LowerLevelForecastNovember$Date <- as.Date(LowerLevelForecastNovember$Date, format = "%d-%m-%Y")
LowerLevelForecastDecember$Date <- as.Date(LowerLevelForecastDecember$Date, format = "%d-%m-%Y")


# aggregate lower level forecast 

LowerLevelForecastOctober <- LowerLevelForecastOctober %>% group_by(`Sub Brand`,`Date`) %>%
  mutate(fcst_lowerlevel_agg = sum(`Best Fit`, na.rm = T)) %>% ungroup()
LowerLevelForecastNovember <- LowerLevelForecastNovember %>% group_by(`Sub Brand`,`Date`) %>%
  mutate(fcst_lowerlevel_agg = sum(`Best Fit`, na.rm = T)) %>% ungroup()
LowerLevelForecastDecember <- LowerLevelForecastDecember %>% group_by(`Sub Brand`,`Date`) %>%
  mutate(fcst_lowerlevel_agg = sum(`Best Fit`, na.rm = T)) %>% ungroup()



#Higher level forecast

HigherLevelOctober <- OctoberHigher %>% select('SKU', 'Date', 'Best Fit')
HigherLevelNovember <- NovemberHigher %>% select('SKU', 'Date', 'Best Fit')
HigherLevelDecember <- DecemberHigher %>% select('SKU', 'Date', 'Best Fit')

# Any NAs
HigherLevelOctober$`Best Fit`[is.na(HigherLevelOctober$`Best Fit`)] <- ''
HigherLevelNovember$`Best Fit`[is.na(HigherLevelNovember$`Best Fit`)] <- ''
HigherLevelDecember$`Best Fit`[is.na(HigherLevelDecember$`Best Fit`)] <- ''

# safe numeric conversion
HigherLevelOctober$`Best Fit` <- as.numeric(HigherLevelOctober$`Best Fit`)
HigherLevelNovember$`Best Fit` <- as.numeric(HigherLevelNovember$`Best Fit`)
HigherLevelDecember$`Best Fit` <- as.numeric(HigherLevelDecember$`Best Fit`)

# Group By
HigherLevelOctober <- HigherLevelOctober %>% group_by(`SKU`,`Date`) %>%
  summarise(`Best Fit` = sum(`Best Fit`, na.rm = T)) %>% ungroup()
HigherLevelNovember <- HigherLevelNovember %>% group_by(`SKU`,`Date`) %>%
  summarise(`Best Fit` = sum(`Best Fit`, na.rm = T)) %>% ungroup()
HigherLevelDecember <- HigherLevelDecember %>% group_by(`SKU`,`Date`) %>%
  summarise(`Best Fit` = sum(`Best Fit`, na.rm = T)) %>% ungroup()


# as.Date
HigherLevelOctober$Date <- as.Date(HigherLevelOctober$Date)
HigherLevelNovember$Date <- as.Date(HigherLevelNovember$Date)
HigherLevelDecember$Date <- as.Date(HigherLevelDecember$Date)

names(HigherLevelOctober)[names(HigherLevelOctober)=='Best Fit'] <- 'Forecast_HLOctober'
names(HigherLevelNovember)[names(HigherLevelNovember)=='Best Fit'] <- 'Forecast_HLNovember'
names(HigherLevelDecember)[names(HigherLevelDecember)=='Best Fit'] <- 'Forecast_HLDecember'

# Hierarchical Forecast

HierarchicalForecastOctober <- left_join(HigherLevelOctober,LowerLevelForecastOctober, by = c("SKU"= "Sub Brand", "Date"))
HierarchicalForecastNovember <- left_join(HigherLevelNovember, LowerLevelForecastNovember, by = c("SKU"= "Sub Brand", "Date"))
HierarchicalForecastDecember <- left_join(HigherLevelDecember, LowerLevelForecastDecember, by = c("SKU"= "Sub Brand", "Date"))


HierarchicalForecastOctober$DisaggregatedForecast <- (HierarchicalForecastOctober$`Best Fit`/HierarchicalForecastOctober$fcst_lowerlevel_agg)* HierarchicalForecastOctober$Forecast_HLOctober
HierarchicalForecastNovember$DisaggregatedForecast <- (HierarchicalForecastNovember$`Best Fit`/HierarchicalForecastNovember$fcst_lowerlevel_agg) * HierarchicalForecastNovember$Forecast_HLNovember
HierarchicalForecastDecember$DisaggregatedForecast <- (HierarchicalForecastDecember$`Best Fit`/HierarchicalForecastDecember$fcst_lowerlevel_agg) * HierarchicalForecastDecember$Forecast_HLDecember


write.csv(HierarchicalForecastOctober,"OctoberCycleDisaggregatedForecast.csv")
write.csv(HierarchicalForecastNovember,"NovemberCycleDisaggregatedForecast.csv")
write.csv(HierarchicalForecastDecember,"DecemberCycleDisaggregatedForecast.csv")


#Validation


sum(LowerLevelForecastOctober$`Best Fit`, na.rm = T)
sum(HigherLevelOctober$Forecast_HLOctober, na.rm = T)
sum(HierarchicalForecastOctober$DisaggregatedForecast, na.rm = T)





# sellout

# Forecast from Higher Levels
OctoberHigher <- fread("octsubbrandsellout.csv")
NovemberHigher <- fread("novselloutsubbrand.csv")
DecemberHigher <- fread("decsubbrandsellout.csv")


# # Forecast from Lower levels
# OctoberLower <- fread("C:\\Users\\kadambini.indurkar\\GSK\\SKULevelForecasting\\Forecast From SKU Banner Level\\OctLag2Latest.csv")
# NovemberLower <- read_xlsx("C:\\Users\\kadambini.indurkar\\GSK\\SKULevelForecasting\\Forecast From SKU Banner Level\\NovLag2v1.xlsx", sheet = "NovLag2v1")
# DecemberLower <- fread("C:\\Users\\kadambini.indurkar\\GSK\\SKULevelForecasting\\Forecast From SKU Banner Level\\DEcLag2latest.csv")



# temp1 <- strsplit(NovemberLower$SKU,"_")
# d <- do.call(rbind,temp1)
# OctoberLower$SKU_Country <- paste(d[,1],d[,3], sep = "_")
# 
# temp2 <- strsplit(NovemberLower$SKU,"_")
# d2 <- do.call(rbind,temp2)
# OctoberLower$SKU_Country <- paste(d2[,1],d2[,3], sep = "_")
# 
# temp3 <- strsplit(DecemberLower$SKU,"_")
# d3 <- do.call(rbind,temp3)
# OctoberLower$SKU_Country <- paste(d3[,1],d3[,3], sep = "_")



# New Generated cycles for sell in
# what we want sku_country in lower levels,

OctoberLower1 <- fread("skuselloutoctoberv1.csv")
NovemberLower1 <- fread("skuselloutnovember.csv")
DecemberLower1 <- fread("skudecembersellout.csv")
# colnames(OctoberLower1)[7] <- c("Item")
# colnames(NovemberLower1)[5] <- c("Item")
# colnames(DecemberLower1)[5] <- c("Item")

OctoberLower1$sub <- paste(OctoberLower1$`Sub Brand`, OctoberLower1$Country, sep = "_")
NovemberLower1$sub <- paste(NovemberLower1$`Sub Brand`, NovemberLower1$Country, sep = "_")
DecemberLower1$sub <- paste(DecemberLower1$`Sub Brand`, NovemberLower1$Country, sep = "_")
OctoberLower1$`Sub Brand` <- OctoberLower1$sub
NovemberLower1$`Sub Brand` <- NovemberLower1$sub
DecemberLower1$`Sub Brand` <- DecemberLower1$sub
# Selecting important columns
LowerLevelForecastOctober <- OctoberLower1 %>% select("SKU","Sub Brand","Date","Best Fit")
LowerLevelForecastNovember <- NovemberLower1 %>% select("SKU","Sub Brand","Date","Best Fit")
LowerLevelForecastDecember <-  DecemberLower1 %>% select("SKU","Sub Brand","Date","Best Fit")

# Best Fit NA anywhere?
LowerLevelForecastOctober$`Best Fit`[is.na(LowerLevelForecastOctober$`Best Fit`)] <- ''
LowerLevelForecastNovember$`Best Fit`[is.na(LowerLevelForecastNovember$`Best Fit`)] <- ''
LowerLevelForecastDecember$`Best Fit`[is.na(LowerLevelForecastDecember$`Best Fit`)] <- ''

# Safe as.numeric

LowerLevelForecastOctober$`Best Fit` <- as.numeric(LowerLevelForecastOctober$`Best Fit`)
LowerLevelForecastNovember$`Best Fit` <- as.numeric(LowerLevelForecastNovember$`Best Fit`)
LowerLevelForecastDecember$`Best Fit` <- as.numeric(LowerLevelForecastDecember$`Best Fit`)

# Group bys
LowerLevelForecastOctober <- LowerLevelForecastOctober %>% group_by(`SKU`,`Sub Brand`,`Date`) %>%
  summarise(`Best Fit` = sum(`Best Fit`, na.rm = T)) %>% ungroup()
LowerLevelForecastNovember <- LowerLevelForecastNovember %>% group_by(`SKU`,`Sub Brand`,`Date`) %>%
  summarise(`Best Fit` = sum(`Best Fit`, na.rm = T)) %>% ungroup()
LowerLevelForecastDecember <- LowerLevelForecastDecember %>% group_by(`SKU`,`Sub Brand`,`Date`) %>%
  summarise(`Best Fit` = sum(`Best Fit`, na.rm = T)) %>% ungroup()

# as.date
LowerLevelForecastOctober$Date <- as.Date(LowerLevelForecastOctober$Date, format = "%d-%m-%Y")
LowerLevelForecastNovember$Date <- as.Date(LowerLevelForecastNovember$Date, format = "%d-%m-%Y")
LowerLevelForecastDecember$Date <- as.Date(LowerLevelForecastDecember$Date, format = "%d-%m-%Y")


# aggregate lower level forecast 

LowerLevelForecastOctober <- LowerLevelForecastOctober %>% group_by(`Sub Brand`,`Date`) %>%
  mutate(fcst_lowerlevel_agg = sum(`Best Fit`, na.rm = T)) %>% ungroup()
LowerLevelForecastNovember <- LowerLevelForecastNovember %>% group_by(`Sub Brand`,`Date`) %>%
  mutate(fcst_lowerlevel_agg = sum(`Best Fit`, na.rm = T)) %>% ungroup()
LowerLevelForecastDecember <- LowerLevelForecastDecember %>% group_by(`Sub Brand`,`Date`) %>%
  mutate(fcst_lowerlevel_agg = sum(`Best Fit`, na.rm = T)) %>% ungroup()



#Higher level forecast

HigherLevelOctober <- OctoberHigher %>% select('SKU', 'Date', 'Best Fit')
HigherLevelNovember <- NovemberHigher %>% select('SKU', 'Date', 'Best Fit')
HigherLevelDecember <- DecemberHigher %>% select('SKU', 'Date', 'Best Fit')

# Any NAs
HigherLevelOctober$`Best Fit`[is.na(HigherLevelOctober$`Best Fit`)] <- ''
HigherLevelNovember$`Best Fit`[is.na(HigherLevelNovember$`Best Fit`)] <- ''
HigherLevelDecember$`Best Fit`[is.na(HigherLevelDecember$`Best Fit`)] <- ''

# safe numeric conversion
HigherLevelOctober$`Best Fit` <- as.numeric(HigherLevelOctober$`Best Fit`)
HigherLevelNovember$`Best Fit` <- as.numeric(HigherLevelNovember$`Best Fit`)
HigherLevelDecember$`Best Fit` <- as.numeric(HigherLevelDecember$`Best Fit`)

# Group By
HigherLevelOctober <- HigherLevelOctober %>% group_by(`SKU`,`Date`) %>%
  summarise(`Best Fit` = sum(`Best Fit`, na.rm = T)) %>% ungroup()
HigherLevelNovember <- HigherLevelNovember %>% group_by(`SKU`,`Date`) %>%
  summarise(`Best Fit` = sum(`Best Fit`, na.rm = T)) %>% ungroup()
HigherLevelDecember <- HigherLevelDecember %>% group_by(`SKU`,`Date`) %>%
  summarise(`Best Fit` = sum(`Best Fit`, na.rm = T)) %>% ungroup()


# as.Date
HigherLevelOctober$Date <- as.Date(HigherLevelOctober$Date)
HigherLevelNovember$Date <- as.Date(HigherLevelNovember$Date)
HigherLevelDecember$Date <- as.Date(HigherLevelDecember$Date)

names(HigherLevelOctober)[names(HigherLevelOctober)=='Best Fit'] <- 'Forecast_HLOctober'
names(HigherLevelNovember)[names(HigherLevelNovember)=='Best Fit'] <- 'Forecast_HLNovember'
names(HigherLevelDecember)[names(HigherLevelDecember)=='Best Fit'] <- 'Forecast_HLDecember'

# Hierarchical Forecast

HierarchicalForecastOctober <- left_join(HigherLevelOctober,LowerLevelForecastOctober, by = c("SKU"= "Sub Brand", "Date"))
HierarchicalForecastNovember <- left_join(HigherLevelNovember, LowerLevelForecastNovember, by = c("SKU"= "Sub Brand", "Date"))
HierarchicalForecastDecember <- left_join(HigherLevelDecember, LowerLevelForecastDecember, by = c("SKU"= "Sub Brand", "Date"))


HierarchicalForecastOctober$DisaggregatedForecast <- (HierarchicalForecastOctober$`Best Fit`/HierarchicalForecastOctober$fcst_lowerlevel_agg)* HierarchicalForecastOctober$Forecast_HLOctober
HierarchicalForecastNovember$DisaggregatedForecast <- (HierarchicalForecastNovember$`Best Fit`/HierarchicalForecastNovember$fcst_lowerlevel_agg) * HierarchicalForecastNovember$Forecast_HLNovember
HierarchicalForecastDecember$DisaggregatedForecast <- (HierarchicalForecastDecember$`Best Fit`/HierarchicalForecastDecember$fcst_lowerlevel_agg) * HierarchicalForecastDecember$Forecast_HLDecember


write.csv(HierarchicalForecastOctober,"OctoberCycleDisaggregatedForecastsellout.csv")
write.csv(HierarchicalForecastNovember,"NovemberCycleDisaggregatedForecastsellout.csv")
write.csv(HierarchicalForecastDecember,"DecemberCycleDisaggregatedForecastsellout.csv")


#Validation


sum(LowerLevelForecastOctober$`Best Fit`, na.rm = T)
sum(HigherLevelOctober$Forecast_HLOctober, na.rm = T)
sum(HierarchicalForecastOctober$DisaggregatedForecast, na.rm = T)







