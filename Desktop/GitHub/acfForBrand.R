library(readxl)
library(dplyr)
library(reshape2)
library(tsfeatures)
library(data.table)
#ActualsData <- read_xlsx("C:\\Users\\kadambini.indurkar\\GSK\\o9 grouping Iteration\\SalesData_1Aug2020 (1).xlsx", sheet = "Sheet2")
#C:\Users\kadambini.indurkar\GSK\o9 grouping Iteration\Brand Forecast\Brand Banner\BrandBannerzeroesfilled\Proportions.xlsx
ActualsData <- fread("C:/Users/kadambini.indurkar/GSK/Phase II - Regeneration/Phase II Brand and Item/Brand Actuals Sellin and Sellout_12Oct.csv")
colnames(ActualsData)[which(names(ActualsData) == "Actual")] <- "Shipment"
colnames(ActualsData)[which(names(ActualsData) == "Time.[Planning Month]")] <- "Time"
#================================================================================================================================#

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
ActualsData$Week<-as.Date(ActualsData$Week, format = "%Y - %m - %d")
ActualsData$SKU <- paste(ActualsData$`Item.[L5]`, ActualsData$`Sales Domain.[Channel]`, sep = "_")
# New dataframe ActualsData1 contains only three columns i.e.,
# SKU, Week and Shipment
#ActualsData$SKU <- ActualsData$`Sales Domain.[Ship To]`
#ActualsData$SKU <- paste(ActualsData$`Item.[L5]`,ActualsData$`Sales Domain.[Ship To]`, sep = "_")
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
#============================= Calculating last month of Input data =============================================================#

# This will be used to subset data 
maxdate<-max(actuals1$C445_WK_STRT_DATE)
maxdate <- "2020-04-01"
maxdate <- as.Date(maxdate)
#================================================================================================================================#


#============================== Filtering Shipment data of last 36 months =======================================================#

# subset data using maxdate
actuals1<-subset(actuals1,actuals1$C445_WK_STRT_DATE<=maxdate)
maxdate<-as.Date(maxdate) %m-% months(1)
dateprev36<-as.Date(maxdate) %m-% months(35)
actuals1<-subset(actuals1,actuals1$C445_WK_STRT_DATE>=dateprev36)
#================================================================================================================================#



#================================================================================================================================#
colnames(actuals1)<-c("SKU","Week","Shipment")
#actuals1 <- subset(actuals1, actuals1$Week >=as.Date("2017-01-01"))

#============================= Calculating last month of Input data =============================================================#
#t1 <- (seq(as.Date(as.Date(date1) %m-% months(`Proportion Months`-1)), as.Date(date1), by="month"))

# Filling zeroes
t1 <- seq(as.Date(min(actuals1$Week)), max(as.Date(actuals1$Week)), by="months")

t11 <- as.data.frame(t1)
colnames(t11) <- c("Week")
t11$val <- c(1)

temp1 <- merge(t11,actuals1, by="Week",all.x = T)

temp1Dcast <- reshape2::dcast(temp1, SKU ~Week, value.var = "Shipment", fill = NA)
temp1_melt <- reshape2::melt(temp1Dcast,id.vars = "SKU")


# seg1 %>% group_by(SKU) %>% summarise(Start_Date = min(Week,na.rm = TRUE), End_Date = max(Week,na.rm = TRUE))
start_date <- actuals1 %>% group_by(SKU) %>% summarise(Start_Date = min(Week, na.rm = T))

tempLJ <- left_join(temp1_melt,start_date, by="SKU")
tempLJ$value1 <- ifelse(is.na(tempLJ$value) & as.Date(tempLJ$variable)>=as.Date(tempLJ$Start_Date),0,tempLJ$value)
tempLJ <- tempLJ[,c("SKU","variable","value1")]
colnames(tempLJ) <- c("SKU","Date","Shipment")

data1 <- tempLJ

# To store RMSE values
Seasonality_values<-data.frame(matrix(ncol =3, nrow = 0))
colnames(Seasonality_values)<-c("SKU","nsdiffs","acf_values")

Seasonality_acfvalues<-data.frame(matrix(ncol =14, nrow = 0))
colnames(Seasonality_acfvalues)<-c("SKU","Lag0","Lag1","Lag2","Lag3","Lag4","Lag5","Lag6","Lag7","Lag8","Lag9","Lag10","Lag11","Lag12")
iter <- 1
frequency <- 12
# Calculate acf values of a time series
for(FSKU in sort(unique(data1$SKU))) {
  
  # skuonlydata stores, actuals data of a SKU of all dates in columns
  # skuonlydata=subset(x=data1,(data1$SKU==FSKU))
  # skuData = skuonlydata[1,]
  # size<-ncol
  #FSKU <- "AH1703L_Poland"
  skuonlydata=subset(x=data1,(data1$SKU==FSKU))
  skuonlydata <- reshape2::dcast(skuonlydata,  SKU ~ Date, value.var = "Shipment")
  skuData = skuonlydata[1,]
  size<-ncol(skuData)
  Seasonality_values[iter, "SKU"] <- FSKU
  Seasonality_acfvalues[iter, "SKU"] <- FSKU
  #org[is.na(org)]<- 0
  # Whole data points
  org<-ts(as.numeric(t(skuData[1,2:(size)])),f=frequency)
  org[is.na(org)]<- 0
  t <- acf(org,lag.max = 12, plot = F)
  #csv <- paste(t$acf)
  
  #Seasonality_values[iter, "acf_values"] <- t$acf
  
  # stl_decomp <- decompose(org)
  # decompose_df <- tslm(org ~ trend + fourier(org,6))
  # 
  # 
  # trend <- coef(decompose_df)[1] + coef(decompose_df)['trend']*seq_along(org)
  # components <- cbind(
  #   data = org,
  #   trend = trend,
  #   season = org - trend - residuals(decompose_df),
  #   remainder = residuals(decompose_df)
  # )
  # remainder_variance <- var(components[,4])
  # remainder_seasadj <- var(components[,3]+components[,4])
  # strength <- max(0, 1 - remainder_variance/remainder_seasadj)
  value<-stl_features(org)
  Seasonality_values[iter,"nsdiffs"]<- getElement(value, "seasonal_strength")
  #t <- acf(org,lag.max = 12, plot = F)
  #Seasonality_values[iter,"nsdiffs"] <- strength
  Seasonality_acfvalues[iter,"Lag0"] <- t$acf[1]
  Seasonality_acfvalues[iter,"Lag1"] <- t$acf[2]
  Seasonality_acfvalues[iter,"Lag2"] <- t$acf[3]
  Seasonality_acfvalues[iter,"Lag3"] <- t$acf[4]
  Seasonality_acfvalues[iter,"Lag4"] <- t$acf[5]
  Seasonality_acfvalues[iter,"Lag5"] <- t$acf[6]
  Seasonality_acfvalues[iter,"Lag6"] <- t$acf[7]
  Seasonality_acfvalues[iter,"Lag7"] <- t$acf[8]
  Seasonality_acfvalues[iter,"Lag8"] <- t$acf[9]
  Seasonality_acfvalues[iter,"Lag9"] <- t$acf[10]
  
  Seasonality_acfvalues[iter,"Lag10"] <- t$acf[11]
  Seasonality_acfvalues[iter,"Lag11"] <- t$acf[12]
  Seasonality_acfvalues[iter,"Lag12"] <- t$acf[13]
  
  
  
  iter <- iter + 1
  
}




#Using STL decomposition for trend component
demands <- ts(History$StoreSellOutUnitsActualsNullReplaced, frequency = 7);
fit <- stl(demands, t.window=15, s.window='periodic', robust=TRUE);
#plot(fit);
fitSTL <- data.frame(fit$time.series);
fitSTLSeasonal <- fitSTL$seasonal;
fitSTLRandom <- fitSTL$Random;
fitSTLTrend <- fitSTL$trend;
fitSTLDetrended <- demands - fitSTLTrend;

#Forecast Trend using Exponential Trend Smoothing
TrendForecastETS  <- ts(fitSTLTrend, frequency = 7);
model.ets = ets(TrendForecastETS);
fc.ets = forecast(model.ets,91);
st.ets <- fc.ets[2][1];
ETS <- st.ets$mean;





df <- ts(c(2735.869,2857.105,2725.971,2734.809,2761.314,2828.224,2830.284,
           2758.149,2774.943,2782.801,2861.970,2878.688,3049.229,3029.340,3099.041,
           3071.151,3075.576,3146.372,3005.671,3149.381), start=c(2016,8), frequency=12)
df <- ts(c(2735.869,2857.105,2725.971,2734.809,2761.314,2828.224,2830.284,
           2758.149,2774.943,2782.801,2861.970,2878.688,3049.229,3029.340,3099.041,
           3071.151,3075.576,3146.372,3005.671,3149.381), frequency=12)

library(forecast)
library(ggplot2)
decompose_df <- tslm(df ~ trend + fourier(df, 2))


