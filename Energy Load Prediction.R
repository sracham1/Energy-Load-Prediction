library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
library(forecast)
library(tseries)
library(lubridate)
library(aTSA)
library(prophet) 
library(plyr)
library(opera)
options(warn=-1)
setwd("C:/Users/Sriram Rachamadugu/Documents/MSBA Course Material/Marketing Analytics/Project/Data")
rm(list=ls(all=T))


#Reading the files
AEP=read.csv("AEP_hourly.csv",stringsAsFactors = FALSE)
COMED=read.csv("COMED_hourly.csv",stringsAsFactors = FALSE)
DAYTON=read.csv("DAYTON_hourly.csv",stringsAsFactors = FALSE)
DEOK=read.csv("DEOK_hourly.csv",stringsAsFactors = FALSE)
DOM=read.csv("DOM_hourly.csv",stringsAsFactors = FALSE)
DUQ=read.csv("DUQ_hourly.csv",stringsAsFactors = FALSE)
EKPC=read.csv("EKPC_hourly.csv",stringsAsFactors = FALSE)
FE=read.csv("FE_hourly.csv",stringsAsFactors = FALSE)
NI=read.csv("NI_hourly.csv",stringsAsFactors = FALSE)
PJM=read.csv("PJM_Load_hourly.csv",stringsAsFactors = FALSE)
PJME=read.csv("PJME_hourly.csv",stringsAsFactors = FALSE)
PJMW=read.csv("PJMW_hourly.csv",stringsAsFactors = FALSE)

#Aggregating the data
AEP$Datetime=as.Date(AEP$Datetime,format = "%Y-%m-%d")
AEP_aggregate=aggregate(list(AEP=AEP$AEP_MW),by=list(Category=AEP$Datetime),FUN=mean)

COMED$Datetime=as.Date(COMED$Datetime,format = "%Y-%m-%d")
COMED_aggregate=aggregate(list(COMED=COMED$COMED_MW),by=list(Category=COMED$Datetime),FUN=mean)

DAYTON$Datetime=as.Date(DAYTON$Datetime,format = "%Y-%m-%d")
DAYTON_aggregate=aggregate(list(DAYTON=DAYTON$DAYTON_MW),by=list(Category=DAYTON$Datetime),FUN=mean)

DEOK$Datetime=as.Date(DEOK$Datetime,format = "%Y-%m-%d")
DEOK_aggregate=aggregate(list(DEOK=DEOK$DEOK_MW),by=list(Category=DEOK$Datetime),FUN=mean)

DOM$Datetime=as.Date(DOM$Datetime,format = "%Y-%m-%d")
DOM_aggregate=aggregate(list(DOM=DOM$DOM_MW),by=list(Category=DOM$Datetime),FUN=mean)

DUQ$Datetime=as.Date(DUQ$Datetime,format = "%Y-%m-%d")
DUQ_aggregate=aggregate(list(DUQ=DUQ$DUQ_MW),by=list(Category=DUQ$Datetime),FUN=mean)

EKPC$Datetime=as.Date(EKPC$Datetime,format = "%Y-%m-%d")
EKPC_aggregate=aggregate(list(EKPC=EKPC$EKPC_MW),by=list(Category=EKPC$Datetime),FUN=mean)

FE$Datetime=as.Date(FE$Datetime,format = "%Y-%m-%d")
FE_aggregate=aggregate(list(FE=FE$FE_MW),by=list(Category=FE$Datetime),FUN=mean)

NI$Datetime=as.Date(NI$Datetime,format = "%Y-%m-%d")
NI_aggregate=aggregate(list(NI=NI$NI_MW),by=list(Category=NI$Datetime),FUN=mean)

PJM$Datetime=as.Date(PJM$Datetime,format = "%Y-%m-%d")
PJM_aggregate=aggregate(list(PJM=PJM$PJM_Load_MW),by=list(Category=PJM$Datetime),FUN=mean)

PJME$Datetime=as.Date(PJME$Datetime,format = "%Y-%m-%d")
PJME_aggregate=aggregate(list(PJME=PJME$PJME_MW),by=list(Category=PJME$Datetime),FUN=mean)

PJMW$Datetime=as.Date(PJMW$Datetime,format = "%Y-%m-%d")
PJMW_aggregate=aggregate(list(PJMW=PJMW$PJMW_MW),by=list(Category=PJMW$Datetime),FUN=mean)

#Merging the datasets
final_data=merge(AEP_aggregate,COMED_aggregate,by="Category",all=T)
final_data=merge(final_data,DAYTON_aggregate,by="Category",all=T)
final_data=merge(final_data,DEOK_aggregate,by="Category",all=T)
final_data=merge(final_data,DOM_aggregate,by="Category",all=T)
final_data=merge(final_data,DUQ_aggregate,by="Category",all=T)
final_data=merge(final_data,EKPC_aggregate,by="Category",all=T)
final_data=merge(final_data,FE_aggregate,by="Category",all=T)
final_data=merge(final_data,NI_aggregate,by="Category",all=T)
final_data=merge(final_data,PJM_aggregate,by="Category",all=T)
final_data=merge(final_data,PJME_aggregate,by="Category",all=T)
final_data=merge(final_data,PJMW_aggregate,by="Category",all=T)
write.csv(final_data,"ADS.csv",row.names=FALSE)
##################################################################################################
#Finding the missing values
colSums(is.na(final_data))

#Finding the date ranges
for (i in 2:ncol(final_data)){
  print(colnames(final_data[i]))
  min=min(final_data[!is.na(final_data[,i]),"Category"])
  max=max(final_data[!is.na(final_data[,i]),"Category"])
  print(paste0("Starting Date:", min))
  print(paste0("Ending Date:", max))
}
#Removing NI and PJM
final_data[,c("NI","PJM")]=NULL

#Removing rows containing NA
final_data_raw<-final_data[final_data$Category>="2002-1-1",]
###################################################################################################
period_freq <- 12
no_of_predictions_valid <- 10
new_predictions<- 15
plt=list()
for (i in 2:ncol(final_data_raw)){
  print("-----------------------------------------------------------------------------------------")
  print(paste0("Forecasting for:", colnames(final_data_raw[i])))
  data_cluster=final_data_raw[,i]
  monthly<-aggregate(final_data_raw[,i]~month(Category)+year(Category), data = final_data_raw, mean)
  
  start_time <- min(final_data_raw[!is.na(data_cluster),"Category"]) 
  end_time <- max(final_data_raw[!is.na(data_cluster),"Category"])
  ts_start <- c(year(start_time), month(start_time))
  ts_end <- c(year(end_time), month(end_time))

  test<-final_data_raw[final_data_raw$Category>=start_time,i]
  ts_daily_raw <- ts(test,frequency = 365,start=start_time,end = end_time)
  ts_monthly_raw <- ts(monthly[,3],frequency =period_freq, start=ts_start, end=ts_end)
 
  
  
  #plots
  if(i==2){
    l1=autoplot(ts_monthly_raw) + ggtitle("Energy usage by Years") + ylab("Electricity (MW)") + xlab("Year")
    
    boxplot(ts_monthly_raw~cycle(ts_monthly_raw), xla='Month',xlab='Months', ylab='Electricity (MW)', main='Energy Consumption')
    
    l3=ggseasonplot(ts_monthly_raw, polar=TRUE) + ylab("Electricity(MW)") +
      ggtitle("Polar seasonal plot: Energy Consumption by month") + theme_bw(base_size = 15) 
    
    l4=ggseasonplot(ts_monthly_raw, year.labels=TRUE, year.labels.left=TRUE) + ylab("Electricity(MW)") +
      ggtitle("Seasonal plot: Energy Consumption") +theme_bw(base_size = 15)  
  
    print("stationary test")
    print(aTSA::adf.test(ts_monthly_raw,nlag = 12))  
  }

  
  #stationary test
   
  #print(tseries::adf.test(ts_monthly_raw))
  
  
  
  #Arima              #############################################################
  print("ARIMA")
  train <- window(ts_monthly_raw, end=(ts_end-no_of_predictions_valid/12))
  model_arima<- auto.arima(train, lambda=0, biasadj=TRUE,seasonal = TRUE)
  ARIMA <- forecast::forecast(model_arima, h=no_of_predictions_valid+1+new_predictions)
  print("MAPE:")
  print(accuracy(ARIMA, ts_monthly_raw)["Test set","MAPE"])
  
  
  #ETS                 ############################################################
  
  print("ETS")
  train <- window(ts_monthly_raw, end=(ts_end-no_of_predictions_valid/12))
  model_ets<-forecast::ets(train)
  ETS <- forecast::forecast(model_ets, h=no_of_predictions_valid+1+new_predictions)
  print("MAPE:")
  print(accuracy(ETS, ts_monthly_raw)["Test set","MAPE"])
  
  
  #NNAR                 ############################################################
  
  print("NNAR")
  train <- window(ts_monthly_raw, end=(ts_end-no_of_predictions_valid/12))
  model_nnar<-forecast::nnetar(train)
  NNAR <- forecast::forecast(model_nnar, h=no_of_predictions_valid+1+new_predictions)
  print("MAPE:")
  print(accuracy(NNAR, ts_monthly_raw)["Test set","MAPE"])
  
  #Prophet       ###########################################################
  
  print("Prophet")
  monthly$ds<-paste(monthly$`year(Category)`,"-",monthly$`month(Category)`,"-1",sep="")
  monthly$ds<-as.Date(monthly$ds,format = "%Y- %m- %d")
  train_prophet<-monthly
  train_prophet[,c(1,2)]=NULL
  train_prophet<-plyr::rename(train_prophet,c('final_data_raw[, i]'='y'))
  
  m<-prophet(train_prophet,weekly.seasonality=TRUE, daily.seasonality=TRUE)
  future <- make_future_dataframe(m, periods = new_predictions,freq = "month")
  forecast <- predict(m, future)
  length(train)
  if(i==2){
    l6=plot(m, forecast) + ylab("Electricity (MW)") + xlab("Date")+ggtitle("Energy Consumption")
  }
  
  final<-forecast[,c(1,22)]
  final$ds<-as.Date(final$ds)
  start_time1 <- min(final[,"ds"]) 
  end_time1 <- max(final[,"ds"])
  ts_start1 <- c(year(start_time1), month(start_time1))
  ts_end1 <- c(year(end_time1), month(end_time1))
  
  final_prophet <- ts(final$yhat,frequency =period_freq, start=ts_start1, end=ts_end1)
  final_prophet<-  window(final_prophet, start=(ts_end-no_of_predictions_valid/12))
  print("MAPE:")
  print(accuracy(final_prophet, ts_monthly_raw)["Test set","MAPE"])
  #Equal weights combination      #####################################################
  
  print("Equal Combinations")
  Combination <- (ETS[["mean"]] + ARIMA[["mean"]] +NNAR[["mean"]] )/3
  print("MAPE:")
  print(accuracy(Combination, ts_monthly_raw)["Test set","MAPE"])
  
  
  #Opera Combination        ########################################################
  
  print("Opera results")
  test <- window(ts_monthly_raw, start=(ts_end-no_of_predictions_valid/12))
  NNAR1=forecast::forecast(model_nnar,h=no_of_predictions_valid+1)
  ARIMA1=forecast::forecast(model_arima,h=no_of_predictions_valid+1)
  ETS1=forecast::forecast(model_ets,h=no_of_predictions_valid+1)
  X <- cbind(ETS1=ETS1$mean, ARIMA1=ARIMA1$mean, NNAR1=NNAR1$mean)
 
  MLpol0 <- mixture(model = "MLpol", loss.type = "square")
  weights <- predict(MLpol0, X, test, type='weights')

  z <- ts(predict(MLpol0, X, test, type='response'), start=c(2017,10), freq=12)
  print("MAPE:")
  print(accuracy(z, ts_monthly_raw)["Test set","MAPE"])
  
  #plotting new forecast values
  if(i==2){
    l5=autoplot(ts_monthly_raw) +
      autolayer(final_prophet,series = "Prophet")+
      autolayer(ARIMA, series="ARIMA", PI=FALSE) +
      autolayer(ETS, series="ETS", PI=FALSE) +
      autolayer(NNAR, series="NNAR", PI=FALSE)+
      autolayer(z,series = "Opera", PI=FALSE)+
      autolayer(Combination,series = "Combination", PI=FALSE) +
      xlab("Year") + ylab("Electricity (MW)") + ggtitle("Energy Forecasts")
    
  }
  print("-----------------------------------------------------------------------------------------")

}


#Time Series plot for AEP region
l1
#Polar Seasonal Plot for AEP monthly
l3
#Seasonal Plot for AEP monthly 
l4
#final plot containing all models
l5

