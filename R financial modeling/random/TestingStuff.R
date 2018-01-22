require(quantstrat)
require(PerformanceAnalytics)

GBPUSD<-read.csv(file="C://TickDownloader/tickdata/GBPUSD_D1_UTC+0_00.csv",header=TRUE,stringsAsFactors=FALSE)
names(GBPUSD)<-c("Date","Time","Open","High","Low","Close","Volume")
GBPUSD$DateTime<-paste(GBPUSD$Date, GBPUSD$Time)
GBPUSD$DateTime<-as.POSIXct(GBPUSD$DateTime,format='%Y.%m.%d %H:%M')
GBPUSD<-as.xts(zoo(GBPUSD[,3:7],order.by=(GBPUSD$DateTime)))
GBPUSD2015<-(GBPUSD["2015/"])


DaySurvival <- function(x) {
  t=SMA(x,n=30)
  tt=SMA(x,n=60)
  i=60
  f=1
  inputClose=x$Close
  inputOpen=x$Open
  y=length(inputClose)-60
  yy=length(inputClose)
  inputClose<-data.matrix(inputClose)
  inputOpen<-data.matrix(inputOpen)
  out<-matrix(data=NA, nrow=y,ncol=3)
  while(i<yy){
    out[f,1]<-inputOpen[i]
    out[f,2]<-inputClose[i]
    out[f,3]<-out[i,2]-out[i,1]
    i=i+1
    f=f+1
  }
  return(out)
}

if(t[i]>tt){
  out[i,1]=input
}