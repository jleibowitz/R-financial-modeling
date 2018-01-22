require(quantstrat)
require(PerformanceAnalytics)

GBPUSD<-read.csv(file="C://TickDownloader/tickdata/GBPUSD_M30_UTC+0_00.csv",header=TRUE,stringsAsFactors=FALSE)
names(GBPUSD)<-c("Date","Time","Open","High","Low","Close","Volume")
GBPUSD$DateTime<-paste(GBPUSD$Date, GBPUSD$Time)
GBPUSD$DateTime<-as.POSIXct(GBPUSD$DateTime,format='%Y.%m.%d %H:%M')
GBPUSD<-as.xts(zoo(GBPUSD[,3:7],order.by=(GBPUSD$DateTime)))
GBPUSD<-GBPUSD["2010/",]

rm.strat(portfolio.st) 
rm.strat(strategy.st)
rm.strat(account.st)

strategy.st = 'First'
portfolio.st = 'First'
account.st = 'First'

initPortf(portfolio.st, symbols='GBPUSD', initDate=initDate, currency='USD')
initAcct(account.st, portfolios=portfolio.st, initDate=initDate, currency='USD')
initOrders(portfolio.st, initDate=initDate)

accel <- function(x) {
  i=1
  input=x$Close
  y=length(input)
  input<-data.matrix(input)
  out<-matrix(data=NA, nrow=(length(input)-1),ncol=4)
  while(i<y){
    out[i,1]<-t(t(input[i+1,])-input[i,])
    out[i,2]<-t(t(input[i+4])-input[i,])
    out[i,3]<-t(t(input[i+8])-input[i,])
    out[i,4]<-t(t(input[i+10])-input[i,])
    i=i+1
  }
  return(out)
}

(t(t(xx[i+1,1])-xx[i,1]))>8 & 

vec<-matrix(data=NA,nrow=71018,ncol=1)
i=1
while(i<=71018){
  if((xx[i,1]>0) & (xx[i,2]>0)){
    vec[i,]<-xx[i,1]
  } 
  i=i+1
}


