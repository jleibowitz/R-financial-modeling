GBPUSD<-read.csv(file="C://TickDownloader/tickdata/GBPUSD_H1_UTC+0_00.csv",header=TRUE,stringsAsFactors=FALSE)
names(GBPUSD)<-c("Date","Time","Open","High","Low","Close","Volume")
GBPUSD$DateTime<-paste(GBPUSD$Date, GBPUSD$Time)
GBPUSD$DateTime<-as.POSIXct(GBPUSD$DateTime,format='%Y.%m.%d %H:%M')
GBPUSD<-as.xts(zoo(GBPUSD[,3:7],order.by=(GBPUSD$DateTime)))

beginx=2003
endx=2015
pdf('GBPUSDbiannual.pdf')
while(beginx<=endx){
  GBPUSDx<-GBPUSD[paste(beginx,"/",beginx,"-06",sep="")]
  chartSeries(GBPUSDx,TA='addEMA(20,col="red");addEMA(5);addRSI();addVo()',theme=chartTheme('white',up.col='white',dn.col='black'))
  GBPUSDx<-GBPUSD[paste(beginx,"-07/",beginx,sep="")]
  chartSeries(GBPUSDx,TA='addEMA(20,col="red");addEMA(5);addRSI();addVo()',theme=chartTheme('white',up.col='white',dn.col='black'))
  beginx=beginx+1
}
dev.off()

beginx=2003
endx=2015
pdf("GBPUSDmonthly.pdf")
beginm=5
while(beginx<=endx){
  endm=12
  while(beginm<=endm){
    GBPUSDx<-GBPUSD[paste(beginx,"-",beginm,"/",beginx,"-",beginm,sep="")]
    chartSeries(GBPUSDx,TA='addEMA(10,col="red");addEMA(5);addRSI(5);addVo()',theme=chartTheme('white',up.col='white',dn.col='black'))
    beginm=beginm+1
  }
  beginx=beginx+1
  beginm=1
}
dev.off()



GBPUSD<-read.csv(file="C://TickDownloader/tickdata/GBPUSD_D1_UTC+0_00.csv",header=TRUE,stringsAsFactors=FALSE)
names(GBPUSD)<-c("Date","Time","Open","High","Low","Close","Volume")
GBPUSD$DateTime<-paste(GBPUSD$Date, GBPUSD$Time)
GBPUSD$DateTime<-as.POSIXct(GBPUSD$DateTime,format='%Y.%m.%d %H:%M')
GBPUSD<-as.xts(zoo(GBPUSD[,3:7],order.by=(GBPUSD$DateTime)))
GBPUSD<-GBPUSD["2015/"]

pdf("GBPUSDhourlymonth.pdf")
i=1
working<-split.xts(GBPUSD,f="months")
t=length(working)
while(i<t){
  chartSeries(working[[i]],theme=chartTheme('white',up.col='white',dn.col='black'))
  i=i+1
}


dev.off()

