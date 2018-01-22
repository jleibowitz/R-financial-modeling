t1=Sys.time()
for(i in 1:length(timevector)){  

require(quantstrat)
  require(PerformanceAnalytics)
  require(xts)
  try(rm(list=ls(pos=.blotter),pos=.blotter),silent=TRUE)
  try(rm(list=ls(pos=.strategy),pos=.strategy),silent=TRUE)
  try(rm(list=ls(pos=.instrument),pos=.instrument),silent=TRUE) 
  
  memory.limit(size=163140)
  
  "thresholdgen"<-function(data=mktdata,threshold){
    ret_sig=NULL
    ret_sig<-data[,1]
    ret_sig<-replace(ret_sig,ret_sig>0,threshold)
    return(ret_sig)
  }
  
  "sigAND" <- function(label, data=mktdata, columns,  cross = FALSE) {
    ret_sig = NULL
    colNums <- rep(0, length(columns))
    for(i in 1:length(columns)) {
      colNums[i] <- match.names(columns[i], colnames(data))
    }
    ret_sig <- data[, colNums[1]]
    for(i in 2:length(colNums)) {
      ret_sig <- ret_sig & data[, colNums[i]]
    }
    ret_sig <- ret_sig*1
    if (isTRUE(cross)) 
      ret_sig <- diff(ret_sig) == 1
    colnames(ret_sig) <- label
    return(ret_sig)
  }
  
  "indSubtract"<- function (x,y){
    ret_sig=NULL
    sub=abs(x-y)
    ret_sig<-sub
    return(ret_sig)
    
  }
  
  
  "EMAt" <-
    function (x, n=10, wilder=FALSE, ratio=NULL, mktdata=mktdata, ...) {
      
      # Exponential Moving Average
      x <- try.xts(x, error=as.matrix)
      if( n < 1 || n > NROW(x) )
        stop("Invalid 'n'")
      if( any(nNonNA <- n > colSums(!is.na(x))) )
        stop("n > number of non-NA values in column(s) ",
             paste(which(nNonNA), collapse=", "))
      
      # Check for non-leading NAs
      # Leading NAs are handled in the C code
      x.na <- naCheck(x, n)
      
      # If ratio is specified, and n is not, set n to approx 'correct'
      # value backed out from ratio
      if(missing(n) && !missing(ratio))
        n <- trunc(2/ratio - 1)
      
      # Determine decay ratio
      if(is.null(ratio)) {
        if(wilder) ratio <- 1/n
        else       ratio <- 2/(n+1)
      }
      
      # Call C routine
      ma <- .Call("ema", x, n, ratio, PACKAGE = "TTR")
      
      ma <- reclass(ma,x)
      
      if(!is.null(dim(ma))) {
        colnames(ma) <- "EMA"
      }
      ma<-lag(ma,1)
      ma<-merge(mktdata,ma)
      return(na.locf(ma[,NCOL(ma)]))
      
    }
  
  
  
  Sys.setenv(TZ="UTC")
  library(quantstrat)
  
  
  ## ------------------------------------------------------------------------
  initDate = '2002-10-21'
  .from=initDate
  .to='2002-10-31'
  
  ## ----results='hide'------------------------------------------------------
  currency(c('GBP', 'USD'))
  exchange_rate('GBPUSD', tick_size=0.0001)
  
  
  
  
  GBPUSDlt<-read.csv(file="C://TickDownloader/tickdata/GBPUSD_D1_UTC+0_00.csv",header=TRUE,stringsAsFactors=FALSE)
  names(GBPUSDlt)<-c("Date","Time","Open","High","Low","Close","Volume")
  GBPUSDlt$DateTime<-paste(GBPUSDlt$Date, GBPUSDlt$Time)
  GBPUSDlt$DateTime<-as.POSIXct(GBPUSDlt$DateTime,format='%Y.%m.%d %H:%M')
  GBPUSDlt<-as.xts(zoo(GBPUSDlt[,3:7],order.by=(GBPUSDlt$DateTime)))
  GBPUSDlt<-(GBPUSDlt["2010-01-01/2014-12-31"])
  GBPUSDmt<-read.csv(file="C://TickDownloader/tickdata/GBPUSD_M15_UTC+0_00.csv",header=TRUE,stringsAsFactors=FALSE)
  names(GBPUSDmt)<-c("Date","Time","Open","High","Low","Close","Volume")
  GBPUSDmt$DateTime<-paste(GBPUSDmt$Date, GBPUSDmt$Time)
  GBPUSDmt$DateTime<-as.POSIXct(GBPUSDmt$DateTime,format='%Y.%m.%d %H:%M')
  GBPUSDmt<-as.xts(zoo(GBPUSDmt[,3:7],order.by=(GBPUSDmt$DateTime)))
  GBPUSDmt<-(GBPUSDmt["2010-01-01/2014-12-31"])
  GBPUSD<-read.csv(file="C://TickDownloader/tickdata/GBPUSD_M1_UTC+0_00.csv",header=TRUE,stringsAsFactors=FALSE)
  names(GBPUSD)<-c("Date","Time","Open","High","Low","Close","Volume")
  GBPUSD$DateTime<-paste(GBPUSD$Date, GBPUSD$Time)
  GBPUSD$DateTime<-as.POSIXct(GBPUSD$DateTime,format='%Y.%m.%d %H:%M')
  GBPUSD<-as.xts(zoo(GBPUSD[,3:7],order.by=(GBPUSD$DateTime)))
  GBPUSD<-(GBPUSD["2010-01-01/2014-12-31"])
  
  
  x<-merge(GBPUSD,GBPUSDlt,GBPUSDmt)
  GBPUSD<-na.locf(x[,1:5])
  GBPUSD<-na.locf(GBPUSD[,1:5],fromLast=TRUE)
  
  ## ------------------------------------------------------------------------
  # moving average lengths
  .fastl = 5
  .slowl = 50
  .fastm=5
  .slowm=20
  .trendthresh = .0075
  
  
  # optimization range
  .Fastl = (2:20)
  .Slowl = (10:40)
  .Fastm = (2:20)
  .Slowm = (10:40)
  .Trendrange = seq(10,200,length.out=38)/10000
  
  
  
  # trade parameters
  .threshold = 0.0000
  .orderqty = 10000
  .txnfees = 0  # round-trip fee
  
  # stop loss amount
  .stoploss <- 0.0100
  .StopLoss = seq(0.0010, 0.0200, length.out=20)
  
  #take profit amount
  .takeprofit<-0.0100
  .TakeProfit = seq(0.0010,0.0200,length.out=20)
  
  #trailingstop
  .stoptrailing<-0.0100
  .StopTrailing=seq(0.0010,0.0200,length.out=20)
  
  # trading window
  #.timespan = 'T00:00/T23:59'
  .timespan = NULL
  .timespans.start<-paste(sprintf("T%02d",0:23),':00',sep='')
  .timespans.stop<-paste(sprintf("T%02d",0:23),':59',sep='')
  
  .timespans<-outer(.timespans.start, .timespans.stop, FUN=paste, sep='/')
  
  # number of optimization samples
  .nsamples=4
  
  ## ------------------------------------------------------------------------
  portfolio.st = 'forex'
  account.st = 'IB1'
  strategy.st = 'luxor'
  
  ## ------------------------------------------------------------------------
  rm.strat(portfolio.st)
  rm.strat(account.st)
  
  ## ----results='hide'------------------------------------------------------
  initPortf(portfolio.st, symbols='GBPUSD', initDate=initDate, currency='USD')
  initAcct(account.st, portfolios=portfolio.st,initDate=initDate,currency='USD')
  initOrders(portfolio.st, initDate=initDate)
  strategy(strategy.st, store=TRUE)
  
  add.indicator(strategy.st, name = "EMAt",
                arguments = list(
                  x = quote(Cl(GBPUSDlt)[,1]),
                  n = .fastl
                ),
                label="nFastl"
  )
  
  add.indicator(strategy.st, name="EMAt",
                arguments = list(
                  x = quote(Cl(GBPUSDlt)[,1]),
                  n = .slowl
                ),
                label="nSlowl"
  )
  
 add.indicator(strategy.st, name = "EMAt",
                  arguments = list(
                    x = quote(Cl(GBPUSDmt)[,1]),
                    n = .fastm
                  ),
                  label="nFastm"
    )
  
add.indicator(strategy.st, name="EMAt",
                  arguments = list(
                    x = quote(Cl(GBPUSDmt)[,1]),
                    n = .slowm
                  ),
                  label="nSlowm"
    )
  
  add.indicator(strategy.st, name="indSubtract",
                arguments = list(
                  x = quote(mktdata$EMA.nFastl),
                  y = quote(mktdata$EMA.nSlowl)
                ),
                label="trenddiff"
  )
  
  add.indicator(strategy.st,name="thresholdgen",
                arguments = list(
                  threshold=quote(.trendthresh)
                ), label="threshold"
                )
  
  ## ----results='hide'------------------------------------------------------
  add.signal(strategy.st, name='sigCrossover',
             arguments = list(
               columns=c("trenddiff","threshold"),
               relationship="gte"
             ),
             label='confirmationenter'
  )
  
  add.signal(strategy.st, name='sigCrossover',
             arguments = list(
               columns=c("trenddiff","threshold"),
               relationship="lte"
             ),
             label='confirmationexit'
  )
  
  add.signal(strategy.st, name='sigCrossover',
             arguments = list(
              columns=c("nFastm","nSlowm"),
              relationship="gte"
             ),
             label='shortfirst'
  )
  
  
  add.signal(strategy.st, name='sigCrossover',
             arguments = list(
               columns=c("nFastm","nSlowm"),
               relationship="lte"
             ),
             label='longfirst'
  )
  
  add.signal(strategy.st, name='sigComparison',
             arguments = list(
               columns=c("nFastl","nSlowl"),
               relationship="gt"
             ),
             label='longsecond'
  )
  
  add.signal(strategy.st, name='sigComparison',
             arguments = list(
               columns=c("nFastl","nSlowl"),
               relationship="lt"
             ),
             label='shortsecond'
  )
  
  
  add.signal(strategy.st, name='sigAND',
             arguments = list(
               columns=c("confirmationenter","longsecond"), cross=FALSE
             ),
             label='long'
  )
  
  
  add.signal(strategy.st, name='sigAND',
             arguments = list(
               columns=c("confirmationenter","shortsecond"), cross=FALSE
             ),
             label='short'
  )
  
  
  ## ----results='hide'------------------------------------------------------
  add.rule(strategy.st, name='ruleSignal',
           arguments=list(sigcol='long' , sigval=TRUE,
                          orderside='long' ,
                          ordertype='market',
                          prefer='High',
                          orderqty=+.orderqty,
                          replace=FALSE,
                          TxnFees=0,
                          osFUN=osMaxPos,
                          orderset='ocolong'
           ),
           type='enter',
           timespan=.timespan,
           label='EnterLONG'
  )
  
  
  ## ----results='hide'------------------------------------------------------
  add.rule(strategy.st, name='ruleSignal',
           arguments=list(sigcol='short', sigval=TRUE,
                          orderside='short',
                          ordertype='market',
                          prefer='Low',
                          orderqty=-.orderqty,
                          replace=FALSE,
                          TxnFees=0,
                          osFUN=osMaxPos,
                          orderset='ocoshort'
           ),
           type='enter',
           timespan=.timespan,
           label='EnterSHORT'
  )
  
  
  ## ----results='hide'------------------------------------------------------
  add.rule(strategy.st, name='ruleSignal',
           arguments=list(sigcol='confirmationexit', sigval=TRUE,
                          orderside='long' ,
                          ordertype='market',
                          orderqty='all',
                          TxnFees=.txnfees,
                          replace=TRUE,
                          orderset='ocolong'
           ),
           type='exit',
           timespan=.timespan,
           label='Exit2SHORT'
  )
  
  
  ## ----results='hide'------------------------------------------------------
  add.rule(strategy.st, name='ruleSignal',
           arguments=list(sigcol='confirmationexit' , sigval=TRUE,
                          orderside='short',
                          ordertype='market',
                          orderqty='all',
                          TxnFees=.txnfees,
                          replace=TRUE,
                          orderset='ocoshort'
           ),
           type='exit',
           timespan=.timespan,
           label='Exit2LONG'
  )
  
  
  
  addPosLimit(
    portfolio=portfolio.st,
    symbol='GBPUSD',
    timestamp=initDate,
    maxpos=.orderqty)
  
  
  


  
  add.distribution(strategy.st,
                   paramset.label = 'trend',
                   component.type = 'indicator',
                   component.label = 'threshold',
                   variable = list(threshold = .Trendrange),
                   label = 'nTrendRangeEnter'
  )
  

  
  library(doParallel)
  registerDoSEQ()
  
  t1=Sys.time()
  results <- apply.paramset(strategy.st, paramset.label='trend',
                            portfolio.st=portfolio.st, account.st=account.st, nsamples=0)
  Sys.time()-t1
  
  tS <- results$tradeStats
  idx <- order(tS[,1],tS[,2])
  tS <- tS[idx,]
  
  
  
  NetTradingPL[(length(NetTradingPL)+1):(length(NetTradingPL)+length(c(tS[which(tS$Net.Trading.PL==max(tS$Net.Trading.PL)),1],tS[which(tS$Net.Trading.PL==max(tS$Net.Trading.PL)),2])))]<-c(tS[which(tS$Net.Trading.PL==max(tS$Net.Trading.PL)),1],tS[which(tS$Net.Trading.PL==max(tS$Net.Trading.PL)),2])
  MaxDD[(length(MaxDD)+1):(length(MaxDD)+length(c(tS[which(tS$Max.Drawdown==min(tS$Max.Drawdown)),1],tS[which(tS$Max.Drawdown==min(tS$Max.Drawdown)),2])))]<-c(tS[which(tS$Max.Drawdown==min(tS$Max.Drawdown)),1],tS[which(tS$Max.Drawdown==min(tS$Max.Drawdown)),2])
  ProfitFactor[(length(ProfitFactor)+1):(length(ProfitFactor)+length(c(tS[which(tS$Profit.Factor==max(tS$Profit.Factor)),1],tS[which(tS$Profit.Factor==max(tS$Profit.Factor)),2])))]<-c(tS[which(tS$Profit.Factor==max(tS$Profit.Factor)),1],tS[which(tS$Profit.Factor==max(tS$Profit.Factor)),2])
  AvgTrdPL[(length(AvgTrdPL)+1):(length(AvgTrdPL)+length(c(tS[which(tS$Avg.Trade.PL==max(tS$Avg.Trade.PL)),1],tS[which(tS$Avg.Trade.PL==max(tS$Avg.Trade.PL)),2])))]<-c(tS[which(tS$Avg.Trade.PL==max(tS$Avg.Trade.PL)),1],tS[which(tS$Avg.Trade.PL==max(tS$Avg.Trade.PL)),2])
  ReturnbyMaxDD[(length(ReturnbyMaxDD)+1):(length(ReturnbyMaxDD)+length(c(tS[which(tS$Profit.To.Max.Draw==max(tS$Profit.To.Max.Draw)),1],tS[which(tS$Profit.To.Max.Draw==max(tS$Profit.To.Max.Draw)),2])))]<-c(tS[which(tS$Profit.To.Max.Draw==max(tS$Profit.To.Max.Draw)),1],tS[which(tS$Profit.To.Max.Draw==max(tS$Profit.To.Max.Draw)),2])
  
  NetTradingPL[(length(NetTradingPL)+1)]<-0
  MaxDD[(length(MaxDD)+1)]<-0
  ProfitFactor[(length(ProfitFactor)+1)]<-0
  AvgTrdPL[(length(AvgTrdPL)+1)]<-0
  ReturnbyMaxDD[(length(ReturnbyMaxDD)+1)]<-0
}
t2=Sys.time()
td=t2-t1
td