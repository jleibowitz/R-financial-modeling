  rm(list = ls())
  t1=Sys.time()
  require(quantstrat)
  require(PerformanceAnalytics)
  require(xts)
  try(rm(list=ls(pos=.blotter),pos=.blotter),silent=TRUE)
  try(rm(list=ls(pos=.strategy),pos=.strategy),silent=TRUE)
  try(rm(list=ls(pos=.instrument),pos=.instrument),silent=TRUE) 
  
  memory.limit(size=163140)
  
  
  
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
  
  
  "rett"<-function(x,data=mktdata,n){
    x<-merge(data,x)
    return(na.locf(x[,NCOL(x)]))
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
  GBPUSDlt<-(GBPUSDlt["2004-01-01/2014-12-31"])
  GBPUSDhr<-read.csv(file="C://TickDownloader/tickdata/GBPUSD_H1_UTC+0_00.csv",header=TRUE,stringsAsFactors=FALSE)
  names(GBPUSDhr)<-c("Date","Time","Open","High","Low","Close","Volume")
  GBPUSDhr$DateTime<-paste(GBPUSDhr$Date, GBPUSDhr$Time)
  GBPUSDhr$DateTime<-as.POSIXct(GBPUSDhr$DateTime,format='%Y.%m.%d %H:%M')
  GBPUSDhr<-as.xts(zoo(GBPUSDhr[,3:7],order.by=(GBPUSDhr$DateTime)))
  GBPUSDhr<-(GBPUSDhr["2004-01-01/2014-12-31"])
  GBPUSDmt<-read.csv(file="C://TickDownloader/tickdata/GBPUSD_M15_UTC+0_00.csv",header=TRUE,stringsAsFactors=FALSE)
  names(GBPUSDmt)<-c("Date","Time","Open","High","Low","Close","Volume")
  GBPUSDmt$DateTime<-paste(GBPUSDmt$Date, GBPUSDmt$Time)
  GBPUSDmt$DateTime<-as.POSIXct(GBPUSDmt$DateTime,format='%Y.%m.%d %H:%M')
  GBPUSDmt<-as.xts(zoo(GBPUSDmt[,3:7],order.by=(GBPUSDmt$DateTime)))
  GBPUSDmt<-(GBPUSDmt["2004-01-01/2014-12-31"])
  GBPUSD<-read.csv(file="C://TickDownloader/tickdata/GBPUSD_M1_UTC+0_00.csv",header=TRUE,stringsAsFactors=FALSE)
  names(GBPUSD)<-c("Date","Time","Open","High","Low","Close","Volume")
  GBPUSD$DateTime<-paste(GBPUSD$Date, GBPUSD$Time)
  GBPUSD$DateTime<-as.POSIXct(GBPUSD$DateTime,format='%Y.%m.%d %H:%M')
  GBPUSD<-as.xts(zoo(GBPUSD[,3:7],order.by=(GBPUSD$DateTime)))
  GBPUSD<-(GBPUSD["2004-01-01/2014-12-31"])
  
  
  x<-merge(GBPUSD,GBPUSDlt,GBPUSDmt,GBPUSDhr)
  GBPUSD<-na.locf(x[,1:5])
  GBPUSD<-na.locf(GBPUSD[,1:5],fromLast=TRUE)
  
  ## ------------------------------------------------------------------------
  # moving average lengths
  .fastl = 28
  .slowl = 200
  .fasthravg = 25
  .nothing=0
  
  # optimization range
  .Fastl = (23:35)
  .Slowl = (10:40)
  .Fastm = (2:75)
  .Slowm = (10:40)
  .FastHrAvg= (23:35)
  
  
  
  # trade parameters
  .threshold = 0.0000
  .orderqty = 10000
  .txnfees = 0  # round-trip fee
  
  # stop loss amount
  .stoploss <- 0.0200
  .StopLoss = seq(0.0100, 0.0400, length.out=5)
  
  #take profit amount
  .takeprofit<-0.0200
  .TakeProfit = seq(0.0100, 0.0400, length.out=5)
  
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
                  x = quote(GBPUSDhr$Low),
                  n = .fasthravg
                ),
                label="nFasthravglow"
  )
  
  add.indicator(strategy.st, name = "EMAt",
                arguments = list(
                  x = quote(GBPUSDhr$High),
                  n = .fasthravg
                ),
                label="nFasthravghigh"
  )
  
  add.indicator(strategy.st, name = "rett",
                arguments = list(
                  x = quote(Op(GBPUSDmt)[,1]),
                  n=.nothing
                ),
                label="nFastm"
  )
  
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
  
  
  
  
  
  ## ----results='hide'------------------------------------------------------
  
  
  add.signal(strategy.st, name='sigCrossover',
             arguments = list(
               columns=c("nFastm","nFasthravghigh"),
               relationship="gte"
             ),
             label='shortfirst'
  )
  
  
  add.signal(strategy.st, name='sigCrossover',
             arguments = list(
               columns=c("nFastm","nFasthravglow"),
               relationship="lte"
             ),
             label='longfirst'
  )
  
  add.signal(strategy.st, name='sigCrossover',
             arguments = list(
               columns=c("nFastl","nSlowl"),
               relationship="gte"
             ),
             label='longsecond'
  )
  
  add.signal(strategy.st, name='sigCrossover',
             arguments = list(
               columns=c("nFastl","nSlowl"),
               relationship="lte"
             ),
             label='shortsecond'
  )
  
  add.signal(strategy.st, name='sigAND',
             arguments = list(
               columns=c("longfirst","longsecond"), cross=FALSE
             ),
             label='long'
  )
  
  
  add.signal(strategy.st, name='sigAND',
             arguments = list(
               columns=c("shortfirst","shortsecond"), cross=FALSE
             ),
             label='short'
  )
  
  
  
  ## ----results='hide'------------------------------------------------------
  add.rule(strategy.st, name='ruleSignal',
           arguments=list(sigcol='longsecond' , sigval=TRUE,
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
           arguments=list(sigcol='shortsecond', sigval=TRUE,
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
           arguments=list(sigcol='shortsecond', sigval=TRUE,
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
           arguments=list(sigcol='longsecond' , sigval=TRUE,
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
  
  
  #stoploss
  add.rule(strategy.st, name = 'ruleSignal',
           arguments=list(sigcol='long' , sigval=TRUE,
                          replace=FALSE,
                          orderside='long',
                          ordertype='stoplimit',
                          tmult=FALSE,
                          threshold=quote(.stoploss),
                          TxnFees=.txnfees,
                          orderqty=-.orderqty,
                          orderset='ocolong'
           ),
           type='chain', parent='EnterLONG',
           label='StopLossLONG',
           enabled=FALSE
  )
  
  add.rule(strategy.st, name = 'ruleSignal',
           arguments=list(sigcol='short' , sigval=TRUE,
                          replace=FALSE,
                          orderside='short',
                          ordertype='stoplimit',
                          tmult=FALSE,
                          threshold=quote(.stoploss),
                          TxnFees=.txnfees,
                          orderqty=.orderqty,
                          orderset='ocoshort'
           ),
           type='chain', parent='EnterSHORT',
           label='StopLossSHORT',
           enabled=FALSE
  )
  
  # take-profit
  
  add.rule(strategy.st, name = 'ruleSignal',
           arguments=list(sigcol='long' , sigval=TRUE,
                          replace=FALSE,
                          orderside='long',
                          ordertype='limit', tmult=FALSE, threshold=quote(.takeprofit),
                          TxnFees=.txnfees,
                          orderqty=-.orderqty,
                          orderset='ocolong'
           ),
           type='chain', parent='EnterLONG',
           label='TakeProfitLONG',
           enabled=FALSE
  )
  
  add.rule(strategy.st, name = 'ruleSignal',
           arguments=list(sigcol='short' , sigval=TRUE,
                          replace=FALSE,
                          orderside='short',
                          ordertype='limit', tmult=FALSE, threshold=quote(.takeprofit),
                          TxnFees=.txnfees,
                          orderqty=.orderqty,
                          orderset='ocoshort'
           ),
           type='chain', parent='EnterSHORT',
           label='TakeProfitSHORT',
           enabled=FALSE
  )
  
  
  addPosLimit(
    portfolio=portfolio.st,
    symbol='GBPUSD',
    timestamp=initDate,
    maxpos=.orderqty)
  
  add.distribution(strategy.st,
                   paramset.label = 'EMA',
                   component.type = 'indicator',
                   component.label = 'nFastl',
                   variable = list(n = .Fastl),
                   label = 'nFASTl'
  )
  
  
  library(doParallel)
  registerDoSEQ()
  
  
  results <- apply.paramset(strategy.st, paramset.label='EMA',
                            portfolio.st=portfolio.st, account.st=account.st, nsamples=0)
  
  
  tSA <- results$tradeStats
  Z<-tSA[which(tSA$Net.Trading.PL==max(tSA$Net.Trading.PL)),1]
  
  require(quantstrat)
  require(PerformanceAnalytics)
  require(xts)
  try(rm(list=ls(pos=.blotter),pos=.blotter),silent=TRUE)
  try(rm(list=ls(pos=.strategy),pos=.strategy),silent=TRUE)
  try(rm(list=ls(pos=.instrument),pos=.instrument),silent=TRUE) 
  
  memory.limit(size=163140)
  
  
  
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
  
  
  "rett"<-function(x,data=mktdata,n){
    x<-merge(data,x)
    return(na.locf(x[,NCOL(x)]))
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
  GBPUSDlt<-(GBPUSDlt["2004-01-01/2014-12-31"])
  GBPUSDhr<-read.csv(file="C://TickDownloader/tickdata/GBPUSD_H1_UTC+0_00.csv",header=TRUE,stringsAsFactors=FALSE)
  names(GBPUSDhr)<-c("Date","Time","Open","High","Low","Close","Volume")
  GBPUSDhr$DateTime<-paste(GBPUSDhr$Date, GBPUSDhr$Time)
  GBPUSDhr$DateTime<-as.POSIXct(GBPUSDhr$DateTime,format='%Y.%m.%d %H:%M')
  GBPUSDhr<-as.xts(zoo(GBPUSDhr[,3:7],order.by=(GBPUSDhr$DateTime)))
  GBPUSDhr<-(GBPUSDhr["2004-01-01/2014-12-31"])
  GBPUSDmt<-read.csv(file="C://TickDownloader/tickdata/GBPUSD_M15_UTC+0_00.csv",header=TRUE,stringsAsFactors=FALSE)
  names(GBPUSDmt)<-c("Date","Time","Open","High","Low","Close","Volume")
  GBPUSDmt$DateTime<-paste(GBPUSDmt$Date, GBPUSDmt$Time)
  GBPUSDmt$DateTime<-as.POSIXct(GBPUSDmt$DateTime,format='%Y.%m.%d %H:%M')
  GBPUSDmt<-as.xts(zoo(GBPUSDmt[,3:7],order.by=(GBPUSDmt$DateTime)))
  GBPUSDmt<-(GBPUSDmt["2004-01-01/2014-12-31"])
  GBPUSD<-read.csv(file="C://TickDownloader/tickdata/GBPUSD_M1_UTC+0_00.csv",header=TRUE,stringsAsFactors=FALSE)
  names(GBPUSD)<-c("Date","Time","Open","High","Low","Close","Volume")
  GBPUSD$DateTime<-paste(GBPUSD$Date, GBPUSD$Time)
  GBPUSD$DateTime<-as.POSIXct(GBPUSD$DateTime,format='%Y.%m.%d %H:%M')
  GBPUSD<-as.xts(zoo(GBPUSD[,3:7],order.by=(GBPUSD$DateTime)))
  GBPUSD<-(GBPUSD["2004-01-01/2014-12-31"])
  
  
  
  x<-merge(GBPUSD,GBPUSDlt,GBPUSDmt,GBPUSDhr)
  GBPUSD<-na.locf(x[,1:5])
  GBPUSD<-na.locf(GBPUSD[,1:5],fromLast=TRUE)
  
  ## ------------------------------------------------------------------------
  # moving average lengths
  .fastl = Z
  .slowl = 200
  .fasthravg = 25
  .nothing=0
  
  # optimization range
  .Fastl = (23:35)
  .Slowl = (10:40)
  .Fastm = (2:75)
  .Slowm = (10:40)
  .FastHrAvg= (23:35)
  
  
  
  # trade parameters
  .threshold = 0.0000
  .orderqty = 10000
  .txnfees = 0  # round-trip fee
  
  # stop loss amount
  .stoploss <- 0.0200
  .StopLoss = seq(0.0100, 0.0400, length.out=5)
  
  #take profit amount
  .takeprofit<-0.0200
  .TakeProfit = seq(0.0100, 0.0400, length.out=5)
  
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
                  x = quote(GBPUSDhr$Low),
                  n = .fasthravg
                ),
                label="nFasthravglow"
  )
  
  add.indicator(strategy.st, name = "EMAt",
                arguments = list(
                  x = quote(GBPUSDhr$High),
                  n = .fasthravg
                ),
                label="nFasthravghigh"
  )
  
  add.indicator(strategy.st, name = "rett",
                arguments = list(
                  x = quote(Op(GBPUSDmt)[,1]),
                  n=.nothing
                ),
                label="nFastm"
  )
  
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
  
  
  
  
  
  ## ----results='hide'------------------------------------------------------
  
  
  add.signal(strategy.st, name='sigCrossover',
             arguments = list(
               columns=c("nFastm","nFasthravghigh"),
               relationship="gte"
             ),
             label='shortfirst'
  )
  
  
  add.signal(strategy.st, name='sigCrossover',
             arguments = list(
               columns=c("nFastm","nFasthravglow"),
               relationship="lte"
             ),
             label='longfirst'
  )
  
  add.signal(strategy.st, name='sigComparison',
             arguments = list(
               columns=c("nFastl","nSlowl"),
               relationship="gte"
             ),
             label='longsecond'
  )
  
  add.signal(strategy.st, name='sigComparison',
             arguments = list(
               columns=c("nFastl","nSlowl"),
               relationship="lte"
             ),
             label='shortsecond'
  )
  
  add.signal(strategy.st, name='sigAND',
             arguments = list(
               columns=c("longfirst","longsecond"), cross=FALSE
             ),
             label='long'
  )
  
  
  add.signal(strategy.st, name='sigAND',
             arguments = list(
               columns=c("shortfirst","shortsecond"), cross=FALSE
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
           arguments=list(sigcol='short', sigval=TRUE,
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
           arguments=list(sigcol='long' , sigval=TRUE,
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
  
  
  #stoploss
  add.rule(strategy.st, name = 'ruleSignal',
           arguments=list(sigcol='long' , sigval=TRUE,
                          replace=FALSE,
                          orderside='long',
                          ordertype='stoplimit',
                          tmult=FALSE,
                          threshold=quote(.stoploss),
                          TxnFees=.txnfees,
                          orderqty=-.orderqty,
                          orderset='ocolong'
           ),
           type='chain', parent='EnterLONG',
           label='StopLossLONG',
           enabled=FALSE
  )
  
  add.rule(strategy.st, name = 'ruleSignal',
           arguments=list(sigcol='short' , sigval=TRUE,
                          replace=FALSE,
                          orderside='short',
                          ordertype='stoplimit',
                          tmult=FALSE,
                          threshold=quote(.stoploss),
                          TxnFees=.txnfees,
                          orderqty=.orderqty,
                          orderset='ocoshort'
           ),
           type='chain', parent='EnterSHORT',
           label='StopLossSHORT',
           enabled=FALSE
  )
  
  # take-profit
  
  add.rule(strategy.st, name = 'ruleSignal',
           arguments=list(sigcol='long' , sigval=TRUE,
                          replace=FALSE,
                          orderside='long',
                          ordertype='limit', tmult=FALSE, threshold=quote(.takeprofit),
                          TxnFees=.txnfees,
                          orderqty=-.orderqty,
                          orderset='ocolong'
           ),
           type='chain', parent='EnterLONG',
           label='TakeProfitLONG',
           enabled=FALSE
  )
  
  add.rule(strategy.st, name = 'ruleSignal',
           arguments=list(sigcol='short' , sigval=TRUE,
                          replace=FALSE,
                          orderside='short',
                          ordertype='limit', tmult=FALSE, threshold=quote(.takeprofit),
                          TxnFees=.txnfees,
                          orderqty=.orderqty,
                          orderset='ocoshort'
           ),
           type='chain', parent='EnterSHORT',
           label='TakeProfitSHORT',
           enabled=FALSE
  )
  
  
  addPosLimit(
    portfolio=portfolio.st,
    symbol='GBPUSD',
    timestamp=initDate,
    maxpos=.orderqty)
  
  
  add.distribution(strategy.st,
                   paramset.label = 'EMA',
                   component.type = 'indicator',
                   component.label = 'nFasthravglow',
                   variable = list(n = .FastHrAvg),
                   label = 'nFASThravglow'
  )
  
  add.distribution(strategy.st,
                   paramset.label = 'EMA',
                   component.type = 'indicator',
                   component.label = 'nFasthravghigh',
                   variable = list(n = .FastHrAvg),
                   label = 'nFASThravghigh'
  )  
  
  add.distribution.constraint(strategy.st,
                              paramset.label = 'EMA',
                              distribution.label.1 = 'nFASThravglow',
                              distribution.label.2 = 'nFASThravghigh',
                              operator = '==',
                              label = 'nFastHrAVG'
  )
  
  library(doParallel)
  registerDoSEQ()
# enable.rule('luxor', 'chain', 'StopLoss')
  
 # enable.rule('luxor', 'chain', 'TakeProfit')
  
  
  results <- apply.paramset(strategy.st, paramset.label='EMA',
                            portfolio.st=portfolio.st, account.st=account.st, nsamples=0)
  Sys.time()-t1
  
  tSB <- results$tradeStats
  ZB<-tSB[which(tSB$Net.Trading.PL==max(tSB$Net.Trading.PL)),1]
  Z
  ZB
