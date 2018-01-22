rm(list = ls())
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

"ADFX" <-
  function(HLC, n=14, maType=EMA, mktdata=mktdata, ...) {
    
    # Welles Wilder's Directional Movement Index
    HLC <- try.xts(HLC, error=as.matrix)
    dH  <- momentum(HLC[,1])
    dL  <- -momentum(HLC[,2])
    
    DMIp <- ifelse( dH==dL | (dH< 0 & dL< 0), 0, ifelse( dH >dL, dH, 0 ) )
    DMIn <- ifelse( dH==dL | (dH< 0 & dL< 0), 0, ifelse( dH <dL, dL, 0 ) )
    
    TR    <- ATR(HLC)[,"tr"]
    TRsum <- wilderSum(TR, n=n)
    
    DIp <- 100 * wilderSum(DMIp, n=n) / TRsum
    DIn <- 100 * wilderSum(DMIn, n=n) / TRsum
    
    DX  <- 100 * ( abs(DIp - DIn) / (DIp + DIn) )
    
    
    
    
    ADX <- EMA(DX,n=n,wilder=TRUE)
    
    result <- ADX
    result<- lag(result,1)
    result<-merge(mktdata,result)
    
    return(na.locf(result[,NCOL(result)]))
  }

"ADPX" <-
  function(HLC, n=14, maType=EMA, mktdata=mktdata, ...) {
    
    # Welles Wilder's Directional Movement Index
    HLC <- try.xts(HLC, error=as.matrix)
    dH  <- momentum(HLC[,1])
    dL  <- -momentum(HLC[,2])
    
    DMIp <- ifelse( dH==dL | (dH< 0 & dL< 0), 0, ifelse( dH >dL, dH, 0 ) )
    DMIn <- ifelse( dH==dL | (dH< 0 & dL< 0), 0, ifelse( dH <dL, dL, 0 ) )
    
    TR    <- ATR(HLC)[,"tr"]
    TRsum <- wilderSum(TR, n=n)
    
    DIp <- 100 * wilderSum(DMIp, n=n) / TRsum
    DIn <- 100 * wilderSum(DMIn, n=n) / TRsum
    
    DX  <- 100 * ( abs(DIp - DIn) / (DIp + DIn) )
    
    maArgs <- list(n=n, ...)
    
    
    
    result <- DIp
    result<-merge(mktdata,result)
    
    return(na.locf(result[,NCOL(result)]))
  }

"ADNX" <-
  function(HLC, n=14, maType=EMA, mktdata=mktdata,...) {
    
    # Welles Wilder's Directional Movement Index
    HLC <- try.xts(HLC, error=as.matrix)
    dH  <- momentum(HLC[,1])
    dL  <- -momentum(HLC[,2])
    
    DMIp <- ifelse( dH==dL | (dH< 0 & dL< 0), 0, ifelse( dH >dL, dH, 0 ) )
    DMIn <- ifelse( dH==dL | (dH< 0 & dL< 0), 0, ifelse( dH <dL, dL, 0 ) )
    
    TR    <- ATR(HLC)[,"tr"]
    TRsum <- wilderSum(TR, n=n)
    
    DIp <- 100 * wilderSum(DMIp, n=n) / TRsum
    DIn <- 100 * wilderSum(DMIn, n=n) / TRsum
    
    DX  <- 100 * ( abs(DIp - DIn) / (DIp + DIn) )
    
    maArgs <- list(n=n, ...)
    
    
    
    
    result <- DIn
    result<-merge(mktdata,result)
    
    return(na.locf(result[,NCOL(result)]))
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



GBPUSDlt<-read.csv(file="C://TickDownloader/tickdata/GBPUSD_H1_UTC+0_00.csv",header=TRUE,stringsAsFactors=FALSE)
names(GBPUSDlt)<-c("Date","Time","Open","High","Low","Close","Volume")
GBPUSDlt$DateTime<-paste(GBPUSDlt$Date, GBPUSDlt$Time)
GBPUSDlt$DateTime<-as.POSIXct(GBPUSDlt$DateTime,format='%Y.%m.%d %H:%M')
GBPUSDlt<-as.xts(zoo(GBPUSDlt[,3:7],order.by=(GBPUSDlt$DateTime)))
GBPUSDlt<-(GBPUSDlt["2014-03-01/2014-07-01"])
GBPUSD<-read.csv(file="C://TickDownloader/tickdata/GBPUSD_M1_UTC+0_00.csv",header=TRUE,stringsAsFactors=FALSE)
names(GBPUSD)<-c("Date","Time","Open","High","Low","Close","Volume")
GBPUSD$DateTime<-paste(GBPUSD$Date, GBPUSD$Time)
GBPUSD$DateTime<-as.POSIXct(GBPUSD$DateTime,format='%Y.%m.%d %H:%M')
GBPUSD<-as.xts(zoo(GBPUSD[,3:7],order.by=(GBPUSD$DateTime)))
GBPUSD<-(GBPUSD["2014-03-01/2014-07-01"])


x<-merge(GBPUSD,GBPUSDlt)
GBPUSD<-na.locf(x[,1:5])


## ------------------------------------------------------------------------
# moving average lengths
.fast = 4
.slow = 16
.trend= 4
.trendthresh = 25


# optimization range
.FastSMA = (2:15)
.SlowSMA = (10:20)
.trendrange= (2:20)
.trendthreshrange = (20:30)

# trade parameters
.threshold = 0.000
.orderqty = 10000
.txnfees = -6  # round-trip fee

# stop loss amount
.stoploss <- 0.0040
.StopLoss = seq(0.0010, 0.0200, length.out=20)

#take profit amount
.takeprofit<-0.0200
.TakeProfit = seq(0.0010,0.0200,length.out=20)

#trailingstop
.stoptrailing<-0.0040
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
                n = .fast
              ),
              label="nFast"
)

add.indicator(strategy.st, name="EMAt",
              arguments = list(
                x = quote(Cl(GBPUSDlt)[,1]),
                n = .slow
              ),
              label="nSlow"
)

add.indicator(strategy.st, name="ADFX",
              arguments = list(
                HLC = quote(HLC(GBPUSDlt)[,1:3]),
                n = .trend
              ),
              label="nTrendT"
)

add.indicator(strategy.st, name="ADPX",
              arguments = list(
                HLC = quote(HLC(GBPUSDlt)[,1:3]),
                n = .trend
              ),
              label="nTrendP"
)

add.indicator(strategy.st, name="ADNX",
              arguments = list(
                HLC = quote(HLC(GBPUSDlt)[,1:3]),
                n = .trend
              ),
              label="nTrendN"
)

## ----results='hide'------------------------------------------------------
add.signal(strategy.st, name='sigCrossover',
           arguments = list(
             columns=c("nFast","nSlow"),
             relationship="gte"
           ),
           label='shortfirst'
)


add.signal(strategy.st, name='sigCrossover',
           arguments = list(
             columns=c("nFast","nSlow"),
             relationship="lte"
           ),
           label='longfirst'
)

add.signal(strategy.st, name='sigComparison',
           arguments = list(
             columns=c("e1.nTrendP","e1.nTrendN"),
             relationship="gt"
           ),
           label='longsecond'
)

add.signal(strategy.st, name='sigComparison',
           arguments = list(
             columns=c("e1.nTrendP","e1.nTrendN"),
             relationship="lt"
           ),
           label='shortsecond'
)

add.signal(strategy.st, name='sigThreshold',
           arguments = list(
             column=c("EMA.nTrendT"), threshold=quote(.trendthresh),
             relationship="gte"
           ),
           label='confirmation'
)

add.signal(strategy.st, name='sigAND',
           arguments = list(
             columns=c("longfirst","longsecond","confirmation"), cross=FALSE
           ),
           label='long'
)


add.signal(strategy.st, name='sigAND',
           arguments = list(
             columns=c("shortfirst","shortsecond","confirmation"), cross=FALSE
           ),
           label='short'
)


## ----results='hide'------------------------------------------------------
add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='long' , sigval=TRUE,
                        orderside='long' ,
                        ordertype='stoplimit',
                        prefer='High',
                        threshold=.threshold,
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
                        ordertype='stoplimit',
                        prefer='Low',
                        threshold=-.threshold,
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
         arguments=list(sigcol='longfirst', sigval=TRUE,
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
         arguments=list(sigcol='shortfirst' , sigval=TRUE,
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
                        tmult=TRUE,
                        threshold=quote(.stoploss),
                        TxnFees=.txnfees,
                        orderqty='all',
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
                        tmult=TRUE,
                        threshold=quote(.stoploss),
                        TxnFees=.txnfees,
                        orderqty='all',
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
                        ordertype='stoplimit', tmult=TRUE, threshold=quote(.takeprofit),
                        TxnFees=.txnfees,
                        orderqty='all',
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
                        ordertype='stoplimit', tmult=TRUE, threshold=quote(.takeprofit),
                        TxnFees=.txnfees,
                        orderqty='all',
                        orderset='ocoshort'
         ),
         type='chain', parent='EnterSHORT',
         label='TakeProfitSHORT',
         enabled=FALSE
)

#trailingstop
add.rule(strategy.st, name = 'ruleSignal',
         arguments=list(sigcol='long' , sigval=TRUE,
                        replace=FALSE,
                        orderside='long',
                        ordertype='stoptrailing', tmult=TRUE, threshold=quote(.stoptrailing),
                        TxnFees=.txnfees,
                        orderqty='all',
                        orderset='ocolong'
         ),
         type='chain', parent='EnterLONG',
         label='StopTrailingLONG',
         enabled=FALSE
)

add.rule(strategy.st, name = 'ruleSignal',
         arguments=list(sigcol='short' , sigval=TRUE,
                        replace=FALSE,
                        orderside='short',
                        ordertype='stoptrailing', tmult=TRUE, threshold=quote(.stoptrailing),
                        TxnFees=.txnfees,
                        orderqty='all',
                        orderset='ocoshort'
         ),
         type='chain', parent='EnterSHORT',
         label='StopTrailingSHORT',
         enabled=FALSE
)

addPosLimit(
  portfolio=portfolio.st,
  symbol='GBPUSD',
  timestamp=initDate,
  maxpos=.orderqty)

enable.rule('luxor', 'chain', 'StopLoss')

enable.rule('luxor', 'chain', 'TakeProfit')

enable.rule('luxor', 'chain', 'TrailingStop')

add.distribution(strategy.st,
                 paramset.label = 'StopLoss',
                 component.type = 'chain',
                 component.label = 'StopLossLONG',
                 variable = list(threshold = .StopLoss),
                 label = 'StopLossLONG'
)

add.distribution(strategy.st,
                 paramset.label = 'StopLoss',
                 component.type = 'chain',
                 component.label = 'StopLossSHORT',
                 variable = list(threshold = .StopLoss),
                 label = 'StopLossSHORT'
)

add.distribution.constraint(strategy.st,
                            paramset.label = 'StopLoss',
                            distribution.label.1 = 'StopLossLONG',
                            distribution.label.2 = 'StopLossSHORT',
                            operator = '==',
                            label= "StopLoss"
)

add.distribution(strategy.st,
                 paramset.label = 'TakeProfit',
                 component.type = 'chain',
                 component.label = 'TakeProfitLONG',
                 variable = list(threshold = .TakeProfit),
                 label = 'TakeProfitLONG'
)

add.distribution(strategy.st,
                 paramset.label = 'TakeProfit',
                 component.type = 'chain',
                 component.label = 'TakeProfitSHORT',
                 variable = list(threshold = .TakeProfit),
                 label = 'TakeProfitSHORT'
)

add.distribution.constraint(strategy.st,
                            paramset.label = 'TakeProfit',
                            distribution.label.1 = 'TakeProfitLONG',
                            distribution.label.2 = 'TakeProfitSHORT',
                            operator = '==',
                            label = 'TakeProfit'
)

add.distribution(strategy.st,
                 paramset.label = 'StopTrailing',
                 component.type = 'chain',
                 component.label = 'StopTrailingLONG',
                 variable = list(threshold = .StopTrailing),
                 label = 'StopTrailingLONG'
)

add.distribution(strategy.st,
                 paramset.label = 'StopTrailing',
                 component.type = 'chain',
                 component.label = 'StopTrailingSHORT',
                 variable = list(threshold = .StopTrailing),
                 label = 'StopTrailingSHORT'
)

add.distribution.constraint(strategy.st,
                            paramset.label = 'StopTrailing',
                            distribution.label.1 = 'StopTrailingLONG',
                            distribution.label.2 = 'StopTrailingSHORT',
                            operator = '==',
                            label = 'StopTrailing'
)

## ----results='hide'------------------------------------------------------
add.distribution(strategy.st,
                 paramset.label = 'EMA',
                 component.type = 'indicator',
                 component.label = 'nFast',
                 variable = list(n = .FastSMA),
                 label = 'nFAST'
)



## ----results='hide'------------------------------------------------------
add.distribution(strategy.st,
                 paramset.label = 'EMA',
                 component.type = 'indicator',
                 component.label = 'nSlow',
                 variable = list(n = .SlowSMA),
                 label = 'nSLOW'
)





## ----results='hide'------------------------------------------------------
add.distribution.constraint(strategy.st,
                            paramset.label = 'EMA',
                            distribution.label.1 = 'nFAST',
                            distribution.label.2 = 'nSLOW',
                            operator = '<',
                            label = 'EMA'
)

##
add.distribution(strategy.st,
                 paramset.label = 'trend',
                 component.type = 'indicator',
                 component.label = 'nTrendT',
                 variable = list(n = .trendrange),
                 label = 'nTREND'
)

add.distribution(strategy.st,
                 paramset.label = 'trend',
                 component.type = 'signal',
                 component.label = 'confirmation',
                 variable = list(threshold = .trendthreshrange),
                 label = 'nTRENDTHRESH'
)

##
add.distribution(strategy.st,
                 paramset.label = 'Timespan',
                 component.type = 'rule',
                 component.label = 'confirmation',
                 variable = list(n = .timespans),
                 label = 'Timespan'
)

library(doParallel)
registerDoSEQ()

t1=Sys.time()
results <- apply.paramset(strategy.st, paramset.label='trend',
                          portfolio.st=portfolio.st, account.st=account.st, nsamples=0)
t2=Sys.time()
td=t2-t1
td

tS <- results$tradeStats
idx <- order(tS[,1],tS[,2])
tS <- tS[idx,]
PerformanceAnalytics:::textplot(t(tS)[,1:10])
#####################################################################################################
t1=Sys.time()
results <- apply.paramset(strategy.st, paramset.label='StopTrailing',
                          portfolio.st=portfolio.st, account.st=account.st, nsamples=0, verbose=FALSE)
t2=Sys.time()
t2-t1

z <- tapply(X=tS[,"End.Equity"],INDEX=list(StopLoss=tS[,1],TakeProfit=tS[,3]),FUN=sum)
z[1:5,1:10]

x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))
filled.contour(x=x,y=y,z=z,color = heat.colors,xlab="StopLoss",ylab="TakeProfit")
title("Net Profit")

# maxdd
z <- tapply(X=tS[,"Max.Drawdown"],INDEX=list(StopLoss=tS[,1],TakeProfit=tS[,2]),FUN=sum)
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))
filled.contour(x=x,y=y,z=z,color = heat.colors,xlab="StopLoss",ylab="TakeProfit")
title("Max Drawdown")

# profit factor
z <- tapply(X=tS[,"Profit.Factor"],INDEX=list(StopLoss=tS[,1],TakeProfit=tS[,2]),FUN=sum)
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))
filled.contour(x=x,y=y,z=z,color = heat.colors,xlab="StopLoss",ylab="TakeProfit")
title("Profit Factor")

# avg trade P&L
z <- tapply(X=tS[,"Avg.Trade.PL"],INDEX=list(StopLoss=tS[,1],TakeProfit=tS[,2]),FUN=sum)
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))
filled.contour(x=x,y=y,z=z,color = heat.colors,xlab="StopLoss",ylab="TakeProfit")
title("Average Trade")

# return to maxdd
z <- tapply(X=tS[,"Profit.To.Max.Draw"],
            INDEX=list(StopLoss=tS[,1],TakeProfit=tS[,2]),FUN=sum)
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))
filled.contour(x=x,y=y,z=z,color = heat.colors,xlab="StopLoss",ylab="TakeProfit")
title("Return to Max Drawdown")

rmdd <- tS$Profit.To.Max.Draw
idx <- order(rmdd,decreasing=T)[1:30]
labs <- paste(tS$StopLossLONG[idx],tS$TakeProfitLONG[idx],sep="/")
barplot(rmdd[idx],names.arg=labs,col=4,las=2,main="Return to MaxDrawdown")
