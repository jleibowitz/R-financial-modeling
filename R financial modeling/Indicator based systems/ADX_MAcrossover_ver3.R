rm(list = ls())
require(quantstrat)
require(PerformanceAnalytics)
require(xts)
try(rm(list=ls(pos=.blotter),pos=.blotter),silent=TRUE)
try(rm(list=ls(pos=.strategy),pos=.strategy),silent=TRUE)
try(rm(list=ls(pos=.instrument),pos=.instrument),silent=TRUE) 


GBPUSDlt<-read.csv(file="C://TickDownloader/tickdata/GBPUSD_H1_UTC+0_00.csv",header=TRUE,stringsAsFactors=FALSE)
names(GBPUSDlt)<-c("Date","Time","Open","High","Low","Close","Volume")
GBPUSDlt$DateTime<-paste(GBPUSDlt$Date, GBPUSDlt$Time)
GBPUSDlt$DateTime<-as.POSIXct(GBPUSDlt$DateTime,format='%Y.%m.%d %H:%M')
GBPUSDlt<-as.xts(zoo(GBPUSDlt[,3:7],order.by=(GBPUSDlt$DateTime)))
GBPUSDlt<-(GBPUSDlt["2010-01-01/2015-09-01"])
GBPUSD<-read.csv(file="C://TickDownloader/tickdata/GBPUSD_M15_UTC+0_00.csv",header=TRUE,stringsAsFactors=FALSE)
names(GBPUSD)<-c("Date","Time","Open","High","Low","Close","Volume")
GBPUSD$DateTime<-paste(GBPUSD$Date, GBPUSD$Time)
GBPUSD$DateTime<-as.POSIXct(GBPUSD$DateTime,format='%Y.%m.%d %H:%M')
GBPUSD<-as.xts(zoo(GBPUSD[,3:7],order.by=(GBPUSD$DateTime)))
GBPUSD<-(GBPUSD["2010-01-01/2015-09-01"])

initDate='2008-01-01'
.from=initDate
.to='2015-09-01'

currency(c('GBP', 'USD'))
exchange_rate('GBPUSD', tick_size=0.0001)

"indMerge" <- function(mktdata=quote(mktdata),x){ 
  if (!is.xts(x)) x <- getSymbols(x, auto.assign = FALSE) 
  x <- cbind(mktdata, x[paste(first(index(mktdata)) , 
                              last(index(mktdata)) , sep='/')]) 
  x <- na.locf(x) 
  return(x[,NCOL(x)]) 
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

"EMA" <-
  function (x, n=10, wilder=FALSE, ratio=NULL, ...) {
    
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
    
    ma<-indMerge(x=ma)
    return(ma)
    
  }


"ADFX" <-
  function(HLC, n=14, maType=EMA, ...) {
    
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
    

  
    
    ADX <- EMA(DX,n=14,wilder=TRUE)
    
    
    result <- ADX
    result<-indMerge(x=result)
    
    return(result)
  }

"ADPX" <-
  function(HLC, n=14, maType=EMA, ...) {
    
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
    result<-indMerge(x=result)
    
    return(result)
  }

"ADNX" <-
  function(HLC, n=14, maType=EMA, ...) {
    
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
    result<-indMerge(x=result)
    
    return(result)
  }

# moving average lengths
.fast = 5
.slow = 15
.trend= 14
# optimization range
.FastEMA = (1:30)
.SlowEMA = (20:80)
.TrendADX =(14:30)
# trade parameters
.threshold = 0.0005
.orderqty = 100000
.txnfees = -6 # round-trip fee
# stop loss amount
.stoploss <- .0035
.StopLoss = seq(0.05, 0.6, length.out=48)/100
# trading window
.timespan = 'T00:00/T23:59'
# number of optimization samples
.nsamples=80

portfolio.st = 'forex'
account.st = 'IB1'
strategy.st = 'mine'

rm.strat(portfolio.st)
rm.strat(account.st)

initPortf(portfolio.st, symbols='GBPUSD', initDate=initDate, currency='USD')
initAcct(account.st, portfolios=portfolio.st,initDate=initDate,currency='USD')
initOrders(portfolio.st, initDate=initDate)
strategy(strategy.st, store=TRUE)

addPosLimit(
  portfolio=portfolio.st,
  symbol='GBPUSD',
  timestamp=initDate,
  maxpos=.orderqty)

#indicatiors
add.indicator(strategy.st, name = "EMA",
              arguments = list(
                x = quote(Cl(GBPUSDlt)[,1]),
                n = .fast
              ),
              label="nFast"
)

add.indicator(strategy.st, name="EMA",
              arguments = list(
                x = quote(Cl(mktdata)[,1]),
                n = .slow
              ),
              label="nSlw"
)

add.indicator(strategy.st, name="ADFX",
              arguments = list(
                HLC = quote(HLC(mktdata)[,1:3]),
                n = .trend
              ),
              label="nTrendT"
)

add.indicator(strategy.st, name="ADPX",
              arguments = list(
                HLC = quote(HLC(mktdata)[,1:3]),
                n = .trend
              ),
              label="nTrendP"
)

add.indicator(strategy.st, name="ADNX",
              arguments = list(
                HLC = quote(HLC(mktdata)[,1:3]),
                n = .trend
              ),
              label="nTrendN"
)

#signals
add.signal(strategy.st, name='sigCrossover',
           arguments = list(
             columns=c("EMA.nFast","EMA.nSlw"),
             relationship="gte"
           ),
           label='longfirst'
)


add.signal(strategy.st, name='sigCrossover',
           arguments = list(
             columns=c("EMA.nFast","EMA.nSlw"),
             relationship="lt"
           ),
           label='shortfirst'
)

add.signal(strategy.st, name='sigComparison',
           arguments = list(
             columns=c("X1.nTrendP","X1.nTrendN"),
             relationship="gt"
           ),
           label='entrysignalfirst'
)

add.signal(strategy.st, name='sigComparison',
           arguments = list(
             columns=c("X1.nTrendP","X1.nTrendN"),
             relationship="lt"
           ),
           label='entrysignalsecond'
)

add.signal(strategy.st, name='sigThreshold',
           arguments = list(
             column=c("EMA.nTrendT"), threshold=25,
             relationship="gte"
           ),
           label='entrysignalother'
)

add.signal(strategy.st, name='sigAND',
           arguments = list(
             columns=c("longfirst","entrysignalfirst","entrysignalother"), cross=TRUE
           ),
           label='long'
)


add.signal(strategy.st, name='sigAND',
           arguments = list(
             columns=c("shortfirst","entrysignalsecond","entrysignalother"), cross=TRUE
           ),
           label='short'
)


#rules
add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='long' , sigval=TRUE,
                        orderside='long' ,
                        ordertype='stoplimit',
                        prefer='High',
                        threshold=.threshold,
                        orderqty=+.orderqty,
                        replace=FALSE,
                        osFUN=osMaxPos,
                        orderset='ocolong'
         ),
         type='enter',
         timespan = .timespan,
         label='EnterLONG'
)

add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='short', sigval=TRUE,
                        orderside='short',
                        ordertype='stoplimit',
                        prefer='Low',
                        threshold=-.threshold,
                        orderqty=-.orderqty,
                        replace=FALSE,
                        osFUN=osMaxPos,
                        orderset='ocoshort'
         ),
         type='enter',
         timespan = .timespan,
         label='EnterSHORT'
)

add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='shortfirst', sigval=TRUE,
                        orderside='long' ,
                        ordertype='market',
                        orderqty='all',
                        TxnFees=.txnfees,
                        replace=TRUE,
                        orderset = 'ocolong'
         ),
         type='exit',
         timespan = .timespan,
         label='Exit2SHORT'
)

add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='longfirst' , sigval=TRUE,
                        orderside='short',
                        ordertype='market',
                        orderqty='all',
                        TxnFees=.txnfees,
                        replace=TRUE,
                        orderset = 'ocoshort'
         ),
         type='exit',
         timespan = .timespan,
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

enable.rule('mine', 'chain', 'StopLoss')

out <- applyStrategy(strategy.st, portfolio.st)
updatePortf(portfolio.st, Symbols='GBPUSD',
            Dates=paste('::',as.Date(Sys.time()),sep=''))

tStats <- tradeStats(Portfolios = portfolio.st, use = "trades",
                     inclZeroDays = FALSE)
tStats[, 4:ncol(tStats)] <- round(tStats[, 4:ncol(tStats)], 2)
print(data.frame(t(tStats[,-c(1,2)])))


chart.Posn(portfolio.st, "GBPUSD",
           TA="add_EMA(n=15,col='red');add_EMA(n=50,col='black')"
)

aggPF <- sum(tStats$Gross.Profits) / -sum(tStats$Gross.Losses)
aggCorrect <- mean(tStats$Percent.Positive)
numTrades <- sum(tStats$Num.Trades)
meanAvgWLR <- mean(tStats$Avg.WinLoss.Ratio[
  tStats$Avg.WinLoss.Ratio < Inf], na.rm = TRUE)

portString <- paste0("portfolio.", portfolio.st)
portPL <- .blotter[[portString]]$summary$Net.Trading.PL
portPL <- portPL[-1,] #remove initialization date
plot(cumsum(portPL))

#optimizing
add.distribution(strategy.st,
                 paramset.label = 'EMA',
                 component.type = 'indicator',
                 component.label = 'nFast',
                 variable = list(n = .FastSMA),
                 label = 'nFAST'
)

add.distribution(strategy.st,
                 paramset.label = 'EMA',
                 component.type = 'indicator',
                 component.label = 'nSlow',
                 variable = list(n = .SlowSMA),
                 label = 'nSLOW'
)

add.distribution.constraint(strategy.st,
                            paramset.label = 'EMA',
                            distribution.label.1 = 'nFAST',
                            distribution.label.2 = 'nSLOW',
                            operator = '<',
                            label = 'EMA'
)

add.distribution(strategy.st,
                 paramset.label = 'ADX',
                 component.type = 'indicator',
                 component.label = 'nTrend',
                 variable = list(n = .TrendADX),
                 label = 'nSLOW'
)

add.distribution(strategy.st,
                 paramset.label = 'Timespan',
                 component.type = 'rule',
                 component.label = 'timespan',
                 variable = list(n = .timespans),
                 label = 'Timespan'
)

rm.strat(portfolio.st)
rm.strat(account.st)


initPortf(portfolio.st, symbols='GBPUSD', initDate=initDate, currency='USD')
initAcct(account.st, portfolios=portfolio.st,
         initDate=initDate, currency='USD')
initOrders(portfolio.st, initDate=initDate)

if( Sys.info()['sysname'] == "Windows" )
{
  library(doParallel)
  registerDoSEQ()
} else {
  library(doMC)
  registerDoMC(cores=detectCores())
}

foreach(i=1:8, .combine=c) %dopar% sqrt(i)


results <- apply.paramset(strategy.st, paramset.label='EMA',
                          portfolio.st=portfolio.st, account.st=account.st, nsamples=0)

head(names(results),20)
