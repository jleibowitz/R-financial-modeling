rm(list = ls())
require(quantstrat)
require(PerformanceAnalytics)
require(xts)
try(rm(list=ls(pos=.blotter),pos=.blotter),silent=TRUE)
try(rm(list=ls(pos=.strategy),pos=.strategy),silent=TRUE)
try(rm(list=ls(pos=.instrument),pos=.instrument),silent=TRUE) 


GBPUSDlt<-read.csv(file="C://TickDownloader/tickdata/GBPUSD_M30_UTC+0_00.csv",header=TRUE,stringsAsFactors=FALSE)
names(GBPUSDlt)<-c("Date","Time","Open","High","Low","Close","Volume")
GBPUSDlt$DateTime<-paste(GBPUSDlt$Date, GBPUSDlt$Time)
GBPUSDlt$DateTime<-as.POSIXct(GBPUSDlt$DateTime,format='%Y.%m.%d %H:%M')
GBPUSDlt<-as.xts(zoo(GBPUSDlt[,3:7],order.by=(GBPUSDlt$DateTime)))
GBPUSDlt<-(GBPUSDlt["2010-01-01/2015-09-01"])
GBPUSD<-read.csv(file="C://TickDownloader/tickdata/GBPUSD_M1_UTC+0_00.csv",header=TRUE,stringsAsFactors=FALSE)
names(GBPUSD)<-c("Date","Time","Open","High","Low","Close","Volume")
GBPUSD$DateTime<-paste(GBPUSD$Date, GBPUSD$Time)
GBPUSD$DateTime<-as.POSIXct(GBPUSD$DateTime,format='%Y.%m.%d %H:%M')
GBPUSD<-as.xts(zoo(GBPUSD[,3:7],order.by=(GBPUSD$DateTime)))
GBPUSD<-(GBPUSD["2010-01-01/2015-09-01"])

x<-merge(GBPUSD,GBPUSDlt)
GBPUSD<-na.locf(x[,1:5])

initDate='2003-01-01'
.from=initDate
.to='2015-09-01'

currency(c('GBP', 'USD'))
exchange_rate('GBPUSD', tick_size=0.0001)
Sys.setenv(TZ="UTC")


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

"RSI" <- 
  function(price, n=14, maType, mktdata=mktdata, ...) {
    
    price <- try.xts(price, error=as.matrix)
    
    up <- momentum(price, n=1, na.pad=TRUE)
    which.dn <- which(up < 0)
    dn <- up*0
    dn[which.dn] <- -up[which.dn]
    up[which.dn] <- 0
    
    maArgs <- list(n=n, ...)
    # Default Welles Wilder EMA
    if(missing(maType)) {
      maType <- 'EMA'
      maArgs$wilder <- TRUE
    }
    
    # Case of two different 'maType's for both MAs.
    # e.g. RSI(price, n=14, maType=list(maUp=list(EMA,ratio=1/5), maDown=list(WMA,wts=1:10)) )
    if( is.list(maType) ) {
      
      # Make sure maType is a list of lists
      maTypeInfo <- sapply(maType,is.list)
      if( !(all(maTypeInfo) && length(maTypeInfo) == 2) ) {
        stop("If \'maType\' is a list, you must specify\n ",
             "*two* MAs (see Examples section of ?RSI)")
      }
      
      # If MA function has 'n' arg, see if it's populated in maType;
      # if it isn't, populate it with RSI's formal 'n'
      for(i in 1:length(maType)) {
        if( !is.null( formals(maType[[i]])$n ) && is.null( maType[[i]]$n ) ) {
          maType[[i]]$n <- n
        }
        mavgUp <- do.call( maType[[1]][[1]], c( list(up), maType[[1]][-1] ) )
        mavgDn <- do.call( maType[[2]][[1]], c( list(dn), maType[[2]][-1] ) )
      }
    }
    
    # Case of one 'maType' for both MAs.
    # e.g. RSI(price, n=14, maType="WMA", wts=volume )
    else {
      
      mavgUp <- do.call( maType, c( list(up), maArgs ) )
      mavgDn <- do.call( maType, c( list(dn), maArgs ) )
    }
    
    rsi <- 100 * mavgUp / ( mavgUp + mavgDn )
    
    rsi<-reclass( rsi, price )
    rsi<-merge(mktdata,rsi)
    return(na.locf(rsi[,NCOL(rsi)]))
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
    
    
    
    
    ADX <- EMA(DX,n=14,wilder=TRUE)
    
    result <- ADX
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
    xx<-HLC
    HLC<-na.omit(HLC)
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

# moving average lengths
.osc = 14
.trend= 14
# optimization range
.OscRSI=(14:30)
.TrendADX =(14:30)
# trade parameters
.threshold = 0.0005
.orderqty = 10000
.txnfees = -6 # round-trip fee
# stop loss amount
.stoploss <- .0025
.StopLoss = seq(0.05, 0.6, length.out=48)/100
.takeprofit <- .0050
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
#indicatiors
add.indicator(strategy.st, name = "RSI",
              arguments = list(
                price = quote(Cl(GBPUSDlt)[,1]),
                n = .osc
              ),
              label="nOsc"
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

#signals
add.signal(strategy.st, name='sigThreshold',
           arguments = list(
             column=c("EMA.nOsc"), threshold = 70,
             relationship="gte"
           ),
           label='shortfirst'
)


add.signal(strategy.st, name='sigThreshold',
           arguments = list(
             column=c("EMA.nOsc"), threshold=30,
             relationship="lte"
           ),
           label='longfirst'
)


add.signal(strategy.st, name='sigThreshold',
           arguments = list(
             column=c("EMA.nTrendT"), threshold=25,
             relationship="lt"
           ),
           label='entrysignalother'
)

add.signal(strategy.st, name='sigAND',
           arguments = list(
             columns=c("longfirst","entrysignalother"), cross=TRUE
           ),
           label='long'
)


add.signal(strategy.st, name='sigAND',
           arguments = list(
             columns=c("shortfirst","entrysignalother"), cross=TRUE
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

# take-profit

add.rule(strategy.st, name = 'ruleSignal',
         arguments=list(sigcol='long' , sigval=TRUE,
                        replace=FALSE,
                        orderside='long',
                        ordertype='limit', tmult=TRUE, threshold=quote(.takeprofit),
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
                        ordertype='limit', tmult=TRUE, threshold=quote(.takeprofit),
                        TxnFees=.txnfees,
                        orderqty='all',
                        orderset='ocoshort'
         ),
         type='chain', parent='EnterSHORT',
         label='TakeProfitSHORT',
         enabled=FALSE
)

enable.rule('mine', 'chain', 'StopLoss')
enable.rule('mine', 'chain', 'TakeProfit')

out <- applyStrategy(strategy.st, portfolio.st)
updatePortf(portfolio.st, Symbols='GBPUSD',
            Dates=paste('::',as.Date(Sys.time()),sep=''))

tStats <- tradeStats(Portfolios = portfolio.st, use = "trades",
                     inclZeroDays = FALSE)
tStats[, 4:ncol(tStats)] <- round(tStats[, 4:ncol(tStats)], 2)
print(data.frame(t(tStats[,-c(1,2)])))

chart.Posn(portfolio.st, "GBPUSD"
)

View(.blotter[["portfolio.forex"]]$summary)

ob <- getOrderBook(portfolio.st)$forex$GBPUSD
ob.df <- data.frame(Date=time(ob),coredata(ob))

to<-tradeOrderStats(portfolio.st,"GBPUSD")

aggPF <- sum(tStats$Gross.Profits) / -sum(tStats$Gross.Losses)
aggCorrect <- mean(tStats$Percent.Positive)
numTrades <- sum(tStats$Num.Trades)
meanAvgWLR <- mean(tStats$Avg.WinLoss.Ratio[
  tStats$Avg.WinLoss.Ratio < Inf], na.rm = TRUE)

portString <- paste0("portfolio.", portfolio.st)
portPL <- .blotter[[portString]]$summary$Net.Trading.PL
portPL <- portPL[-1,] #remove initialization date
plot(cumsum(portPL))

portDD <- .blotter[[portString]]$summary$Net.Trading.PL
portDD<-portDD[-1]
CumMax <- cummax(portDD)
Drawdown <- -(CumMax - portDD)
Drawdown<-rbind(xts(-max(portDD),order.by=first(index(Drawdown)-1)),Drawdown)
plot(cumsum(Drawdown))