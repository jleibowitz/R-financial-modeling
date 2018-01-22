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
GBPUSDlt<-(GBPUSDlt["2015-03-01/2015-04-01"])
GBPUSDmt<-read.csv(file="C://TickDownloader/tickdata/GBPUSD_M15_UTC+0_00.csv",header=TRUE,stringsAsFactors=FALSE)
names(GBPUSDmt)<-c("Date","Time","Open","High","Low","Close","Volume")
GBPUSDmt$DateTime<-paste(GBPUSDmt$Date, GBPUSDmt$Time)
GBPUSDmt$DateTime<-as.POSIXct(GBPUSDmt$DateTime,format='%Y.%m.%d %H:%M')
GBPUSDmt<-as.xts(zoo(GBPUSDmt[,3:7],order.by=(GBPUSDmt$DateTime)))
GBPUSDmt<-(GBPUSDmt["2015-03-01/2015-04-01"])
GBPUSD<-read.csv(file="C://TickDownloader/tickdata/GBPUSD_M1_UTC+0_00.csv",header=TRUE,stringsAsFactors=FALSE)
names(GBPUSD)<-c("Date","Time","Open","High","Low","Close","Volume")
GBPUSD$DateTime<-paste(GBPUSD$Date, GBPUSD$Time)
GBPUSD$DateTime<-as.POSIXct(GBPUSD$DateTime,format='%Y.%m.%d %H:%M')
GBPUSD<-as.xts(zoo(GBPUSD[,3:7],order.by=(GBPUSD$DateTime)))
GBPUSD<-(GBPUSD["2015-03-01/2015-04-01"])


x<-merge(GBPUSD,GBPUSDlt,GBPUSDmt)
GBPUSD<-na.locf(x[,1:5])


## ------------------------------------------------------------------------
# moving average lengths
.fastl = 20
.slowl = 10
.fastm=14
.slowm=30


# optimization range
.Fastl = (2:20)
.Slowl = (10:40)
.Fastm = (2:20)
.Slowm = (10:40)



# trade parameters
.threshold = 0.000
.orderqty = 10000
.txnfees = 0  # round-trip fee

# stop loss amount
.stoploss <- 0.0130
.StopLoss = seq(0.0010, 0.0200, length.out=20)

#take profit amount
.takeprofit<-0.0170
.TakeProfit = seq(0.0010,0.0200,length.out=20)

#trailingstop
.stoptrailing<-0.1040
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


## ----results='hide'------------------------------------------------------
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
         arguments=list(sigcol='longfirst' , sigval=TRUE,
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
         arguments=list(sigcol='shortfirst', sigval=TRUE,
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
         arguments=list(sigcol='shortfirst', sigval=TRUE,
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
         arguments=list(sigcol='longfirst' , sigval=TRUE,
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

#trailingstop
add.rule(strategy.st, name = 'ruleSignal',
         arguments=list(sigcol='long' , sigval=TRUE,
                        replace=FALSE,
                        orderside='long',
                        ordertype='stoptrailing', tmult=FALSE, threshold=quote(.stoptrailing),
                        TxnFees=.txnfees,
                        orderqty=-.orderqty,
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
                        ordertype='stoptrailing', tmult=FALSE, threshold=quote(.stoptrailing),
                        TxnFees=.txnfees,
                        orderqty=.orderqty,
                        orderset='ocoshort'
         ),
         type='chain', parent='EnterSHORT',
         label='StopTrailingSHORT',
         enabled=FALSE
)



enable.rule('luxor', 'chain', 'StopLoss')

enable.rule('luxor', 'chain', 'TakeProfit')

enable.rule('luxor', 'chain', 'TrailingStop')



## ----results='hide'------------------------------------------------------


##


##




#####################################################################################################

addPosLimit(
  portfolio=portfolio.st,
  symbol='GBPUSD',
  timestamp=initDate,
  maxpos=.orderqty)

out <- applyStrategy(strategy.st, portfolio.st)
updatePortf(portfolio.st, Symbols='GBPUSD',
            Dates=paste('::',as.Date(Sys.time()),sep=''))

tStats <- tradeStats(Portfolios = portfolio.st, use = "trades",
                     inclZeroDays = FALSE)
tStats[, 4:ncol(tStats)] <- round(tStats[, 4:ncol(tStats)], 2)
xx<-data.frame(t(tStats[,-c(1,2)]))
print(data.frame(t(tStats[,-c(1,2)])))


chart.Posn(portfolio.st, "GBPUSD",
           TA="add_EMA(n=30,col=2);add_EMA(n=23/4,col=4)")

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

mk.df <- data.frame(Date=time(mktdata),coredata(mktdata))
PerformanceAnalytics:::textplot(mk.df,show.rownames=F)