rm(list = ls())
require(quantstrat)
require(PerformanceAnalytics)
rm(list=ls())
try(rm(list=ls(pos=.blotter),pos=.blotter),silent=TRUE)
try(rm(list=ls(pos=.strategy),pos=.strategy),silent=TRUE)
try(rm(list=ls(pos=.instrument),pos=.instrument),silent=TRUE)

GBPUSD<-read.csv(file="C://TickDownloader/tickdata/GBPUSD_D1_UTC+0_00.csv",header=TRUE,stringsAsFactors=FALSE)
names(GBPUSD)<-c("Date","Time","Open","High","Low","Close","Volume")
GBPUSD$DateTime<-paste(GBPUSD$Date, GBPUSD$Time)
GBPUSD$DateTime<-as.POSIXct(GBPUSD$DateTime,format='%Y.%m.%d %H:%M')
GBPUSD<-as.xts(zoo(GBPUSD[,3:7],order.by=(GBPUSD$DateTime)))
GBPUSD<-(GBPUSD["2010-01-01/2015-09-01"])

initDate='2010-01-01'
.from=initDate
.to='2015-09-01'

currency(c('GBP', 'USD'))
exchange_rate('GBPUSD', tick_size=0.0001)

# moving average lengths
.fast=10
.med = 25
.slow = 50
# optimization range
.FastEMA = (1:20)
.SlowEMA = (20:80)
.MedEMA =(20:40 )
# trade parameters
.threshold = 0.0005
.orderqty = 100000
.txnfees = -6 # round-trip fee
# stop loss amount
.stoploss <- 0.30/100
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

add.indicator(strategy.st, name = "EMA",
              arguments = list(
                x = quote(Cl(mktdata)[,1]),
                n = .fast
              ),
              label="nFast"
)

add.indicator(strategy.st, name="EMA",
              arguments = list(
                x = quote(Cl(mktdata)[,1]),
                n = .slow
              ),
              label="nSlow"
)

add.indicator(strategy.st, name="EMA",
              arguments = list(
                x = quote(Cl(mktdata)[,1]),
                n = .med
              ),
              label="nMed"
)

#SIGNAL

add.signal(strategy.st, name='sigComparison',
           arguments = list(
             columns=c("nMed","nSlow"),
             relationship="lte"
           ),
           label='1long'
)

add.signal(strategy.st, name='sigCrossover',
           arguments = list(
             columns=c("nFast","nSlow"),
             relationship="gt"
           ),
           label='2long'
)

add.signal(strategy.st, name='sigCrossover',
           arguments = list(
             columns=c("nFast","nMed"),
             relationship="gt"
           ),
           label='exitshort'
)

add.signal(strategy.st, name='sigComparison',
           arguments = list(
             columns=c("nMed","nSlow"),
             relationship="gte"
           ),
           label='1short'
)

add.signal(strategy.st, name='sigCrossover',
           arguments = list(
             columns=c("nFast","nSlow"),
             relationship="lt"
           ),
           label='2short'
)

add.signal(strategy.st, name='sigCrossover',
           arguments = list(
             columns=c("nFast","nMed"),
             relationship="lt"
           ),
           label='exitlong'
)

add.signal(strategy.st, name='sigComparison',
           arguments=list(
             columns=c("1long","2long"),
             relationship="eq"
           ),
           label='long'
)

add.signal(strategy.st, name='sigComparison',
           arguments=list(
             columns=c("1short","2short"),
             relationship="eq"
           ),
           label='short'
)

#RULES

add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='long' , sigval=TRUE,
                        orderside='long' ,
                        ordertype='stoplimit',
                        prefer='High',
                        threshold=.threshold,
                        orderqty=+.orderqty,
                        replace=FALSE
         ),
         type='enter',
         label='EnterLONG'
)

add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='short', sigval=TRUE,
                        orderside='short',
                        ordertype='stoplimit',
                        prefer='Low',
                        threshold=-.threshold,
                        orderqty=-.orderqty,
                        replace=FALSE
         ),
         type='enter',
         label='EnterSHORT'
)

add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='exitlong', sigval=TRUE,
                        orderside='long' ,
                        ordertype='market',
                        orderqty='all',
                        TxnFees=.txnfees,
                        replace=TRUE
         ),
         type='exit',
         label='Exit2SHORT'
)

add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='exitshort' , sigval=TRUE,
                        orderside='short',
                        ordertype='market',
                        orderqty='all',
                        TxnFees=.txnfees,
                        replace=TRUE
         ),
         type='exit',
         label='Exit2LONG'
)

out <- applyStrategy(strategy.st, portfolio.st)
updatePortf(portfolio.st, Symbols='GBPUSD',
            Dates=paste('::',as.Date(Sys.time()),sep=''))

tStats <- tradeStats(Portfolios = portfolio.st, use = "trades",
                     inclZeroDays = FALSE)
tStats[, 4:ncol(tStats)] <- round(tStats[, 4:ncol(tStats)], 2)
print(data.frame(t(tStats[,-c(1,2)])))


chart.Posn(portfolio.st, "GBPUSD",
           TA="add_EMA(n=15,col='red');add_EMA(n=25,col='green');add_EMA(n=50,col='black')"
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
