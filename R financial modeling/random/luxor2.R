require(quantstrat)

##### PLACE DEMO AND TEST DATES HERE #################

if(isTRUE(options('in_test')$in_test))
  # use test dates
  {initDate="2011-01-01" 
  endDate="2012-12-31"   
  } else
  # use demo defaults
  {initDate="1999-12-31"
  endDate=Sys.Date()}

initDate = "1990-01-01 00:00:00" 
from = "2010-01-01 00:00:00" 
to = "2013-12-31 00:00:00"

GBPUSD<-read.csv(file="C://TickDownloader/tickdata/GBPUSD_M30_UTC+0_00.csv",header=TRUE,stringsAsFactors=FALSE)
names(GBPUSD)<-c("Date","Time","Open","High","Low","Close","Volume")
GBPUSD$DateTime<-paste(GBPUSD$Date, GBPUSD$Time)
GBPUSD$DateTime<-as.POSIXct(GBPUSD$DateTime,format='%Y.%m.%d %H:%M')
GBPUSD<-as.xts(zoo(GBPUSD[,3:7],order.by=(GBPUSD$DateTime)))
GBPUSD<-GBPUSD["2010/",]


strategy.st = 'luxor'
portfolio.st = 'forex'
account.st = 'IB'

.orderqty = 100000
.threshold = 0.0005
.txnfees = -6

.fast = 10
.slow = 30


### blotter

initPortf(portfolio.st, symbols='GBPUSD', initDate=initDate, currency='USD')
initAcct(account.st, portfolios=portfolio.st, initDate=initDate, currency='USD')

### quantstrat

initOrders(portfolio.st, initDate=initDate)

### define strategy

strategy(strategy.st, store=TRUE)

### indicators

add.indicator(strategy.st, name = "SMA",
              arguments = list(
                x = quote(Cl(mktdata)[,1]),
                n = .fast
              ),
              label="nFast"
)

add.indicator(strategy.st, name="SMA",
              arguments = list(
                x = quote(Cl(mktdata)[,1]),
                n = .slow
              ),
              label="nSlow"
)

### signals

add.signal(strategy.st, name='sigCrossover',
           arguments = list(
             columns=c("nFast","nSlow"),
             relationship="gte"
           ),
           label='long'
)

add.signal(strategy.st, name='sigCrossover',
           arguments = list(
             columns=c("nFast","nSlow"),
             relationship="lt"
           ),
           label='short'
)

### rules

add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='long' , sigval=TRUE,
                        orderside='short',
                        ordertype='market',
                        orderqty='all',
                        TxnFees=.txnfees,
                        replace=TRUE
         ),
         type='exit',
         label='Exit2LONG'
)

add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='short', sigval=TRUE,
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
         arguments=list(sigcol='long' , sigval=TRUE,
                        orderside='long' ,
                        ordertype='stoplimit', prefer='High', threshold=.threshold,
                        orderqty=+.orderqty,
                        replace=FALSE
         ),
         type='enter',
         label='EnterLONG'
)

add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='short', sigval=TRUE,
                        orderside='short',
                        ordertype='stoplimit', prefer='Low', threshold=-.threshold,
                        orderqty=-.orderqty,
                        replace=FALSE
         ),
         type='enter',
         label='EnterSHORT'
)

###############################################################################

t1 <- Sys.time() 
applyStrategy(strategy.st, portfolio.st)
t2 <- Sys.time()
print(t2 - t1)

View(getOrderBook(portfolio.st)[[portfolio.st]]$GBPUSD)

###############################################################################

updatePortf(portfolio.st, Symbols='GBPUSD', Dates=paste('::',as.Date(Sys.time()),sep=''))

chart.Posn(portfolio.st, "GBPUSD")

###############################################################################

View(t(tradeStats(portfolio.st, 'GBPUSD')))

tStats <- tradeStats(Portfolios = portfolio.st, use = "trades", inclZeroDays = FALSE) 
tStats[, 4:ncol(tStats)] <- round(tStats[, 4:ncol(tStats)], 2)
print(data.frame(t(tStats[,-c(1,2)]))) 
aggPF <- sum(tStats$Gross.Profits) / -sum(tStats$Gross.Losses) 
aggCorrect <- mean(tStats$Percent.Positive) 
numTrades <- sum(tStats$Num.Trades) 
meanAvgWLR <- mean(tStats$Avg.WinLoss.Ratio[ tStats$Avg.WinLoss.Ratio < Inf], na.rm = TRUE)