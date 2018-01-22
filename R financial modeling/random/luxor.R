require(quantmod)
require(quantstrat)

initDate = "1990-01-01" 
from = "2003-05-05" 
to = "2013-12-31" 
options(width = 70)

symbols<-("GBPUSD.xts")
GBPUSD<-read.csv(file="C://TickDownloader/tickdata/GBPUSD_M30_UTC+0_00.csv",header=TRUE,stringsAsFactors=FALSE)
names(GBPUSD)<-c("Date","Time","Open","High","Low","Close","Volume")
GBPUSD$DateTime<-paste(GBPUSD$Date, GBPUSD$Time)
GBPUSD$DateTime<-as.POSIXct(GBPUSD$DateTime,format='%Y.%m.%d %H:%M')
GBPUSD.xts<-as.xts(zoo(GBPUSD[,3:7],order.by=as.Date(GBPUSD$DateTime)))

currency('USD')
Sys.setenv(TZ="UTC")

threshold=0.0005
orderqty=100000

stock(symbols, currency = "USD", multiplier = 1)

strategy.st <- 'luxor'
portfolio.st <- 'luxor'
account.st <- 'luxor'
rm.strat(portfolio.st) 
rm.strat(strategy.st)
rm.strat(account.st)

initPortf(portfolio.st, symbols = symbols, initDate = initDate, currency = 'USD')

initAcct(account.st, portfolios = portfolio.st, initDate = initDate, currency = 'USD', initEq = orderqty)

initOrders(portfolio.st, initDate = initDate)


strategy(
  name=strategy.st,
  store=TRUE
)

add.indicator(
  strategy='luxor',
  name='SMA',
  arguments=list(
    x=quote(Cl(mktdata)[,1]),
    n=10
  ),
  label="nFast"
)

add.indicator(
  strategy='luxor',
  name='SMA',
  arguments=list(
    x=quote(Cl(mktdata)[,1]),
    n=30
  ),
  label="nSlow"
)

add.signal(
  strategy = 'luxor',
  name='sigCrossover',
  arguments=list(
    columns=c("nFast","nSlow"),
    relationship="gt"
  ),
  label='long'
)

add.signal(
  strategy = 'luxor',
  name='sigCrossover',
  arguments=list(
    columns=c("nFast","nSlow"),
    relationship="lt"
  ),
  label='short'
)

ruleSignal(
  sigcol = 'long',
  sigval=TRUE,
  orderside='long',
  ordertype='stoplimit',
  prefer='High',
  threshold=0.0005,
  orderqty=100000,
  replace=FALSE
)

add.rule(
  strategy='luxor',
  name='ruleSignal',
  arguments=list(
    sigcol='long',
    sigval=TRUE,
    orderside='long',
    ordertype='stoplimit', 
    prefer='High', 
    threshold=+threshold,
    orderqty=+orderqty,
    replace=FALSE
  ),
  type='enter',
  label='EnterLONG'
)

add.rule(
  strategy='luxor',
  name='ruleSignal',
  arguments=list(
    sigcol='short',
    sigval=TRUE,
    orderside='short',
    ordertype='stoplimit', 
    prefer='Low', 
    threshold=-threshold,
    orderqty=-orderqty,
    replace=FALSE
  ),
  type='enter',
  label='EnterSHORT'
)

add.rule(
  strategy='luxor', 
   name='ruleSignal',
   argument=list(
     sigcol='long' , 
     sigval=TRUE,
     orderside='short',
     ordertype='market',
     orderqty='all',
     #TxnFees=.txn.fees,
     replace=TRUE
   ),
   type='exit',
   label='Exit2LONG'
)

add.rule(
  strategy='luxor',
   name='ruleSignal',
   argument=list(
     sigcol='short' , 
     sigval=TRUE,
     orderside='long',
     ordertype='market',
     orderqty='all',
     #TxnFees=.txn.fees,
     replace=TRUE
   ),
   type='exit',
   label='Exit2SHORT'
)

t1 <- Sys.time() 
out <- applyStrategy(strategy = strategy.st, portfolios = portfolio.st)
t2 <- Sys.time()
print(t2 - t1)

updatePortf(portfolio.st) 
dateRange <- time(getPortfolio(portfolio.st)$summary)[-1] 
updateAcct(portfolio.st, dateRange) 
updateEndEq(account.st)

tStats <- tradeStats(Portfolios = portfolio.st, use = "trades", inclZeroDays = FALSE) 
tStats[, 4:ncol(tStats)] <- round(tStats[, 4:ncol(tStats)], 2)
print(data.frame(t(tStats[,-c(1,2)]))) 
aggPF <- sum(tStats$Gross.Profits) / -sum(tStats$Gross.Losses) 
aggCorrect <- mean(tStats$Percent.Positive) 
numTrades <- sum(tStats$Num.Trades) 
meanAvgWLR <- mean(tStats$Avg.WinLoss.Ratio[ tStats$Avg.WinLoss.Ratio < Inf], na.rm = TRUE)
  