# trend opt

rm.strat(portfolio.st)
rm.strat(account.st)

## ----results='hide'------------------------------------------------------
initPortf(portfolio.st, symbols='GBPUSD', initDate=initDate, currency='USD')

## ----results='hide'------------------------------------------------------

## ----results='hide'------------------------------------------------------
initAcct(account.st, portfolios=portfolio.st,initDate=initDate,currency='USD')
initOrders(portfolio.st, initDate=initDate)

enable.rule('luxor', 'chain', 'StopLoss')

enable.rule('luxor', 'chain', 'TakeProfit')

enable.rule('luxor', 'chain', 'TrailingStop')

addPosLimit(
  portfolio=portfolio.st,
  symbol='GBPUSD',
  timestamp=initDate,
  maxpos=.orderqty)

add.distribution(strategy.st,
                 paramset.label = 'trend',
                 component.type = 'indicator',
                 component.label = 'nTrendP',
                 variable = list(n = .trendrange),
                 label = 'nTRENDP'
)

add.distribution(strategy.st,
                 paramset.label = 'trend',
                 component.type = 'indicator',
                 component.label = 'nTrendN',
                 variable = list(n = .trendrange),
                 label = 'nTRENDN'
)


add.distribution(strategy.st,
                 paramset.label = 'trend',
                 component.type = 'indicator',
                 component.label = 'nTrendT',
                 variable = list(n = .trendrange),
                 label = 'nTRENDT'
)

add.distribution.constraint(strategy.st,
                            paramset.label = 'trend',
                            distribution.label.1 = 'nTRENDP',
                            distribution.label.2 = 'nTRENDN',
                            operator = '==',
                            label= "TREND"
)

add.distribution.constraint(strategy.st,
                            paramset.label = 'trend',
                            distribution.label.1 = 'nTRENDP',
                            distribution.label.2 = 'nTRENDT',
                            operator = '==',
                            label= "TRENDD"
)

add.distribution(strategy.st,
                 paramset.label = 'trend',
                 component.type = 'signal',
                 component.label = 'confirmation',
                 variable = list(threshold = .trendthreshrange),
                 label = 'nTRENDTHRESH'
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

par(mfrow=c(1,3))
plot(tS$nTRENDP, tS$Net.Trading.PL, type='b', xlab='Trend',
     ylab='Net.Trading.PL', main='Net Profit vs Trend',col=4)
plot(tS$nTRENDP, tS$Max.Drawdown, type='b', xlab='Stoploss',
     ylab='Max.Drawdown', main='MaxDrawdown vs Trend',col=4)
plot(tS$nTRENDP, tS$Profit.To.Max.Draw, type='b', xlab='Stoploss',
     ylab='Profit.To.Max.Draw', main='Return/MaxDD vs Trend',col=4)
par(mfrow=c(1,1))

#####################################################################################################

z <- tapply(X=tS[,"End.Equity"],INDEX=list(Trend=tS[,1],Threshold=tS[,4]),FUN=sum)


x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))
filled.contour(x=x,y=y,z=z,color = heat.colors,xlab="Trend",ylab="Threshold")
title("Net Profit")

# maxdd
z <- tapply(X=tS[,"Max.Drawdown"],INDEX=list(Trend=tS[,1],Threshold=tS[,4]),FUN=sum)
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))
filled.contour(x=x,y=y,z=z,color = heat.colors,xlab="Trend",ylab="Threshold")
title("Max Drawdown")

# profit factor
z <- tapply(X=tS[,"Profit.Factor"],INDEX=list(Trend=tS[,1],Threshold=tS[,4]),FUN=sum)
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))
filled.contour(x=x,y=y,z=z,color = heat.colors,xlab="Trend",ylab="Threshold")
title("Profit Factor")

# avg trade P&L
z <- tapply(X=tS[,"Avg.Trade.PL"],INDEX=list(Trend=tS[,1],Threshold=tS[,4]),FUN=sum)
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))
filled.contour(x=x,y=y,z=z,color = heat.colors,xlab="Trend",ylab="Threshold")
title("Average Trade")

# return to maxdd
z <- tapply(X=tS[,"Profit.To.Max.Draw"],
            INDEX=list(Trend=tS[,1],Threshold=tS[,4]),FUN=sum)
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))
filled.contour(x=x,y=y,z=z,color = heat.colors,xlab="Trend",ylab="Threshold")
title("Return to Max Drawdown")

rmdd <- tS$Profit.To.Max.Draw
idx <- order(rmdd,decreasing=T)[1:30]
labs <- paste(tS$nTRENDP[idx],tS$nTRENDTHRESH[idx],sep="/")
barplot(rmdd[idx],names.arg=labs,col=4,las=2,main="Return to MaxDrawdown")
