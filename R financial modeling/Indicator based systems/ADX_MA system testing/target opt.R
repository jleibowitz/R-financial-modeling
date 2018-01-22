#SL, TP, TS
rm.strat(portfolio.st)
rm.strat(account.st)

## ----results='hide'------------------------------------------------------
initPortf(portfolio.st, symbols='GBPUSD', initDate=initDate, currency='USD')
## ----results='hide'------------------------------------------------------
initAcct(account.st, portfolios=portfolio.st,initDate=initDate,currency='USD')
initOrders(portfolio.st, initDate=initDate)



## ----results='hide'------------------------------------------------------
addPosLimit(
  portfolio=portfolio.st,
  symbol='GBPUSD',
  timestamp=initDate,
  maxpos=.orderqty)


enable.rule('luxor', 'chain', 'StopLoss')

enable.rule('luxor', 'chain', 'TakeProfit')



library(doParallel)
registerDoSEQ()


#TP##############################################################################
add.distribution(strategy.st,
                 paramset.label = 'StopLoss',
                 component.type = 'chain',
                 component.label = 'TakeProfitLONG',
                 variable = list(threshold = .TakeProfit),
                 label = 'TakeProfitLONG'
)

add.distribution(strategy.st,
                 paramset.label = 'StopLoss',
                 component.type = 'chain',
                 component.label = 'TakeProfitSHORT',
                 variable = list(threshold = .TakeProfit),
                 label = 'TakeProfitSHORT'
)

add.distribution.constraint(strategy.st,
                            paramset.label = 'StopLoss',
                            distribution.label.1 = 'TakeProfitLONG',
                            distribution.label.2 = 'TakeProfitSHORT',
                            operator = '==',
                            label = 'TakeProfit'
)


t1=Sys.time()
results <- apply.paramset(strategy.st, paramset.label='TakeProfit',
                          portfolio.st=portfolio.st, account.st=account.st, nsamples=0, verbose=FALSE)
t2=Sys.time()
t2-t1


tS <- results$tradeStats
idx <- order(tS[,1],tS[,2])
tS <- tS[idx,]


par(mfrow=c(1,3))
plot(tS$TakeProfitLONG, tS$Net.Trading.PL, type='b', xlab='TakeProfit',
     ylab='Net.Trading.PL', main='Net Profit vs TakeProfit',col=4)
plot(tS$TakeProfitLONG, tS$Max.Drawdown, type='b', xlab='TakeProfit',
     ylab='Max.Drawdown', main='MaxDrawdown vs TakeProfit',col=4)
plot(tS$TakeProfitLONG, tS$Profit.To.Max.Draw, type='b', xlab='TakeProfit',
     ylab='Profit.To.Max.Draw', main='Return/MaxDD vs TakeProfit',col=4)
par(mfrow=c(1,1))

PerformanceAnalytics:::textplot(t(tS)[,1:10])

z <- tapply(X=tS[,"End.Equity"],INDEX=list(StopLoss=tS[,1],TakeProfit=tS[,3]),FUN=sum)


x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))
filled.contour(x=x,y=y,z=z,color = heat.colors,xlab="StopLoss",ylab="TakeProfit")
title("Net Profit")

# maxdd
z <- tapply(X=tS[,"Max.Drawdown"],INDEX=list(StopLoss=tS[,1],TakeProfit=tS[,3]),FUN=sum)
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))
filled.contour(x=x,y=y,z=z,color = heat.colors,xlab="StopLoss",ylab="TakeProfit")
title("Max Drawdown")

# profit factor
z <- tapply(X=tS[,"Profit.Factor"],INDEX=list(StopLoss=tS[,1],TakeProfit=tS[,3]),FUN=sum)
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))
filled.contour(x=x,y=y,z=z,color = heat.colors,xlab="StopLoss",ylab="TakeProfit")
title("Profit Factor")

# avg trade P&L
z <- tapply(X=tS[,"Avg.Trade.PL"],INDEX=list(StopLoss=tS[,1],TakeProfit=tS[,3]),FUN=sum)
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))
filled.contour(x=x,y=y,z=z,color = heat.colors,xlab="StopLoss",ylab="TakeProfit")
title("Average Trade")

# return to maxdd
z <- tapply(X=tS[,"Profit.To.Max.Draw"],
            INDEX=list(StopLoss=tS[,1],TakeProfit=tS[,3]),FUN=sum)
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))
filled.contour(x=x,y=y,z=z,color = heat.colors,xlab="StopLoss",ylab="TakeProfit")
title("Return to Max Drawdown")

rmdd <- tS$Profit.To.Max.Draw
idx <- order(rmdd,decreasing=T)[1:30]
labs <- paste(tS$StopLossLONG[idx],tS$TakeProfitLONG[idx],sep="/")
barplot(rmdd[idx],names.arg=labs,col=4,las=2,main="Return to MaxDrawdown")
