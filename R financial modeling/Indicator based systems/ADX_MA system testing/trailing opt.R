#SL, TP, TS
rm.strat(portfolio.st)
rm.strat(account.st)

## ----results='hide'------------------------------------------------------
initPortf(portfolio.st, symbols='GBPUSD', initDate=initDate, currency='USD')

## ----results='hide'------------------------------------------------------
addPosLimit(
  portfolio=portfolio.st,
  symbol='GBPUSD',
  timestamp=initDate,
  maxpos=.orderqty)

## ----results='hide'------------------------------------------------------
initAcct(account.st, portfolios=portfolio.st,initDate=initDate,currency='USD')
initOrders(portfolio.st, initDate=initDate)

enable.rule('luxor', 'chain', 'StopLoss')

enable.rule('luxor', 'chain', 'TakeProfit')

enable.rule('luxor', 'chain', 'TrailingStop')

library(doParallel)
registerDoSEQ()


#Trailing##############################################################
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


t1=Sys.time()
results <- apply.paramset(strategy.st, paramset.label='StopTrailing',
                          portfolio.st=portfolio.st, account.st=account.st, nsamples=0, verbose=FALSE)
t2=Sys.time()
t2-t1

tS <- results$tradeStats
idx <- order(tS[,1],tS[,2])
tS <- tS[idx,]
PerformanceAnalytics:::textplot(t(tS)[,1:10])

par(mfrow=c(1,3))
plot(tS$StopTrailingLONG, tS$Net.Trading.PL, type='b', xlab='StopTrailing',
     ylab='Net.Trading.PL', main='Net Profit vs Stop Trailing',col=4)
plot(tS$StopTrailingLONG, tS$Max.Drawdown, type='b', xlab='StopTrailing',
     ylab='Max.Drawdown', main='MaxDrawdown vs Stop Trailing',col=4)
plot(tS$StopTrailingLONG, tS$Profit.To.Max.Draw, type='b', xlab='StopTrailing',
     ylab='Profit.To.Max.Draw', main='Return/MaxDD vs Stop Trailing',col=4)
par(mfrow=c(1,1))




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
