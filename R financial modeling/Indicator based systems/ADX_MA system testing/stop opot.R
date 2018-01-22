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



library(doParallel)
registerDoSEQ()



# SL
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





#######################################################################333

t1=Sys.time()
results <- apply.paramset(strategy.st, paramset.label='StopLoss',
                          portfolio.st=portfolio.st, account.st=account.st, nsamples=0, verbose=FALSE)
t2=Sys.time()
t2-t1


tS <- results$tradeStats
idx <- order(tS[,1],tS[,2])
tS <- tS[idx,]

par(mfrow=c(1,3))
plot(tS$StopLossLONG, tS$Net.Trading.PL, type='b', xlab='Stoploss',
     ylab='Net.Trading.PL', main='Net Profit vs Stop Loss',col=4)
plot(tS$StopLossLONG, tS$Max.Drawdown, type='b', xlab='Stoploss',
     ylab='Max.Drawdown', main='MaxDrawdown vs Stop Loss',col=4)
plot(tS$StopLossLONG, tS$Profit.To.Max.Draw, type='b', xlab='Stoploss',
     ylab='Profit.To.Max.Draw', main='Return/MaxDD vs Stop Loss',col=4)
par(mfrow=c(1,1))

z <- tapply(X=tS[,"End.Equity"],INDEX=list(StopLoss=tS[,1],StopLoss=tS[,2]),FUN=sum)

x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))
filled.contour(x=x,y=y,z=z,color = heat.colors,xlab="StopLoss",ylab="StopLoss")
title("Net Profit")

# maxdd
z <- tapply(X=tS[,"Max.Drawdown"],INDEX=list(StopLoss=tS[,1],StopLoss=tS[,2]),FUN=sum)
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))
filled.contour(x=x,y=y,z=z,color = heat.colors,xlab="StopLoss",ylab="StopLoss")
title("Max Drawdown")

# profit factor
z <- tapply(X=tS[,"Profit.Factor"],INDEX=list(StopLoss=tS[,1],StopLoss=tS[,2]),FUN=sum)
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))
filled.contour(x=x,y=y,z=z,color = heat.colors,xlab="StopLoss",ylab="StopLoss")
title("Profit Factor")

# avg trade P&L
z <- tapply(X=tS[,"Avg.Trade.PL"],INDEX=list(StopLoss=tS[,1],StopLoss=tS[,2]),FUN=sum)
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))
filled.contour(x=x,y=y,z=z,color = heat.colors,xlab="StopLoss",ylab="StopLoss")
title("Average Trade")

# return to maxdd
z <- tapply(X=tS[,"Profit.To.Max.Draw"],
            INDEX=list(StopLoss=tS[,1],StopLoss=tS[,2]),FUN=sum)
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))
filled.contour(x=x,y=y,z=z,color = heat.colors,xlab="StopLoss",ylab="StopLoss")
title("Return to Max Drawdown")

rmdd <- tS$Profit.To.Max.Draw
idx <- order(rmdd,decreasing=T)[1:30]
labs <- paste(tS$StopLossLONG[idx],tS$StopLossLONG[idx],sep="/")
barplot(rmdd[idx],names.arg=labs,col=4,las=2,main="Return to MaxDrawdown")
