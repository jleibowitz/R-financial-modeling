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



library(doParallel)
registerDoSEQ()

add.distribution(strategy.st,
                 paramset.label = 'EMA',
                 component.type = 'indicator',
                 component.label = 'nFastl',
                 variable = list(n = .Fastl),
                 label = 'nFASTl'
)



## ----results='hide'------------------------------------------------------
add.distribution(strategy.st,
                 paramset.label = 'EMA',
                 component.type = 'indicator',
                 component.label = 'nSlowl',
                 variable = list(n = .Slowl),
                 label = 'nSLOWl'
)





## ----results='hide'------------------------------------------------------
add.distribution.constraint(strategy.st,
                            paramset.label = 'EMA',
                            distribution.label.1 = 'nFASTl',
                            distribution.label.2 = 'nSLOWl',
                            operator = '<',
                            label = 'EMA'
)

library(doParallel)
registerDoSEQ()

t1=Sys.time()
results <- apply.paramset(strategy.st, paramset.label='EMA',
                          portfolio.st=portfolio.st, account.st=account.st, nsamples=0)
t2=Sys.time()
td=t2-t1
td

tS <- results$tradeStats
idx <- order(tS[,1],tS[,2])
tS <- tS[idx,]
PerformanceAnalytics:::textplot(t(tS)[,1:10])


# net profit
z <- tapply(X=tS[,"End.Equity"],INDEX=list(Fast=tS[,1],Slow=tS[,2]),FUN=sum)

x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))
filled.contour(x=x,y=y,z=z,color = heat.colors,xlab="Fast MA",ylab="Slow MA")
title("Net Profit")

# maxdd
z <- tapply(X=tS[,"Max.Drawdown"],INDEX=list(Fast=tS[,1],Slow=tS[,2]),FUN=sum)
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))
filled.contour(x=x,y=y,z=z,color = heat.colors,xlab="Fast MA",ylab="Slow MA")
title("Max Drawdown")

# profit factor
z <- tapply(X=tS[,"Profit.Factor"],INDEX=list(Fast=tS[,1],Slow=tS[,2]),FUN=sum)
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))
filled.contour(x=x,y=y,z=z,color = heat.colors,xlab="Fast MA",ylab="Slow MA")
title("Profit Factor")

# avg trade P&L
z <- tapply(X=tS[,"Avg.Trade.PL"],INDEX=list(Fast=tS[,1],Slow=tS[,2]),FUN=sum)
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))
filled.contour(x=x,y=y,z=z,color = heat.colors,xlab="Fast MA",ylab="Slow MA")
title("Average Trade")

# return to maxdd
z <- tapply(X=tS[,"Profit.To.Max.Draw"],
            INDEX=list(Fast=tS[,1],Slow=tS[,2]),FUN=sum)
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))
filled.contour(x=x,y=y,z=z,color = heat.colors,xlab="Fast MA",ylab="Slow MA")
title("Return to Max Drawdown")

rmdd <- tS$Profit.To.Max.Draw
idx <- order(rmdd,decreasing=T)[1:30]
labs <- paste(tS$nFAST[idx],tS$nSLOW[idx],sep="/")
barplot(rmdd[idx],names.arg=labs,col=4,las=2,main="Return to MaxDrawdown")
