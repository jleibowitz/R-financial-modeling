require(quantstrat)
require(PerformanceAnalytics)

initDate = "1990-01-01" 
from = "2003-01-01" 
to = "2013-12-31" 
options(width = 70)



symbols <- c("XLB", #SPDR Materials sector 
             "XLE", #SPDR Energy sector 
             "XLF", #SPDR Financial sector 
             "XLP", #SPDR Consumer staples sector 
             "XLI", #SPDR Industrial sector 
             "XLU", #SPDR Utilities sector 
             "XLV", #SPDR Healthcare sector 
             "XLK", #SPDR Tech sector 
             "XLY", #SPDR Consumer discretionary sector 
             "RWR", #SPDR Dow Jones REIT ETF 
             "EWJ", #iShares Japan 
             "EWG", #iShares Germany 
             "EWU", #iShares UK 
             "EWC", #iShares Canada 
             "EWY", #iShares South Korea 
             "EWA", #iShares Australia 
             "EWH", #iShares Hong Kong 
             "EWS", #iShares Singapore 
             "IYZ", #iShares U.S. Telecom 
             "EZU", #iShares MSCI EMU ETF 
             "IYR", #iShares U.S. Real Estate 
             "EWT", #iShares Taiwan 
             "EWZ", #iShares Brazil 
             "EFA", #iShares EAFE 
             "IGE", #iShares North American Natural Resources 
             "EPP", #iShares Pacific Ex Japan 
             "LQD", #iShares Investment Grade Corporate Bonds 
             "SHY", #iShares 1-3 year TBonds 
             "IEF", #iShares 3-7 year TBonds 
             "TLT" #iShares 20+ year Bonds 
             )

if(!"XLB" %in% ls()) { 
  # If data is not present, get it from yahoo 
  suppressMessages(getSymbols(symbols, src = "yahoo", adjust = TRUE)) 
}

currency('USD')
Sys.setenv(TZ="UTC")

stock(symbols, currency = "USD", multiplier = 1)

tradeSize <- 10000 
initEq <- tradeSize * length(symbols)

strategy.st <- "Clenow_Simple" 
portfolio.st <- "Clenow_Simple" 
account.st <- "Clenow_Simple" 
rm.strat(portfolio.st) 
rm.strat(strategy.st)

initPortf(portfolio.st, symbols = symbols, initDate = initDate, currency = 'USD')

initAcct(account.st, portfolios = portfolio.st, initDate = initDate, currency = 'USD', initEq = initEq)

initOrders(portfolio.st, initDate = initDate)

strategy(strategy.st, store=TRUE)

nLag = 252 
pctATR = 0.02 
period = 10

        
"lagATR" <- function(HLC, n = 14, maType, lag = 1, ...) { 
  ATR <- ATR(HLC, n = n, maType = maType, ...) 
  ATR <- lag(ATR, lag) 
  out <- ATR$atr 
  colnames(out) <- "atr" 
  return(out) 
}

"osDollarATR" <- function(orderside, tradeSize, pctATR, maxPctATR = pctATR, data, timestamp, symbol, prefer = "Open", portfolio, integerQty = TRUE, atrMod = "", rebal = FALSE, ...) {
  if(tradeSize > 0 & orderside == "short"){ 
    tradeSize <- tradeSize * -1 
  }
  
  pos <- getPosQty(portfolio, symbol, timestamp) 
  atrString <- paste0("atr", atrMod) 
  atrCol <- grep(atrString, colnames(mktdata))
  
  if(length(atrCol) == 0) { 
    stop(paste("Term", atrString, "not found in mktdata column names.")) 
  }
  
  atrTimeStamp <- mktdata[timestamp, atrCol]
  if(is.na(atrTimeStamp) | atrTimeStamp == 0) { 
    stop(paste("ATR corresponding to", atrString, "is invalid at this point in time. Add a logical operator to account for this.")) 
  }
  
  dollarATR <- pos * atrTimeStamp 
  desiredDollarATR <- pctATR * tradeSize 
  remainingRiskCapacity <- tradeSize * maxPctATR - dollarATR
  
  if(orderside == "long"){ 
    qty <- min(tradeSize * pctATR / atrTimeStamp, remainingRiskCapacity / atrTimeStamp)
  } else { 
    qty <- max(tradeSize * pctATR / atrTimeStamp, remainingRiskCapacity / atrTimeStamp) 
  }
  
  if(integerQty) { 
    qty <- trunc(qty) 
    }
  if(!rebal) { 
    if(orderside == "long" & qty < 0) { 
      qty <- 0 
      } 
    if(orderside == "short" & qty > 0) { 
      qty <- 0 
      }
    }
  if(rebal) { 
    if(pos == 0) { 
      qty <- 0 
      }
    }
  return(qty)
}




namedLag <- function(x, k = 1, na.pad = TRUE, ...) {
  out <- lag(x, k = k, na.pad = na.pad, ...)
  out[is.na(out)] <- x[is.na(out)] 
  colnames(out) <- "namedLag" 
  return(out)
}

add.indicator(strategy.st, name = "namedLag", arguments = list(x = quote(Cl(mktdata)), k = nLag), label = "ind")

add.indicator(strategy.st, name = "lagATR", arguments = list(HLC = quote(HLC(mktdata)), n = period), label = "atrX")


add.signal(strategy.st, name = "sigCrossover", arguments = list(columns = c("Close", "namedLag.ind"), relationship = "gt"), label = "coverOrBuy")

add.signal(strategy.st, name = "sigCrossover", arguments = list(columns = c("Close", "namedLag.ind"), relationship = "lt"), label = "sellOrShort")

add.rule(strategy.st, name = "ruleSignal", arguments = list(sigcol = "coverOrBuy", sigval = TRUE, ordertype = "market", orderside = "long", replace = FALSE, prefer = "Open", osFUN = osDollarATR, tradeSize = tradeSize, pctATR = pctATR, atrMod = "X"), type = "enter", path.dep = TRUE)

add.rule(strategy.st, name = "ruleSignal", arguments = list(sigcol = "sellOrShort", sigval = TRUE, orderqty = "all", ordertype = "market", orderside = "long", replace = FALSE, prefer = "Open"), type = "exit", path.dep = TRUE)

add.rule(strategy.st, name = "ruleSignal", arguments = list(sigcol = "sellOrShort", sigval = TRUE, ordertype = "market", orderside = "short", replace = FALSE, prefer = "Open", osFUN = osDollarATR, tradeSize = -tradeSize, pctATR = pctATR, atrMod = "X"), type = "enter", path.dep = TRUE)

add.rule(strategy.st, name = "ruleSignal", arguments = list(sigcol = "coverOrBuy", sigval = TRUE, orderqty = "all", ordertype = "market", orderside = "short", replace = FALSE, prefer = "Open"), type = "exit", path.dep = TRUE)

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