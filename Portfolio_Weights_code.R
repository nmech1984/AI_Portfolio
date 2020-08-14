myPortfolio_gro <- as.character(myGrowthPortfolio$Ticker)
class(myPortfolio_gro) 

getSymbols(myPortfolio_gro, src="yahoo")

#remove stocks without price data
out <- sapply(myPortfolio_gro, function(s) tryCatch({
  getSymbols(s , env = NULL)
  }, error = function(e) NA)
  )
dd <- lapply(out, function(x) if (any(is.na(x))) NA else Ad(x))
dd <- do.call(cbind, dd)
myPortfolio_clean_gro = list()
for (xx in (1:ncol(dd))){myPortfolio_clean_gro[[xx]] <- ifelse(as.numeric(dd[nrow(dd),xx])>0,myPortfolio_gro[xx],NA)}
myPortfolio_clean_gro = do.call(rbind, myPortfolio_clean_gro)
myPortfolio_clean_gro <- na.omit(myPortfolio_clean_gro)

Stocks = lapply(myPortfolio_clean_gro, function(sym) {
  weeklyReturn(na.omit(getSymbols(sym, from=StartDate, to=EndDate, auto.assign=FALSE)))
})

asset_returns_withNA <- do.call(merge, Stocks)
asset_returns <- na.omit(asset_returns_withNA) #remove rows with NAs

portfolio_returns = function(x) {
  port.returns = 0
  # Multiplication of the i-th asset by the i-th weight in "x"
  for (i in 1:length(x)) {
    port.returns = port.returns + asset_returns[,i] * x[i]
  }
  return (port.returns)
}


sharpe = function(x) {
  port.returns = portfolio_returns(x)
  return (mean(port.returns)/sqrt(var(port.returns)))
}

constraint = function(x) {
  boundary_constr = (sum(x)-1)**2   # "sum x = 1" constraint
  for (i in 1:length(x)) {
    boundary_constr = boundary_constr + 
      max(c(0,x[i]-1))**2 +  # "x <= 1" constraint
      max(c(0,-x[i]))**2     # "x >= 0" constraint
  }
  return (boundary_constr)
}

obj = function(x) {
  # We want the maximum Sharpe ratio, so we multiply it by -1 to fit an optimization problem
  return (-sharpe(x)+100*constraint(x))
}

ga_res = ga(
  type="real-valued", # Tell the genetic algorithm that the weights are real variables
  function(x){-obj(x)}, # "ga" performs maximization, so we must multiply the objective function by -1
  lower = rep(0,ncol(asset_returns)), # x_i >= 0
  upper = rep(1,ncol(asset_returns)), # x_i <= 1
  maxiter = 50000, # Maximum number of iterations 
  run=50, # If the max fitness remains the same for 50 consecutive transactions, stop the algorithm
  parallel=TRUE, # Exploit multi-core properties of your CPU
  monitor=TRUE, # We want to see the partial results of the process while it performs
  seed=1 # Seed useful for replicating the results
)

sol_gro = as.vector(summary(ga_res)$solution)
sol_gro
AverageWeight = mean(sol_gro)*100
cat("Average Weight: ", AverageWeight)

qqnorm(sol_gro, pch = 1, frame = FALSE, main='Normal QQ Plot, Growth Portfolio')
qqline(sol_gro, col = "steelblue", lwd = 2)

myPortfolio_clean_gro
barplot(sol_gro*100,
        main = "Growth Investing, Weights per Asset",
        ylab = "%",
        names.arg = myPortfolio_clean_gro,
        col = "grey",
        horiz = FALSE)
abline(h=AverageWeight)

#Information regarding Growth stocks portfolio
df_myPortfolio_gro <- data.frame(transpose(as.list(myPortfolio_clean_gro)))
colnames(df_myPortfolio_gro) <- c("Ticker")
GrowthPortfolioData <- merge(df_myPortfolio_gro, Data, by = "Ticker")
boxplot(as.numeric(GrowthPortfolioData$`Market Cap ($M)`))
mean(as.numeric(GrowthPortfolioData$`Market Cap ($M)`))
boxplot(as.numeric(GrowthPortfolioData$Beta))
mean(as.numeric(GrowthPortfolioData$Beta))

#Growth Stock Prices Portfolio
GrowthPriceList = list()
for (iii in 1:length(myPortfolio_clean_gro)){
  ssstock = myPortfolio_clean_gro[iii]
  print(ssstock)
  ssstock_all = getSymbols(ssstock, from=EndDate, to=as.Date(EndDate)+1, auto.assign=FALSE)
  colnames(ssstock_all) <- c("open","high","low","close","volume","Adjusted")
  ssstock_close <- ssstock_all$close
  print(ssstock_close)
  GrowthPriceList[[iii]]<-ssstock_close
}
GrowthPriceList = do.call(rbind, GrowthPriceList)
#
barplot(GrowthPriceList,
        main = "Growth Investing, Stock Prices",
        ylab = "%",
        names.arg = myPortfolio_clean_gro,
        col = "grey",
        horiz = FALSE)
abline(h=mean(GrowthPriceList))
mean(GrowthPriceList)
sum(GrowthPriceList)


#######

Stocks_proj = lapply(myPortfolio_clean_gro, function(sym) {
  weeklyReturn(na.omit(getSymbols(sym, from=EndDate, auto.assign=FALSE)))
})

asset_returns_proj_withNA <- do.call(merge, Stocks_proj)
asset_returns_proj <- na.omit(asset_returns_proj_withNA) #remove rows with NAs

portfolio_returns_proj = function(x) {
  port.returns = 0
  # Multiplication of the i-th asset by the i-th weight in "x"
  for (i in 1:length(x)) {
    port.returns = port.returns + asset_returns_proj[,i] * x[i]
  }
  return (port.returns)
}

optimal_returns = portfolio_returns_proj(sol_gro)
Growth_Rp_cum_optimal <- cumsum(optimal_returns)



###################################################################################
###################################################################################
###################################################################################
#Value Investor

myPortfolio_val <- as.character(myValuePortfolio$Ticker)
class(myPortfolio_val) 

getSymbols(myPortfolio_val, src="yahoo")

#remove stocks without price data
out <- sapply(myPortfolio_val, function(s) tryCatch({
  getSymbols(s , env = NULL)
}, error = function(e) NA)
)
dd <- lapply(out, function(x) if (any(is.na(x))) NA else Ad(x))
dd <- do.call(cbind, dd)
myPortfolio_clean_val = list()
for (xx in (1:ncol(dd))){myPortfolio_clean_val[[xx]] <- ifelse(as.numeric(dd[nrow(dd),xx])>0,myPortfolio_val[xx],NA)}
myPortfolio_clean_val = do.call(rbind, myPortfolio_clean_val)
myPortfolio_clean_val <- na.omit(myPortfolio_clean_val)

Stocks = lapply(myPortfolio_clean_val, function(sym) {
  weeklyReturn(na.omit(getSymbols(sym, from=StartDate, to=EndDate, auto.assign=FALSE)))
})

asset_returns_withNA <- do.call(merge, Stocks)
asset_returns <- na.omit(asset_returns_withNA) #remove rows with NAs

portfolio_returns = function(x) {
  port.returns = 0
  # Multiplication of the i-th asset by the i-th weight in "x"
  for (i in 1:length(x)) {
    port.returns = port.returns + asset_returns[,i] * x[i]
  }
  return (port.returns)
}


sharpe = function(x) {
  port.returns = portfolio_returns(x)
  return (mean(port.returns)/sqrt(var(port.returns)))
}

constraint = function(x) {
  boundary_constr = (sum(x)-1)**2   # "sum x = 1" constraint
  for (i in 1:length(x)) {
    boundary_constr = boundary_constr + 
      max(c(0,x[i]-1))**2 +  # "x <= 1" constraint
      max(c(0,-x[i]))**2     # "x >= 0" constraint
  }
  return (boundary_constr)
}

obj = function(x) {
  # We want the maximum Sharpe ratio, so we multiply it by -1 to fit an optimization problem
  return (-sharpe(x)+100*constraint(x))
}

ga_res = ga(
  type="real-valued", # Tell the genetic algorithm that the weights are real variables
  function(x){-obj(x)}, # "ga" performs maximization, so we must multiply the objective function by -1
  lower = rep(0,ncol(asset_returns)), # x_i >= 0
  upper = rep(1,ncol(asset_returns)), # x_i <= 1
  maxiter = 50000, # Maximum number of iterations 
  run=50, # If the max fitness remains the same for 50 consecutive transactions, stop the algorithm
  parallel=TRUE, # Exploit multi-core properties of your CPU
  monitor=TRUE, # We want to see the partial results of the process while it performs
  seed=1 # Seed useful for replicating the results
)

sol_val = as.vector(summary(ga_res)$solution)
sol_val
AverageWeight = mean(sol_val)*100
cat("Average Weight: ", AverageWeight)

qqnorm(sol_val, pch = 1, frame = FALSE, main='Normal QQ Plot, Value Portfolio')
qqline(sol_val, col = "steelblue", lwd = 2)

myPortfolio_clean_val
barplot(sol_val*100,
        main = "Value Investing, Weights per Asset",
        ylab = "%",
        names.arg = myPortfolio_clean_val,
        col = "grey",
        horiz = FALSE)
abline(h=AverageWeight)

#Information regarding Value stocks portfolio
df_myPortfolio_val <- data.frame(transpose(as.list(myPortfolio_clean_val)))
colnames(df_myPortfolio_val) <- c("Ticker")
ValuePortfolioData <- merge(df_myPortfolio_val, Data, by = "Ticker")
boxplot(as.numeric(ValuePortfolioData$`Market Cap ($M)`))
mean(as.numeric(ValuePortfolioData$`Market Cap ($M)`))
boxplot(as.numeric(ValuePortfolioData$Beta))
mean(as.numeric(ValuePortfolioData$Beta))

#Value Stock Prices Portfolio
ValuePriceList = list()
for (iii in 1:length(myPortfolio_clean_val)){
  ssstock = myPortfolio_clean_val[iii]
  print(ssstock)
  ssstock_all = getSymbols(ssstock, from=EndDate, to=as.Date(EndDate)+1, auto.assign=FALSE)
  colnames(ssstock_all) <- c("open","high","low","close","volume","Adjusted")
  ssstock_close <- ssstock_all$close
  print(ssstock_close)
  ValuePriceList[[iii]]<-ssstock_close
}
ValuePriceList = do.call(rbind, ValuePriceList)
#
barplot(ValuePriceList,
        main = "Value Investing, Stock Prices",
        ylab = "%",
        names.arg = myPortfolio_clean_val,
        col = "grey",
        horiz = FALSE)
abline(h=mean(ValuePriceList))
mean(ValuePriceList)
sum(ValuePriceList)

#######

Stocks_proj = lapply(myPortfolio_clean_val, function(sym) {
  weeklyReturn(na.omit(getSymbols(sym, from=EndDate, auto.assign=FALSE)))
})

asset_returns_proj_withNA <- do.call(merge, Stocks_proj)
asset_returns_proj <- na.omit(asset_returns_proj_withNA) #remove rows with NAs

portfolio_returns_proj = function(x) {
  port.returns = 0
  # Multiplication of the i-th asset by the i-th weight in "x"
  for (i in 1:length(x)) {
    port.returns = port.returns + asset_returns_proj[,i] * x[i]
  }
  return (port.returns)
}

optimal_returns = portfolio_returns_proj(sol_val)
Value_Rp_cum_optimal <- cumsum(optimal_returns)

###################################################################################
###################################################################################
###################################################################################
#Plots and S&P500 comparison

getSymbols('SPY', src="yahoo")
SPY_x = lapply('SPY', function(sym) {
  weeklyReturn(na.omit(getSymbols(sym, from=EndDate, auto.assign=FALSE)))
})
SPY_returns_withNA <- do.call(merge, SPY_x)
SPY_returns <- na.omit(SPY_returns_withNA) #remove rows with NAs

plot(cumsum(SPY_returns),type="l", col="black", main='S&P500 vs Optimal Portfolios, Cum. Returns',lwd=5, ylim=c(-0.3,0.3)) #optimum solution plot
lines(Value_Rp_cum_optimal,col="blue") #S&P500
lines(Growth_Rp_cum_optimal,col="red") #S&P500
legend("left",legend=c("Line 1", "Line 2", "Line 3"),
       col=c("black", "blue", "red"), cex=0.8, bg='lightblue')
