Technical_grsn = list()
Iterations_grsn = list()
for (jj in 1:nrow(BestGrowth)){
  Ticker <- data.frame(sapply(BestGrowth$Ticker, as.character), stringsAsFactors=FALSE)[jj,1]

  StartDate = Sys.Date()-300
  EndDate = "2020-06-17"
  PriceData <- try(getSymbols(Ticker[[1]], src="yahoo", from=StartDate, to=EndDate ,auto.assign = FALSE),silent=TRUE)
  colnames(PriceData) <- c("open","high","low","close","volume","Adjusted")
  
  Value_core <- coredata(PriceData)
  class(Value_core)
  Value_index <- index(PriceData)
  class(Value_index)
  Value <- Value_core[,4]
  
  ValueLag_withNA <- data.frame(Value,x1=Lag(Value,22),x2=Lag(Value,44),x3=Lag(Value,66),x4=Lag(Value,88),x5=Lag(Value,110))
  names(ValueLag_withNA) <- c('Value','Value1','Value2','Value3','Value4','Value5')
  ValueLag5 <- na.omit(ValueLag_withNA) #remove rows with NAs
  
  ruleDef <- list(expr = grule(op(expr, expr), func(expr), var),
                  func = grule(sin, cos, exp),
                  op = grule('+', '-', '*', '/', '^'),
                  var = grule(ValueLag5$Value1,ValueLag5$Value2,ValueLag5$Value3,ValueLag5$Value4,ValueLag5$Value5))
  grammarDef <- CreateGrammar(ruleDef)
  
  SymRegFitFunc <- function(expr) {
    result <- eval(expr)
    if (any(is.nan(result)))
      return(Inf)
    return (mean(log(1 + abs(ValueLag5$Value - result))))
  }
  
  ge <- GrammaticalEvolution(grammarDef, SymRegFitFunc, terminationCost = 0.1, iterations = 1000, max.depth = 5)
  print(jj)
  
  #NewLine_Value5 <- ValueLag5$Value[nrow(ValueLag5)-110]
  #NewLine_Value4 <- ValueLag5$Value[nrow(ValueLag5)-88]
  #NewLine_Value3 <- ValueLag5$Value[nrow(ValueLag5)-66]
  #NewLine_Value2 <- ValueLag5$Value[nrow(ValueLag5)-44]
  #NewLine_Value1 <- ValueLag5$Value[nrow(ValueLag5)-22]
  #NewLine_Value <- ValueLag5$Value[nrow(ValueLag5)]
  #NewLine <- data.frame(NewLine_Value,NewLine_Value1,NewLine_Value2,NewLine_Value3,NewLine_Value4,NewLine_Value5)
  #names(NewLine) <- c('Value','Value1','Value2','Value3','Value4','Value5')
  #ValueLag5 <- rbind(ValueLag5, NewLine)
  
  predicted <- ifelse(is.na(PriceData[nrow(PriceData),4])==TRUE,0,eval(ge$best$expressions))
  FairPrice_GP <- ifelse(is.na(PriceData[nrow(PriceData),4])==TRUE,0,predicted[length(predicted)])
  
  close <- unclass(PriceData$close)
  LastClosePrice <- close[length(close)]
  Growth <- ifelse(is.na(PriceData[nrow(PriceData),4])==TRUE,-100,FairPrice_GP*100/LastClosePrice-100)
  Technical_grsn[[jj]] = Growth
  Iterations_grsn[[jj]] = ge$population$currentIteration
  print(jj)
}

#Technical_sn[[31]] = -100
#Technical_sn[[142]] = -100
#Technical_sn[[227]] = -100
#Technical_sn[[313]] = -100

techgrowth_gr = do.call(rbind, Technical_grsn)
v_Iterations_gr = do.call(rbind, Iterations_grsn)
v_Technical_gr <- data.frame(matrix(BestGrowth[,1], nrow=length(BestGrowth[,1]), byrow=T))
v_Technical_gr$Growth <- techgrowth_gr[,1]
setnames(v_Technical_gr, c('Ticker','Growth'))
hist(v_Technical_gr$Growth, main='No of Stocks per Exp. % Change')
v_Technical_gr <- v_Technical_gr[order(v_Technical_gr$Growth, rev(v_Technical_gr$Growth), decreasing = TRUE), ]
v_Technical_gr <- subset(v_Technical_gr, v_Technical_gr$Growth>0)
hist(v_Technical_gr$Growth, main='No of Stocks per Exp. % Change')

growthstats <- boxplot(v_Technical_gr$Growth, main='Growth Investing, Expected Price Change %')
growthstats$stats
PortfolioGrowth <- v_Technical_gr$Growth[which(v_Technical_gr$Growth>growthstats$stats[3])]
myGrowthPortfolio <- v_Technical_gr %>% top_n(length(PortfolioGrowth))

###################################################################################
###################################################################################
###################################################################################
#Value Investor

Technical_vasn = list()
Iterations_vasn = list()
for (jj in 1:nrow(BestValue)){
  Ticker <- data.frame(sapply(BestValue$Ticker, as.character), stringsAsFactors=FALSE)[jj,1]
  
  StartDate = Sys.Date()-300
  PriceData <- try(getSymbols(Ticker[[1]], src="yahoo", from=StartDate,auto.assign = FALSE),silent=TRUE)
  colnames(PriceData) <- c("open","high","low","close","volume","Adjusted")
  Value_core <- coredata(PriceData)
  class(Value_core)
  Value_index <- index(PriceData)
  class(Value_index)
  Value <- Value_core[,4]
  
  ValueLag_withNA <- data.frame(Value,x1=Lag(Value,22),x2=Lag(Value,44),x3=Lag(Value,66),x4=Lag(Value,88),x5=Lag(Value,110))
  names(ValueLag_withNA) <- c('Value','Value1','Value2','Value3','Value4','Value5')
  ValueLag5 <- na.omit(ValueLag_withNA) #remove rows with NAs
  
  ruleDef <- list(expr = grule(op(expr, expr), func(expr), var),
                  func = grule(sin, cos, exp),
                  op = grule('+', '-', '*', '/', '^'),
                  var = grule(ValueLag5$Value1,ValueLag5$Value2,ValueLag5$Value3,ValueLag5$Value4,ValueLag5$Value5))
  grammarDef <- CreateGrammar(ruleDef)
  
  SymRegFitFunc <- function(expr) {
    result <- eval(expr)
    if (any(is.nan(result)))
      return(Inf)
    return (mean(log(1 + abs(ValueLag5$Value - result))))
  }
  
  ge <- GrammaticalEvolution(grammarDef, SymRegFitFunc, terminationCost = 0.1, iterations = 1000, max.depth = 5)
  #print(jj)
  
  #NewLine_Value5 <- ValueLag5$Value[nrow(ValueLag5)-110]
  #NewLine_Value4 <- ValueLag5$Value[nrow(ValueLag5)-88]
  #NewLine_Value3 <- ValueLag5$Value[nrow(ValueLag5)-66]
  #NewLine_Value2 <- ValueLag5$Value[nrow(ValueLag5)-44]
  #NewLine_Value1 <- ValueLag5$Value[nrow(ValueLag5)-22]
  #NewLine_Value <- ValueLag5$Value[nrow(ValueLag5)]
  #NewLine <- data.frame(NewLine_Value,NewLine_Value1,NewLine_Value2,NewLine_Value3,NewLine_Value4,NewLine_Value5)
  #names(NewLine) <- c('Value','Value1','Value2','Value3','Value4','Value5')
  #ValueLag5 <- rbind(ValueLag5, NewLine)
  
  predicted <- ifelse(is.na(PriceData[nrow(PriceData),4])==TRUE,0,eval(ge$best$expressions))
  FairPrice_GP <- ifelse(is.na(PriceData[nrow(PriceData),4])==TRUE,0,predicted[length(predicted)])
  
  close <- unclass(PriceData$close)
  LastClosePrice <- close[length(close)]
  Growth <- ifelse(is.na(PriceData[nrow(PriceData),4])==TRUE,-100,FairPrice_GP*100/LastClosePrice-100)
  Technical_vasn[[jj]] = Growth
  Iterations_vasn[[jj]] = ge$population$currentIteration
  print(jj)
}

#Technical_sn[[31]] = -100
#Technical_sn[[142]] = -100
#Technical_sn[[227]] = -100
#Technical_sn[[313]] = -100

techgrowth_va = do.call(rbind, Technical_vasn)
v_Iterations_va = do.call(rbind, Iterations_vasn)
v_Technical_va <- data.frame(matrix(BestValue[,1], nrow=length(BestValue[,1]), byrow=T))
v_Technical_va$Growth <- techgrowth_va[,1]
setnames(v_Technical_va, c('Ticker','Growth'))
hist(v_Technical_va$Growth, main='No of Stocks per Exp. % Change')
v_Technical_va <- v_Technical_va[order(v_Technical_va$Growth, rev(v_Technical_va$Growth), decreasing = TRUE), ]
v_Technical_va <- subset(v_Technical_va, v_Technical_va$Growth>0)
hist(v_Technical_va$Growth, main='No of Stocks per Exp. % Change')


valuestats <- boxplot(v_Technical_va$Growth, main='Value Investing, Expected Price Change %')
valuestats$stats
PortfolioValue <- v_Technical_va$Growth[which(v_Technical_va$Growth>growthstats$stats[3])]
myValuePortfolio <- v_Technical_va %>% top_n(length(PortfolioValue))
