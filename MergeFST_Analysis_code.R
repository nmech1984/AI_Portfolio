library("readxl")
library("NLP")
library("twitteR")
library("SnowballC")
library("stringi")
library("topicmodels")
library("syuzhet") #sentiment library
library("twitteR")
library("ROAuth")
library(rvest) #for web scrap
library(tm)
library(tmap)
library(hunspell)
library(stringr)

library(rjson)
library(jsonlite)
library(purrr)
library(data.table)
library(ndjson)

library(gridExtra)
library(dbscan)
library(mclust)

library(quantmod) 
library(doParallel) #this library is for parallel core processing 
library("readxl")
library("gramEvol")
library("parallel") #parallel computation

library("readxl")
library(Quandl)

library(quantmod)
library(GA)

library(ppclust)
library(factoextra)
library(cluster)
library(fclust)
library(dplyr)

library(gridExtra)
library(car)
library(tidyverse)

library(tseries)


# library(BatchGetSymbols)
# df.SP500 <- GetSP500Stocks()
# tickers <- df.SP500$Tickers

setwd("C:/Users/marou/OneDrive/MSc QuantFin/Dissertation")
Data_LargeCap <- read_excel("SP_500.xlsx")
Data_LargeCap$Index <- 'SP500'
Data_SmallCap <- read_excel("russell_2000.xlsx")
Data_SmallCap$Index <- 'R2k'
Data <- rbind(Data_LargeCap,Data_SmallCap)
Data$`Market Cap ($M)`[is.na(Data$`Market Cap ($M)`)] <- 0
Data$`Market Cap ($M)` <- as.numeric(Data$`Market Cap ($M)`)

write.table(Data, file="clipboard-16384", sep="\t")

hist(Data$`Market Cap ($M)`,
     main="Russell 2000 and S&P500 Companies Market Cap ($M)",
     xlab="Market Cap ($M)",
     ylab="No of Companies",
     xlim = c(0,1400000),
     col="grey",
     freq=TRUE)

hist(Data$`Market Cap ($M)`,
     main="Russell 2000 and S&P500 Companies Market Cap ($M)",
     xlab="Market Cap ($M)",
     ylab="No of Companies",
     xlim = c(150000,1400000),
     ylim = c(0,50),
     col="grey",
     freq=TRUE)

####
#Fundamental Analysis
####

####
#Sentimental Analysis
####


Best <- merge(BestSenti, BestFunda, by = "Ticker")
#write.table(VF, file="clipboard-16384", sep="\t")

boxplot(Best$MarketCap)
boxplot(Best$Equity2Debt)
boxplot(Best$ROE)
boxplot(Best$CurrentRatio)
boxplot(Best$PE)

Q <- quantile(Best$PE, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(Best$PE)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range
Best_nowh<- subset(Best, Best$PE > (Q[1] - 1.5*iqr) & Best$PE < (Q[2]+1.5*iqr))

boxplot(Best_nowh$PE)
bxpt_Best_nowh <- boxplot(Best_nowh$PE)
bxpt_Best_nowh

BestGrowth <- subset(Best_nowh, Best_nowh$PE >= bxpt_Best_nowh$stats[3])
BestValue <- subset(Best_nowh, Best_nowh$PE < bxpt_Best_nowh$stats[3])




#BestFunda <- subset(VFin_all, VFin_all$ClustersEQ==1)

####
#Technical Analysis
####

