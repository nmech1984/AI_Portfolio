#Import Data


setwd("C:/Users/marou/OneDrive/MSc QuantFin/Dissertation")
Data_LargeCap <- read_excel("SP_500.xlsx")
Data_SmallCap <- read_excel("russell_2000.xlsx")
Data <- rbind(Data_LargeCap,Data_SmallCap)

#nrow(Data)
Sentiment_sn = list()
for (jj in 1:nrow(Data)){
  jj=1
  Ticker <- Data[jj,1]
  Company <- Data[jj,2]
  Company[[1]]
  
  jsonurl = paste('https://stocknewsapi.com/api/v1?tickers=',Ticker[[1]],'&sortby=rank&type=article&items=50&token=hcmwcrfv5xukbyi7l8n98ldjkc1yvffjiu8ihqo2',sep='')
  jsonlist <- fromJSON(jsonurl)
  stocknewsapi <- map(jsonlist, as.data.table)
  stocknews_raw <- rbindlist(stocknewsapi, fill = TRUE, idcol = T)
  stocknews_raw <- data.table(t(stocknews_raw))
  stocknews_raw_title <- t(stocknews_raw[4,])
  stocknews_raw_artic <- t(stocknews_raw[5,])
  stocknews_raw_titar <- as.list(c(stocknews_raw_title,stocknews_raw_artic))
  stocknews <- do.call(rbind, stocknews_raw_titar)
  
  stocknews<- tolower(stocknews) #convert all text to lower case
  stocknews <- gsub("rt", "", stocknews) # Replace blank space ("rt")
  stocknews <- gsub("@\\w+", "", stocknews) # Replace @UserName
  stocknews <- gsub("[[:punct:]]", "", stocknews) # Remove punctuation
  stocknews <- gsub("http\\w+", "", stocknews) # Remove links
  stocknews <- gsub("[ |\t]{2,}", "", stocknews) # Remove tabs
  stocknews <- gsub("[^\x01-\x7F]", "", stocknews) #Remove emoticons
  stocknews <- gsub("^ ", "", stocknews) # Remove blank spaces at the beginning
  stocknews <- gsub(" $", "", stocknews) # Remove blank spaces at the end
  
  #clean up by removing stop words
  stocknewsCorpus <- Corpus(VectorSource(stocknews))
  stocknewsCorpus <- tm_map(stocknewsCorpus,function(x)removeWords(x,stopwords()))
  stocknewsremoveURL <- function(x) gsub("http[[:alnum:]]*", "", x)
  stocknewsCorpus <- tm_map(stocknewsCorpus, content_transformer(stocknewsremoveURL))
  stocknews_cs_df <- data.frame(text = get("content", stocknewsCorpus))
  stocknews_cs_df <- data.frame(lapply(stocknews_cs_df, as.character), stringsAsFactors=FALSE)
  stocknews_text <- unlist(stocknews_cs_df)
  
  sentiment_stocknews<-get_nrc_sentiment(stocknews_text) #getting emotions using in-built function
  Sentimentscores_news<-data.frame(colSums(sentiment_stocknews[,])) #calculationg total score for each sentiment
  print(jj)
  Sentiment_sn[[jj]] = (Sentimentscores_news[10,1] - Sentimentscores_news[9,1])
}
v_Sentiment_sn = do.call(rbind, Sentiment_sn)
write.table(v_Sentiment_sn, file="clipboard-16384", sep="\t")

##

Sentiment_st = list()
for (jj in 392:nrow(Data)){
  Ticker <- Data[jj,1]
  Company <- Data[jj,2]
  Company[[1]]
  
  stocktwits_url <- paste('https://api.stocktwits.com/api/2/streams/symbol/ric/',Ticker[[1]],'.json',sep='')
  Raw <- fromJSON(stocktwits_url)
  print(jj)
  entities <- Raw[['messages']][['entities']]['sentiment']
  ind <- entities[,1]
  ind_unlist <- unlist(ind)
  Sentiment_st[[jj]] <- length(grep("Bullish", ind_unlist))-length(grep("Bearish", ind_unlist))
}
v_Sentiment_st = do.call(rbind, Sentiment_st)
write.table(v_Sentiment_st, file="clipboard-16384", sep="\t")


Sentiment <- cbind(Data[,1],Data[,5],rep(Sys.Date(), each = nrow(Data)),v_Sentiment_st,v_Sentiment_sn)
setnames(Sentiment, c('Ticker','MarketCap','SentiReportDate','SentiTraders','SentiNews'))



#########################################################################################
#################################################################
#v_Sentiment <- read_excel("v_Sentimental_df.xlsx")

VF <- as.data.frame(sapply(v_Sentiment, as.numeric)) #<- sapply is here
VF[is.na(VF)] <- 0
VF <- VF %>% mutate_if(is.numeric, function(x) ifelse(is.infinite(x), 0, x))
VF<-cbind(Data[,1],Data[,5],v_Sentiment[,2])
setnames(VF, c('Ticker','MarketCapMilUSD','Sn'))
VF$MarketCapMilUSD <- as.numeric(as.factor(VF$MarketCapMilUSD))
row.names(VF) <- NULL
Ratios <- VF[,2:3]
Ratios <- as.data.frame(Ratios)

plot(Ratios$MarketCapMilUSD,Ratios$Sn,
     xlab='Market Cap M$',
     ylab='Sentiment')
cor(Ratios, use="complete.obs", method="kendall")

dfSnt = list()
dfSnt$Ticker <- Data$Ticker
dfSnt$Sentiment <- Ratios$Sn
dfSnt$MarketCap <- Ratios$MarketCapMilUSD
dfSnt <- data.frame(dfSnt)

#################################################################
#################################################################
#################################################################
#
#Check normality
hist(dfSnt$Sentiment, probability = TRUE, main="Histogram of Sentiment")
x <- min(dfSnt$Sentiment):max(dfSnt$Sentiment)
y <- dnorm(x = x, mean = mean(dfSnt$Sentiment), sd = sd(dfSnt$Sentiment))
lines(x = x, y = y, col = "blue")

mean(dfSnt$Sentiment)
sd(dfSnt$Sentiment)

#
#Boxplot filter
boxplot(dfSnt$Sentiment, main='Sentiment')
hist(dfSnt$Sentiment)
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}
dfSnt$SentimentFlt <- remove_outliers(dfSnt$Sentiment)
boxplot(dfSnt$SentimentFlt)

#Removing NAs and normality test
dfSntFlt <- na.omit(dfSnt)
hist(dfSntFlt$Sentiment, probability = TRUE, main="Histogram of Sentiment (filtered)")
x <- min(dfSntFlt$Sentiment):max(dfSntFlt$Sentiment)
y <- dnorm(x = x, mean = mean(dfSntFlt$Sentiment), sd = sd(dfSntFlt$Sentiment))
lines(x = x, y = y, col = "blue")
#
mean(dfSntFlt$Sentiment)
sd(dfSntFlt$Sentiment)
#Jarque-Bera Normality test
jarque.bera.test(dfSntFlt$Sentiment)

#Correl after filtering
plot(dfSntFlt$MarketCap,dfSntFlt$Sentiment,
     xlab='Market Cap M$',
     ylab='Sentiment')
cor(dfSntFlt$MarketCap,dfSntFlt$Sentiment, use="complete.obs", method="kendall")

#Subset the best Senti
bxpt_Best_senti <- boxplot(dfSntFlt$Sentiment)
BestSenti <- list()
BestSenti <- subset(dfSntFlt, dfSntFlt$Sentiment >= bxpt_Best_senti$stats[4])
#
mean(BestSenti$Sentiment)
mean(BestSenti$MarketCap)
hist(BestSenti$MarketCap)

