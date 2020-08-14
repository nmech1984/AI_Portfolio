#https://www.datacamp.com/community/tutorials/k-means-clustering-r

Quandl.api_key("h_1_7xtjXXzX3HePaCG5")


# #https://www.quandl.com/databases/MF1/data
# Data_qndl <- Quandl.datatable('MER/F1', ticker='AAPL')
# ROE <- Data_qndl$amount[which(Data_qndl$currency=='USD' & Data_qndl$mapcode=='-5011' & Data_qndl$reporttype=='A' & Data_qndl$reportid==max(Data_qndl$reportid[which(Data_qndl$currency=='USD' & Data_qndl$mapcode=='-5011' & Data_qndl$reporttype=='A')]))]
# ROA <- Data_qndl$amount[which(Data_qndl$currency=='USD' & Data_qndl$mapcode=='-5009' & Data_qndl$reporttype=='A' & Data_qndl$reportid==max(Data_qndl$reportid[which(Data_qndl$currency=='USD' & Data_qndl$mapcode=='-5009' & Data_qndl$reporttype=='A')]))]
# CFps <- Data_qndl$amount[which(Data_qndl$currency=='USD' & Data_qndl$mapcode=='-7562' & Data_qndl$reporttype=='A' & Data_qndl$reportid==max(Data_qndl$reportid[which(Data_qndl$currency=='USD' & Data_qndl$mapcode=='-7562' & Data_qndl$reporttype=='A')]))]
# FCFpS <- Data_qndl$amount[which(Data_qndl$currency=='USD' & Data_qndl$mapcode=='-1465' & Data_qndl$reporttype=='A' & Data_qndl$reportid==max(Data_qndl$reportid[which(Data_qndl$currency=='USD' & Data_qndl$mapcode=='-1465' & Data_qndl$reporttype=='A')]))]
# Db2Eq <- Data_qndl$amount[which(Data_qndl$currency=='USD' & Data_qndl$mapcode=='-6159' & Data_qndl$reporttype=='A' & Data_qndl$reportid==max(Data_qndl$reportid[which(Data_qndl$currency=='USD' & Data_qndl$mapcode=='-6159' & Data_qndl$reporttype=='A')]))]
# EBITDAmrg <- Data_qndl$amount[which(Data_qndl$currency=='USD' & Data_qndl$mapcode=='-1012' & Data_qndl$reporttype=='A' & Data_qndl$reportid==max(Data_qndl$reportid[which(Data_qndl$currency=='USD' & Data_qndl$mapcode=='-1012' & Data_qndl$reporttype=='A')]))]
# Netmrg <- Data_qndl$amount[which(Data_qndl$currency=='USD' & Data_qndl$mapcode=='-4074' & Data_qndl$reporttype=='A' & Data_qndl$reportid==max(Data_qndl$reportid[which(Data_qndl$currency=='USD' & Data_qndl$mapcode=='-4074' & Data_qndl$reporttype=='A')]))]
# Opemrg <- Data_qndl$amount[which(Data_qndl$currency=='USD' & Data_qndl$mapcode=='-5917' & Data_qndl$reporttype=='A' & Data_qndl$reportid==max(Data_qndl$reportid[which(Data_qndl$currency=='USD' & Data_qndl$mapcode=='-5917' & Data_qndl$reporttype=='A')]))]
# QuR <- Data_qndl$amount[which(Data_qndl$currency=='USD' & Data_qndl$mapcode=='-959' & Data_qndl$reporttype=='A' & Data_qndl$reportid==max(Data_qndl$reportid[which(Data_qndl$currency=='USD' & Data_qndl$mapcode=='-959' & Data_qndl$reporttype=='A')]))]
# CuR <- Data_qndl$amount[which(Data_qndl$currency=='USD' & Data_qndl$mapcode=='-948' & Data_qndl$reporttype=='A' & Data_qndl$reportid==max(Data_qndl$reportid[which(Data_qndl$currency=='USD' & Data_qndl$mapcode=='-948' & Data_qndl$reporttype=='A')]))]

FundamentalList = list()
for (kk in 1:nrow(Data)){
  Ticker = Data[kk,1][[1]]
  #https://www.quandl.com/databases/SF1/data
  Data_qndl <- Quandl.datatable('SHARADAR/SF1', dimension = 'MRY',ticker=Ticker)
  nrqn <- nrow(Data_qndl)
  Data_tickers <- Quandl.datatable('SHARADAR/TICKERS', ticker=Ticker)
  print(kk)
  CompanyWebsite <- Data_tickers$companysite[1]
  Vlt_BS_ED <- ifelse(nrqn==0,NA,Data_qndl$equity[which(Data_qndl$reportperiod==max(Data_qndl$reportperiod))]/Data_qndl$debt[which(Data_qndl$reportperiod==max(Data_qndl$reportperiod))])
  Vlt_BS_ROE <- ifelse(nrqn==0,NA,Data_qndl$roe[which(Data_qndl$reportperiod==max(Data_qndl$reportperiod))])
  Vlt_BS_CR <- ifelse(nrqn==0,NA,Data_qndl$currentratio[which(Data_qndl$reportperiod==max(Data_qndl$reportperiod))])
  Vlt_Inv_PE <- ifelse(nrqn==0,NA,Data_qndl$pe[which(Data_qndl$reportperiod==max(Data_qndl$reportperiod))])
  Vlt_Inv_DY <- ifelse(nrqn==0,NA,Data_qndl$divyield[which(Data_qndl$reportperiod==max(Data_qndl$reportperiod))])
  Qndl_row <- c(Ticker,CompanyWebsite,Vlt_BS_DE,Vlt_BS_ROE,Vlt_BS_CR,Vlt_Inv_PE,Vlt_Inv_DY)
  FundamentalList[[kk]] <- Qndl_row
  }

v_Fundamental = do.call(rbind, FundamentalList)
#v_Fundamental <- read_excel("v_Fundamental_df.xlsx")
colnames(v_Fundamental) <- c('Ticker','Website','Equity2Debt','ROE','CurrentRatio','PE','DivYield')
#write.table(v_Fundamental, file="clipboard-16384", sep="\t")

sapply(v_Fundamental, class)

#################################################################

VF <- as.data.frame(sapply(v_Fundamental, as.numeric)) #<- sapply is here
VF[is.na(VF)] <- 0
VF <- VF %>% mutate_if(is.numeric, function(x) ifelse(is.infinite(x), 0, x))
VF<-cbind(v_Fundamental[,1:2],VF[,3:7])
row.names(VF) <- NULL
Ratios1 <- VF[,3:5]
Ratios2 <- VF[,6:7]

#################################################################

boxplot(Ratios1$Equity2Debt, main='Equity to Debt')
boxplot(Ratios1$ROE, main='ROE')
boxplot(Ratios1$CurrentRatio, main='Current Ratio')

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

rte2d <- remove_outliers(Ratios1$Equity2Debt)
rtroe <- remove_outliers(Ratios1$ROE)
rtcur <- remove_outliers(Ratios1$CurrentRatio)

boxplot(rte2d)
boxplot(rtroe)
boxplot(rtcur)

#################################################################
Ratios <- Ratios1

#Data preparation
#Remove outliers
#https://www.r-bloggers.com/outlier-detection-with-mahalanobis-distance/
m_dist <- mahalanobis(Ratios[, 1:3], colMeans(Ratios[, 1:3]), cov(Ratios[, 1:3]))
Ratios$m_dist <- round(m_dist, 2)
Q <- quantile(Ratios$m_dist, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(Ratios$m_dist)
up <-  Q[2]+3*iqr # Upper Range
dn <-  Q[2]-3*iqr # Lower Range  
Ratios$outlier_maha <- "No"
Ratios$outlier_maha[Ratios$m_dist > up | Ratios$m_dist < dn] <- "Yes"
VFin_all <- subset(VF, Ratios$outlier_maha=="No")
VFout_all <- subset(VF, Ratios$outlier_maha=="Yes")
VFin <- VFin_all[,3:5]
VFin_pca <- VFin #log(VFin)

boxplot(VFin_pca$Equity2Debt)
boxplot(VFin_pca$ROE)
boxplot(VFin_pca$CurrentRatio)


#Check relationships between variables before PCA
scatterplotMatrix(VFin_pca)
# plot(VFin_pca$Equity2Debt, VFin_pca$ROE, main = "ROE vs Equity/Debt",
#      xlab = "E/D", ylab = "ROE",
#      pch = 19, frame = FALSE)
# plot(VFin_pca$ROE, VFin_pca$CurrentRatio, main = "Current Ratio vs ROE",
#      xlab = "ROE", ylab = "Current Ratio",
#      pch = 19, frame = FALSE)
# plot(VFin_pca$CurrentRatio, VFin_pca$Equity2Debt, main = "Equity/Debt vs Current Ratio",
#      xlab = "Current Ratio", ylab = "E/D",
#      pch = 19, frame = FALSE)

#PCA
#https://www.r-bloggers.com/computing-and-visualizing-pca-in-r/
VFin_sc_pca <- prcomp(VFin_pca, center = TRUE, scale = TRUE) 
summary(VFin_sc_pca)
plot(VFin_sc_pca, type = "l", main='Variances')


#kMeans
#https://cran.r-project.org/web/packages/ppclust/vignettes/fcm.html#21_run_fcm_with_single_start
#https://uc-r.github.io/kmeans_clustering
#https://stats.stackexchange.com/questions/133656/how-to-understand-the-drawbacks-of-k-means
VFin_sc_kmn <- scale(VFin[,1:3])

#find number of clusters
fviz_nbclust(VFin_sc_kmn, kmeans, method = "wss")
kmcl_s <- fviz_nbclust(VFin_sc_kmn, kmeans, method = "silhouette")
#gap_stat <- clusGap(VFin_sc_kmn, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
#fviz_gap_stat(gap_stat)
#number of clusters
clm <- kmcl_s$data$y
clmn <- which(clm == max(clm), arr.ind = TRUE)
#kmeans
Fin_clusters1 <- kmeans(VFin_sc_kmn, clmn-1, nstart = 50)
Fin_clusters2 <- kmeans(VFin_sc_kmn, clmn+0, nstart = 50)
Fin_clusters3 <- kmeans(VFin_sc_kmn, clmn+1, nstart = 50)
Fin_clusters4 <- kmeans(VFin_sc_kmn, clmn+2, nstart = 50)
# plots to compare
p1 <- fviz_cluster(Fin_clusters1, geom = "point", data = VFin_sc_kmn) + ggtitle(clmn-1)
p2 <- fviz_cluster(Fin_clusters2, geom = "point", data = VFin_sc_kmn) + ggtitle(clmn+0)
p3 <- fviz_cluster(Fin_clusters3, geom = "point", data = VFin_sc_kmn) + ggtitle(clmn+1)
p4 <- fviz_cluster(Fin_clusters4, geom = "point", data = VFin_sc_kmn) + ggtitle(clmn+2)
grid.arrange(p1,p2,p3,p4, nrow = 2)
Fin_clusters2$cluster
hist(Fin_clusters2$cluster)

#Spectral Clustering
#https://rpubs.com/nurakawa/spectral-clustering
spectral_clustering <- function(X, # matrix of data points
                                nn = 10, # the k nearest neighbors to consider
                                n_eig = 2) # m number of eignenvectors to keep
{
  mutual_knn_graph <- function(X, nn = 10)
  {
    D <- as.matrix( dist(X) ) # matrix of euclidean distances between data points in X
    # intialize the knn matrix
    knn_mat <- matrix(0, nrow = nrow(X), ncol = nrow(X))
    # find the 10 nearest neighbors for each point
    for (i in 1: nrow(X)) {
      neighbor_index <- order(D[i,])[2:(nn + 1)]
      knn_mat[i,][neighbor_index] <- 1 
    }
    # Now we note that i,j are neighbors iff K[i,j] = 1 or K[j,i] = 1 
    knn_mat <- knn_mat + t(knn_mat) # find mutual knn
    knn_mat[ knn_mat == 2 ] = 1
    return(knn_mat)
  }
  graph_laplacian <- function(W, normalized = TRUE)
  {
    stopifnot(nrow(W) == ncol(W)) 
    g = colSums(W) # degrees of vertices
    n = nrow(W)
    if(normalized)
    {
      D_half = diag(1 / sqrt(g) )
      return( diag(n) - D_half %*% W %*% D_half )
    }
    else
    {
      return( diag(g) - W )
    }
  }
  
  W = mutual_knn_graph(X) # 1. matrix of similarities
  L = graph_laplacian(W) # 2. compute graph laplacian
  ei = eigen(L, symmetric = TRUE) # 3. Compute the eigenvectors and values of L
  n = nrow(L)
  return(ei$vectors[,(n - n_eig):(n - 1)]) # return the eigenvectors of the n_eig smallest eigenvalues
}
# do spectral clustering procedure
X_sc <- spectral_clustering(VFin_sc_kmn)
# run kmeans on the 2 eigenvectors
X_sc_kmeans <- kmeans(X_sc, 3)
fviz_cluster(X_sc_kmeans, geom = "point", data = VFin_sc_kmn)
hist(X_sc_kmeans$cluster)


#DBSCAN
#https://towardsdatascience.com/how-dbscan-works-and-why-should-i-use-it-443b4a191c80
## find suitable eps parameter using a k-NN plot for k = dim + 1
## Look for the knee!
kNNdistplot(VFin_sc_kmn, k = clmn)
abline(h=.5, col = "red", lty=2)
res <- dbscan(VFin_sc_kmn, eps = .3, minPts = 5)
hullplot(VFin_sc_kmn,res$cluster)
hist(res$cluster)


#Model-Based
#https://cran.r-project.org/web/packages/mclust/vignettes/mclust.html
#https://www.rdocumentation.org/packages/mclust/versions/5.4.6/topics/Mclust
mod1 <- Mclust(VFin_sc_kmn)
summary(mod1)
hist(mod1$classification)
plot(mod1, what = "classification")
#
mod2 <- Mclust(VFin_sc_kmn, G = clmn)
summary(mod2, parameters = TRUE)
hist(mod2$classification)
plot(mod2, what = "classification")
mod2i <- Mclust(VFin_sc_kmn, G = clmn+1)
summary(mod2i, parameters = TRUE)
hist(mod2i$classification)
plot(mod2i, what = "classification")
# Using prior
mod3 <- Mclust(VFin_sc_kmn, prior = priorControl())
summary(mod3)
hist(mod3$classification)
plot(mod3, what = "classification")
#
mod4 <- Mclust(VFin_sc_kmn, prior = priorControl(functionName="defaultPrior", shrinkage=0.1))
summary(mod4)
hist(mod4$classification)
plot(mod4, what = "classification")

# Clustering of faithful data with some artificial noise added 
nNoise <- 100
set.seed(0) # to make it reproducible
Noise <- apply(VFin_sc_kmn, 2, function(x) 
  runif(nNoise, min = min(x)-.1, max = max(x)+.1))
data <- rbind(VFin_sc_kmn, Noise)
plot(VFin_sc_kmn)
points(Noise, pch = 20, cex = 0.5, col = "lightgrey")
set.seed(0)
NoiseInit <- sample(c(TRUE,FALSE), size = nrow(faithful)+nNoise, 
                    replace = TRUE, prob = c(3,1)/4)
mod5 <- Mclust(data, initialization = list(noise = NoiseInit))
summary(mod5, parameter = TRUE)
hist(mod5$classification)
plot(mod5, what = "classification")




#
Funda_clusters<-Fin_clusters2$cluster
VFin_all$ClustersEQ <- Funda_clusters
BestFunda <- subset(VFin_all, VFin_all$ClustersEQ==1)
BestFunda <- cbind(BestFunda$Ticker, BestFunda[,2:7])
colnames(BestFunda) <- c('Ticker','Website','Equity2Debt','ROE','CurrentRatio','PE','DivYield')
