rm(list=ls())

setwd("/Users/christian/Documents/Studium/FS 2021/Practitioner Seminar/Data Clustering")

#Hierarchical Agglomerative Clustering (HAC)
#Agglomerative: Bottom-up, we start with one cluster for each observation
#merge until all observations in single cluster


#Data Preparation

library(corrplot)
library(cluster)
library(purrr)
library(factoextra)
library(dplyr)
library(dendextend)
library(tidyverse)
library(caret)

data <- read.csv('Ready_for_csv_export.csv', sep = ';')

data_nafix = na_if(data, "n.a.")
data_nafix = na_if(data_nafix, 0)
data_naomit = na.omit(data_nafix)


rownames(data_naomit) = data_naomit$Name

#colnames(data_naomit)

colnum <- as.vector(colnames(data_naomit[3:18]))


data_naomit[colnum] <- lapply(data_naomit[colnum], function(x) as.numeric(as.character(x)))


#####Set of ratios
# 1. Ratio: Gross Loans
data_naomit$GrossLoans <- data_naomit$Loans / data_naomit$TotalAssetes

# 2. Ratio: Trade
data_naomit$Trade <- data_naomit$TradingLiabilities / data_naomit$TotalAssetes

# 3. Ratio: Trading Book


# 4. Ratio: Interbank Lending
data_naomit$InterbankLending <- (data_naomit$LoansAdvances + data_naomit$AssetRepos)  / data_naomit$TotalAssetes

# 5. Ratio: Interbank Borrowing
data_naomit$InterbankBorrowing <- (data_naomit$BankDeposits + data_naomit$LiabilityRepos)  / data_naomit$TotalAssetes

# 6. Ratio: Wholesale Debt
data_naomit$WholesaleDebt <- (data_naomit$OtherDepositsShortBorrowings + data_naomit$LongFunding)  / data_naomit$TotalAssetes

# 7. Ratio: Stable Funding
data_naomit$StableFunding <- (data_naomit$CustomerDeposits + data_naomit$LongFunding)  / data_naomit$TotalAssetes

# 8. Ratio: Deposits
data_naomit$Deposits <- data_naomit$CustomerDeposits / data_naomit$TotalAssetes



#Create data subset

data_subset <- data_naomit[19:25]
data_subset <- scale(data_subset)

# Creating correllation matrix
matrix <- cor(scale(data_subset))
corrplot(corr=matrix, method="number", title="Corellation Matrix of Potential Proxies")

#data_subset

data_clustering <- cbind(data_naomit[1],data_subset)

#data_clustering <- select(data_clustering,GrossLoans,InterbankLending,WholesaleDebt)

######CLUSTERING

rownames(data_clustering) = data_clustering$Name
data_clustering1 <- data_clustering[2:8]

## hclust, method 'complete'
d <- dist(data_clustering, method = "euclidean")
hc1 <- hclust(d, method = "complete" )
plot(hc1, cex = 0.6, hang = -1)


## agnes, method comparison
#Note that agnes(*, method="ward") corresponds to hclust(*, "ward.D2")
hc2 <- agnes(data_clustering, method = "complete")
hc2$ac
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")
ac <- function(x) {
  agnes(data_clustering, method = x)$ac
}
map_dbl(m, ac)  

## agnes, method 'ward'
hc3 <- agnes(data_clustering, method = "ward")
pltree(hc3, cex = 0.6, hang = -1, main = "Dendrogram of agnes")
rect.hclust(hc3, k = 5, border = 2:10)



clust <- cutree(hc3, k = 5)
fviz_cluster(list(data = data_clustering1, cluster = clust))

hc4 <- agnes(data_clustering1, method = "ward")
pltree(hc4, cex = 0.6, hang = -1, main = "Dendrogram of agnes")
rect.hclust(hc4, k = 5, border = 2:10)

## dendextend
hc_single <- agnes(data, method = "single")
hc_complete <- agnes(data, method = "complete")
hc_ward <- agnes(data, method = "ward")
# converting to dendogram objects as dendextend works with dendogram objects 
hc_single <- as.dendrogram(hc_single)
hc_complete <- as.dendrogram(hc_complete)
hc_ward <- as.dendrogram(hc_ward)
tanglegram(hc_ward,hc_complete)

