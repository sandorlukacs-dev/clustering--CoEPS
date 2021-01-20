
# Clear Environment
rm(list=ls())

setwd("/Users/christian/Documents/Studium/FS 2021/Practitioner Seminar/Data Clustering")

# Import libra

library(corrplot)
library(cluster)
library(purrr)
library(factoextra)
library(dplyr)
library(dendextend)
library(tidyverse)
library(caret)

'''

DATA

'''

# Reading in data
data <- read.csv('Ready_for_csv_export.csv', sep = ';')

# Preparing data
data_nafix = na_if(data, "n.a.")
data_nafix = na_if(data_nafix, 0)
data_naomit = na.omit(data_nafix)

rownames(data_naomit) = data_naomit$Name

# Naming columns

colnum <- as.vector(colnames(data_naomit[3:18]))

# Converting to numeric
data_naomit[colnum] <- lapply(data_naomit[colnum], function(x) as.numeric(as.character(x)))

'''

RATIOS

'''
# 1. Ratio: Gross Loans (LTA)
data_naomit$LTA <- data_naomit$Loans / data_naomit$TotalAssetes
# 2. Ratio: Trade (TTA)
data_naomit$TTA <- (data_naomit$TotalAssetes - data_naomit$LiquidAssets - data_naomit$Loans - data_naomit$Intangibles) / data_naomit$TotalAssetes

# 3. Ratio: Trading Book

# 4. Ratio: Interbank Lending (ILTA)
data_naomit$ILTA <- (data_naomit$LoansAdvances + data_naomit$AssetRepos)  / data_naomit$TotalAssetes
# 5. Ratio: Interbank Borrowing (IBTA)
data_naomit$IBTA <- (data_naomit$BankDeposits + data_naomit$LiabilityRepos)  / data_naomit$TotalAssetes
# 6. Ratio: Wholesale Debt (WDA)
data_naomit$WDA <- (data_naomit$OtherDepositsShortBorrowings + data_naomit$LongFunding)  / data_naomit$TotalAssetes
# 7. Ratio: Stable Funding (SFA)
data_naomit$SFA <- (data_naomit$CustomerDeposits + data_naomit$LongFunding)  / data_naomit$TotalAssetes
# 8. Ratio: Deposits (CDA)
data_naomit$CDA <- data_naomit$CustomerDeposits / data_naomit$TotalAssetes
# 9. Ratio: Interest Income (IIO)
data_naomit$IIO = data_naomit$NetInterestIncome/data_naomit$OperatingIncome


#Create data subset
selected =c("LTA", "IBTA", "TTA", "ILTA", "WDA", "SFA", "CDA", "IIO")
data_clustering = select(data_naomit, all_of(selected))

# Creating correllation matrix
matrix <- cor(scale(data_clustering))
corrplot(corr=matrix, method="number", title="Corellation Matrix of Potential Proxies")

##CLUSTERING

container = list()
counter = 1
for (i in 1:8) {
  
  combinations = combn(selected,i)
  
  for (k in 1:ncol(combinations)) {
    
    
    data_subset = select(data_naomit, combinations[,k])
    cluster <- (hclust(dist(data_subset, method="euclidean"), method="complete"))
    container[[counter]] <-  cluster
    
    counter = counter + 1
    
  }
        
}

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
rect.hclust(hc3, k = 4, border = 2:10)



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
