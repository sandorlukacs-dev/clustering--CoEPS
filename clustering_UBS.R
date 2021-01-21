
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
library(plyr)
library(fpc)

#######################
######## DATA #########
#######################

# Reading in data
data <- read.csv('data.csv', sep = ';')

# Preparing data
data = na_if(data, "n.a.")
data = na.omit(data)

rownames(data) = data$Name

# Naming columns

colnum <- as.vector(colnames(data[2:16]))

# Converting to numeric
data[colnum] <- lapply(data[colnum], function(x) as.numeric(as.character(x)))

#######################
####### RATIOS ########
#######################

# 1. Ratio: Gross Loans (LTA)
data$LTA <- data$Loans / data$TotalAssetes
# 2. Ratio: Trade (TTA)
data$TTA <- (data$TotalAssetes - data$LiquidAssets - data$Loans - data$Intangibles) / data$TotalAssetes

# 3. Ratio: Trading Book

# 4. Ratio: Interbank Lending (ILTA)
data$ILTA <- (data$LoansAdvances + data$AssetRepos)  / data$TotalAssetes
# 5. Ratio: Interbank Borrowing (IBTA)
data$IBTA <- (data$BankDeposits + data$LiabilityRepos)  / data$TotalAssetes
# 6. Ratio: Wholesale Debt (WDA)
data$WDA <- (data$OtherDepositsShortBorrowings + data$LongFunding)  / data$TotalAssetes
# 7. Ratio: Stable Funding (SFA)
data$SFA <- (data$CustomerDeposits + data$LongFunding)  / data$TotalAssetes
# 8. Ratio: Deposits (CDA)
data$CDA <- data$CustomerDeposits / data$TotalAssetes
# 9. Ratio: Interest Income (IIO)
data$IIO = data$NetInterestIncome/data$OperatingIncome


#Create data subset
selected =c("LTA", "IBTA", "TTA", "ILTA", "WDA", "SFA", "CDA", "IIO")
data_clustering = select(data, all_of(selected))

# Creating correllation matrix
matrix <- cor(scale(data_clustering))
corrplot(corr=matrix, method="number", title="Corellation Matrix of Potential Proxies")

#!!!SFA Should be excluded
selected =c("LTA", "IBTA", "TTA", "ILTA", "WDA", "CDA", "IIO")
data_clustering = select(data, all_of(selected))

###########################
####### CLUSTERING ########
###########################


# Running clustering for every combination of proxies

container = list()
ac = c()
counter = 1
for (i in 3:(length(selected))) {
  
  combinations = combn(selected,i)
  
  for (k in 1:ncol(combinations)) {
    
    
    data_subset = select(data, combinations[,k])
    data_subset = na.omit(data_subset)
    cluster <- (agnes(data_subset, method="ward"))
    container[[counter]] <-  cluster
    ac = append(ac, cluster$ac)
    counter = counter + 1
    
  }
        
}

# Get the number of best clustering based on AC scor
max_index = which(ac==max(ac))

#Save best clustering
best_clustering = container[[max_index]]
used_ratios = colnames(best_clustering$data)

#Create clusters
clust <- cutree(best_clustering, k = 4)

#Plot clusters

pltree(best_clustering, hang=-1, cex = 0.6)

rect.hclust(best_clustering, k = 4, border = 2:10)

fviz_cluster(list(data = best_clustering$data, cluster = clust), labelsize =7)



results_clustering <- data.frame(
                                  Name=best_clustering$order.lab, 
                                  Segment=cutree(best_clustering,4)[best_clustering$order]
                                  )
data_clustering$Name <- rownames(data_clustering)
data_clustering <- data_clustering[order(data_clustering$Name),]
results_clustering <- results_clustering[order(results_clustering$Name),]

results_clustering = cbind(results_clustering, data_clustering)

selected = append("Segment", used_ratios)

results_clustering = select(results_clustering, all_of(selected))


results_clustering["Segment"] <- lapply(results_clustering["Segment"], function(x) as.numeric(x))


Mean.Values <- aggregate(x=results_clustering, by=list(results_clustering$Segment), FUN=mean)
Mean.Values <- select(Mean.Values, all_of(used_ratios))
heatmap(as.matrix(Mean.Values))

## hclust, method 'complete'
d <- dist(data_clustering, method = "euclidean")
hc1 <- hclust(d, method = "ward.D2" )
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
