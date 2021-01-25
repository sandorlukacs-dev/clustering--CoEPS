
# Clear Environment
rm(list=ls())
# Import libra

library(corrplot)
library(cluster)
library(purrr)
library(factoextra)
library(dplyr)
library(dendextend)
library(tidyverse)
library(plyr)
library(fpc)
library(MASS)
library(clusterSim)

#######################
######## DATA #########
#######################

# Reading in data
data <- read.csv('data1.csv', sep = ';')
selected =c("Name","LoansAdvances", "LiabilityRepos", "BankDeposits", "LongFunding", "CustomerDeposits", "LiquidAssets", "Loans", "Intangibles", "NetInterestIncome", "OperatingIncome", "Equity", "TotalAssetes")
data = dplyr::select(data, all_of(selected))



# Preparing data
data = na_if(data, "NA")
data = na.omit(data)

rownames(data) = data$Name

# Naming columns

colnum <- as.vector(colnames(data[2:12]))

# Converting to numeric
data[colnum] <- lapply(data[colnum], function(x) as.numeric(as.character(x)))

#######################
####### RATIOS ########
#######################

# 1. Ratio: Gross Loans (LTA)
data$LTA <- data$Loans / data$TotalAssetes
# 2. Ratio: Trade (TTA)
data$TTA <- (data$TotalAssetes - data$LiquidAssets - data$Loans - data$Intangibles) / data$TotalAssetes
# 4. Ratio: Interbank Lending (ILTA)
#data$ILTA <- (data$LoansAdvances + data$AssetRepos)  / data$TotalAssetes
# 5. Ratio: Interbank Borrowing (IBTA)
data$IBTA <- (data$BankDeposits + data$LiabilityRepos)  / data$TotalAssetes
# 6. Ratio: Wholesale Debt (WDA)
#data$WDA <- (data$OtherDepositsShortBorrowings + data$LongFunding)  / data$TotalAssetes
# 7. Ratio: Stable Funding (SFA)
data$SFA <- (data$CustomerDeposits + data$LongFunding)  / data$TotalAssetes
# 8. Ratio: Deposits (CDA)
data$CDA <- data$CustomerDeposits / data$TotalAssetes
# 9. Ratio: Interest Income (IIO)
data$IIO = data$NetInterestIncome/data$OperatingIncome


#Create data subset
selected =c("LTA", "IBTA", "TTA", "SFA", "CDA", "IIO")
data_clustering = dplyr::select(data, all_of(selected))

# Creating correllation matrix
matrix <- cor(scale(data_clustering))
corrplot(corr=matrix, method="number", title="Corellation Matrix of Potential Proxies")

#!!!SFA Should be excluded
selected =c("LTA", "IBTA", "TTA", "CDA", "IIO")
data_clustering = dplyr::select(data, all_of(selected))

###########################
####### CLUSTERING ########
###########################


# Running clustering for every combination of proxies

container = list()
ac = c()
counter = 1
for (i in 4:(length(selected))) {
  
  combinations = combn(selected,i)
  
  for (k in 1:ncol(combinations)) {
    
    
    data_subset = dplyr::select(data, combinations[,k])
    data_subset = na.omit(data_subset)
    cluster <- (agnes(data_subset, method="ward"))
    container[[counter]] <-  cluster
    ac = append(ac, cluster$ac)
    counter = counter + 1
    
  }
        
}

# Get the number of best clustering based on AC score
max_index = which(ac==max(ac))

#Save best clustering
#best_clustering <- (agnes(data_subset, method="ward"))
best_clustering = container[[max_index]]
used_ratios = colnames(best_clustering$data)

res <- array(0,c(6-3+1,2))
res[,1] <- 3:6
clusters <- NULL

#for (i in 3:6) {
  
#  cl <- pam(best_clustering$data,i)
#  res[i-3+1,2] <- G1 <- index.G1(best_clustering$data,cl$cluster,centrotypes="centroids")
#  clusters <- rbind(clusters, cl$cluster)
  
#}

#print(paste("max G1 for",(3:6)[which.max(res[,2])],"clusters=",max(res[,2])))
#print("clustering for max G1")
#plot(res, type="p", pch=0, xlab="Number of clusters", ylab="G1", xaxt="n")
#axis(1, c(3:6))

#Create clusters
clust <- cutree(best_clustering, k = 3)

#Plot clusters

pltree(best_clustering, hang=-1, cex = 0.6)

rect.hclust(best_clustering, k = 3, border = 2:10)

fviz_cluster(list(data = best_clustering$data, cluster = clust), labelsize =7)



results_clustering <- data.frame(
                                  Name=best_clustering$order.lab, 
                                  Segment=cutree(best_clustering,3)[best_clustering$order]
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

