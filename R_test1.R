rm(list=ls())

setwd("/Users/christian/Documents/Studium/FS 2021/Practitioner Seminar/Data Clustering")

#Hierarchical Agglomerative Clustering (HAC)
#Agglomerative: Bottom-up, we start with one cluster for each observation
#merge until all observations in single cluster


#Data Preparation

library(cluster)
library(purrr)
library(factoextra)
library(car)
data <- Freedman
data <- na.omit(data)
data <- scale(data)

colnames(Freedman)
sapply(Freedman, class)
head(Freedman)

d <- dist(data, method = "euclidean")
hc1 <- hclust(d, method = "complete" )
plot(hc1, cex = 0.6, hang = -1)
hc2 <- agnes(data, method = "complete")
hc2$ac
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")
ac <- function(x) {
  agnes(data, method = x)$ac
}
map_dbl(m, ac)  
hc3 <- agnes(data, method = "ward")
pltree(hc3, cex = 0.6, hang = -1, main = "Dendrogram of agnes")
hc4 <- diana(data)
hc4$dc
pltree(hc4, cex = 0.6, hang = -1, main = "Dendrogram of diana")
clust <- cutree(hc4, k = 5)
fviz_cluster(list(data = data, cluster = clust))
pltree(hc4, hang=-1, cex = 0.6)
rect.hclust(hc4, k = 9, border = 2:10)


library(dendextend)
hc_single <- agnes(data, method = "single")
hc_complete <- agnes(data, method = "complete")
# converting to dendogram objects as dendextend works with dendogram objects 
hc_single <- as.dendrogram(hc_single)
hc_complete <- as.dendrogram(hc_complete)
tanglegram(hc_single,hc_complete)

