## Preparing data
data <- read.csv('Data/clust_data/total1.csv')
data_features <- data[,-1]
data_std <- scale(data_features)

## Clarifying distance measures
# install.packages("factoextra")
library(cluster)
library(factoextra)
# get dist takes a few seconds
dists_ <- list(euclid = get_dist(data_std,method="euclidean"),
	       pearson = get_dist(data_std,method="pearson"))
for(i in 1:2){
	names_ <- c("Euclidean","Pearson")
	png(paste0("fviz_dist_",names_[i],".png"))
	fviz_dist(dists_[[i]],show_labels=FALSE,
		  gradient = list(low="#00AFBB",mid="white",high="#FC4E07"))
	dev.off()
}

## Basic clustering methods
# Partitioning clustering
# 1. kmeans
# Compute and visualize k-menas clustering
for(i in 1:3){
	methods_ <- c("silhouette","wss","gap_stat")
	png(paste0("fviz_nbclust_",methods_[i],".png"))
	fviz_nbclust(data_std,kmeans,method=methods_[i])
	dev.off()
}

