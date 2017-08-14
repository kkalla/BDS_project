## Preparing data
file_names_ <- dir('Data/clust_data')
data_lists_ <- list()
sub_lists_ <- list()
for(i in 1:length(file_names_)){
	data_lists_[[i]]<- read.csv(paste0('Data/clust_data/',file_names_[i]))
	n <- length(colnames((data_lists_[[i]])))
	if(n>30)
		sub <- data_lists_[[i]][,c(1:2,n-20,((n-18):n))]
	else
		sub <- data_lists_[[i]]
	sub_lists_[[i]] <- sub
}
for(i in 1:length(sub_lists_)){
	if(sub_lists_[[i]]$asset_ID[1] == 2015){
		col_names_ <- colnames(sub_lists_[[i]])
		col_names_[2] <- "asset_ID"
		colnames(sub_lists_[[i]]) <- col_names_
		sub_lists_[[i]] <- sub_lists_[[i]][,c(-1)]
	}
}
col_names_ <- colnames(sub_lists_[[19]])
for(i in 1:length(sub_lists_)){
	sub_lists_[[i]]<-sub_lists_[[i]][,col_names_]
}
data <- data.frame()
for(i in 1:length(sub_lists_)){
	data <- rbind(data,sub_lists_[[i]])
}
data_features <- data[,-1]
data_std <- scale(data_features[data_features$valueBysize2 != Inf | 
		  !is.na(data_features$valueBysize2),-1])

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
pdf("fviz_nbclust.pdf")
for(i in 1:3){
	methods_ <- c("silhouette","wss","gap_stat")
	fviz_nbclust(data_std,kmeans,method=methods_[i])
}
dev.off()
# Cluster plot with optimal number of clusters
km_result <- kmeans(data_std,nc,nstart=25)
# visualize
fviz_cluster(km_result,data=data_std,frame.type="convex")+theme_minimal()

# Compute PAM
pam_res <- pam(data_std,nc)
fviz_cluster(pam_res)


# 2. Hierarchical clustering
d <- dist(data_std,method="euclidean")
h_res <- hclust(d,method="ward.D2")
# Cut tree into several groups
grp <- cutree(h_res,k=4)
# Visualize
plot(h_res,cex=0.6,labels=FALSE)
rect.hclust(h_res,k=4,border=2:5) # add rectangle

## using factoextra
h_res <- hcut(data_std,k=4)
fviz_dend(h_res,rect=TRUE,cex=0.5,k_colors=c("red","yellow","green","blue"))
