## hierarchical clustring
## dataset with 23 columns

## Make dataset to analyze
combine_all <- function(){
	file_names_ <- dir('Data/clust_data')
	col_need <- c("MT1","CS2","PS3","SC4", "AC5","PK6","OL7","SW8","BK9",
		      "CT1","AG2","PO3","AT4","AD5","FD6","CE7","HP8","PM9",
		      "size_factor","valueBysize1")
	result <- data.frame()
	for(i in 1:length(file_names_)){
		data <- read.csv(paste0('Data/clust_data/',file_names_[i]))
		if(length(colnames(data)) > 27){
			asset_id <- data[,c(2)]
		}else{
			asset_id <- data[,c(1)]
		}
		sub1 <- data[,col_need]
		sub2 <- cbind(asset_id,sub1)
		result <- rbind(result,sub2)
	}
	return(result)	
}
result <- combine_all()
sub1 <- result[result$valueBysize1!=Inf,-1]
## Decide number of clusters
# install.packages('cluster')
library(cluster)
d_matrix <- daisy(sub1,metric="gower")
method <- c("complete","mcquitty","ward.D","ward.D2")
clust_results_ <- list()
for(i in 1:4){
	hc <- hclust(d_matrix,method=method[i])
	clust_results_[[i]] <- hc
}
par(mfrow=c(1,4))
for(i in 1:4){
	plot(clust_results_[[i]])
}
