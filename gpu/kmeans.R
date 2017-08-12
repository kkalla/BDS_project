## kmeans clustering

## Decide number of centers
# install.packages('NbClust')
library(NbClust)
all_data <- read.csv('Data/clust_data/total1.csv')
data <- all_data[,-1]
nc <- NbClust(data,min.nc=2, max.nc=15, method="kmeans")
pdf('Nbclust_plot1.pdf')
par(mfrow=c(1,1))
barplot(table(nc$Best.nc[1,]),xlab="Number of Clusters",
	        ylab="Number of Criteria",main="Number of Clusters Chosen")
dev.off()

## Using Within sum of squares
library(ggplot2)
plot_wss_kmeans <- function(data,nc=15,seed=2222,algorithm="Hartigan-Wong"){
	wss_ <- c()
    	for(i in 2:nc){
		wss <- kmeans(data,centers=i,algorithm=algorithm)$tot.withinss
                wss_ <- c(wss_,wss)
	}
        result <- data.frame(x=2:nc,y=wss_)
        g <- ggplot(result,aes(x,y)) + geom_point()+geom_line()+
		labs(x="Number of clusters",y="Within sum of squares",
		     title="Kmeans")
	return(g)
	}
pdf('Wss_of_kmeans.pdf')
plot_wss_kmeans(data)
dev.off()

result_kmeans <- function(data,nc=15,
			  algorithm=c("Hartigan-Wong","Lloyd",
				      "Forgy","MacQueen")){
	results_ <- list()
        for(i in 1:15){
	            result <- kmeans(data,i,algorithm = algorithm)
                    results_[[i]] <- result
	}
        return(results_)
}

