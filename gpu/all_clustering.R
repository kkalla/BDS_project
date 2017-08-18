library(cluster)
library(factoextra)

## read data
suwon_r1000 <- read.csv('Data/suwon_std.csv')
suwon_r500 <- read.csv('Data/Suwon_r500_std.csv')
suwon_r500 <- suwon_r500[,-1]

## hclustering
dists_1 <- list(euc = get_dist(suwon_r1000,method="euclidean"),
		pear = get_dist(suwon_r1000,method="pearson"))
dists_2 <- list(euc = get_dist(suwon_r500,method="euclidean"),
		pear = get_dist(suwon_r500,method="pearson"))

h1_euc_r1000 <- hclust(dists_1[[1]])
h2_pear_r1000 <- hclust(dists_1[[2]])
h3_euc_ward_r1000 <- hclust(dists_1[[1]],method='ward.D')
h4_pear_ward_r1000 <- hclust(dists_1[[2]],method='ward.D')
h1_euc_r500 <- hclust(dists_2[[1]])
h2_pear_r500 <- hclust(dists_2[[2]])
h3_euc_ward_r500 <- hclust(dists_2[[1]],method='ward.D')
h4_pear_ward_r500 <- hclust(dists_2[[2]],method='ward.D')

pdf('hclust_plot.pdf')
plot(h1_euc_r1000,labels = F,sub = "Euclidean-complete",xlab="r1000")
plot(h2_pear_r1000,labels = F,sub = "Pearson-complete",xlab="r1000")
plot(h3_euc_ward_r1000,labels = F,sub = "Euclidean-ward",xlab="r1000")
plot(h4_pear_ward_r1000,labels = F,sub = "Pearson-complete",xlab="r1000")
plot(h1_euc_r500,labels = F,sub = "Euclidean-complete",xlab="r500")
plot(h2_pear_r500,labels = F,sub = "Pearson-complete",xlab="r500")
plot(h3_euc_ward_r500,labels = F,sub = "Euclidean-ward",xlab="r500")
plot(h4_pear_ward_r500,labels = F,sub = "Pearson-complete",xlab="r500")
dev.off()

## kmeans
pdf('kmeans_plot.pdf')
methods_ <- c('silhouette','wss')
for(i in 1:2){
	fviz_nbclust(suwon_r1000,kmeans,k.max=15,diss=dists_1[[2]],
		     method=methods_[i])
	fviz_nbclust(suwon_r500,kmeans,k.max=15,diss=dists_2[[2]],
		     method=methods_[i])
}
dev.off()

gap_stat <- clusGap(suwon_r500,kmeans,nstart=25,K.max=10,B=100)
gap_stat2 <- clusGap(suwon_r1000,kmeans,nstart=25, K.max=15,B=100)
print(gap_stat,method="firstmax")
pdf('Gap_stat_plot.pdf')
fviz_gap_stat(gap_stat)
fviz_gap_stat(gap_stat2)
dev.off()

# kmeans with best number of clusters
nc <- 4
km_result <- kmeans(suwon_r500,nc,nstart=25)
suwon_r500_kmeans <- km_result$cluster
# Using wss
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
plot_wss_kmeans(suwon_r1000)
plot_wss_kmeans(suwon_r500)
dev.off()

# Using Nbclust
library(NbClust)
nc1 <- NbClust(suwon_r1000,min.nc=2,max.nc=15,method="kmeans")
png('Nbclust_suwon_r1000.png')
barplot(table(nc1$Best.nc[1,]),xlab='Number of Clusters',
	ylab="Number of Critria",main="Number of Clusters Chosen(r1000)")
dev.off()
nc2 <- NbClust(suwon_r500,min.nc=2,max.nc=15,method="kmeans")
png('Nbclust_suwon_r500.png')
barplot(table(nc2$Best.nc[1,]),xlab='Number of Clusters',
	ylab="Number of Critria",main="Number of Clusters Chosen(r500)")
dev.off()



##Random forest
library(randomForest)
rf_r1000 <- randomForest(suwon_r1000)
rf_r1000_prox <- randomForest(suwon_r1000,ntree=1000,proximity=TRUE)$proximity
rf_r500_prox <- randomForest(suwon_r500,ntree=1000,proximity=TRUE)$proximity


km_rf_r1000 <- kmeans(rf_r1000_prox, 4, iter.max=100,nstart=10)
km_rf_r500 <- kmeans(rf_r500_prox,4,iter.max=100,nstart=10)
cl_rf <- randomForest(suwon_r1000,as.factor(km_rf_r1000$cluster),ntree=1000)
cl_rf_r500 <- randomForest(suwon_r500,as.factor(km_rf_r500$cluster),ntree=1000)


