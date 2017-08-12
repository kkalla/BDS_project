###############################################################################
## kmeans clustering
###############################################################################
data <- iris
species <- iris$Species
data <- iris[,c(1,2,3,4)]
## 중심의 갯수를 결정
# nbclust
install.packages("NbClust")
library(NbClust)
nc <- NbClust(data, min.nc=2,max.nc=15,method="kmeans")
par(mfrow=c(1,1))
barplot(table(nc$Best.nc[1,]),xlab="Number of Clusters",
        ylab="Number of Criteria",main="Number of Clusters Chosen")

# Wss
library(ggplot2)
plot_wss_kmeans <- function(data,nc=15,seed=2222,algorithm="Hartigan-Wong"){
    wss_ <- c()
    for(i in 2:nc){
        wss <- kmeans(data,centers=i,algorithm=algorithm)$tot.withinss
        wss_ <- c(wss_,wss)
    }
    result <- data.frame(x=2:nc,y=wss_)
    g <- ggplot(result,aes(x,y)) + geom_point()+geom_line()+
        labs(x="Number of clusters",y="Within sum of squares",title="Kmeans")
    return(g)
}
plot_wss_kmeans(data)

result_kmeans <- function(data,nc=15,algorithm=c("Hartigan-Wong",
                                                 "Lloyd", "Forgy","MacQueen")){
    results_ <- list()
    for(i in 1:15){
        result <- kmeans(data,i,algorithm = algorithm)
        results_[[i]] <- result
    }
    return(results_)
}

## Fuzzy clustering
#install.packages("fclust")
library(fclust)
fkm_result <- FKM(X = data, #matrix or dataframe,
                  k = 2, #number of clusters,
                  m = 2, # Parameter of fuzziness,
                  #startU = Rational starting point
)
str(fkm_result)

## hierarchcal
hc <- hclust(dist(data))
plot(hc)
rect.hclust(hc,k=3,border="red")
