file_names_ <- dir('Data/ggpa2')
source('make_data_for_clustering.R',encoding = "UTF-8")
## Preprocessing - 시간이 오래걸림 주의!
for(i in c(16)){
    data <- read.csv(paste0("Data/ggpa2/",file_names_[i]),stringsAsFactor=FALSE)
    radius=1000
    ana_data <- make_tidy(data,radius)
    colnames_ <- colnames(ana_data)
    colnames_[2] <- "asset_ID"
    colnames(ana_data) <- colnames_
    ana_data2$valueBysize1 <- ana_data2$valueBysize1/10000
    ana_data2$valueBysize2 <- ana_data2$valueBysize2/10000
    
    write.csv(ana_data2,paste0('Data/clust_data/',substr(file_names_[i],1,7),
                               '_r',radius,'.csv'),row.names = F)
    
}
# 16,18,19,20,21,23,27,28,29,31번해야
## subsetting



## Do clustering
library(cluster)
d_matrix <- daisy(sub1,metric="gower")
method = c("single","complete","centroid","average","median","mcquitty",
           "ward.D","ward.D2")
clust_results_ <- list()
for(i in 1:length(method)){
    hc <- hclust(d_matrix,method=method[i])
    clust_results_[[i]] <- hc
}
library(fpc)
par(mfrow=c(1,4))
for(i in 5:8){
    plot(clust_results_[[i]])
}
