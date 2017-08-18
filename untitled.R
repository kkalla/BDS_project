data1 <- read.csv('Data/suwon_publicfc_r500.csv',stringsAsFactors = F)
data2 <-read.csv('Data/suwon_publicfc_r1000.csv',stringsAsFactors = F)
data3 <- read.csv('Data/suwon_publicassets_r500.csv',stringsAsFactors = F)
str(data3)
str(data1)

file_names_ <- dir('Data/SW_COUNT')
tot_data <- data.frame()
cols <- colnames(data)
cols <- cols[-c(5,23)]
for(i in 1:6){
    data <- read.csv(paste0('Data/SW_COUNT/',file_names_[i]))
    data <- data[,cols]
    tot_data <- rbind(tot_data,data)    
}
str(tot_data)

write.csv(tot_data,'Data/tot_publicfc.csv',row.names = F)
tot_data_cat <- tot_data[,1]
tot_data_features <- tot_data %>% select("Land_size"=7,"Land_value"=6,
                                         4,15:31)
str(tot_data_features)
tot_data_features$Land_Value_bySize <- tot_data_features$Land_value/tot_data_features$Land_size
tot_data_std <- data.frame(scale(tot_data_features))
str(tot_data_std)
tot_data_std <- cbind(tot_data_cat,tot_data_std)
colnames(tot_data_std)[1] <- "category"
write.csv(tot_data_std,'Data/tot_data_std.csv',row.names = F)


str(data3)
data3 <- data3[,c(2,10,11,25,27:44)]
str(data3)
colnames(data3)[c(2,3,4)] <- c("Land_size","Land_value","Land_Value_bySize")
str(data3)
write.csv(data3,'Data/Suwon_r500.csv',row.names = F)
suwon_r500 <- read.csv('gpu/Data/Suwon_r500.csv',stringsAsFactors = F)
feats <- data.frame(scale(suwon_r500[suwon_r500$Land_Value_bySize!=Inf,-1]))
str(feats)
asset_ID <- suwon_r500[suwon_r500$Land_Value_bySize!=Inf,1]
suwon_r500_std <- cbind(asset_ID,feats)
str(suwon_r500_std)
write.csv(suwon_r500_std,'gpu/Data/Suwon_r500_std.csv',row.names = F)

library(factoextra)
install.packages('factoextra')
?fviz_nbclust
