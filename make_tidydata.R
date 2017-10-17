## make tidy data

file_names <- dir('Data/gg_public_assets')
data_list <- list()

for(i in 1:length(file_names)){
    df <- read.csv(paste('Data/gg_public_assets/',file_names[i],sep = ""))
    data_list[[i]] <- df
}

source('get_longlat.R')
tidy <- list()
for(i in 1:length(file_names)){
    df <- data_list[[i]]
    df <- make_full_address(df)
    tidy[[i]] <- df[,!colnames(df) %in% 
                        c('X','회계구분명','시군구명','읍면동명','리명','본번',
                          '부번','입력시스템명','취득승인여부')]
}


num <- c(1:19,22:32)
for (i in num){
    write.csv(tidy[[i]],paste('Data/ggpa2/',file_names[i],sep=""),
              row.names = FALSE)
}


