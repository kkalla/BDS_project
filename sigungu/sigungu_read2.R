## setwd('../project/Data/sigungu')
library(xlsx)

fill_na <- function(df,choose=c('center','none')){
    sido_name <- unique(na.omit(as.character(df$sido)))
    sido <- c()
    for(i in 1:(length(sido_name)-1)){
        n <- which(df$sido==sido_name[i + 1])-which(df$sido==sido_name[i])
        sido <- c(sido,rep(sido_name[i],n))
    }
    sido <- c(sido,rep(sido_name[length(sido_name)],nrow(df)-length(sido)))
    df$sido <- as.factor(sido)
    if (choose=='center'){
        centers_ <- unique(na.omit(as.character(df$center)))
        center <- c()
        for(i in 1:(length(centers_)-1)){
            n <- which(df$center==centers_[i + 1]) - which(df$center==centers_[i])
            center <- c(center,rep(centers_[i],n))
        }
        center <- c(center,rep(centers_[length(centers_)],nrow(df)-length(center)))
        df$center <- as.factor(center)
    }
    
    return(df)
}

## Loading 2008 data
start <- Sys.time()
data_2008 <- read.xlsx('시군구별+등급판정결과+현황_2008.xls',
                       sheetIndex=1,header=FALSE,startRow = 5,colIndex=c(1:9),
                       encoding = 'UTF-8')
loading_time <- Sys.time() - start
column_names <- c('sido','sigungu','total',
                  'grade1','grade2','grade2','out_A','out_B','out_C')
colnames(data_2008)<-column_names
str(data_2008$sigungu)
sido_name <- as.character(unique(data_2008$sido)[-2])
sido <- c()
for(i in 1:(length(sido_name)-1)){
    sido <- c(sido,rep(sido_name[i],
                       which(data_2008$sido==sido_name[i + 1])-
                           which(data_2008$sido==sido_name[i])))
}
sido <- c(sido,rep(sido_name[length(sido_name)],nrow(data_2008)-length(sido)))
data_2008$sido <- sido
is.na(data_2008$sigungu)
str(data_2008)
## Loading 2009 data - 경기도만
start <- Sys.time()
data_2009 <- read.csv('경기_2009.txt',header = FALSE,sep=' ',
                      dec=',',fileEncoding='UTF-8',skip=1)
loading_time <- Sys.time() - start
column_names_2009 <- readLines('경기_2009.txt',encoding = 'UTF-8',n=1)
column_names_2009 <- strsplit(column_names_2009,split=' ')
colnames(data_2009) <- unlist(column_names_2009)
str(data_2009)
is.na(data_2009$시군구)

## Loading 2010 data - 경기도만
data_2010 <- read.csv('2010.txt',header=TRUE,sep=' ',dec=',',
                      fileEncoding='UTF-8')
str(data_2010)
data_2010_rows <- read.csv('2010_label.csv',header=FALSE,fileEncoding='UTF-8')
data_2010_rows <- rep(as.character(data_2010_rows[,1]),each=5)
data_2010$center <- data_2010_rows


## loading 2011 data
start <- Sys.time()
data_2011 <- read.xlsx('노인장기요양보험_등급판정현황_2011.xls',sheetIndex=1,
                  header=FALSE, startRow = 6,colIndex=c(1:9),encoding = 'UTF-8')
loading_time <- Sys.time() - start
str(data_2011)
data_2011_cols <- c('sido','center','type','applicants','total',
                    'grade1','grade2','grade3','outer')
colnames(data_2011) <- data_2011_cols
data_2011 <- fill_na(data_2011)
sum(is.na(df$center))
sum(is.na(df$sido))

## loading 2012 data - 경기도만
data_2012 <- read.csv('2012.txt',header=FALSE,sep=' ',dec=',',
                      fileEncoding='UTF-8')
centers <- readLines('2012_centers.txt')
centers <- rep(centers,each=5)
data_2012$center <- centers
str(data_2012)

##loading 2013 data
start <- Sys.time()
data_2013 <- read.xlsx('노인장기요양보험_등급판정현황_자격별_2013.xls',
                       sheetIndex=1,header=FALSE, 
                       startRow = 6,colIndex=c(1:9),encoding = 'UTF-8')
loading_time <- Sys.time() - start
colnames(data_2013) <- data_2011_cols
str(data_2013)
data_2013 <- fill_na(data_2013)
sum(is.na(df$center))
sum(is.na(df$sido))


## loading 2014 data
start <- Sys.time()
data_2014 <- read.xlsx('노인장기요양보험_등급판정현황_자격별_2014.xls',
                       sheetIndex=1,header=FALSE, 
                       startRow = 6,colIndex=c(1:11),encoding = 'UTF-8')
loading_time <- Sys.time() - start
colnames(data_2014) <- c(data_2011_cols[-length(data_2011_cols)],
                         'grade4','grade5','outer')
str(data_2014)
df <- fill_na(data_2014)
sum(is.na(data_2014$center))
sum(is.na(data_2014$sido))
data_2014 <- df


## loading 2015 data
start <- Sys.time()
data_2015 <- read.xlsx('노인장기요양보험_등급판정현황_자격별_2015.xls',
                       sheetIndex=1,header=FALSE, 
                       startRow = 6,colIndex=c(1:11),encoding = 'UTF-8')
loading_time <- Sys.time() - start
colnames(data_2015) <- colnames(data_2014)
str(data_2015)

df <- fill_na(data_2015)
sum(is.na(df$center))
sum(is.na(data_2015$sido))
data_2015 <- df
tail(data_2015)
head(df)
head(data_2015)
## loading 2016 data
start <- Sys.time()
data_2016 <- read.xlsx('운영센터별_등급판정현황_자격별_2016.xls',
                       sheetIndex=1,header=FALSE, 
                       startRow = 6,colIndex=c(2:12),encoding = 'UTF-8')
loading_time <- Sys.time() - start
colnames(data_2016) <- colnames(data_2014)
data_2016 <- data_2016[-nrow(data_2016),]


data_2016 <- fill_na(data_2016,choose = 'none')
tail(data_2016)

## loading 2017 jan-june data
data_list_2017 <- list()
filename <- '운영센터별_등급판정현황_자격별_2017_'
start <- Sys.time()
for( i in 1:6) {
    data <- read.xlsx(paste(filename,i,'월.xls',sep=""),
                              sheetIndex=1,header=FALSE, 
                              startRow = 6,colIndex=c(2:12),encoding = 'UTF-8')
    colnames(data) <- colnames(data_2016)
    data_list_2017[[i]] <- data
}
loading_time <- Sys.time() - start
str(data_list_2017)
for(i in 1:length(data_list_2017)){
    df <- data_list_2017[[i]]
    df <- df[-nrow(df),]
    df <- fill_na(df,"none")
    data_list_2017[[i]] <- df
}
for(i in 1:length(data_list_2017)){
    sum(is.na(data_list_2017[[i]]$sido))
    sum(is.na(data_list_2017[[i]]$center))
}

## Write data frames to csv file
list_data <- list(data_2008,data_2009,data_2010,data_2011,data_2012,data_2013,
                  data_2014,data_2015,data_2016)
filename <- "sigungu_"
years <- c(paste(rep("200",2),c(8:9),sep=""),paste(rep("20",7),c(10:16),sep=""))
for(i in 1:length(list_data)){
    data <- list_data[[i]]
    fn <- paste(filename,years[i],".csv",sep="")
    write.csv(data,file = fn,fileEncoding = 'UTF-8')
}
months <- c('jan','feb','mar','apr','may','jun')
for(i in 1:length(data_list_2017)){
    filename <- "sigungu_2017_"
    data <- data_list_2017[[i]]
    fn <- paste(filename,months[i],".csv",sep="")
    write.csv(data,file=fn,fileEncoding = "")
}
