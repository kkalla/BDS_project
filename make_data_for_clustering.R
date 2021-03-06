## Make datasets for clustering

## Read dataset and make subset with 2 columns
# data <- read.csv('Data/ggpa2/경기도 하남시.csv',stringsAsFactor=FALSE)

## get longitude and latitude using naver api
# source('get_longlat.R')
# sub <- make_subset(data)
# lonlat <- get_geocode(sub$address)
# str(lonlat)
# sub2 <- cbind(sub,lonlat)
# head(sub2)
# str(sub2)

## nearbysearch -> compute sum by category


sum_by_category <- function(
    df,
    categories=c("MT1","CS2","PS3","SC4","AC5","PK6","OL7","SW8","BK9","CT1",
                 "AG2","PO3","AT4","AD5","FD6","CE7","HP8","PM9"),
    radius=5000){
    source('using_kakaoapi.R')
    totals_ <- data.frame()
    start <- Sys.time()
    for(i in 1:nrow(df)){
        x = df$longitude[i]
        y = df$latitude[i]
        counts_ <- c()
        for(j in 1:length(categories)){
            counts_ <- c(counts_,get_total_count(categories[j],x,y,radius))
        }
        totals_ <- rbind(totals_,counts_)
    }
    print(Sys.time()-start)
    colnames(totals_) <- categories
    return(totals_)
}
# totals_ <- sum_by_category(sub2)
# str(totals_)
## cbind with subset
# ana_data <- cbind(sub3,totals_)


make_tidy <- function(df,variables=c("재산번호"),sizeAsFactor=TRUE,
                      isLonglat=FALSE,
                      except = c("공원","공장용지","과수원","구거","답","대",
                                 "도로","목장용지","묘지","미등록","사적지",
                                 "수도용지","양어장","염전","유원지","유지",
                                 "임야","잡종지","전","제방","종교용지",
                                 "주유소용지","주차장","창고용지","철도용지",
                                 "체육용지","하천","학교용지")[7],
                      categories=c("MT1","CS2","PS3","SC4","AC5","PK6","OL7",
                                   "SW8","BK9","CT1","AG2","PO3","AT4","AD5",
                                   "FD6","CE7","HP8","PM9"),radius=5000){
    # Parameters
    # --------------
    # df: dataframe
    #   분석할 데이터 원본
    # variables: character vector
    #   결과에 포함시킬 변수명을 vector로 넣어주세요
    # sizeAsFactor: boolean(default = FALSE)
    #   TRUE -> 재산면적을 factor로 변환
    # isLonglat: boolean(default = FALSE)
    #   FALSE -> 위도와 경도가 원본에 포함되어있지 않을때
    #   TRUE -> 위도와 경도가 포함되어 있을때
    # except: character vector (default = "도로")
    #   제외시킬 실지목명(또는 공부지목명) 목록
    # categories: character vector
    #   c("MT1","CS2","PS3","SC4","AC5","PK6","OL7","SW8","BK9","CT1",
    #      "AG2","PO3","AT4","AD5","FD6","CE7","HP8","PM9") 중에서 선택
    #
    # Returns
    # ------------
    # result: dataframe
    #   분석에 사용할 수 있는 데이터프레임을 반환합니다.
    total_exetime_start <- Sys.time()
    sub1 <- df[!df$실지목명 %in% except,]
    sub1 <- sub1[!is.na(sub1$재산면적),]
    if(nrow(sub1)>5500){
        n <- sample(nrow(sub1),5500)
        sub1 <- sub1[n,]
    }
    if(!isLonglat){
        source('get_longlat.R')
        start <- Sys.time()
        lonlat <- get_geocode(sub1$address)
        colnames(lonlat)<- c("longitude","latitude")
        print(paste("Get lon&lat in",Sys.time()-start))
    }
    sub1 <- cbind(sub1,lonlat)
    sub1 <- sub1[sub1$longitude<777,]
    if(sizeAsFactor){
        size_factor <- sapply(sub1$재산면적,function(x) {
            if(x<300) x=1
            else if(x>=300 && x<500) x=2
            else if(x>=500 && x<1000) x=3
            else x=4
        })
        size_factor <- as.factor(size_factor)
    }
    
    category_sum <- sum_by_category(sub1,categories,radius)
    if(sub1$재산면적!=0){
        valueBysize1 <- sub1$대장가액.원. / sub1$재산면적
        valueBysize2 <- sub1$기준가액.원. / sub1$재산면적
    }else{
        valueBysize1 <- sub1$대장가액.원.
        valueBysize2 <- sub1$기준가액.원.
    }
    
    result <- cbind(sub1,size_factor,valueBysize1,
                    valueBysize2,category_sum)
    print(paste("Total exe :",Sys.time()-total_exetime_start))
    return(result)
}

get_distance <- function(p1,p2,formula = "haversine"){
    # Parameters
    # -------------
    # p1, p2: vector c(lon,lat)
    #   거리계산하고 싶은 두 지점의 좌표값
    # formula: choose formula used to calculate distance
    #
    # Returns
    # --------------
    # d: num
    #   distance between two points
    R <- 6371 # mean radius of earth
    if(formula == "haversine"){
        phi_1 <- p1[2]*pi/180
        phi_2 <- p2[2]*pi/180
        delta_pi <- phi_2 - phi_1
        delta_lambda <- (p2[1]*pi/180 - p1[1]*pi/180)
        
        a <- sin(delta_pi/2)^2 + cos(phi_1)*cos(phi_2)*(sin(delta_lambda/2)^2)
        c <- 2*atan2(sqrt(a),sqrt(1-a))
        return(R*c)
    }
}
