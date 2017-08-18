###############################################################################
## 텍스트 파일에서 정보추출하기 위한 소스
## Import 할때 시간이 조금 걸립니다.(텍스트파일 reading때문에)
## working directory -- Data -- SGG_CD.txt
##                            |                      
##                           -- APMM_NV_LAND -- APMM_NV_LAND.txt
## 위 두개의 txt 파일이 필요함.
## downloaded here : 
###############################################################################

## 공공기관데이터 -> address, longitude, latitude
## SGG_CD.txt -> 시군구 code
## APMM_NV_LAND.txt -> colnames in xlsx file

## sample data = Data/public_fc/gov/gg_gov.csv
# file_path = "Data/SW_COUNT/5.sports_facility_SW_count.csv"
#data <- read.csv(file_path,stringsAsFactors = F)
get_name_addr <- function(path="Data/public_fc/gov/gg_gov.csv"){
    # dataset에서 장소이름과 주소만 반환
    # Param
    # -----
    # file-path
    #
    # Returns
    # -------
    # dataframe
    #   with name and address columns
    data <- read.csv(path,stringsAsFactors = FALSE)
    data <- subset(data,select=c(name,address))
    return(data)
}

make_sgg <- function(data){
    # 주소에서 시군구만 선택해 character로 바꿔줍니다.
    # Param
    #-------
    # dataframe: address column이 있어야함
    #
    # Returns
    # --------
    # character vector: 본번 부번이 빠진 
    
    addr <- strsplit(data$address," ")    
    c_lists_ <- lapply(addr,function(x) {
        result <- c()
        for(i in 1:length(x)){
            if(!grepl("[0-9]|[(]|[)]",x[i])){
                result <- paste(result,x[i])
            }
        }
        return(result)
    })
    return(trimws(unlist(c_lists_),"left"))
}

make_bobubn <- function(data){
    # 주소에서 본번과 부번 반환
    # Param
    # ------
    # dataframe: address column이 있어야함
    #
    # Returns
    #--------
    # list: 본번 부번이 있는 list / 번지가 없으면 "번지없음"으로 표시
    
    addr <- strsplit(data$address," ")
    
    
    bobubn <- lapply(addr,function(x){
        result <- c()
        for(i in 1:length(x)){
            if(grepl("^[0-9]+(-)?[0-9]+번지$",x[i]))
                result <- paste(result,x[i])
        }
        return(result)
    })
    bobubn <- lapply(bobubn,function(x){
        if(is.null(x))
            x <- "None"
        else{
            x <- unlist(strsplit(x,"번지"))
            x <- unlist(strsplit(x,"-"))
        }
    })
    # bobubn <- lapply(bobubn,function(x){
    #     if(is.null(x))
    #         x<-"None"
    #     else
    #         x<-trimws(x,"left")})
    # bobubn <- unlist(bobubn)
    # result <- list()
    # for(i in 1:length(bobubn)){
    #     if(bobubn[i] != "None"){
    #         a <- bobubn[i]
    #         a <- substr(a,1,(stringr::str_length(a)-2))
    #         a <- strsplit(a,"-")
    #     }
    #     else{
    #         a <- "None"
    #     }
    #     result[[i]] <- a
    # }
    return(bobubn)
}

sgg_cd <- read.csv('Data/SGG_CD.txt',sep='\t',stringsAsFactors = FALSE)
colnames(sgg_cd) <- c('sgg_cd','address','isExist')
get_sgg_code <- function(sgg){
    # 시군구명을 입력하면 코드로 반환
    # Param
    # ------
    # character: address
    #
    # Returns
    # -------
    # sgg code: int / 0: error
    if(sgg %in% sgg_cd$address)
        return(sgg_cd[sgg == sgg_cd$address,]$sgg_cd)
    else
        return(0)
}
apmm_nv_land <- read.csv('Data/APMM_NV_LAND/APMM_NV_LAND.txt',sep = '|',
                         stringsAsFactors = FALSE)
get_variables <- function(file_path,
                          variable_names_=c('PNU','PNILP','PAREA','JIMOK',
                                            'SPFC1','SPFC2','LAND_USE','GEO_HL',
                                            'GEO_FORM','ROAD_SIDE')){
    # Returns dataframe with variables to analyze
    # Params
    # ------
    # file_path
    # variable_names_
    #
    # Returns
    # -------
    # dataframe
    origin_data <- read.csv(file_path,stringsAsFactors = FALSE)
    data <- get_name_addr(file_path)
    sggs_ <- make_sgg(data)
    bobubns_ <- make_bobubn(data)
    sgg_codes_ <- c()
    # It takes some time
    for(i in 1:length(sggs_)){
        sgg <- get_sgg_code(sggs_[i])
        sgg_codes_[i] <- sgg
    }
    pnus_ <- make_pnu(sgg_codes_,bobubns_)
    
    result_ <- data.frame(row.names = FALSE)
    # Takes some time
    for(i in 1:nrow(pnus_)){
        vars <- c()
        if(pnus_$sub1[i]!="0"){
            j <- 0
            while(j < 10){
                my_pnu <- paste0(pnus_$sub1[i],j,pnus_$sub2[i],pnus_$sub3[i])
                vars <- apmm_nv_land[apmm_nv_land$PNU==my_pnu &
                                         apmm_nv_land$PNILP > 0,variable_names_]
                if(nrow(vars)!=0)
                    break
                j <- j + 1
            }
            if(nrow(vars)==0){
                vars <- rep("0",length(variable_names_))
                vars <- data.frame(t(vars),stringsAsFactors = FALSE)
                colnames(vars)<-variable_names_
            }
            ?data.frame
        }else{
            vars <- rep("0",length(variable_names_))
            vars <- data.frame(t(vars),stringsAsFactors = FALSE)
            colnames(vars)<-variable_names_
        }
        result_ <- rbind(result_,vars)
    }
    if(nrow(result_)>nrow(origin_data)){
        n <- nrow(origin_data)
    }else
        n <- nrow(result_)
    result_ <- result_[1:n,]
    origin_data <- origin_data[1:n,]
    result_ <- cbind(origin_data[,c("category","address","longitude","latitude",
                                    "SW8","dist_nearestRV")],result_)
    return(result_)
}

make_pnu <- function(sgg_codes_,bobubns_){
    sub1 <- as.character(sgg_codes_)
    sub2 <- c()
    sub3 <- c()
    for(i in 1:length(bobubns_)){
        if(length(bobubns_[[i]]) > 1){
            aa <- trimws(bobubns_[[i]][1],"left")
            bb <- bobubns_[[i]][2]
            if(stringr::str_length(aa)<4){
                zeros <- stringr::str_dup("0",4-stringr::str_length(aa))
                aa <- paste(paste0(zeros),aa,sep="")
            }
            if(stringr::str_length(bb)<4){
                zeros <- stringr::str_dup("0",4-stringr::str_length(bb))
                bb <-paste(zeros,bb,sep="")
            }
        }else if(bobubns_[[i]]=="None"){
            aa <- "0000";bb<-"0000"
        }else{
            aa <- trimws(bobubns_[[i]],"left")
            if(stringr::str_length(aa)<4){
                zeros <- stringr::str_dup("0",4-stringr::str_length(aa))
                aa <- paste(zeros,aa,sep="")
            }
            bb <- "0000"
        }
        sub2 <- c(sub2,aa)
        sub3 <- c(sub3,bb)
    }
    n <- length(sub1)
    sub2 <- sub2[1:n]
    sub3 <- sub3[1:n]
    result <- data.frame(sub1,sub2,sub3,stringsAsFactors = FALSE)
    return(result)
}

