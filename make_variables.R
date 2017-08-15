###############################################################################
## 텍스트 파일에서 정보추출하기 위한 소스
###############################################################################

## 공공기관데이터 -> address, longitude, latitude
## SGG_CD.txt -> 시군구 code
## APMM_NV_LAND.txt -> colnames in xlsx file

## sample data = Data/public_fc/gov/gg_gov.csv
file_path = "Data/public_fc/gov/gg_gov.csv"
data <- get_name_addr(path="Data/public_fc/gov/gg_gov.csv")
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
    # character vactor: 본번 부번이 빠진 
    
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
get_variables <- function(file_path){
    #분석에 필요한 variables 반환
    data <- get_name_addr(file_path)
    sggs_ <- make_sgg(data)
    bobubns_ <- make_bobubn(data)
    sgg_codes_ <- c()
    # 이게 왜 오래걸리지?
    for(i in 1:length(sggs_)){
        sgg <- get_sgg_code(sggs_[i])
        sgg_codes_[i] <- sgg
    }
    pnus_ <- make_pnu(sgg_codes_,bobubns_)
    variable_names_ <- c('PNILP','PAREA','JIMOK','SPFC1','SPFC2','LAND_USE',
                         'GEO_HL','GEO_FORM','ROAD_SIDE')
    result_ <- data.frame()
    
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
    result <- data.frame(sub1,sub2,sub3,stringsAsFactors = FALSE)
    return(result)
}
aa <- make_pnu(sgg_codes_,bobubns_)
