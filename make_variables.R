###############################################################################
## 텍스트 파일에서 정보추출하기 위한 소스
###############################################################################

## 공공기관데이터 -> address, longitude, latitude
## SGG_CD.txt -> 시군구 code
## APMM_NV_LAND.txt -> colnames in xlsx file

## sample data = Data/public_fc/gov/gg_gov.csv

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
            x<-"None"
        else
            x<-trimws(x,"left")})
    bobubn <- unlist(bobubn)
    result <- list()
    for(i in 1:length(bobubn)){
        if(bobubn[i] != "None"){
            a <- bobubn[i]
            a <- substr(a,1,(stringr::str_length(a)-2))
            a <- strsplit(a,"-")
        }
        else{
            a <- "None"
        }
        result[[i]] <- a
    }
    return(result)
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
    variable_names_ <- c('PNILP','PAREA','JIMOK','SPFC1','SPFC2','LAND_USE',
                         'GEO_HL','GEO_FORM','ROAD_SIDE')
    result_ <- data.frame()
    for(i in 1:length(sgg_codes_)){
        if(bobubns_[[i]] != "None"){
            bobn <- as.numeric(unlist(bobubns_[[i]][1]))
            if(length(bobubns_[[i]])>1){
                bubn <- as.numeric(unlist(bobubns_[[i]][2]))
                result <- apmm_nv_land[
                    sgg_codes_[i]==as.numeric(substr(apmm_nv_land$PNU,1,10)) & 
                        bobn == as.numeric(apmm_nv_land$BOBN) & 
                        bubn == as.numeric(apmm_nv_land$BUBN),variable_names_]
            }
            else{
                result <- apmm_nv_land[
                    sgg_codes_[i]==as.numeric(substr(apmm_nv_land$PNU,1,10)) & 
                        bobn == as.numeric(apmm_nv_land$BOBN),variable_names_]
            } 
        }else{
            result <- rep(0,length(variable_names_))
        }
        result_ <- rbind(result_,result)
        
    }
    return(result_)
}
make_pnu <- function(sgg_codes_,bobubns_){
    sub1 <- c()
    sub2 <- c()
    for(i in 1:length(sgg_codes_)){
        sub1 <- c(sub1,sgg_codes_)
    }
    for(i in 1:length(bobubns_)){
        if(bobubns_[[i]] != "None"){
            if(length(bobubns_[[i]][1])>1){
                bb <- unlist(bobubns_[[i]][1])
                if(stringr::str_length(bb[1])<4)
                    zeros <- as.character(rep(0,(4-stringr::str_length(bb[1]))))
                bb[1] <- paste(zeros,bb[1],sep="")
                if(stringr::str_length(bb[2])<4)
                    zeros <- as.character(rep(0,(4-stringr::str_length(bb[2]))))
                bb[2] <- paste(zeros,bb[2],sep="")
                
                aa <- paste(bb[1],bb[2],sep="")
        
            } else{
                bb <- unlist(bobubns_[[i]][1])
                if(stringr::str_length(bb)<4)
                    zeros <- as.character(rep(0,(4-stringr::str_length(bb))))
                bb <- paste(zeros,bb,sep="")
                aa <- paste(bb[1],
                                   "0000",sep="")
               
            }
        }else{
            aa <- "00000000"
        }
        sub2[i] <- aa
    }
    result <- data.frame(sub1,sub2,stringsAsFactors = FALSE)
    return(result)
}
unlist(bobubns_[[602]][1])[2]
data2 <- get_variables("Data/public_fc/gov/gg_gov.csv")
aaa <- make_pnu(sgg_codes_,bobubns_)
