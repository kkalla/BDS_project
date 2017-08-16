###############################################################################
## kakao api를 이용하는 함수들 모음입니다.
## 사용전에 api key를 확인해주세요.
###############################################################################
# 검색에서 사용하는 카테고리목록
# code	의미
# ------------------------
# MT1	대형마트
# CS2	편의점
# PS3	어린이집, 유치원
# SC4	학교
# AC5	학원
# PK6	주차장
# OL7	주유소, 충전소
# SW8	지하철역
# BK9	은행
# CT1	문화시설
# AG2	중개업소
# PO3	공공기관
# AT4	관광명소
# AD5	숙박
# FD6	음식점
# CE7	카페
# HP8	병원
# PM9	약국
# ---------------------------
# Error code
# 상태 코드	설명	비고
# 200	성공	응답 바디(Response Body)의 경우 각 API별로 틀릴 수 있음
# 400	실패	일반적인 오류. 주로 API에 필요한 필수 파라미터와 관련
# 401	실패	인증 오류. 주로 사용자 토큰과 관련
# 403	실패	권한/퍼미션등의 오류
# 500	실패	시스템 오류
# 502	실패	시스템 오류
# 503	실패	서비스 점검중
###############################################################################
library(httr)
library(jsonlite)

categories = c("MT1","CS2","PS3","SC4", "AC5","PK6","OL7","SW8","BK9","CT1",
               "AG2","PO3","AT4","AD5","FD6","CE7","HP8","PM9")
api_key <- read.csv("kakao_api_key.txt",skip = 1,header = FALSE,
                    stringsAsFactors = FALSE)[2,1]
url <- "https://dapi.kakao.com/v2/local/search/category.json"

nearBySearch <- function(category_group_code=
                             c("MT1", "CS2","PS3","SC4", "AC5","PK6","OL7",
                               "SW8","BK9","CT1","AG2","PO3","AT4","AD5",
                               "FD6","CE7","HP8","PM9")[15], x, y, radius=5000,
                         page=1,size=15,sort=c("distance","accuracy")[2]){
    # Parameters
    # ---------------
    # category_group_code(Required) => categories참조(default = "FD6")
    # x, y(required) => longitude and latitude
    # radius(required) => Distance within which to return place information
    #                   (default = 5000, range = 0 ~ 20000)
    # page => result page number (default = 1, range = 1~45)
    # size => number of documents in a page (default = 15, range = 1~15)
    # sort => specifies the order in which result is listed
    #       (default = accuracy)
    # Returns
    # --------------
    
    results_ <- list()
    ## Check validation of parameters
    if(length(category_group_code)>1){
        return(print("Error: Two or more category group are selected!"))
    }else if(!category_group_code %in% categories){
        return(print("Error: Invalid category"))
    }else{
        category <- paste0("?category_group_code=",category_group_code)
    }
    
    coord <- paste0("&x=",x,"&y=",y)
    
    if(radius <0 || radius>20000||!is.numeric(radius)){
        return(print("Error: Invalid radius"))
    }else{
        radius <- paste0("&radius=",radius)
    }
    
    if(page<1 || page>45 || !is.numeric(page)){
        return(print("Error: Invalid page"))
    }else{
        page <- paste0("&page=",page)
    }
    
    if(size<1 || size>15 || !is.numeric(size)){
        return(print("Error: Invalid size"))
    }else{
        size <- paste0("&size=",size)
    }
    
    sort <- paste0("&sort=",sort)
    
    ## make request url
    request_url <- paste0(url,category,coord,radius,page,size,sort)
    ## get result
    result <- get_result(request_url)
    results_[[1]] <- result
    total_count <- result$meta$total_count
    page_num <- round(total_count/15,0)
    if(page_num>3){
        for(i in 2:3){
            page <- paste0("&page=",i)
            request_url <- paste0(url,category,coord,radius,page,size,sort)
            results_[[i]] <- get_result(request_url)
        }
    }else{
        for(i in 2:page_num){
            page <- paste0("&page=",i)
            request_url <- paste0(url,category,coord,radius,page,size,sort)
            results_[[i]] <- get_result(request_url)
        }
    }
    return(results_)
}

get_result <- function(request_url){
    # Parameters
    # -----------
    # request_url = url for requesting response to kakao api
    # 
    # Returns
    # if Error occurs -> print error code
    # else -> returns result as list
    response <- GET(request_url,
                    add_headers(Authorization=paste("KakaoAK",api_key)))
    ##Check error and save result
    if(response$status_code == 200){
        # save result of page1
        result <- fromJSON(toJSON(content(response)))
        return(result)
        
    }else{
        return(print(c("Error : status_code = ",response$status_code)))
    }
}

get_total_count <- function(category_group_code=
                                c("MT1", "CS2","PS3","SC4","AC5","PK6","OL7",
                                  "SW8","BK9","CT1","AG2","PO3","AT4","AD5",
                                  "FD6","CE7","HP8","PM9")[16], x, y, 
                            radius=5000,sort=c("distance","accuracy")[2]){
    request_url <- paste0(url,
                          "?category_group_code=",category_group_code,
                          "&x=",x,"&y=",y,"&radius=",radius,
                          "&sort=",sort)
    result <- get_result(request_url)
    return(result$meta$total_count)
}