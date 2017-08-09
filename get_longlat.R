library(httr)
library(jsonlite)
###############################################################################
## make_full_address => make address column and returns dataframe
## get_geocode => returns longitude and latitude using naver map api
###############################################################################


clientID <- read.csv('naver_api.txt',header=FALSE,stringsAsFactor=FALSE)[2,1]
clientSecret <-read.csv('naver_api.txt',header=FALSE,
                        stringsAsFactor=FALSE)[2,2]

make_full_address <- function(df){
    ## Parameter = dataframe (includes 시군구명,읍면동명,리명,본번,부번)
    df$address <- paste(df[,9],df[,10],df[,11],df[,12],"-",df[,13])
    df$address <- gsub("NA","",df$address)
    df$address <- gsub(" - 0","",df$address)
    return(df)
}

get_geocode <- function(adr){
    ## adr is vector of addresses
    n <- length(adr)
    api <- 'https://openapi.naver.com/v1/map/geocode'
    lonlat <- data.frame()
    for(i in 1:n){
        adr_i <- adr[i]
        adr_i <- URLencode(adr_i)
        request_url <- paste(api,"?query=",adr_i,sep="")
        geo_json <- GET(request_url,
                        add_headers('X-Naver-Client-Id'=clientID,
                                    'X-Naver-Client-Secret'=clientSecret))
        if(geo_json$status_code == 200){
            geo <- fromJSON(toJSON(content(geo_json)))
            lonlat <- rbind(lonlat,geo$result$items$point)
        } else if(geo_json$status_code == 404){
            ## No results
            lonlat <- rbind(lonlat,c(x=999,y=999))
        } else if(geo_json$status_code == 400){
            ## Incorrect query request
            lonlat <- rbind(lonlat,c(x=888,y=888))
        } else{
            ## Other errors
            lonlat <- rbind(lonlat,c(x=777,y=777))
        }
    }
    return(lonlat)
}
