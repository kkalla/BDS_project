library(dplyr)

suwon_r1000 <- read.csv('Data/suwon_clustered.csv',stringsAsFactors = F)
suwon_r500 <- read.csv('Data/suwon_r500_clustered.csv',stringsAsFactors = F)
thunder_api_key <- read.csv('Data/api_keys.txt',
                            stringsAsFactors = F)$thunderforest

## Data pre-processing

suwon_pre <- suwon_pbassets %>% 
    select("Asset_ID"=2,"Land_size"=10,"Land_value"=11,"Address"=address,
           longitude,latitude,"Land_Value_bySize"=valueBysize1,27:44)

suwon_pre <- suwon_pre[suwon_pre$Land_Value_bySize != Inf,]
