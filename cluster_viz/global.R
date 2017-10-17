library(dplyr)

suwon_r1000 <- read.csv('Data/suwon_clustered.csv',stringsAsFactors = F)
suwon_r500 <- read.csv('Data/suwon_r500_clustered.csv',stringsAsFactors = F)
pbfc <- read.csv('Data/tot_publicfc.csv')
thunder_api_key <- read.csv('Data/api_keys.txt',
                            stringsAsFactors = F)$thunderforest
suwon_all <- read.csv('Data/suwon_all.csv',stringsAsFactors = F)
suwon_cat_2 <- read.csv('Data/suwon_cat_2.csv',stringsAsFactors = F)
## Data preprocessing
df <- suwon_r500
df$Mart <- df$MT1 + df$CS2
df$Education <- df$PS3 + df$SC4 + df$AC5
df$Parking_lot <- df$PK6
df$Gas_station <- df$OL7
df$Subway <- df$SW8
df$Bank <- df$BK9
df$Public_office <- df$PO3
df$Culture <- df$CT1
df$Travel <- df$AT4 + df$AD5
df$Food_cafe <- df$FD6 + df$CE7
df$Hospital_Pham <- df$HP8 + df$PM9
# str(df)
suwon_r500<- df %>% select(-MT1,-CS2,-PS3,-SC4, -AC5,-PK6,-OL7,-SW8,-BK9,-CT1,
                           -AG2,-PO3,-AT4,-AD5,-FD6,-CE7,-HP8,-PM9)
# str(suwon_r500)
df <- suwon_r1000
df$Mart <- df$MT1 + df$CS2
df$Education <- df$PS3 + df$SC4 + df$AC5
df$Parking_lot <- df$PK6
df$Gas_station <- df$OL7
df$Subway <- df$SW8
df$Bank <- df$BK9
df$Public_office <- df$PO3
df$Culture <- df$CT1
df$Travel <- df$AT4 + df$AD5
df$Food_cafe <- df$FD6 + df$CE7
df$Hospital_Pham <- df$HP8 + df$PM9
# str(df)
suwon_r1000<- df %>% select(-MT1,-CS2,-PS3,-SC4, -AC5,-PK6,-OL7,-SW8,-BK9,-CT1,
                           -AG2,-PO3,-AT4,-AD5,-FD6,-CE7,-HP8,-PM9)
# str(suwon_r1000)

df <- pbfc
df$Mart <- df$MT1 + df$CS2
df$Education <- df$PS3 + df$SC4 + df$AC5
df$Parking_lot <- df$PK6
df$Gas_station <- df$OL7
df$Subway <- df$SW8
df$Bank <- df$BK9
df$Public_office <- df$PO3
df$Culture <- df$CT1
df$Travel <- df$AT4 + df$AD5
df$Food_cafe <- df$FD6 + df$CE7
df$Hospital_Pham <- df$HP8 + df$PM9
# str(df)
pbfc<- df %>% select(-MT1,-CS2,-PS3,-SC4, -AC5,-PK6,-OL7,-SW8,-BK9,-CT1,
                            -AG2,-PO3,-AT4,-AD5,-FD6,-CE7,-HP8,-PM9)

pbfc_summary <- pbfc %>% select(1,5,6,14:24) %>% group_by(category) %>%
    summarize(avg_mart = mean(Mart),avg_edu = mean(Education),
              avg_pk = mean(Parking_lot),avg_gs=mean(Gas_station),avg_sw=mean(Subway),
              avg_bk = mean(Bank),avg_ct = mean(Culture),avg_tv = mean(Travel),
              avg_fc = mean(Food_cafe),avg_hp = mean(Hospital_Pham))
# str(pbfc_summary)
