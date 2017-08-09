## split datasets by land sizes

split_bysize <- function(df){
    output <- list()
    size_na <- df[is.na(df$재산면적),]
    df <- df[!is.na(df$재산면적),]
    under_300 <- df[df$재산면적 < 300,]
    under_500 <- df[df$재산면적 >=300 & df$재산면적 < 500,]
    under_1000 <- df[df$재산면적 >=500 & df$재산면적 < 1000,]
    over_1000 <- df[df$재산면적 >=1000,]
    output <- list(size_na,under_300,under_500,under_1000,over_1000)
    return(output)
}
