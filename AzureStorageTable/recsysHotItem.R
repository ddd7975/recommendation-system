library(RODBC)
conn <- odbcConnect(dsn = "ACLSQL7_SQL2008R2", uid = "twWebApp", pwd = "twweb")
dat <- sqlQuery(conn, "SELECT TOP 200000 [UserID], [URL], [RequestTime] 
                FROM [WebTracking].[dbo].[RequestLog]
                WHERE [URL] LIKE '%mod%' AND RequestTime >= '2015-02-01'")
dataInfo_id <- as.matrix(read.csv("C:\\Users\\David79.Tseng\\Dropbox\\David79.Tseng\\advantechProject\\Recommendation_CF\\dataInfo_id.csv", header=T))

library(recommenderlab)
library(dplyr)
library(reshape)
library(clickstream)
##
ml <- 0
for (j in 1:nrow(dataInfo_id)){
  #print(j/nrow(dataInfo_id))
  tmpRow <- dataInfo_id[j, ]
  blank <- as.numeric(which(tmpRow == ""))
  if (length(blank) != 0){
    ml[j] <- as.character(tmpRow[min(blank) - 1])
  }else{
    ml[j] <- as.character(tmpRow[length(tmpRow)])
  }
}


##
## split the url
##
dat <- as.data.frame(dat)
dat <- dat[-which(dat$UserID == "00000000-0000-0000-0000-000000000000"), ]
url <- as.character(select(dat, URL)[, 1])
splitURL <- function(url_i){
  tmp1 <- strsplit(url_i, "\\://|\\.|\\/")[[1]]
  #   if ("products" %in% tmp1 & "aspx" %in% tmp1){
  if (sum(grepl("products", tmp1)) > 0 & sum(grepl("aspx", tmp1)) > 0){
    num <- which(substring(tmp1, first = 1, 3) == "mod" | substring(tmp1, first = 1, 3) == "sub") 
    if (length(num) != 0){
      num <- max(num)
      mer <- toupper(substring(tmp1[num], first = 5))
    }else{
      mer <- toupper(tmp1[length(tmp1) - 2])
    }
  }else{
    mer <- NA
  }
  return(mer)
}
merchandise <- unlist(sapply(1:length(url), function(i){
  print(i/length(url))
  splitURL(url[i])
}))
merchandise[which(merchandise == "ETHERNET")] <- "EN50155_INDUSTRIAL_ETHERNET_SWITCHES"
merchandise[which(merchandise == "BACNET_REMOTE_I")] <- "BACNET_REMOTE_I/O_MODULES"
merchandise <- merchandise[-which(merchandise == "PRODUCTS")]

hotItem <- sort(table(merchandise), decreasing = T)
num <- as.character(c(0:(length(hotItem) - 1)))

outTab <- matrix(0, ncol = 3, nrow = length(hotItem))
outTab[, 1] <- num
outTab[, 2] <- "blank"
outTab[, 3] <- names(hotItem)
colnames(outTab) <- c("PartitionKey", "RowKey", "recList")

write.csv(outTab, "C:\\Users\\David79.Tseng\\Dropbox\\David79.Tseng\\advantechProject\\Recommendation_CF\\hotItemList.csv", row.names = FALSE)

