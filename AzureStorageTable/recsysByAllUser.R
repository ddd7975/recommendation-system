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

datMer <- cbind(dat, merchandise)
# merNA <- which(is.na(merchandise) == T)
# datMer <- filter(datMer, UserID != "00000000-0000-0000-0000-000000000000")
UserClick <- select(datMer, UserID, merchandise, RequestTime)

UserClick <- UserClick[which(UserClick$merchandise %in% dataInfo_id[, 1]), ]

uniUserID <- unique(UserClick$UserID)
uniModel <- unique(UserClick$merchandise)
tmp <- UserClick[sort.list(UserClick$UserID), ]
nameLength <- as.numeric((table(as.character(tmp[, 1]))))
t1 <- proc.time()
itemByUser <-split(tmp$merchandise, rep(1:length(uniUserID), nameLength))
t2 <- proc.time()
t2 - t1
names(itemByUser) <- sort.list(uniUserID)


# which(lapply(1:length(uniUserID), function(i)merchandise[1] %in% itemByUser[[i]]) == T)
# t2 <- proc.time()
# t2 - t1
t1 <- proc.time()
traMatrix <- matrix(0, nrow = length(uniUserID), ncol = length(uniModel))
for (u in 1:length(uniUserID)){
  print(u/length(uniUserID))
  zeroVec <- rep(0, length(uniModel))
  traMatrix[u, which(uniModel %in% as.character(itemByUser[[u]]))] <- 1
}
t2 <- proc.time()
t2 - t1

#
# correlation matrix
#
corrMatrix <- matrix(0, nrow = length(uniModel), ncol = length(uniModel))
rownames(corrMatrix) <- colnames(corrMatrix) <- uniModel

g1 = proc.time()
for (a in 1:(length(uniModel) - 1)){
  print(a/length(uniModel))
  for (b in (a + 1):length(uniModel)){
    #         corrMatrix[a, b] <- cor(traMatrix[, a], traMatrix[, b])
    tmp <- which(traMatrix[, a] != 0 & traMatrix[, b] != 0)
    if (length(tmp) != 0){
      corrMatrix[a, b] <- 1/(1 + sum((traMatrix[tmp, a] - traMatrix[tmp, b])^2))
    }
  }
}
g2 = proc.time()
g2 - g1

#
# 
#
g3 = proc.time()
tmpv <- 1:length(uniModel)
for (v in 1:length(uniUserID)){
  print(v/length(uniUserID))
  vec <- traMatrix[v, ]
  nonzero <- which(vec != 0)
  for (w in tmpv[-nonzero]){
    sij <- rep(0, length(nonzero))
    for (z in 1:length(nonzero)){
      if (nonzero[z] > w){
        sij[z] <- corrMatrix[w, nonzero[z]]
      }else{
        sij[z] <- corrMatrix[nonzero[z], w]
      }
    }
    if (sum(sij) != 0){
      score <- sum(sij*vec[nonzero])/sum(sij)
      traMatrix[v, w] <- score
    }
    
  }
}
g4 = proc.time()
g4 - g3
#
#
#
g5 = proc.time()
recListByUser <- matrix(0, nrow = length(uniUserID), ncol = 2)
recListByUser[, 1] <- as.character(uniUserID)
for (r in 1:length(uniUserID)){
  print(r/length(uniUserID))
  uniModel[sort(traMatrix[r, ], index.return=TRUE, decreasing = T)$ix[1:10]]
  #   sort(rank(traMatrix[r, ], ties.method = "random"), decreasing = T, index.return=TRUE)
  recItem <- uniModel[sort(rank(traMatrix[r, ], ties.method = "random"), decreasing = T, index.return=TRUE)$ix][1:10]
  recListByUser[r, 2] <- paste(recItem, collapse = ",")
}
g6 = proc.time()
g6 - g5
value <- recListByUser[, 2]
recListByUser <- cbind(recListByUser, value)
recListByUser[, 2] <- "blank"
write.csv(recListByUser, "C:\\Users\\David79.Tseng\\Dropbox\\David79.Tseng\\advantechProject\\Recommendation_CF\\recListByUser.csv", row.names = FALSE)
