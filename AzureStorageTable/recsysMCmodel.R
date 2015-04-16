library(RODBC)
conn <- odbcConnect(dsn = "ACLSQL7_SQL2008R2", uid = "twWebApp", pwd = "twweb")
dat <- sqlQuery(conn, "SELECT TOP 50000 [UserID], [URL], [RequestTime] 
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

#method2 
uniUserID <- unique(UserClick$UserID)
tmp <- UserClick[sort.list(UserClick$UserID), ]
nameLength <- as.numeric((table(as.character(tmp[, 1]))))
t1 <- proc.time()
itemByUser <-split(tmp$merchandise, rep(1:length(uniUserID), nameLength))
t2 <- proc.time()
t2 - t1
names(itemByUser) <- sort.list(uniUserID)
# #--- every userID's transaction data
# uniUserID <- unique(UserClick$UserID)
# datByUser <- list()
# datByUser <- lapply(1:length(uniUserID), function(i){
#   print(i/length(uniUserID))
#   filter(UserClick, UserID == uniUserID[i])
# })
# names(datByUser) <- uniUserID

##
clickstreams_1 <- 0
clickstreams_2 <- 0
for (i in 1:length(itemByUser)){  
  print(i/length(itemByUser))
  #tmp <- c(as.character(datByUser[[i]][1, 1]), as.character(datByUser[[i]][, 2]))
  tmp <- itemByUser[[i]]
  if (length(tmp) < 3){
    clickstreams_1[i] <- 0  
  }else{
    clickstreams_1[i] <- paste(tmp, collapse = ",")  
  }
  
  if (length(tmp) < 4){
    clickstreams_2[i] <- 0  
  }else{
    clickstreams_2[i] <- paste(tmp, collapse = ",")  
  }
}
clickstreams_1 <- clickstreams_1[which(clickstreams_1 != "0")]
clickstreams_2 <- clickstreams_2[which(clickstreams_2 != "0")]

## order = 1
csf_1 <- tempfile()
writeLines(clickstreams_1, csf_1)
start <- proc.time()[3]
cls_1 <- readClickstreams(csf_1, header=TRUE)
temp <- lapply(1:length(cls_1), function(r)length(cls_1[[r]]))
cls_1[which(unlist(temp) < 2)] <- NULL
end <- proc.time()[3]
end - start
start2 <- proc.time()[3]
mc_1 <- fitMarkovChain(cls_1)
end2 <- proc.time()[3]
end2 - start2

slotnames_1 <- slotNames(mc_1) 
slotlist_1 <- vector("list", length(slotnames_1)) 
names(slotlist_1) <- slotnames_1
for(i in slotnames_1) slotlist_1[[i]] <- slot(mc_1, i) 
probMatrix_1 <- slotlist_1$transitions$`1`

## order = 2
csf_2 <- tempfile()
writeLines(clickstreams_2, csf_2)
cls_2 <- readClickstreams(csf_2, header=TRUE)
temp <- lapply(1:length(cls_2), function(r)length(cls_2[[r]]))
cls_2[which(unlist(temp) < 5)] <- NULL

mc_2 <- fitMarkovChain(cls_2, order = 2)

slotnames_2 <- slotNames(mc_2) 
slotlist_2 <- vector("list", length(slotnames_2)) 
names(slotlist_2) <- slotnames_2
for(i in slotnames_2) slotlist_2[[i]] <- slot(mc_2, i) 
probMatrix_2 <- slotlist_2$transitions$`1`

# calculate the most clicked minimumLayer
hotClick <- sort(table(UserClick$merchandise), decreasing = T)

##
# names(datByUser)
topmin <- 5; numOfModel <- 10

proEarly <- "5DDD04C5-6A43-4351-A91D-6A4A1DAFF272"; proLate <- "8DCEE4B7-FBDE-4C5D-9752-ED2F2FBD00BF"
proEarly <- "8DCEE4B7-FBDE-4C5D-9752-ED2F2FBD00BF"; proLate <- "F5C0AF37-6966-42B6-9BE9-5AEB2445D945"
proEarly <- "8DCEE4B7-FBDE-4C5D-9752-ED2F2FBD00BF"; proLate <- "44434F5A-C264-41B2-844C-A3FC63082E5A"
proEarly <- "700FA860-868E-4F1E-A7DD-408B0467E4FC"; proLate <- "FF8534CC-E858-40BA-9BC2-386F19BFEE4A"
proEarly <- "EE7D611E-AAE2-4943-B40A-BAE1BDCA15C7"; proLate <- "B009C4B4-4B7C-4736-B16F-241978245E6A"
proEarly <- dataInfo_id[1, 1]; proLate <- dataInfo_id[7, 1] #只要後面的state沒有在資料裡面被點過，就沒有推薦清單
proEarly <- dataInfo_id[3, 1]; proLate <- dataInfo_id[7, 1]
proEarly <- dataInfo_id[1, 1]; proLate <- dataInfo_id[3, 1]
proEarly <- dataInfo_id[3, 1]; proLate <- dataInfo_id[11, 1] # 後一個狀態在資料中有被點擊過，但資料大於零的product數量少(少於10個)

proEarly <- dataInfo_id[102, 1]; proLate <- dataInfo_id[546, 1]
proEarly <- dataInfo_id[1, 1]; proLate <- dataInfo_id[4, 1]
proEarly <- dataInfo_id[i, 1]; proLate <- dataInfo_id[j, 1]

recsysList <- function(proEarly, proLate, user = NULL, numOfModel = 10){
  clicked <- c(proEarly, proLate)
  if (length(which(clicked == "")) > 0){
    clicked <- clicked[-which(clicked == "")]
  }    
  
  lastClick <- clicked[length(clicked)]
  

  if (length(clicked) > 1){
    whichCol <- probMatrix_2[, which(colnames(probMatrix_2) == lastClick)]
    tmpMat <- as.matrix(whichCol[which(as.numeric(whichCol) > 0)])
  }else{
    whichCol <- probMatrix_1[, which(colnames(probMatrix_1) == lastClick)]
    tmpMat <- as.matrix(whichCol[which(as.numeric(whichCol) > 0)])
  }
  
  if (nrow(tmpMat) > 0){
    if (nrow(tmpMat) >= numOfModel){
      recList <- names(sort(tmpMat[, 1], decreasing = T))[1:numOfModel]
    }else{
      recListCandidate1 <- names(sort(tmpMat[, 1], decreasing = T))[1:nrow(tmpMat)]
      miniEarly <- ml[which(proEarly == dataInfo_id, arr.ind = T)[1, 1]]
      miniLate <- ml[which(proLate == dataInfo_id, arr.ind = T)[1, 1]]
      
      belongMiniMod_early <- dataInfo_id[which(miniEarly == dataInfo_id, arr.ind = T)[, 1], 1]
      belongMiniMod_late <- dataInfo_id[which(miniLate == dataInfo_id, arr.ind = T)[, 1], 1]
      
      recCandidate1 <- belongMiniMod_early[which(belongMiniMod_early %in% names(hotClick))]
      recCandidate2 <- belongMiniMod_late[which(belongMiniMod_late %in% names(hotClick))]
      recCandidate <- c(recCandidate1, recCandidate2)
      recList <- sample(recCandidate, size = min(numOfModel, length(recCandidate)), replace = F)
      if (length(recList) < numOfModel){
        recListAlter <- c(sample(c(belongMiniMod_early, belongMiniMod_late), numOfModel - length(recList), replace = T))
        recList <- unique(c(recList, recListAlter))
      }
    }
  }else{
    # late state wasn't be clicked in the data. nrow(tmpMat) will be zero.
    miniEarly <- ml[which(proEarly == dataInfo_id, arr.ind = T)[1, 1]]
    miniLate <- ml[which(proLate == dataInfo_id, arr.ind = T)[1, 1]]
    
    belongMiniMod_early <- dataInfo_id[which(miniEarly == dataInfo_id, arr.ind = T)[, 1], 1]
    belongMiniMod_late <- dataInfo_id[which(miniLate == dataInfo_id, arr.ind = T)[, 1], 1]
    
    recCandidate1 <- belongMiniMod_early[which(belongMiniMod_early %in% names(hotClick))]
    recCandidate2 <- belongMiniMod_late[which(belongMiniMod_late %in% names(hotClick))]
    recCandidate <- c(recCandidate1, recCandidate2)
    recList <- sample(recCandidate, size = min(numOfModel, length(recCandidate)), replace = F)
    if (length(recList) < numOfModel){
      recListAlter <- c(sample(c(belongMiniMod_early, belongMiniMod_late), numOfModel - length(recList), replace = T))
      recList <- unique(c(recList, recListAlter))
    }
  }
  return(recList)
}

time1 <- proc.time()
recsysList(proEarly, proLate)
recsysList(proEarly, "")
recsysList(proLate, "")
time2 <- proc.time()
time2 - time1

time1 <- proc.time()
uniMod <- unique(dataInfo_id[, 1])
outTab <- matrix(0, nrow = length(uniMod)^2 + length(uniMod), ncol = 2)
storageTable <- matrix(0, nrow = length(uniMod)^2 + length(uniMod), ncol = 1)
col1 <- c(rep(uniMod, length(uniMod)), rep("", length(uniMod)))
col2 <- c(rep(uniMod, each = length(uniMod)), uniMod)
outTab[, 1] <- col1; outTab[, 2] <- col2
key <- apply(outTab, MARGIN = 1, paste, collapse = ";")
for (i in 1:nrow(outTab)){
  print(i/nrow(outTab))
  early <- outTab[i, 1]; late <- outTab[i, 2]
  storageTable[i, 1] <- paste(recsysList(early, late), collapse = ";")
}
# colnames(outTab) <- c("firstStep", "secondStep", "recList")
colnames(storageTable) <- c("recList")
rownames(storageTable) <- key
# uniMod <- unique(dataInfo_id[, 1])
# outTab <- matrix(0, nrow = length(uniMod), ncol = length(uniMod))
# outTab <- matrix(0, nrow = length(uniMod)^2 + length(uniMod), ncol = 3)
# outTab <- matrix(0, nrow = length(uniMod)^2 + length(uniMod), ncol = 1)
# col1 <- c(rep(uniMod, length(uniMod)), rep("", length(uniMod)))
# col2 <- c(rep(uniMod, each = length(uniMod)), uniMod)
# allCom <- expand.grid(uniMod, uniMod)
# key1 <- apply(allCom, MARGIN = 1, paste, collapse = ";")
# key2 <- paste(rep("", length(uniMod)), )
# outTab[, 1] <- col1; outTab[, 2] <- col2
# for (i in 1:nrow(outTab)){
#   print(i/nrow(outTab))
#   early <- outTab[i, 1]; late <- outTab[i, 2]
#   outTab[i, 3] <- paste(recsysList(early, late), collapse = ";")
# }
# colnames(outTab) <- c("firstStep", "secondStep", "recList")
# for (i in 1:length(uniMod)){
#   print(i/length(uniMod))
#   early <- uniMod[i]
#   for (j in 1:length(uniMod)){
#     #     print(j)
#     late <- uniMod[j]
#     outTab[i, j] <- paste(recsysList(early, late), collapse = ";")
#   }
# }
# rownames(outTab) <- colnames(outTab) <- uniMod
time2 <- proc.time()
time2 - time1

write.csv(outTab, "C:\\Users\\David79.Tseng\\Desktop\\recomListMod201502.csv")
