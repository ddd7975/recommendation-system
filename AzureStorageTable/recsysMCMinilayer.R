library(RODBC)
conn <- odbcConnect(dsn = "ACLSQL7_SQL2008R2", uid = "twWebApp", pwd = "twweb")
dat <- sqlQuery(conn, "SELECT TOP 50000 [UserID], [URL], [RequestTime] 
                FROM [WebTracking].[dbo].[RequestLog]
                WHERE [URL] LIKE '%sub%' AND RequestTime >= '2015-02-01'")
dataInfo_id <- as.matrix(read.csv("C:\\Users\\David79.Tseng\\Dropbox\\David79.Tseng\\advantechProject\\Recommendation_CF\\dataInfo_id.csv", header=T))

# install.packages("dplyr")
library(truncnorm)
library(dplyr)
library(clickstream)

# pick the minimum layer
ml <- 0
for (j in 1:nrow(dataInfo_id)){
  print(j)
  tmpRow <- dataInfo_id[j, ]
  blank <- as.numeric(which(tmpRow == ""))
  if (length(blank) != 0){
    ml[j] <- as.character(tmpRow[min(blank) - 1])
  }else{
    ml[j] <- as.character(tmpRow[length(tmpRow)])
  }
}
minimumLayer <- unique(ml)
###########################
##                       ##
##  Step1: Split the url ##  
##                       ##
###########################
dat <- as.data.frame(dat)
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
datMer <- filter(datMer, UserID != "00000000-0000-0000-0000-000000000000")
UserClick <- select(datMer, UserID, merchandise, RequestTime)

UserClick <- UserClick[which(UserClick$merchandise %in% minimumLayer), ]
#method2 
tmp <- UserClick[sort.list(UserClick$UserID), ]
nameLength <- as.numeric((table(as.character(tmp[, 1]))))
t1 <- proc.time()
itemByUser <-split(tmp$merchandise, rep(1:length(uniUserID), nameLength))
t2 <- proc.time()
t2 - t1
names(itemByUser) <- sort.list(uniUserID)
#超多as.character，改掉!!
ttt <- lapply(1:length(itemByUser), function(i){
  c(as.character(uniUserID)[sort.list(as.character(uniUserID))][i], as.character(itemByUser[[i]]))
})

#--- every userID's transaction data
uniUserID <- unique(UserClick$UserID)
datByUser <- list()
datByUser <- lapply(1:length(uniUserID), function(i){
  print(i/length(uniUserID))
  filter(UserClick, UserID == uniUserID[i])
})
names(datByUser) <- uniUserID
## unside down the PIS_Category_Hierarchy.csv
dataInfo_id <- as.matrix(read.csv("C:\\Users\\David79.Tseng\\Dropbox\\David79.Tseng\\advantechProject\\Recommendation_CF\\dataInfo_id.csv", 
                                  header=T))

# clickFreq <- table(merchandise)
# which(names(clickFreq) %in% dataInfo_id[, 1])
###################
###################
###################


#new
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
#old slow, use datByUser not use itemByUser
# clickstreams_1 <- 0
# clickstreams_2 <- 0
# for (i in 1:length(datByUser)){  
#   print(i/length(datByUser))
#   #   tmp <- c(as.character(datByUser[[i]][1, 1]), as.character(datByUser[[i]][, 2]))
#   tmp <- itemByUser[[i]]
#   if (length(tmp) < 3){
#     clickstreams_1[i] <- 0  
#   }else{
#     clickstreams_1[i] <- paste(tmp, collapse = ",")  
#   }
#   
#   if (length(tmp) < 4){
#     clickstreams_2[i] <- 0  
#   }else{
#     clickstreams_2[i] <- paste(tmp, collapse = ",")  
#   }
# }
# clickstreams_1 <- clickstreams_1[which(clickstreams_1 != "0")]
# clickstreams_2 <- clickstreams_2[which(clickstreams_2 != "0")]

## order = 1
csf_1 <- tempfile()
writeLines(clickstreams_1, csf_1)
start <- proc.time()[3]
cls_1 <- readClickstreams(csf_1, header=TRUE)
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
user <- as.character(datByUser[[5]][1, 1])

proEarly <- "MEMORY_MODULE"; proLate <- "1-2JKEW9"
recsysList <- function(proEarly, proLate, user = NULL, topmin = 5, numOfModel = 10){
  #   user <- as.character(datByUser[[k]][1, 1])
  #   tab <- datByUser[[user]]
  #   clicked <- unique(as.character(tab$merchandise))
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
  minLayer <- which(rownames(tmpMat) %in% minimumLayer)
  
  if (length(minLayer) != 0){
    tmpMat <- as.matrix(tmpMat[minLayer, ])
    belongList <- list()
    for (i in 1:nrow(tmpMat)){
      click_i <- rownames(tmpMat)[i]
      loc <- which(dataInfo_id == click_i, arr.ind = T)
      belongList[[i]] <- dataInfo_id[loc[, 1], 1]
    }
    names(belongList) <- rownames(tmpMat)
  }else{
    belongList <- list()
    #     for (i in 1:length(clicked)){
    #       belongList[[i]] <- dataInfo_id[which(clicked[i] == dataInfo_id, arr.ind = T)[, 1], 1]
    #     }
    #     names(belongList) <- clicked
    for (i in 1:3){
      belongList[[i]] <- dataInfo_id[which(names(hotClick)[i] == dataInfo_id, arr.ind = T)[, 1], 1]
    }
    names(belongList) <- names(hotClick)[1:3]
  }
  
  
  
  # if (lastClick %in% rownames(tmpMat)) belongList[[which(rownames(tmpMat) == lastClick)]] <- NULL
  
  ##
  ## pick 5 top value of transition probability from these minimum layer.
  ## pick 10 models from these top 5 minimum category.
  ##
  
  if (length(minLayer != 0)){
    topmin <- min(topmin, nrow(tmpMat))
    topminLayer <- tmpMat[order(tmpMat, decreasing = T)[1:topmin], 1]
    proportion <- topminLayer/sum(topminLayer, na.rm = T)
    numOfEachLayer <- round(numOfModel*proportion)
  }else{
    topmin <- min(topmin, length(belongList))
    proportion <- rep(1/length(belongList), length(belongList))
    numOfEachLayer <- round(numOfModel*proportion)
  }
  
  
  if (length(minLayer) != 0){
    belongListTop <- belongList[which(names(belongList) %in% names(topminLayer))]
  }else{
    belongListTop <- belongList
  }

  recList <- lapply(1:topmin, function(i){
    if (length(belongListTop[[i]]) > numOfEachLayer[i]){
      reclist <- as.character(sample(belongListTop[[i]], as.numeric(numOfEachLayer[i]), replace = F))
    }else{
      reclist <- as.character(belongListTop[[i]])
    }
    return(reclist)
  })
  return(unlist(recList))
}

time1 <- proc.time()
recsysList(proEarly, proLate)
time2 <- proc.time()
time2 - time1

# outTab <- matrix(0, nrow = length(minimumLayer)^2 + length(minimumLayer), ncol = 1)
# allCom <- expand.grid(minimumLayer, minimumLayer)
# singleState <- c(rep("", length(minimumLayer)), minimumLayer)
# singleMat <- matrix(singleState, ncol = 2)
# colnames(singleMat) <- colnames(allCom)
# key1 <- apply(allCom, MARGIN = 1, paste, collapse = ";")
# key2 <- paste(rep("", length(minimumLayer)), minimumLayer, sep = ";")
# namesMat <- rbind(allCom, singleMat)
outTab <- matrix(0, nrow = length(minimumLayer)^2 + length(minimumLayer), ncol = 2)
storageTable <- matrix(0, nrow = length(minimumLayer)^2 + length(minimumLayer), ncol = 1)
col1 <- c(rep(minimumLayer, length(minimumLayer)), rep("", length(minimumLayer)))
col2 <- c(rep(minimumLayer, each = length(minimumLayer)), minimumLayer)
outTab[, 1] <- col1; outTab[, 2] <- col2
key <- apply(outTab, MARGIN = 1, paste, collapse = ";")
for (i in 1:nrow(outTab)){
  print(i/nrow(outTab))
  early <- outTab[i, 1]; late <- outTab[i, 2]
  storageTable[i, 1] <- paste(recsysList(early, late), collapse = ";")
}
# colnames(outTab) <- c("firstStep", "secondStep", "recList")
colnames(storageTable) <- c("recList")
rownames(storageTable) <-key
# col1 <- c(rep(minimumLayer, length(minimumLayer)), rep("", length(minimumLayer)))
# col2 <- c(rep(minimumLayer, each = length(minimumLayer)), minimumLayer)
# outTab[, 1] <- col1; outTab[, 2] <- col2
# for (i in 1:nrow(namesMat)){
#   print(i/nrow(namesMat))
#   early <- as.character(namesMat[i, 1]); late <- as.character(namesMat[i, 2])
#   outTab[i, 1] <- paste(recsysList(early, late), collapse = ";")
# }
# tt <- apply(outTab[, 1:2], MARGIN = 1, paste, collapse = ";")
# colnames(outTab) <- c("recList")
# rownames(outTab) <- c(key1, key2)
# outTab <- matrix(0, nrow = length(minimumLayer), ncol = length(minimumLayer))
# for (i in 1:length(minimumLayer)){
#   print(i/length(minimumLayer))
#   early <- minimumLayer[i]
#   for (j in 1:length(minimumLayer)){
#     late <- minimumLayer[j]  
#     outTab[i, j] <- paste(recsysList(early, late), collapse = ";")
#   }
# }

#---------
#---------
storage <- lapply(1:length(datByUser), function(k){
  print(k/length(datByUser))
  user <- as.character(datByUser[[k]][1, 1])
  tab <- datByUser[[user]]
  clicked <- unique(as.character(tab$merchandise))
  
  lastClick <- clicked[length(clicked)]
  
  if (sum(clicked %in% dataInfo_id[, 1]) > 0){
    mod <- clicked[which(clicked %in% dataInfo_id[, 1])]
    belongList <- list()
    for (i in 1:length(mod)){
      modelRow <- dataInfo_id[which(mod[i] == dataInfo_id, arr.ind = T)[1, 1], ]
      belongMinLayer <- as.character(modelRow[, min(which(modelRow == "")) - 1])
      belongList[[i]] <- dataInfo_id[which(belongMinLayer == dataInfo_id, arr.ind = T)[, 1], 1]
    }
    names(belongList) <- mod
    # if clickstream includes model and category, remove the model, regards the remain category as usual.
    if (length(mod) != length(clicked)){
      cate <- clicked[-which(clicked %in% dataInfo_id[, 1])]
      lastClick <- cate[length(cate)]
      if (length(cate) > 1){
        whichCol <- probMatrix_2[, which(colnames(probMatrix_2) == lastClick)]
        tmpMat <- as.matrix(whichCol[which(as.numeric(whichCol) > 0)])
      }else{
        whichCol <- probMatrix_1[, which(colnames(probMatrix_1) == lastClick)]
        tmpMat <- as.matrix(whichCol[which(as.numeric(whichCol) > 0)])
      }
      minLayer <- which(rownames(tmpMat) %in% minimumLayer)
      # if the user's clickstream didn't include any minimum category or model.
      if (length(minLayer) != 0){
        tmpMat <- as.matrix(tmpMat[minLayer, ])
        belongList_cate <- list()
        for (i in 1:nrow(tmpMat)){
          click_i <- rownames(tmpMat)[i]
          loc <- which(dataInfo_id == click_i, arr.ind = T)
          belongList_cate[[i]] <- dataInfo_id[loc[, 1], 1]
        }
        names(belongList_cate) <- rownames(tmpMat)
        belongList <- c(belongList, belongList_cate)
      }else{
        belongList_cate <- list()
        for (i in 1:length(cate)){
          belongList_cate[[i]] <- dataInfo_id[which(cate[i] == dataInfo_id[, 1], arr.ind = T)[, 1], 1]
        }
        names(belongList_cate) <- cate
        #       belongList <- c(belongList, belongList_cate)
      }
    }
    #   modelRow <- dataInfo_id[which(lastClick == dataInfo_id, arr.ind = T)[1, 1], ]
    #   belongMinLayer <- modelRow[, min(which(modelRow == "")) - 1]
    #   belongList <- list(dataInfo_id[which("DISTRIBUTED_MOTION_CONTROL_SOLUTION" == dataInfo_id, arr.ind = T)[, 1], 1])
    #   names(belongList) <- as.character(belongMinLayer)
  }else{
    if (length(clicked) > 1){
      whichCol <- probMatrix_2[, which(colnames(probMatrix_2) == lastClick)]
      tmpMat <- as.matrix(whichCol[which(as.numeric(whichCol) > 0)])
    }else{
      whichCol <- probMatrix_1[, which(colnames(probMatrix_1) == lastClick)]
      tmpMat <- as.matrix(whichCol[which(as.numeric(whichCol) > 0)])
    }
    minLayer <- which(rownames(tmpMat) %in% minimumLayer)
    
    if (length(minLayer) != 0){
      tmpMat <- as.matrix(tmpMat[minLayer, ])
      belongList <- list()
      for (i in 1:nrow(tmpMat)){
        click_i <- rownames(tmpMat)[i]
        loc <- which(dataInfo_id == click_i, arr.ind = T)
        belongList[[i]] <- dataInfo_id[loc[, 1], 1]
      }
      names(belongList) <- rownames(tmpMat)
    }else{
      belongList <- list()
      for (i in 1:length(clicked)){
        belongList[[i]] <- dataInfo_id[which(clicked[i] == dataInfo_id, arr.ind = T)[, 1], 1]
      }
      names(belongList) <- clicked
    }
  }
  
  
  # if (lastClick %in% rownames(tmpMat)) belongList[[which(rownames(tmpMat) == lastClick)]] <- NULL
  
  ##
  ## pick 5 top value of transition probability from these minimum layer.
  ## pick 10 models from these top 5 minimum category.
  ##
  
  if (sum(clicked %in% dataInfo_id[, 1]) > 0){
    mod <- clicked[which(clicked %in% dataInfo_id[, 1])]
    if (exists("belongList_cate")){ # with category and model together
      topmin <- min(topmin, length(mod))
      numOfEachLayer_1 <- rep(round(numOfModel/2/length(mod)), length(mod))
      
      topmin <- min(topmin, nrow(tmpMat))
      topminLayer <- tmpMat[order(tmpMat, decreasing = T)[1:topmin], 1]
      proportion <- topminLayer/sum(topminLayer, na.rm = T)
      numOfEachLayer_2 <- rep(round(numOfModel/2/length(mod)), length(mod))
      
      numOfEachLayer_2 <- c(numOfEachLayer_1, numOfEachLayer_2)
      belongList <- c(belongList, belongList_cate)
    }else{
      topmin <- min(topmin, length(mod))
      numOfEachLayer <- rep(round(numOfModel/length(mod)), length(mod))
    }
  }else{
    if (length(minLayer != 0)){
      topmin <- min(topmin, nrow(tmpMat))
      topminLayer <- tmpMat[order(tmpMat, decreasing = T)[1:topmin], 1]
      proportion <- topminLayer/sum(topminLayer, na.rm = T)
      numOfEachLayer <- round(numOfModel*proportion)
    }else{
      topmin <- min(topmin, length(clicked))
      proportion <- rep(1/length(clicked), length(clicked))
      numOfEachLayer <- round(numOfModel*proportion)
    }
  }
  
  
  if (sum(clicked %in% dataInfo_id[, 1]) > 0){
    belongListTop <- belongList
  }else{
    if (length(minLayer) != 0){
      belongListTop <- belongList[which(names(belongList) %in% names(topminLayer))]
    }else{
      belongListTop <- belongList
    }
  }
  recList <- lapply(1:topmin, function(i){
    if (length(belongListTop[[i]]) > numOfEachLayer[i]){
      reclist <- as.character(sample(belongListTop[[i]], as.numeric(numOfEachLayer[i]), replace = F))
    }else{
      reclist <- as.character(belongListTop[[i]])
    }
    return(reclist)
  })
  return(unlist(recList))
})
names(storage) <- names(datByUser)

## set a new guy with only one state (current state), assume this state is "1-2JKEVU".
currentState <- "1-2JKEVU"
head(probMatrix_1)



