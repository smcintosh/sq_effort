# ------------------------------------------------
addReviewingEffort <- function(data.process){
  data.process <- cbind(data.process, ReEf_TLOC=data.process$TLOC)
  data.process <- cbind(data.process, BUGDENSITY=(data.process$POST/data.process$ReEf_TLOC*1000))

  # add some filters [2014/03/03]
  idx <- (data.process$TLOC == 0)
  data.process[idx,"BUGDENSITY"] <- 0
  
  return (data.process)
}

# ------------------------------------------------
addFixingEffort <- function(data.process,data.list, BUGTYPE="POST"){
  # TLOC
  bid.effort <- tapply(data.list$ReEf_TLOC, data.list$BID, sum)
  bid <- data.frame(BID=names(bid.effort), FxEf_TLOC=bid.effort)
  data.list <- merge(data.list, bid, by.x="BID",by.y="BID",all.x=T)
  file.effort <- tapply(data.list$FxEf_TLOC.y, data.list$FILE, sum)
  file1 <- data.frame(FILE=names(file.effort), FxEf_TLOC=file.effort)

  # CHURN
  bid.effort <- tapply(data.list$FxEf_CHURN, data.list$BID, sum)
  bid <- data.frame(BID=names(bid.effort), FxEf_CHURN2=bid.effort)
  data.list <- merge(data.list, bid, by.x="BID",by.y="BID",all.x=T)
  file.effort.churn <- tapply(data.list$FxEf_CHURN2, data.list$FILE, sum)
  file2 <- data.frame(FILE=names(file.effort.churn), FxEf_CHURN=file.effort.churn)

  # merge
  data.process$FILE <- as.character(data.process$FILE)
  file1$FILE <- as.character(file1$FILE)
  file2$FILE <- as.character(file2$FILE)
  newdata <- merge(data.process, file1, by.x="FILE", by.y="FILE", all.x=T)
  newdata <- merge(newdata,      file2, by.x="FILE", by.y="FILE", all.x=T)

  # ------------------------------------------------
  # Bug per Effort
  # ------------------------------------------------
  # Bug per FxEf_TLOC
  newdata$FxEf_TLOC[is.na(newdata$FxEf_TLOC)] <- 0
  newdata <- cbind(newdata, BUG_FxEf_TLOC=(newdata[,BUGTYPE] / newdata$FxEf_TLOC * 1000))
  newdata$BUG_FxEf_TLOC[is.nan(newdata$BUG_FxEf_TLOC)] <- 0
  newdata$BUG_FxEf_TLOC[is.infinite(newdata$BUG_FxEf_TLOC)] <- 0

  # Bug per FxEf_CHURN  
  newdata$FxEf_CHURN[is.na(newdata$FxEf_CHURN)] <- 0
  newdata <- cbind(newdata, BUG_FxEf_CHURN=(newdata[,BUGTYPE] / newdata$FxEf_CHURN * 1000))
  newdata$BUG_FxEf_CHURN[is.nan(newdata$BUG_FxEf_CHURN)] <- 0
  newdata$BUG_FxEf_CHURN[is.infinite(newdata$BUG_FxEf_CHURN)] <- 0

  # Bug per All_TLOC and ALL_CHURN
  newdata <- cbind(newdata, AllEf_TLOC=(newdata$ReEf_TLOC + newdata$FxEf_TLOC))
  newdata <- cbind(newdata, AllEf_CHURN=(newdata$ReEf_TLOC + newdata$FxEf_CHURN))

  newdata <- cbind(newdata, BUG_AllEf_TLOC=(newdata[,BUGTYPE] / newdata$AllEf_TLOC * 1000))
  newdata$BUG_AllEf_TLOC[is.nan(newdata$BUG_AllEf_TLOC)] <- 0
  newdata$BUG_AllEf_TLOC[is.infinite(newdata$BUG_AllEf_TLOC)] <- 0
  
  newdata <- cbind(newdata, BUG_AllEf_CHURN=(newdata[,BUGTYPE] / newdata$AllEf_CHURN * 1000))
  newdata$BUG_AllEf_CHURN[is.nan(newdata$BUG_AllEf_CHURN)] <- 0
  newdata$BUG_AllEf_CHURN[is.infinite(newdata$BUG_AllEf_CHURN)] <- 0
  
  return(newdata)
}

# ------------------------------------------------
# simulation
doSimulateAtBugLevel <- function(TYPE="FxEf_TLOC"){
  a <- tapply(data.list[,TYPE],data.list$BID,sum)
  list.effort <- sort(a)
  list.bugnum <- rep(1, length(a))
  
  e <- sum(list.effort) * 0.2
  cum.e <- cumsum(list.effort)
  idx.e <- (cum.e <= e)
  
  e20 <- sum(list.effort[idx.e]) / 1000
  b20 <- sum(list.bugnum[idx.e])
  
  return( list(effort=list.effort, bugs=list.bugnum, e20=e20, b20=b20))
}

# simulate for fixing effort based on indexes
# arg: index keys to say the order to be fixed
# note: we will use both of test and data.list
doSimulateForFixingEffort <- function(values, TYPE="FxEf_TLOC", Decreasing=T, Dynamic=F, Review=F){
  original.values <- values
  idxs <- order(values, decreasing=Decreasing)
  
  # do simulate
  filenames <- test$FILE
  
  done.bug <- c()
  done.idx <- c()
  list.effort <- c()
  list.bugnum <- c()
  list.idx    <- c()
  
  # loop
  for(i in 1:length(idxs)){
    tmp.effort <- c()
    tmp.bug <- c()
    tmp.file <- c()
    
    # index and filename of the selectd file
    idx      <- idxs[i]
    filename <- filenames[idx]
    
    # choose bids releated to the selected file
    sub.buglist <- data.list[(data.list$FILE == filename),]
    
    # resort by random
    tmp.idx <- sample(1:nrow(sub.buglist))
    tmp <- sub.buglist[tmp.idx,]
    
    #detect a bug
    if(nrow(sub.buglist) > 0){
      for(j in 1:nrow(sub.buglist)){
        bug <- sub.buglist[j,"BID"]
        
        # whether or not this bug has already been detected
        if(any(done.bug == bug) == F){
          # list up the new bug
          done.bug <- c(done.bug,bug)
          done.idx <- c(done.idx,idx)
          
          # the list to relate files to be fixed
          sub.filelist <- data.list[(data.list[,"BID"] == bug),]
          
          # all effort to be fixed
          tmp.effort <- c(tmp.effort, sum(sub.filelist[,TYPE]))
          tmp.bug  <- c(tmp.bug, bug)
          tmp.file <- c(tmp.file, as.character(sub.filelist[,"FILE"]))
          
          if(Dynamic){
            # which files is updated?
            updated.file <- as.character(sub.filelist[,"FILE"])
            all.file <- as.character(filenames)
            updated.idx  <- is.element(all.file, intersect(all.file, updated.file))
            
            ### pattern 1
            est.bug <- values[updated.idx] * MEDIAN.EFFORT / 1000
            est.bug <- est.bug - 1
            est.bug[(est.bug < 0)] <- 0        
            
            est.effort <- MEDIAN.EFFORT - sum(sub.filelist[,TYPE])
            est.effort[(est.effort < 0)] <- 0.1 
            
            diff <- est.bug / est.effort * 1000
            values[updated.idx] <- diff
            
            ### pattern 2
            #diff <- 1 / sum(sub.filelist[,TYPE]) * 1000
            #values[updated.idx] <- values[updated.idx] - diff
            
            tmp.idxs   <- order(values, decreasing=Decreasing)
            deleted.idxs  <- is.element(tmp.idxs, intersect(idxs[1:i], tmp.idxs))
            tmp.idxs  <- tmp.idxs[!deleted.idxs]
            idxs <- c(idxs[1:i], tmp.idxs)
          }
        }
      }
    }
    
    # whether or not this file detects one bug at least
    if(length(tmp.bug) > 0){
      tmp <- sum(tmp.effort)
      if(Review){
        tmp <- tmp + test[(test$FILE == filename), "ReEf_TLOC"]
      }
      
      list.effort <- c(list.effort, tmp)
      list.bugnum <- c(list.bugnum, length(tmp.bug))
      list.idx    <- c(list.idx, idx)
    }else{
      if(Review){
        tmp <- test[(test$FILE == filename), "ReEf_TLOC"]        
        list.effort <- c(list.effort, tmp)
        list.bugnum <- c(list.bugnum, 0)
        list.idx    <- c(list.idx, idx)        
      }
    }
  }
  
  e <- sum(list.effort) * 0.2
  cum.e <- cumsum(list.effort)
  idx.e <- (cum.e <= e)
  
  e20 <- sum(list.effort[idx.e]) / 1000
  b20 <- sum(list.bugnum[idx.e])
  
  return( list(effort=list.effort, bugs=list.bugnum, e20=e20, b20=b20))
}

# simulate for fixing effort based on prediction
# arg: predictor
# note: we wrap doSimulateForFixingEffort
doSimulateForFixingEffortWithPrediction <- function(predictor, TYPE="FxEf_TLOC", Decreasing=T, Dynamic=F, Review=F){
  model <- randomForest(predictor, data=train)
  pred <- predict(model, test)
  res.pred <- doSimulateForFixingEffort(pred, TYPE, Decreasing, Dynamic, Review)
  
  return (res.pred)
}

# ------------------------------------------------
# read function
calcEffort <- function(filename,TYPE="FxEf_CHURN"){
  tmp.effort <- c()
  tmp.bug <- c()
  tmp.file <- c()
  
  # choose bids releated to the selected file
  sub.buglist <- data.list[(data.list$FILE == filename),]
  
  # resort by random
  tmp.idx <- sample(1:nrow(sub.buglist))
  tmp <- sub.buglist[tmp.idx,]
  
  #detect a bug
  if(nrow(sub.buglist) > 0){
    for(j in 1:nrow(sub.buglist)){
      bug <- sub.buglist[j,"BID"]
      
      # the list to relate files to be fixed
      sub.filelist <- data.list[(data.list[,"BID"] == bug),]
      
      # all effort to be fixed
      tmp.effort <- c(tmp.effort, sum(sub.filelist[,TYPE]))
      tmp.bug  <- c(tmp.bug, bug)
      tmp.file <- c(tmp.file, as.character(sub.filelist[,"FILE"]))
    }
  }
  
  # whether or not this file detects one bug at least
  if(length(tmp.bug) > 0){
    ###-------------------------------------
    # count effort for each file only once
    #tmp.unique.file <- unique(tmp.file)
    #idx.tmp <- rep(F,nrow(data.effort))
    #for(k in 1:length(tmp.unique.file)){
    #  idx.tmp <- idx.tmp | (data.effort[,1] == tmp.unique.file[k])
    #}
    ###-------------------------------------
    #return (sum(data.effort[idx.tmp,"TLOC"]))
    
    return (sum(tmp.effort))
  }else{
    #    tmp <- data.effort[(data.effort[,1] == filename),2]
    #    return (tmp)
    
    return (0)
  }
}

################################################################################
# each element of target is in list?
is.inList <- function(target, list) {
  idx <- apply(t(target), 2, function(x)(any(list == x)))
  return (idx)
}

# ------------------------------------------------
# read function
findRelatedBugs <- function(filename){
  # choose bids releated to the selected file
  sub.buglist <- data.list[(data.list[,"FILE"] == filename),]
  return(sub.buglist[,"BID"])
}

