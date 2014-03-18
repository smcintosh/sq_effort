# ----
# My memo for TODO
#  check inconsistency between process and product
# My memo for variables
#  data.process: main and process
#  data.product:          product
#  FxEf_TLOC: Fix effort to fix all bugs related to the file
#  BUG_FxEf_TLOC: Bug per Fix effort 
# ------------------------------------------------
# define function
getDepth <- function(list){
  list <- gsub("\\w","", list, perl=T)
  list <- gsub(" ","",list)
  list <- gsub(".","",list,fixed=T)
  return((nchar(list) + 1))
}

# ------------------------------------------------
# read data
data.process.old <- read.csv(paste("sq_", project, "_", v.pre, "_dataset.txt.csv", sep=""), header=T); # metrics data
data.buglist.old <- read.csv(paste("sq_", project, "_", v.pre, "_buglist_POST.txt", sep=""), header=F); # bugid + modified files
data.buglist.old.PRE <- read.csv(paste("sq_", project, "_" , v.pre, "_buglist_PRE.txt", sep=""), header=F); # bugid + modified files
names(data.buglist.old)     <- c("BID","FILE","FxEf_CHURN","ReEf_TLOC")
names(data.buglist.old.PRE) <- c("BID","FILE","FxEf_CHURN","ReEf_TLOC")
data.list.old     <- cbind(data.buglist.old,     FxEf_TLOC= data.buglist.old$ReEf_TLOC)
data.list.old.PRE <- cbind(data.buglist.old.PRE, FxEf_TLOC= data.buglist.old.PRE$ReEf_TLOC)

data.process.new <- read.csv(paste("sq_", project, "_", v.post, "_dataset.txt.csv", sep=""), header=T); # metrics data
data.buglist     <- read.csv(paste("sq_", project, "_", v.post, "_buglist_POST.txt", sep=""), header=F); # bugid + modified files
data.buglist.PRE <- read.csv(paste("sq_", project, "_", v.post, "_buglist_PRE.txt", sep=""), header=F); # bugid + modified files
names(data.buglist)     <- c("BID","FILE","FxEf_CHURN","ReEf_TLOC")
names(data.buglist.PRE) <- c("BID","FILE","FxEf_CHURN.PRE","ReEf_TLOC")
data.list     <- cbind(data.buglist,     FxEf_TLOC= data.buglist$ReEf_TLOC)
data.list.PRE <- cbind(data.buglist.PRE, FxEf_TLOC= data.buglist.PRE$ReEf_TLOC)

# ------------------------------------------------
# preprocessing
# merge process + product to add reviewing effort
data.process.old <- addReviewingEffort(data.process.old);
data.process.new <- addReviewingEffort(data.process.new);

origin.old <- data.process.old
origin.new <- data.process.new

# adding more information (fixing effort)
data.process.new <- addFixingEffort(data.process.new,data.list)
pre <- addFixingEffort(origin.new, data.list.PRE, BUGTYPE="PRE")

idx <- charmatch(c("FxEf_TLOC", "FxEf_CHURN", "BUG_FxEf_TLOC", "BUG_FxEf_CHURN"), names(pre))
pre <- pre[,idx]
names(pre) <- c("PRE.FxEf_TLOC", "PRE.FxEf_CHURN", "PRE.BUG_FxEf_TLOC", "PRE.BUG_FxEf_CHURN")
data.process.new <- cbind(data.process.new, pre)

# add the depth
tmp <- as.character(data.process.new[,c("FILE")])
depth <- getDepth(tmp)
tmp <- cbind(FILE=tmp, DIRS=depth)
data.process.new <- merge(tmp,data.process.new, by.x="FILE", by.y="FILE",all.y=T)
data.process.new$FILE <- as.character(data.process.new$FILE) # category -> character 

# repeat same data preparation for old version
data.process.old <- addFixingEffort(data.process.old,data.list.old)
pre <- addFixingEffort(origin.old, data.list.old.PRE, BUGTYPE="PRE")

idx <- charmatch(c("FxEf_TLOC", "FxEf_CHURN", "BUG_FxEf_TLOC", "BUG_FxEf_CHURN"), names(pre))
pre <- pre[,idx]
names(pre) <- c("PRE.FxEf_TLOC", "PRE.FxEf_CHURN", "PRE.BUG_FxEf_TLOC", "PRE.BUG_FxEf_CHURN")
data.process.old <- cbind(data.process.old, pre)

tmp <- as.character(data.process.old[,c("FILE")])
depth <- getDepth(tmp)
tmp <- cbind(FILE=tmp, DIRS=depth)
data.process.old <- merge(tmp,data.process.old, by.x="FILE", by.y="FILE",all.y=T)
data.process.old$FILE <- as.character(data.process.old$FILE)  # category -> character 
