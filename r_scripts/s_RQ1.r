# settings
library(randomForest)
try(setwd("/scratch1/kamei/str/data"))
try(setwd("/Users/kamei/Research/sq_effort/data"))
set.seed(1)

# ------------------------------------------------
# read util functions
try(source("../r_scripts/util.r"))

# ------------------------------------------------
# read data
try(source("../r_scripts/readfile.r"))

# ------------------------------------------------
train <- data.process.old
test <- data.process.new

idx <- ""
if(TYPE == "FxEf_CHURN"){
  idx <- test$BUG_AllEf_CHURN
}else{
  idx <- test$BUG_AllEf_TLOC
}

# File-Level
res.opt <- doSimulateForFixingEffort(idx, TYPE, Review=REVIEW)
res.post <- doSimulateForFixingEffort(test$POST, TYPE, Review=REVIEW)
res.den <- doSimulateForFixingEffort(test$BUGDENSITY, TYPE, Review=REVIEW)
res.rnd <- doSimulateForFixingEffort(runif(length(idx)), TYPE, Review=REVIEW)

# Bug-Level
res.bug <- doSimulateAtBugLevel(TYPE)

pdf.name <- paste("../graph/", project, "_plot_fixing_", TYPE , "-", v.pre, "-", v.post, "_", TYPE , "-Review-", REVIEW,  "_RQ1.pdf", sep="")

sink(file = paste("../graph/", project,"_result_rq1.txt", sep=""), append=T) #

cat("result:::", pdf.name, "\n")
cat("file-level:", res.opt$b20, "  bug-level:",res.bug$b20, "  how much percentages of bugs do we miss?:", ((res.opt$b20 - res.bug$b20) / res.bug$b20 * 100), "\n\n")

sink()

# output
pdf(pdf.name ,width=12,height=7)

lab <- ""
if(TYPE == "FxEf_CHURN"){
  lab <- paste("# of Fixing EFFORT (KSLOC) using CHURN", sep="")
}else{
  lab <- paste("# of Fixing EFFORT (KSLOC) using LOC", sep="")
}
  
# the number of detected bugs using prior effort-aware model
plot(cumsum(res.bug$effort)/1000, cumsum(res.bug$bugs), type="l", xlab=lab, ylab="# of fixed bugs", col=2, xlim=c(0,sum(res.bug$effort/1000)))
# bug density
lines(cumsum(res.opt$effort)/1000, cumsum(res.opt$bugs))
lines(cumsum(res.post$effort)/1000, cumsum(res.post$bugs),col=4)
lines(cumsum(res.den$effort)/1000, cumsum(res.den$bugs),col=6)
lines(cumsum(res.rnd$effort)/1000, cumsum(res.rnd$bugs), lty=2)

legend("topleft", c("bug-LV - so optimal (bugs)","file-LV - optimal (bugs in file)","file-LV (post) traditional optimal","file-LV (deinsity) effort optimal" ,"file-LV (random)"),col = c(2,1,4,6,1), lty = c(1, 1, 1, 1, 2))

dev.off()
