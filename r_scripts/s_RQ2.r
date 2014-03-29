# settings
library(randomForest)
set.seed(1)

# ------------------------------------------------
# read util functions
try(source("../r_scripts/util.r"))

# ------------------------------------------------
# read data
try(source("../r_scripts/readfile.r"))

# ------------------------------------------------
# fix phase
# ------------------------------------------------
train <- data.process.old
test <- data.process.new

MEDIAN.EFFORT <- train[(train[,TYPE] != 0), TYPE]
MEDIAN.EFFORT <- median(MEDIAN.EFFORT)

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

res.loc.inc   <- doSimulateForFixingEffort(test$TLOC,  Decreasing=F, TYPE, Review=REVIEW)
res.loc.dec   <- doSimulateForFixingEffort(test$TLOC, TYPE, Review=REVIEW)

res.churn.inc <- doSimulateForFixingEffort(test$CHURN, Decreasing=F, TYPE, Review=REVIEW)
res.churn.dec <- doSimulateForFixingEffort(test$CHURN, TYPE, Review=REVIEW)

res.age.inc <- doSimulateForFixingEffort(test$AGE, Decreasing=F,  TYPE, Review=REVIEW)
res.age.dec <- doSimulateForFixingEffort(test$AGE, TYPE, Review=REVIEW)

res.pre.inc <- doSimulateForFixingEffort(test$PRE, Decreasing=F,  TYPE, Review=REVIEW)
res.pre.dec <- doSimulateForFixingEffort(test$PRE, TYPE, Review=REVIEW)

# random
res.rand1 <- doSimulateForFixingEffort(runif(nrow(test)), Decreasing=F,  TYPE, Review=REVIEW)
res.rand2 <- doSimulateForFixingEffort(runif(nrow(test)), Decreasing=F,  TYPE, Review=REVIEW)
res.rand3 <- doSimulateForFixingEffort(runif(nrow(test)), Decreasing=F,  TYPE, Review=REVIEW)
res.rand4 <- doSimulateForFixingEffort(runif(nrow(test)), Decreasing=F,  TYPE, Review=REVIEW)
res.rand5 <- doSimulateForFixingEffort(runif(nrow(test)), Decreasing=F,  TYPE, Review=REVIEW)

# Bug-Level
res.bug <- doSimulateAtBugLevel(TYPE)

# Prediction
predictor <- POST~CHURN+ADD+DEL+TPC+AGE+LAST+BFC+PRE+REFAC
pred.post  <- doSimulateForFixingEffortWithPrediction(predictor, TYPE)
predictor <- BUGDENSITY~CHURN+ADD+DEL+TPC+AGE+LAST+BFC+PRE+REFAC
pred.den  <- doSimulateForFixingEffortWithPrediction(predictor, TYPE)

if(TYPE == "FxEf_CHURN"){
  predictor <- BUG_FxEf_CHURN~CHURN+ADD+DEL+TPC+AGE+LAST+BFC+PRE+REFAC
}else{
  predictor <- BUG_FxEf_TLOC~CHURN+ADD+DEL+TPC+AGE+LAST+BFC+PRE+REFAC
}
model <- randomForest(predictor, data=train)
pred <- predict(model, test)
pred.opt <- doSimulateForFixingEffort(pred, TYPE, Review=REVIEW)


############################################################
# Hybrid
############################################################
hy.loc.dec <- rank(test$TLOC) * rank(pred)
hy.loc.inc <- rank(1/test$TLOC) * rank(pred)
pred.hy.loc.inc <- doSimulateForFixingEffort(hy.loc.inc, TYPE, Review=REVIEW)
pred.hy.loc.dec <- doSimulateForFixingEffort(hy.loc.dec, TYPE, Review=REVIEW)

hy.churn.dec <- rank(test$CHURN)   * rank(pred)
hy.churn.inc <- rank(1/test$CHURN) * rank(pred)
pred.hy.churn.inc <- doSimulateForFixingEffort(hy.churn.inc, TYPE, Review=REVIEW)
pred.hy.churn.dec <- doSimulateForFixingEffort(hy.churn.dec, TYPE, Review=REVIEW)

hy.age.dec <- rank(test$AGE) * rank(pred)
hy.age.inc <- rank(1/test$AGE) * rank(pred)
pred.hy.age.inc <- doSimulateForFixingEffort(hy.age.inc, TYPE, Review=REVIEW)
pred.hy.age.dec <- doSimulateForFixingEffort(hy.age.dec, TYPE, Review=REVIEW)

hy.pre.dec <- rank(test$PRE) * rank(pred)
hy.pre.inc <- rank(1/test$PRE) * rank(pred)
pred.hy.pre.inc <- doSimulateForFixingEffort(hy.pre.inc, TYPE, Review=REVIEW)
pred.hy.pre.dec <- doSimulateForFixingEffort(hy.pre.dec, TYPE, Review=REVIEW)

############################################################
# Switch using AGE
############################################################
if(F){
rnk <- rank(test$AGE)
th  <- length(rnk) * 0.10
r.idx <- (rnk < th)

sw.loc.dec <- (r.idx) * rnk + ((!r.idx) * rank(test$TLOC) + (!r.idx) * th)
sw.loc.inc <- (r.idx) * rnk + ((!r.idx) * rank(1/test$TLOC) + (!r.idx) * th)
pred.sw.loc.inc <- doSimulateForFixingEffort(sw.loc.inc, Decreasing=F, TYPE, Review=REVIEW)
pred.sw.loc.dec <- doSimulateForFixingEffort(sw.loc.dec, Decreasing=F, TYPE, Review=REVIEW)

sw.churn.dec <- (r.idx) * rnk + ((!r.idx) * rank(test$CHURN) + (!r.idx) * th)
sw.churn.inc <- (r.idx) * rnk + ((!r.idx) * rank(1/test$CHURN) + (!r.idx) * th)
pred.sw.churn.inc <- doSimulateForFixingEffort(sw.churn.inc, Decreasing=F, TYPE, Review=REVIEW)
pred.sw.churn.dec <- doSimulateForFixingEffort(sw.churn.dec, Decreasing=F, TYPE, Review=REVIEW)

sw.pre.dec <- (r.idx) * rnk + ((!r.idx) * rank(test$PRE) + (!r.idx) * th)
sw.pre.inc <- (r.idx) * rnk + ((!r.idx) * rank(1/test$PRE) + (!r.idx) * th)
pred.sw.pre.inc <- doSimulateForFixingEffort(sw.pre.inc, Decreasing=F, TYPE, Review=REVIEW)
pred.sw.pre.dec <- doSimulateForFixingEffort(sw.pre.dec, Decreasing=F, TYPE, Review=REVIEW)

sw.opt <- (r.idx) * rnk + ((!r.idx) * rank(1/pred) + (!r.idx) * th)
pred.sw.opt <- doSimulateForFixingEffort(sw.opt, Decreasing=F, TYPE, Review=REVIEW)

if(TYPE == "FxEf_CHURN"){
  predictor <- FxEf_CHURN~CHURN+ADD+DEL+TPC+AGE+LAST+BFC+PRE+REFAC
}else{
  predictor <- FxEf_TLOC~CHURN+ADD+DEL+TPC+AGE+LAST+BFC+PRE+REFAC
}
pred.effort  <- doSimulateForFixingEffortWithPrediction(predictor, TYPE, Decreasing=F)

sink(file = "result_rq2.txt", append=T) #

pdf.name <- paste("../graph/", "simple_plot_fixing_", TYPE , "-", v.pre, "-", v.post, "_", TYPE , "-Review-", REVIEW,  "_RQ2.pdf", sep="")
cat("result:::", pdf.name, "\n")
cat("file-level:", res.opt$b20, "  bug-level:",res.bug$b20, "  how much percentages of bugs do we miss?:", ((res.opt$b20 - res.bug$b20) / res.bug$b20 * 100), "\n\n")

sink()
}

######################################################################
# output
######################################################################
pdf.name <- paste("../graph/", project, "_plot_fixing_", TYPE , "-", v.pre, "-", v.post, "_", TYPE , "-Review-", REVIEW,  "_RQ2.pdf", sep="")
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

# pred
lines(cumsum(pred.post$effort)/1000, cumsum(pred.post$bugs), col=4, lty=2)
# pred
lines(cumsum(pred.den$effort)/1000, cumsum(pred.den$bugs), col=6, lty=2)

lines(cumsum(res.loc.dec$effort)/1000, cumsum(res.loc.dec$bugs),col=3)
lines(cumsum(res.loc.inc$effort)/1000, cumsum(res.loc.inc$bugs),col=3, lty=2)

lines(cumsum(res.churn.dec$effort)/1000, cumsum(res.churn.dec$bugs),col=5)
lines(cumsum(res.churn.inc$effort)/1000, cumsum(res.churn.inc$bugs),col=5, lty=2)

lines(cumsum(res.age.dec$effort)/1000, cumsum(res.age.dec$bugs),col=8)
lines(cumsum(res.age.inc$effort)/1000, cumsum(res.age.inc$bugs),col=8, lty=2)

lines(cumsum(res.pre.dec$effort)/1000, cumsum(res.pre.dec$bugs),col=7,lwd=3)
lines(cumsum(res.pre.inc$effort)/1000, cumsum(res.pre.inc$bugs),col=7,lwd=3, lty=2)

lines(cumsum(res.rand1$effort)/1000, cumsum(res.rand1$bugs), lty=2)
lines(cumsum(res.rand2$effort)/1000, cumsum(res.rand2$bugs), lty=2)
lines(cumsum(res.rand3$effort)/1000, cumsum(res.rand3$bugs), lty=2)
lines(cumsum(res.rand4$effort)/1000, cumsum(res.rand4$bugs), lty=2)
lines(cumsum(res.rand5$effort)/1000, cumsum(res.rand5$bugs), lty=2)

legend("bottomright", c("bug-LV - so optimal (bugs)" ,"file-LV - optimal (bugs in file)", "file-LV (post) traditional optimal","file-LV (deinsity) effort optimal","[pred.] post-bugs", "[pred.] bug density","LOC(dec)","LOC(inc)","CHURN(dec)","CHURN(inc)","AGE(dec)","AGE(inc)","Pre-Bug(dec)", "Pre-Bug(inc)","Random"),col = c(2,1,4,6,4,6,3,3,5,5,8,8,7,7,1), lty = c(1,1,1,1,2,2, 1,2,1,2,1,2,1,2,2))

dev.off()

######################################################################
# output
######################################################################
pdf.name <- paste("../graph/", project, "_plot_fixing_", TYPE , "-", v.pre, "-", v.post, "_", TYPE , "-Review-", REVIEW,  "_RQ3.pdf", sep="")
pdf(pdf.name ,width=12,height=7)

lab <- ""
if(TYPE == "FxEf_CHURN"){
  lab <- paste("# of Fixing EFFORT (KSLOC) using CHURN", sep="")
}else{
  lab <- paste("# of Fixing EFFORT (KSLOC) using LOC", sep="")
}

plot(cumsum(pred.hy.loc.dec$effort)/1000, cumsum(pred.hy.loc.dec$bugs), type="l", xlab=lab, ylab="# of fixed bugs", col=2, xlim=c(0,sum(res.bug$effort/1000)))
lines(cumsum(pred.hy.loc.inc$effort)/1000, cumsum(pred.hy.loc.inc$bugs), col=2, lty=2)

lines(cumsum(res.post$effort)/1000, cumsum(res.post$bugs),col=4)
lines(cumsum(res.den$effort)/1000, cumsum(res.den$bugs),col=6)

# pred
lines(cumsum(pred.post$effort)/1000, cumsum(pred.post$bugs), col=4, lty=2)
# pred
lines(cumsum(pred.den$effort)/1000, cumsum(pred.den$bugs), col=6, lty=2)

lines(cumsum(res.loc.dec$effort)/1000, cumsum(res.loc.dec$bugs),col=3)
lines(cumsum(res.loc.inc$effort)/1000, cumsum(res.loc.inc$bugs),col=3, lty=2)

lines(cumsum(res.churn.dec$effort)/1000, cumsum(res.churn.dec$bugs),col=5)
lines(cumsum(res.churn.inc$effort)/1000, cumsum(res.churn.inc$bugs),col=5, lty=2)

lines(cumsum(res.age.dec$effort)/1000, cumsum(res.age.dec$bugs),col=8)
lines(cumsum(res.age.inc$effort)/1000, cumsum(res.age.inc$bugs),col=8, lty=2)

lines(cumsum(res.pre.dec$effort)/1000, cumsum(res.pre.dec$bugs),col=7,lwd=3)
lines(cumsum(res.pre.inc$effort)/1000, cumsum(res.pre.inc$bugs),col=7,lwd=3, lty=2)

lines(cumsum(res.rand1$effort)/1000, cumsum(res.rand1$bugs), lty=2)
lines(cumsum(res.rand2$effort)/1000, cumsum(res.rand2$bugs), lty=2)
lines(cumsum(res.rand3$effort)/1000, cumsum(res.rand3$bugs), lty=2)
lines(cumsum(res.rand4$effort)/1000, cumsum(res.rand4$bugs), lty=2)
lines(cumsum(res.rand5$effort)/1000, cumsum(res.rand5$bugs), lty=2)

lines(cumsum(pred.opt$effort)/1000, cumsum(pred.opt$bugs))

legend("bottomright", c("file-LV (post) traditional optimal","file-LV (deinsity) effort optimal","[pred.] post-bugs", "[pred.] bug density","LOC(dec)","LOC(inc)","CHURN(dec)","CHURN(inc)","AGE(dec)","AGE(inc)","Pre-Bug(dec)", "Pre-Bug(inc)","Hybrid(LOC*[pred] opt) (dec)","Hybrid(LOC*[pred] opt", "[pred] opt","Random"),col = c(4,6,4,6,3,3,5,5,8,8,7,7,2,2,1,1), lty = c(1,1,2,2, 1,2,1,2,1,2,1,2,1,2,1,2))

dev.off()


######################################################################
# output
######################################################################
pdf.name <- paste("../graph/", project, "_plot_fixing_", TYPE , "-", v.pre, "-", v.post, "_", TYPE , "-Review-", REVIEW,  "_Hybrid.pdf", sep="")
pdf(pdf.name ,width=12,height=7)

lab <- ""
if(TYPE == "FxEf_CHURN"){
  lab <- paste("# of Fixing EFFORT (KSLOC) using CHURN", sep="")
}else{
  lab <- paste("# of Fixing EFFORT (KSLOC) using LOC", sep="")
}

plot(cumsum(res.post$effort)/1000, cumsum(res.post$bugs),col=4, type="l", xlab=lab, ylab="# of fixed bugs", xlim=c(0,sum(res.bug$effort/1000)))
lines(cumsum(pred.post$effort)/1000, cumsum(pred.post$bugs), col=4, lty=2)

lines(cumsum(res.den$effort)/1000, cumsum(res.den$bugs),col=6)
lines(cumsum(pred.den$effort)/1000, cumsum(pred.den$bugs), col=6, lty=2)

lines(cumsum(pred.hy.loc.dec$effort)/1000, cumsum(pred.hy.loc.dec$bugs),col=3)
lines(cumsum(pred.hy.loc.inc$effort)/1000, cumsum(pred.hy.loc.inc$bugs),col=3, lty=2)

lines(cumsum(pred.hy.churn.dec$effort)/1000, cumsum(pred.hy.churn.dec$bugs),col=5)
lines(cumsum(pred.hy.churn.inc$effort)/1000, cumsum(pred.hy.churn.inc$bugs),col=5, lty=2)

lines(cumsum(pred.hy.age.dec$effort)/1000, cumsum(pred.hy.age.dec$bugs),col=8)
lines(cumsum(pred.hy.age.inc$effort)/1000, cumsum(pred.hy.age.inc$bugs),col=8, lty=2)

lines(cumsum(pred.hy.pre.dec$effort)/1000, cumsum(pred.hy.pre.dec$bugs),col=7,lwd=3)
lines(cumsum(pred.hy.pre.inc$effort)/1000, cumsum(pred.hy.pre.inc$bugs),col=7,lwd=3, lty=2)

lines(cumsum(res.rand1$effort)/1000, cumsum(res.rand1$bugs), lty=2)
lines(cumsum(res.rand2$effort)/1000, cumsum(res.rand2$bugs), lty=2)
lines(cumsum(res.rand3$effort)/1000, cumsum(res.rand3$bugs), lty=2)
lines(cumsum(res.rand4$effort)/1000, cumsum(res.rand4$bugs), lty=2)
lines(cumsum(res.rand5$effort)/1000, cumsum(res.rand5$bugs), lty=2)

lines(cumsum(pred.opt$effort)/1000, cumsum(pred.opt$bugs))

legend("bottomright", c("file-LV (post) traditional optimal","file-LV (deinsity) effort optimal","[pred.] post-bugs", "[pred.] bug density","LOC(dec)","LOC(inc)","CHURN(dec)","CHURN(inc)","AGE(dec)","AGE(inc)","Pre-Bug(dec)", "Pre-Bug(inc)","[pred] opt","Random"),col = c(4,6,4,6,3,3,5,5,8,8,7,7,1,1), lty = c(1,1,2,2,1,2,1,2,1,2,1,2,1,2))

dev.off()

