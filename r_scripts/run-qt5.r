try(setwd("/Users/kamei/Research/sq_effort/data"))
try(setwd("/Users/kamei/Research/sq_effort/shane/sq_effort/dataset"))

project <- "qt5"
####################
v.pre  <- "5.0"
v.post <- "5.0"
TYPE <- "FxEf_TLOC"
REVIEW=T
DYNAMIC=T
try(source("../r_scripts/s_RQ1.r"))

v.pre  <- "5.1"
v.post <- "5.1"
TYPE <- "FxEf_TLOC"
REVIEW=T
DYNAMIC=T
try(source("../r_scripts/s_RQ1.r"))

####################
v.pre  <- "5.0"
v.post <- "5.1"
TYPE <- "FxEf_TLOC"
REVIEW=F
DYNAMIC=F
try(source("../r_scripts/s_RQ2.r"))

####################
v.pre  <- "5.0"
v.post <- "5.0"
TYPE <- "FxEf_CHURN"
REVIEW=T
DYNAMIC=T
try(source("../r_scripts/s_RQ1.r"))

v.pre  <- "5.1"
v.post <- "5.1"
TYPE <- "FxEf_CHURN"
REVIEW=T
DYNAMIC=T
try(source("../r_scripts/s_RQ1.r"))

####################
v.pre  <- "5.0"
v.post <- "5.1"
TYPE <- "FxEf_CHURN"
REVIEW=F
DYNAMIC=F
try(source("../r_scripts/s_RQ2.r"))

####################
v.pre  <- "5.0"
v.post <- "5.0"
TYPE <- "FxEf_TLOC"
REVIEW=T
DYNAMIC=F
try(source("../r_scripts/s_RQ1.r"))

v.pre  <- "5.1"
v.post <- "5.1"
TYPE <- "FxEf_TLOC"
REVIEW=T
DYNAMIC=F
try(source("../r_scripts/s_RQ1.r"))

####################
v.pre  <- "5.0"
v.post <- "5.0"
TYPE <- "FxEf_CHURN"
REVIEW=T
DYNAMIC=F
try(source("../r_scripts/s_RQ1.r"))

v.pre  <- "5.1"
v.post <- "5.1"
TYPE <- "FxEf_CHURN"
REVIEW=T
DYNAMIC=F
try(source("../r_scripts/s_RQ1.r"))
