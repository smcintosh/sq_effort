try(setwd("/Users/kamei/Research/sq_effort/data"))

project <- "qt5"

####################
v.pre  <- "5.0"
v.post <- "5.0"
TYPE <- "FxEf_TLOC"
REVIEW=F
try(source("../r_scripts/s_RQ1.r"))

v.pre  <- "5.1"
v.post <- "5.1"
TYPE <- "FxEf_TLOC"
REVIEW=F
try(source("../r_scripts/s_RQ1.r"))

####################
v.pre  <- "5.0"
v.post <- "5.1"
TYPE <- "FxEf_TLOC"
REVIEW=F
try(source("../r_scripts/s_RQ2.r"))

####################
v.pre  <- "5.0"
v.post <- "5.0"
TYPE <- "FxEf_CHURN"
REVIEW=F
try(source("../r_scripts/s_RQ1.r"))

v.pre  <- "5.1"
v.post <- "5.1"
TYPE <- "FxEf_CHURN"
REVIEW=F
try(source("../r_scripts/s_RQ1.r"))

####################
v.pre  <- "5.0"
v.post <- "5.1"
TYPE <- "FxEf_CHURN"
REVIEW=F
try(source("../r_scripts/s_RQ2.r"))