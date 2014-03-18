try(setwd("/Users/kamei/Research/sq_effort/data"))

project <- "maven"

####################
v.pre  <- "3.0"
v.post <- "3.0"
TYPE <- "FxEf_TLOC"
REVIEW=F
try(source("../r_scripts/s_RQ1.r"))

v.pre  <- "3.1"
v.post <- "3.1"
TYPE <- "FxEf_TLOC"
REVIEW=F
try(source("../r_scripts/s_RQ1.r"))

####################
v.pre  <- "3.0"
v.post <- "3.1"
TYPE <- "FxEf_TLOC"
REVIEW=F
try(source("../r_scripts/s_RQ2.r"))

####################
v.pre  <- "3.0"
v.post <- "3.0"
TYPE <- "FxEf_CHURN"
REVIEW=F
try(source("../r_scripts/s_RQ1.r"))

v.pre  <- "3.1"
v.post <- "3.1"
TYPE <- "FxEf_CHURN"
REVIEW=F
try(source("../r_scripts/s_RQ1.r"))

####################
v.pre  <- "3.0"
v.post <- "3.1"
TYPE <- "FxEf_CHURN"
REVIEW=F
try(source("../r_scripts/s_RQ2.r"))