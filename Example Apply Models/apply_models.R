########################################
# Project:  UDOT-18.602 Traffic signal pedestrians
# Authors:  Patrick Singleton (patrick.singleton@usu.edu)
# File:     apply_models.R
# Date:     2020 Summer
# About:    Apply models (factoring) to estimate pedestrian volumes from signal data
########################################

########################################
# Notes

# Library
# library("")

########################################
# Prepare

# Load functions for ata processing
source("funs.R")

# Load model data
models <- readRDS(file.path("..", "Models", "models.rds"))
mod <- models[[7]]

# Load averaged data
datavg <- readRDS(file.path("..", "Models", "dat_ts_wdhr.rds"))

# Load signals data
signals <- readRDS(file.path("..", "Visualization", "ped-covid19", "Data", "signals.rds"))

########################################
# Load and process raw data

# Select signal
mysig <- 5306L

# Load data and convert types
tempa <- getsig(fpath=file.path(paste0(mysig, ".csv")), printsum=T)

# Sort by time, extract ped values
tempa <- tempa[order(tempa$TIME),]
tempa <- tempa[tempa$EVENT %in% c(0,21,22,23,45,89,90),]
row.names(tempa) <- NULL

# Rename
dat_raw <- tempa

########################################
# Aggregate by phase and hour

# Get time sequence
tseq <- seqsig(tempa, tunit="hour")

# Get max event time differences
tdmax <- timesigmiss(dat=tempa, tseq)$TDMAX

# Initialize table
t0 <- data.frame(SIGNAL=unique(tempa$SIGNAL), 
                 TIME1=tseq[1:(length(tseq)-1)], 
                 TIME2=tseq[2:(length(tseq))])
t0$TDIFF <- as.numeric(difftime(t0$TIME2, t0$TIME1, units="min"))
t0$P <- 0L
t0[,c("A00", "A21", "A45", "A90")] <- 0L
t0[,c("A45A", "A45B", "A45C", "A90A", "A90B", "A90C")] <- 0L
t0$TDMAX <- tdmax

# Get unique ped phases
tphase <- sort(unique(tempa$PARAM[tempa$EVENT!=0]))

# Skip if no ped data
if (length(tphase)==0) {
  temp <- t0
  temp$P <- NA
} else {
  
  # Initalize output table
  temp <- t0[0,]
  
  # For each phase
  for (j in 1:length(tphase)) {
    # initialize
    t1 <- t0
    # add phase number
    t1$P <- tphase[j]
    # add tabulated data
    t1$A00 <- tabsig(dat=subsig(dat=tempa, events=c(00), params=tphase[j]), tseq)$COUNT
    t1$A21 <- tabsig(dat=subsig(dat=tempa, events=c(21), params=tphase[j]), tseq)$COUNT
    t1$A45 <- tabsig(dat=subsig(dat=tempa, events=c(45), params=tphase[j]), tseq)$COUNT
    t1$A90 <- tabsig(dat=subsig(dat=tempa, events=c(90), params=tphase[j]), tseq)$COUNT
    # add imputed data
    t1$A45A <- tabsigseq(dat=subsig(dat=tempa, events=c(0,21,22,90), params=tphase[j]), tseq, eseq=c("0 90", "22 90"))$COUNT
    t1$A45B <- tabsigseq(dat=subsig(dat=tempa, events=c(0,21,90), params=tphase[j]), tseq, eseq=c("0 90", "21 90"))$COUNT
    t1$A45C <- tabsigseq(dat=subsig(dat=tempa, events=c(0,90), params=tphase[j]), tseq, eseq=c("0 90"))$COUNT
    t1$A90A <- tabsigtdif(dat=subsig(dat=tempa, events=c(90), params=tphase[j]), tseq, tdif=5)$COUNT
    t1$A90B <- tabsigtdif(dat=subsig(dat=tempa, events=c(90), params=tphase[j]), tseq, tdif=10)$COUNT
    t1$A90C <- tabsigtdif(dat=subsig(dat=tempa, events=c(90), params=tphase[j]), tseq, tdif=15)$COUNT
    temp <- rbind(temp, t1)
    rm(t1)
  }; rm(j)
  
  # Inspect
  print(summary(temp))
}

# Rename
dat_hr <- temp

# Cleanup
rm(tseq, tdmax, t0, tphase)

########################################
# Add columns for modeling

# Group signals by high vs. low pedestrian activity
tdatavg <- datavg[["A45B"]]
tdatavg <- tdatavg[,names(which(colSums(datavg$N[4:ncol(datavg$N)] >= 8)==168))]
tdatavg <- tdatavg[,colSums(is.na(tdatavg))==0]
tdatavg <- tdatavg[,colSums(is.infinite(as.matrix(tdatavg)))==0]
tdatavg <- tdatavg[,colSums(tdatavg!=0)>0]
# 2 --> split at 350
grp1 <- as.integer(gsub("sig", "", names(tdatavg)[which(colSums(tdatavg)/7 > 350)]))
grp2 <- as.integer(gsub("sig", "", names(tdatavg)[which(colSums(tdatavg)/7 <= 350)]))
signals$GROUP <- ifelse(signals$SIGNAL %in% grp1, 1L, ifelse(signals$SIGNAL %in% grp2, 2L, NA))
rm(grp1, grp2, tdatavg)

# Determine HAWK signals
signals$HAWK <- ifelse(signals$TYPE=="HAWK", T, F)

# Decide missingness criteria (in minutes)
tdmiss <- data.frame(HOUR=c(0:23), TDMISS=c(180,180,rep(240,3),180,60,30,20,rep(15,10),20,20,30,60,120))
tdmiss

# Create function to determine ped recall
getPedRecall <- function(dat) {
  
  # contruct variables
  dat$PR0 <- dat$A00==0
  dat$PR1 <- pmax((dat$A00 - dat$A21), 0)
  dat$PR2 <- pmax(pmin((dat$A21 - dat$A45A), (dat$A21 - dat$A45B), (dat$A21 - dat$A45C)),0)
  dat$PREC1 <- 1 - (dat$PR1 / dat$A00)
  dat$PREC2 <- dat$PR2 / dat$A21
  dat$PREC3 <- dat$PR2 / (dat$PR1 + dat$PR2)
  dat$PREC <- (dat$PREC1 + dat$PREC2 + dat$PREC3) / 3
  
  # initialize with NA
  dat$RECALL <- NA
  
  # if A21 == 0 --> not ped recall (b/c no walk)
  dat$RECALL[dat$A21==0 & !is.na(dat$A21) & is.na(dat$RECALL)] <- FALSE
  
  # if A90 == 0 --> no ped
  # and if A21 > 1 --> yes ped recall (b/c walk with no ped)
  dat$RECALL[dat$A90==0 & !is.na(dat$A90) & dat$A21>1 & is.na(dat$RECALL)] <- TRUE
  # and if A21 == 1 and A00 == 1 --> yes ped recall (b/c walk = phase but no ped)
  dat$RECALL[dat$A90==0 & !is.na(dat$A90) & dat$A21==1 & dat$A00==1 & is.na(dat$RECALL)] <- TRUE
  # and if A21 == 1 and A00 > 1 --> no ped recall (assume ped at end of previous hour)
  dat$RECALL[dat$A90==0 & !is.na(dat$A90) & dat$A21==1 & dat$A00>1 & is.na(dat$RECALL)] <- FALSE
  
  # if (A00 - A21) <= 0 --> equal or more walk than phase
  # and if (A21 - A45) > 1 --> yes ped recall (b/c fewer pedact than walk)
  dat$RECALL[dat$PR1==0 & !is.na(dat$PR1) & dat$PR2>1 & !is.na(dat$PR2) & is.na(dat$RECALL)] <- TRUE
  
  # if (A21 - A45) %in% c(0,1) --> nearly equal walk and pedact
  # and if (A00 - A21) > 1 --> no ped recall (b/c fewer walk than phase)
  dat$RECALL[dat$PR2 %in% c(0,1) & !is.na(dat$PR2) & dat$PR1>1 & !is.na(dat$PR1) & is.na(dat$RECALL)] <- FALSE
  
  # if PR1 >= 1 and if PR2 > 1
  # and if PREC > 0.25 for all three --> ped recall (b/c )
  dat$RECALL[dat$PR1>=1 & dat$PR2>1 & dat$PREC1>0.25 & dat$PREC2>0.25 & dat$PREC3>0.25 & dat$SIGNAL!=8302 & !is.na(dat$A90) & is.na(dat$RECALL)] <- TRUE
  
  # if PR1 <= 1 and if PR2 <= 1
  # and if TDIFF == 60 and if A00 < 20 and if A21 < 20 --> no ped recall (b/c few phases)
  dat$RECALL[dat$PR1 %in% c(0,1) & dat$PR2 %in% c(0,1) & dat$TDIFF==60 & dat$A00<20 & dat$A21<20 & !is.na(dat$A90) & is.na(dat$RECALL)] <- FALSE
  
  # for remainder, use model
  mod <- glm(RECALL ~ A00 + A21 + A45B, family="binomial", data=dat) # ; summary(mod)
  dat$PROB <- predict(mod, dat, type="response")
  dat$RECALL[is.na(dat$RECALL)] <- dat$PROB[is.na(dat$RECALL)] >= 0.50
  rm(mod)
  
  # remove columns
  dat[,c("PR0", "PR1", "PR2", "PREC1", "PREC2", "PREC3", "PREC", "PROB")] <- NULL
  
  # return
  return(dat$RECALL)
}

# Determine missing data
# assign values to is.na(TDMAX)
summary(temp$TDMAX)
trows1 <- which(is.na(temp$TDMAX))
trows <- sort(unique(c(trows1-2, trows1-1, trows1, trows1+1, trows1+2)))
temp[trows,]
rm(trows1, trows)
if (nrow(temp)>1) {
  # forward assignment
  for (j in 2:nrow(temp)) {
    if(is.na(temp$TDMAX[j])) {
      if(!is.na(temp$TDMAX[j-1])) {
        temp$TDMAX[j] <- temp$TDMAX[j-1]
      }
    }
  }; rm(j)
  # backward assignment (if NA at start)
  for (j in (nrow(temp)-1):1) {
    if(is.na(temp$TDMAX[j])) {
      if(!is.na(temp$TDMAX[j+1])) {
        temp$TDMAX[j] <- temp$TDMAX[j+1]
      }
    }
  }; rm(j)
}
# add hour column
temp$HOUR <- as.integer(format(temp$TIME1, "%H"))
# determine missing data
temp$MISS <- F
for (j in 1:nrow(tdmiss)) {
  temp$MISS[which(temp$HOUR==tdmiss$HOUR[j] & temp$TDMAX>=tdmiss$TDMISS[j])] <- T
}; rm(j)

# Determine if on ped recall
temp$RECALL <- suppressWarnings(getPedRecall(temp))

# Calculate approximate average cycle length
temp$CYCLE <- (temp$TDIFF / temp$A00)

# Determine signal typology or HAWk, model to apply
temp$HAWK <- signals$HAWK[signals$SIGNAL==mysig]
temp$GROUP <- signals$GROUP[signals$SIGNAL==mysig]

# Apply factoring methods
temp$PED <- predict(mod, temp)

# Inspect
str(temp)
summary(temp)

# Rename
dat_est <- temp

########################################
# Finish

# Save
write.csv(dat_hr, paste0(mysig, " hr.csv"), row.names=F)
write.csv(dat_est, paste0(mysig, " hr est.csv"), row.names=F)

# Remove
rm(models, mod, signals, datavg, tdmiss, mysig)
rm(getPedRecall)
rm(avgsig, getsig, seqsig, subsig, tabsig, tabsigseq, tabsigtdif, timesigmiss)
rm(temp, tempa)
rm(dat_raw, dat_hr, dat_est)
gc()

########################################
# END
########################################