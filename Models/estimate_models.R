########################################
# Project:  UDOT-18.602 Traffic signal pedestrians
# Authors:  Patrick Singleton (patrick.singleton@usu.edu)
# File:     estimate_models.R
# Date:     2020 Summer
# About:    Estimate models (factoring) predicting pedestrian volumes from signal data
########################################

########################################
# Notes

# Library
# library("")

########################################
# Load data

# videos
load(file=file.path("..", "Data", "videos_all.RData"))
# events
load(file=file.path("..", "Data", "events_all.RData"))
# combined
load(file=file.path("..", "Data", "dat.RData"))

# Save temporary
temp_dat <- dat
# dat <- temp_dat

# Get averaged data
datavg <- readRDS("dat_ts_wdhr.rds")

# Inspect
summary(dat)

########################################
# Descriptives

# inspect
summary(events_all)
summary(videos_all)

# number of signals = 90
length(unique(events_all$SIGNALID))
length(unique(videos_all$SIGNALID))
# number of crossings at signals = 320
length(unique(paste(dat$SIGNAL, dat$P)))

# total hours of video = 10,828.75
sum(difftime(videos_all$TIME2, videos_all$TIME1, units="hours"))
# total crossing hours = 24,085.49
sum(difftime(dat$TIME2, dat$TIME1, units="hours"))

# totals of people
colSums(events_all[,c("PED", "XPED", "XDUP", "CPED")])
table(events_all$BOWHAT)
sum(events_all$BONUM[events_all$BOWHAT=="BIKE"])
sum(events_all$BONUM[events_all$BOWHAT=="SCOOT"])
sum(events_all$BONUM[events_all$BOWHAT=="SKATE"])
sum(events_all$BONUM[events_all$BOWHAT=="WHEEL"])
sum(events_all$BONUM[events_all$BOWHAT=="PEDOUT"])
sum(events_all$BONUM[events_all$BOWHAT=="OTHER"])
colSums(dat[,c("PED", "BIKE", "SCOOT", "SKATE", "WHEEL", "PEDOUT", "OTHER")])

########################################
# Add variables

# Create groups based on clusters
# A45B
tdatavg <- datavg[["A45B"]]
tdatavg <- tdatavg[,names(which(colSums(datavg$N[4:ncol(datavg$N)] >= 8)==168))]
tdatavg <- tdatavg[,colSums(is.na(tdatavg))==0]
tdatavg <- tdatavg[,colSums(is.infinite(as.matrix(tdatavg)))==0]
tdatavg <- tdatavg[,colSums(tdatavg!=0)>0]
# 2 --> split: 350
grp1 <- as.integer(gsub("sig", "", names(tdatavg)[which(colSums(tdatavg)/7 > 350)]))
grp2 <- as.integer(gsub("sig", "", names(tdatavg)[which(colSums(tdatavg)/7 <= 350)]))
dat$GROUP <- ifelse(dat$SIGNAL %in% grp1, 1L, ifelse(dat$SIGNAL %in% grp2, 2L, NA))
rm(grp1, grp2, tdatavg)

# Add hawk signals
dat$HAWK <- ifelse(dat$SIGNAL %in% c(1801, 1803, 5260, 7475, 7719), T, F)

# Add ped recall
source("pedrecall.R")

# Construct other independent variables
dat$CYCLE <- (dat$TDIFF / dat$A00)

# Construct other dependent variables
dat$PEDall <- apply(dat[,c("PED","SKATE","WHEEL")],1,sum)
dat$SIDEall <- apply(dat[,c("PED","BIKE","SCOOT","SKATE","WHEEL")],1,sum)

########################################
# Remove rows

# Remove missing data
# NAs on A45/A90/A45A/A90A
dat <- dat[!is.na(dat$A90),]
summary(dat)

# Remove outliers
# on A45/A90/A45A/A90A
head(dat[order(dat$A45, decreasing=T),], 25)
head(dat[order(dat$A45B, decreasing=T),], 25)
head(dat[order(dat$A90, decreasing=T),], 25)
head(dat[order(dat$A90A, decreasing=T),], 25)
# remove b/c malfunctioning ped push-button
dat <- dat[!(dat$SIGNAL==8208 & dat$P==6L & dat$TIME1==as.POSIXct("2019-04-10 00:00:00", tz="America/Denver")),]
dat <- dat[!(dat$SIGNAL==8208 & dat$P==6L & dat$TIME1==as.POSIXct("2019-04-10 01:00:00", tz="America/Denver")),]
dat <- dat[!(dat$SIGNAL==8208 & dat$P==6L & dat$TIME1==as.POSIXct("2019-04-10 02:00:00", tz="America/Denver")),]
dat <- dat[!(dat$SIGNAL==8208 & dat$P==6L & dat$TIME1==as.POSIXct("2019-04-10 03:00:00", tz="America/Denver")),]
# on PED
head(dat[order(dat$PED, decreasing=T),], 25)
# remove b/c large music event at Vivent arena
dat <- dat[!(dat$SIGNAL==7126 & dat$TIME1==as.POSIXct("2019-11-20 14:00:00", tz="America/Denver")),]
dat <- dat[!(dat$SIGNAL==7126 & dat$TIME1==as.POSIXct("2019-11-20 15:00:00", tz="America/Denver")),]
dat <- dat[!(dat$SIGNAL==7126 & dat$TIME1==as.POSIXct("2019-11-20 16:00:00", tz="America/Denver")),]
dat <- dat[!(dat$SIGNAL==7126 & dat$TIME1==as.POSIXct("2019-11-20 17:00:00", tz="America/Denver")),]
dat <- dat[!(dat$SIGNAL==7126 & dat$TIME1==as.POSIXct("2019-11-20 18:00:00", tz="America/Denver")),]
dat <- dat[!(dat$SIGNAL==7126 & dat$TIME1==as.POSIXct("2019-11-20 19:00:00", tz="America/Denver")),]
dat <- dat[!(dat$SIGNAL==7126 & dat$TIME1==as.POSIXct("2019-11-20 20:00:00", tz="America/Denver")),]
dat <- dat[!(dat$SIGNAL==7126 & dat$TIME1==as.POSIXct("2019-11-20 21:00:00", tz="America/Denver")),]
dat <- dat[!(dat$SIGNAL==7126 & dat$TIME1==as.POSIXct("2019-11-20 22:00:00", tz="America/Denver")),]
# remove b/c large sports event at Vivent arena
dat <- dat[!(dat$SIGNAL==7126 & dat$TIME1==as.POSIXct("2019-11-22 17:00:00", tz="America/Denver")),]
dat <- dat[!(dat$SIGNAL==7126 & dat$TIME1==as.POSIXct("2019-11-22 18:00:00", tz="America/Denver")),]
dat <- dat[!(dat$SIGNAL==7126 & dat$TIME1==as.POSIXct("2019-11-22 19:00:00", tz="America/Denver")),]
dat <- dat[!(dat$SIGNAL==7126 & dat$TIME1==as.POSIXct("2019-11-22 20:00:00", tz="America/Denver")),]
dat <- dat[!(dat$SIGNAL==7126 & dat$TIME1==as.POSIXct("2019-11-22 21:00:00", tz="America/Denver")),]
dat <- dat[!(dat$SIGNAL==7126 & dat$TIME1==as.POSIXct("2019-11-22 22:00:00", tz="America/Denver")),]
# on PED
head(dat[order(dat$PED, decreasing=T),], 25)
# mostly 7126, 1801, 8302 --> okay

# Remove short time periods
summary(as.numeric(dat$TDIFF))
table(dat$TDIFF<15); table(dat$TDIFF<30); table(dat$TDIFF<45)
dat <- dat[dat$TDIFF>=45,]

########################################
# Inspect and prepare

# Inspect
summary(dat)

# Initialize
models <- list()
modelfits <- data.frame(model=character(), rmse=numeric(), mae=numeric(), cor=numeric(), stringsAsFactors=F)

########################################
# HAWK signals, all crossings

# subset
tdat <- dat[dat$HAWK==T,]

# initialize
tdatp <- data.frame(one=0:max(tdat[,"A90C"]))
names(tdatp)[1] <- "A90C"

# quadratic: Y = b1*X + b2*X^2
mod <- lm(PEDall ~ 0 + A90C + I(A90C^2), tdat)
tdatp$quadratic <- predict(mod, tdatp)
mod_fit <- mod$fitted.values
mod_res <- tdat$PEDall - mod_fit
modelfits[1,] <- list("hawk quadratic", sqrt(mean(mod_res^2)), mean(abs(mod_res)), cor(tdat$PEDall, mod_fit))
models[[1]] <- mod

# model
summary(models[[1]])
modelfits[1,]
tdatp

# plot
# residuals
plot(tdat$A90C, mod_res, pch=16, col=rgb(0,0,0,alpha=0.1), las=1, ylab="Pedestrian crossing volume", xlab="A90C", type="p")
points(c(0,max(tdat$A90C)), c(0,0), type="l", col="black")
title(main="Hawk signals, all crossings\nResiduals")
legend("topleft", c(paste0("observations (N = ", nrow(tdat), ")"), "quadratic model"), 
       col=c(rgb(0,0,0,alpha=0.25), "black", "black"), 
       pch=c(16, rep(NA,2)), lty=c(NA, rep("solid",2)))
# model
plot(tdat$A90C, tdat$PEDall, pch=16, col=rgb(0,0,0,alpha=0.1), las=1, ylab="Pedestrian crossing volume", xlab="A90C", type="p")
# plot(tdat$A90C, tdat$PEDall, pch=16, col=rgb(0,0,0,alpha=0.1), las=1, ylab="Pedestrian crossing volume", xlab="A90C", type="p", xlim=c(0,20), ylim=c(0,75))
points(tdatp$A90C, tdatp$quadratic, type="l", col="black")
title(main="Hawk signals, all crossings")
legend("topleft", c(paste0("observations (N = ", nrow(tdat), ")"), "quadratic model"), 
       col=c(rgb(0,0,0,alpha=0.25), "black", "black"), 
       pch=c(16, rep(NA,2)), lty=c(NA, rep("solid",2)))

# cleanup
rm(mod, mod_fit, mod_res)
rm(tdat, tdatp)

########################################
# High-volume signals, crossings with pedestrian recall

# subset
tdat <- dat[dat$GROUP==1 & !is.na(dat$GROUP) & dat$RECALL==T,]

# initialize
tdatp <- data.frame(one=0:max(tdat[,"A45B"]))
names(tdatp)[1] <- "A45B"

# quadratic: Y = b1*X + b2*X^2
mod <- lm(PEDall ~ 0 + A45B + I(A45B^2), tdat)
tdatp$quadratic <- predict(mod, tdatp)
mod_fit <- mod$fitted.values
mod_res <- tdat$PEDall - mod_fit
modelfits[2,] <- list("high pedrecall quadratic", sqrt(mean(mod_res^2)), mean(abs(mod_res)), cor(tdat$PEDall, mod_fit))
models[[2]] <- mod

# model
summary(models[[2]])
modelfits[2,]
tdatp

# plot
# residuals
plot(tdat$A45B, mod_res, pch=16, col=rgb(0,0,0,alpha=0.1), las=1, ylab="Pedestrian crossing volume", xlab="A45B", type="p")
points(c(0,max(tdat$A45B)), c(0,0), type="l", col="black")
title(main="High-activity signals, crossings with pedestrian recall\nResiduals")
legend("topleft", c(paste0("observations (N = ", nrow(tdat), ")"), "quadratic model"), 
       col=c(rgb(0,0,0,alpha=0.25), "black", "black"), 
       pch=c(16, rep(NA,2)), lty=c(NA, rep("solid",2)))
# model
plot(tdat$A45B, tdat$PEDall, pch=16, col=rgb(0,0,0,alpha=0.1), las=1, ylab="Pedestrian crossing volume", xlab="A45B", type="p")
# plot(tdat$A45B, tdat$PEDall, pch=16, col=rgb(0,0,0,alpha=0.1), las=1, ylab="Pedestrian crossing volume", xlab="A45B", type="p", xlim=c(0,20), ylim=c(0,75))
points(tdatp$A45B, tdatp$quadratic, type="l", col="black")
title(main="High-activity signals, crossings with pedestrian recall")
legend("topleft", c(paste0("observations (N = ", nrow(tdat), ")"), "quadratic model"), 
       col=c(rgb(0,0,0,alpha=0.25), "black", "black"), 
       pch=c(16, rep(NA,2)), lty=c(NA, rep("solid",2)))

# cleanup
rm(mod, mod_fit, mod_res)
rm(tdat, tdatp)

########################################
# Low-volume signals, crossings with pedestrian recall

# subset
tdat <- dat[dat$GROUP==2 & !is.na(dat$GROUP) & dat$RECALL==T,]

# initialize
tdatp <- data.frame(one=0:max(tdat[,"A45B"]))
names(tdatp)[1] <- "A45B"

# quadratic: Y = b1*X + b2*X^2
mod <- lm(PEDall ~ 0 + A45B + I(A45B^2), tdat)
tdatp$quadratic <- predict(mod, tdatp)
mod_fit <- mod$fitted.values
mod_res <- tdat$PEDall - mod_fit
modelfits[3,] <- list("low pedrecall quadratic", sqrt(mean(mod_res^2)), mean(abs(mod_res)), cor(tdat$PEDall, mod_fit))
models[[3]] <- mod

# model
summary(models[[3]])
modelfits[3,]
tdatp

# plot
# residuals
plot(tdat$A45B, mod_res, pch=16, col=rgb(0,0,0,alpha=0.1), las=1, ylab="Pedestrian crossing volume", xlab="A45B", type="p")
points(c(0,max(tdat$A45B)), c(0,0), type="l", col="black")
title(main="Low-activity signals, crossings with pedestrian recall\nResiduals")
legend("topleft", c(paste0("observations (N = ", nrow(tdat), ")"), "quadratic model"), 
       col=c(rgb(0,0,0,alpha=0.25), "black", "black"), 
       pch=c(16, rep(NA,2)), lty=c(NA, rep("solid",2)))
# model
plot(tdat$A45B, tdat$PEDall, pch=16, col=rgb(0,0,0,alpha=0.1), las=1, ylab="Pedestrian crossing volume", xlab="A45B", type="p")
# plot(tdat$A45B, tdat$PEDall, pch=16, col=rgb(0,0,0,alpha=0.1), las=1, ylab="Pedestrian crossing volume", xlab="A45B", type="p", xlim=c(0,20), ylim=c(0,75))
points(tdatp$A45B, tdatp$quadratic, type="l", col="black")
title(main="Low-activity signals, crossings with pedestrian recall")
legend("topleft", c(paste0("observations (N = ", nrow(tdat), ")"), "quadratic model"), 
       col=c(rgb(0,0,0,alpha=0.25), "black", "black"), 
       pch=c(16, rep(NA,2)), lty=c(NA, rep("solid",2)))

# cleanup
rm(mod, mod_fit, mod_res)
rm(tdat, tdatp)

########################################
# All signals, crossings without pedestrian recall, cycle length < 1.5 min
# All signals, crossings without pedestrian recall, cycle length >= 1.5 min

# subset
tdat <- dat[dat$HAWK==F & dat$RECALL==F,]

# initialize
tdatp <- data.frame(A90C=0:max(tdat[,"A90C"]))
tdatp1 <- tdatp; tdatp1$CYCLE=1
tdatp2 <- tdatp; tdatp2$CYCLE=2

# test break points
test <- data.frame(br1=tdatp$A90C, loglik=0, rmse=0)
for (i in 1:nrow(test)) {
  tmod <- lm(PEDall ~ 0 + A90C + I((A90C - test$br1[i])*(A90C > test$br1[i])):I(CYCLE>=1.5), tdat)
  test$loglik[i] <- logLik(tmod)
  test$rmse[i] <- sd(tmod$residuals)
  rm(tmod)
}; rm(i)
head(test[order(test$loglik, decreasing=T),],10)
head(test[order(test$rmse, decreasing=F),],10)
plot(test$br1, test$loglik, pch=20, type="b")
# first local max = 28
rm(test)

# piecewise linear 1: Y = b1*X + b2*X*(X>br1)
mod <- lm(PEDall ~ 0 + A90C + I((A90C-28)*(A90C > 28)):I(CYCLE>=1.5), tdat)
tdatp$short <- predict(mod, tdatp1)
tdatp$long <- predict(mod, tdatp2)
mod_fit <- mod$fitted.values
mod_res <- tdat$PEDall - mod_fit
modelfits[6,] <- list("all notrecall cycle piecewise", sqrt(mean(mod_res^2)), mean(abs(mod_res)), cor(tdat$PEDall, mod_fit))
models[[6]] <- mod

# model
summary(models[[6]])
modelfits[6,]
tdatp

# plot
# residuals
plot(tdat$A90C, mod_res, pch=16, col=rgb(0,0,0,alpha=0.1), las=1, ylab="Pedestrian crossing volume", xlab="A90C", type="p")
points(c(0,max(tdat$A90C)), c(0,0), type="l", col="black")
title(main="Crossings without pedestrian recall, cycle length < vs. >= 1.5 min\nResiduals")
legend("topleft", c(paste0("observations (N = ", nrow(tdat), ")"), "piecewise models"), 
       col=c(rgb(0,0,0,alpha=0.25), "black", "black"), 
       pch=c(16, rep(NA,2)), lty=c(NA, rep("solid",2)))
# model
plot(tdat$A90C, tdat$PEDall, pch=16, col=rgb(0,0,0,alpha=0.1), las=1, ylab="Pedestrian crossing volume", xlab="A90C", type="p")
# plot(tdat$A90C, tdat$PEDall, pch=16, col=rgb(0,0,0,alpha=0.1), las=1, ylab="Pedestrian crossing volume", xlab="A90C", type="p", xlim=c(0,20), ylim=c(0,75))
points(tdatp$A90C, tdatp$short, type="l", col="black")
points(tdatp$A90C, tdatp$long, type="l", col="black")
title(main="Crossings without pedestrian recall, cycle length < vs. >= 1.5 min")
legend("topleft", c(paste0("observations (N = ", nrow(tdat), ")"), "piecewise models"), 
       col=c(rgb(0,0,0,alpha=0.25), "black", "black"), 
       pch=c(16, rep(NA,2)), lty=c(NA, rep("solid",2)))

# cleanup
rm(mod, mod_fit, mod_res)
rm(tdat, tdatp, tdatp1, tdatp2)

# All signals, crossings without pedestrian recall, cycle length < 1.5 min

# subset
tdat <- dat[dat$HAWK==F & dat$RECALL==F & dat$CYCLE<1.5,]

# initialize
tdatp <- data.frame(A90C=0:max(tdat[,"A90C"]))

# piecewise linear 1: Y = b1*X + b2*X*(X>br1)
mod <- lm(PEDall ~ 0 + offset(models[[6]]$coefficients[1]*A90C) + I((A90C-28)*(A90C > 28)), tdat)
tdatp$piecewise <- predict(mod, tdatp)
mod_fit <- mod$fitted.values
mod_res <- tdat$PEDall - mod_fit
modelfits[4,] <- list("all notrecall shortcycle piecewise", sqrt(mean(mod_res^2)), mean(abs(mod_res)), cor(tdat$PEDall, mod_fit))
models[[4]] <- mod

# model
summary(models[[4]])
modelfits[4,]
tdatp

# plot
# residuals
plot(tdat$A90C, mod_res, pch=16, col=rgb(0,0,0,alpha=0.1), las=1, ylab="Pedestrian crossing volume", xlab="A90C", type="p")
points(c(0,max(tdat$A90C)), c(0,0), type="l", col="black")
title(main="Crossings without pedestrian recall, cycle length < 1.5 min\nResiduals")
legend("topleft", c(paste0("observations (N = ", nrow(tdat), ")"), "piecewise models"), 
       col=c(rgb(0,0,0,alpha=0.25), "black", "black"), 
       pch=c(16, rep(NA,2)), lty=c(NA, rep("solid",2)))
# model
plot(tdat$A90C, tdat$PEDall, pch=16, col=rgb(0,0,0,alpha=0.1), las=1, ylab="Pedestrian crossing volume", xlab="A90C", type="p")
# plot(tdat$A90C, tdat$PEDall, pch=16, col=rgb(0,0,0,alpha=0.1), las=1, ylab="Pedestrian crossing volume", xlab="A90C", type="p", xlim=c(0,20), ylim=c(0,75))
points(tdatp$A90C, tdatp$piecewise, type="l", col="black")
title(main="Crossings without pedestrian recall, cycle length < 1.5 min")
legend("topleft", c(paste0("observations (N = ", nrow(tdat), ")"), "piecewise models"), 
       col=c(rgb(0,0,0,alpha=0.25), "black", "black"), 
       pch=c(16, rep(NA,2)), lty=c(NA, rep("solid",2)))

# cleanup
rm(mod, mod_fit, mod_res)
rm(tdat, tdatp)

# All signals, crossings without pedestrian recall, cycle length >= 1.5 min

# subset
tdat <- dat[dat$HAWK==F & dat$RECALL==F & dat$CYCLE>=1.5,]

# initialize
tdatp <- data.frame(A90C=0:max(tdat[,"A90C"]))

# piecewise linear 1: Y = b1*X + b2*X*(X>br1)
mod <- lm(PEDall ~ 0 + offset(models[[6]]$coefficients[1]*A90C) + I((A90C-28)*(A90C > 28)), tdat)
tdatp$piecewise <- predict(mod, tdatp)
mod_fit <- mod$fitted.values
mod_res <- tdat$PEDall - mod_fit
modelfits[5,] <- list("all notrecall longcycle piecewise", sqrt(mean(mod_res^2)), mean(abs(mod_res)), cor(tdat$PEDall, mod_fit))
models[[5]] <- mod

# model
summary(models[[5]])
modelfits[5,]
tdatp

# plot
# residuals
plot(tdat$A90C, mod_res, pch=16, col=rgb(0,0,0,alpha=0.1), las=1, ylab="Pedestrian crossing volume", xlab="A90C", type="p")
points(c(0,max(tdat$A90C)), c(0,0), type="l", col="black")
title(main="Crossings without pedestrian recall, cycle length >= 1.5 min\nResiduals")
legend("topleft", c(paste0("observations (N = ", nrow(tdat), ")"), "piecewise models"), 
       col=c(rgb(0,0,0,alpha=0.25), "black", "black"), 
       pch=c(16, rep(NA,2)), lty=c(NA, rep("solid",2)))
# model
plot(tdat$A90C, tdat$PEDall, pch=16, col=rgb(0,0,0,alpha=0.1), las=1, ylab="Pedestrian crossing volume", xlab="A90C", type="p")
# plot(tdat$A90C, tdat$PEDall, pch=16, col=rgb(0,0,0,alpha=0.1), las=1, ylab="Pedestrian crossing volume", xlab="A90C", type="p", xlim=c(0,20), ylim=c(0,75))
points(tdatp$A90C, tdatp$piecewise, type="l", col="black")
title(main="Crossings without pedestrian recall, cycle length >= 1.5 min")
legend("topleft", c(paste0("observations (N = ", nrow(tdat), ")"), "piecewise models"), 
       col=c(rgb(0,0,0,alpha=0.25), "black", "black"), 
       pch=c(16, rep(NA,2)), lty=c(NA, rep("solid",2)))

# cleanup
rm(mod, mod_fit, mod_res)
rm(tdat, tdatp)

########################################
# One model to encompass all models

# subset
tdat <- dat[,]

# initialize
tdatp <- data.frame(A45B=c(0:max(max(tdat[,"A45B"], tdat[,"A90C"]))), A90C=c(0:max(max(tdat[,"A45B"], tdat[,"A90C"]))))
tdatp1 <- tdatp; tdatp1$HAWK=T; tdatp1$GROUP=0; tdatp1$RECALL=F; tdatp1$CYCLE=0
tdatp2 <- tdatp; tdatp2$HAWK=F; tdatp2$GROUP=1; tdatp2$RECALL=T; tdatp2$CYCLE=0
tdatp3 <- tdatp; tdatp3$HAWK=F; tdatp3$GROUP=2; tdatp3$RECALL=T; tdatp3$CYCLE=0
tdatp4 <- tdatp; tdatp4$HAWK=F; tdatp4$GROUP=0; tdatp4$RECALL=F; tdatp4$CYCLE=1
tdatp5 <- tdatp; tdatp5$HAWK=F; tdatp5$GROUP=0; tdatp5$RECALL=F; tdatp5$CYCLE=2

# complete model
mod <- lm(PEDall ~ 0 + I(A90C*I(HAWK==T)) + I(I(A90C^2)*I(HAWK==T)) + I(A45B*I(GROUP==1 & !is.na(GROUP) & RECALL==T)) + I(I(A45B^2)*I(GROUP==1 & !is.na(GROUP) & RECALL==T)) + I(A45B*I(GROUP==2 & !is.na(GROUP) & RECALL==T)) + I(I(A45B^2)*I(GROUP==2 & !is.na(GROUP) & RECALL==T)) + I(A90C*I(HAWK==F & RECALL==F)) + I(I((A90C-28)*(A90C > 28))*I(HAWK==F & RECALL==F)):I(CYCLE>=1.5), tdat)
tdatp$M1 <- predict(mod, tdatp1)
tdatp$M2 <- predict(mod, tdatp2)
tdatp$M3 <- predict(mod, tdatp3)
tdatp$M4 <- predict(mod, tdatp4)
tdatp$M5 <- predict(mod, tdatp5)
mod_fit <- mod$fitted.values
mod_res <- tdat$PEDall - mod_fit
modelfits[7,] <- list("all models", sqrt(mean(mod_res^2)), mean(abs(mod_res)), cor(tdat$PEDall, mod_fit))
models[[7]] <- mod

# model
summary(models[[7]])
modelfits[7,]
tdatp

# plot
tdat1 <- tdat[tdat$HAWK==T | tdat$RECALL==F,]
tdat2 <- tdat[tdat$HAWK==F & tdat$RECALL==T,]
# residuals
plot(tdat2$A45B, mod_res[tdat$HAWK==F & tdat$RECALL==T], pch=16, col=rgb(0,0,0,alpha=0.1), las=1, ylab="Pedestrian crossing volume", xlab="A45B", type="p")
points(c(0,max(tdat2$A45B)), c(0,0), type="l", col="black")
title(main="Crossings with pedestrian recall\nResiduals")
legend("topleft", c(paste0("observations (N = ", nrow(tdat2), ")"), "models"), 
       col=c(rgb(0,0,0,alpha=0.25), "black", "black"), 
       pch=c(16, rep(NA,2)), lty=c(NA, rep("solid",2)))
# model
plot(tdat2$A45B, tdat2$PEDall, pch=16, col=rgb(0,0,0,alpha=0.1), las=1, ylab="Pedestrian crossing volume", xlab="A45B", type="p")
# plot(tdat2$A45B, tdat2$PEDall, pch=16, col=rgb(0,0,0,alpha=0.1), las=1, ylab="Pedestrian crossing volume", xlab="A45B", type="p", xlim=c(0,20), ylim=c(0,75))
points(tdatp$A45B, tdatp$M2, type="l", col="black")
points(tdatp$A45B, tdatp$M3, type="l", col="black")
title(main="Crossings with pedestrian recall")
legend("topleft", c(paste0("observations (N = ", nrow(tdat2), ")"), "models"), 
       col=c(rgb(0,0,0,alpha=0.25), "black", "black"), 
       pch=c(16, rep(NA,2)), lty=c(NA, rep("solid",2)))
# residuals
plot(tdat1$A90C, mod_res[tdat$HAWK==T | tdat$RECALL==F], pch=16, col=rgb(0,0,0,alpha=0.1), las=1, ylab="Pedestrian crossing volume", xlab="A90C", type="p")
points(c(0,max(tdat1$A90C)), c(0,0), type="l", col="black")
title(main="HAWK signals & crossings without pedestrian recall\nResiduals")
legend("topleft", c(paste0("observations (N = ", nrow(tdat1), ")"), "models"), 
       col=c(rgb(0,0,0,alpha=0.25), "black", "black"), 
       pch=c(16, rep(NA,2)), lty=c(NA, rep("solid",2)))
# model
plot(tdat1$A90C, tdat1$PEDall, pch=16, col=rgb(0,0,0,alpha=0.1), las=1, ylab="Pedestrian crossing volume", xlab="A90C", type="p")
# plot(tdat1$A90C, tdat1$PEDall, pch=16, col=rgb(0,0,0,alpha=0.1), las=1, ylab="Pedestrian crossing volume", xlab="A90C", type="p", xlim=c(0,20), ylim=c(0,75))
points(tdatp$A90C, tdatp$M1, type="l", col="black")
points(tdatp$A90C, tdatp$M4, type="l", col="black")
points(tdatp$A90C, tdatp$M5, type="l", col="black")
title(main="HAWK signals & crossings without pedestrian recall")
legend("topleft", c(paste0("observations (N = ", nrow(tdat1), ")"), "models"), 
       col=c(rgb(0,0,0,alpha=0.25), "black", "black"), 
       pch=c(16, rep(NA,2)), lty=c(NA, rep("solid",2)))

# cleanup
rm(tdat1, tdat2)
rm(tdatp1, tdatp2, tdatp3, tdatp4, tdatp5)
rm(mod, mod_fit, mod_res)
rm(tdat, tdatp)

########################################
# Save

# Inspect
modelfits

# Save
saveRDS(models, "models.rds")

########################################
# Cleanup

# Remove
rm(models, modelfits)
rm(dat, temp_dat, videos_all, events_all, clusters, datavg)
gc()

########################################
# END
########################################