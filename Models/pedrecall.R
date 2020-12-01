########################################
# Project:  UDOT-18.602 Traffic signal pedestrians
# Authors:  Patrick Singleton (patrick.singleton@usu.edu)
# File:     pedrecall.R
# Date:     2020 Summer
# About:    Construct ped recall
########################################

########################################
# Notes

# Use: call script from est_models.R
# assumes dat is the data frame, if not: 
# load(file.path("Data", "Combined", "dat.RData"))

########################################
# Construct ped recall

# contruct variables
dat$PR0 <- dat$A00==0
dat$PR1 <- pmax((dat$A00 - dat$A21), 0)
# dat$PR2 <- pmin(abs(dat$A21 - dat$A45), abs(dat$A21 - dat$A45A), abs(dat$A21 - dat$A45B), abs(dat$A21 - dat$A45C))
# dat$PR2 <- pmin(abs(dat$A21 - dat$A45A), abs(dat$A21 - dat$A45B), abs(dat$A21 - dat$A45C), dat$A21)
dat$PR2 <- pmax(pmin((dat$A21 - dat$A45A), (dat$A21 - dat$A45B), (dat$A21 - dat$A45C)),0)
dat$PREC1 <- 1 - (dat$PR1 / dat$A00)
dat$PREC2 <- dat$PR2 / dat$A21
dat$PREC3 <- dat$PR2 / (dat$PR1 + dat$PR2)
dat$PREC <- (dat$PREC1 + dat$PREC2 + dat$PREC3) / 3
# summary(dat$PR1); hist(dat$PR1)
# summary(dat$PR2); hist(dat$PR2)
# summary(dat$PREC1); hist(dat$PREC1)
# summary(dat$PREC2); hist(dat$PREC2)
# summary(dat$PREC3); hist(dat$PREC3)
# summary(dat$PREC); hist(dat$PREC)

# initialize with NA
dat$RECALL <- NA

# if A21 == 0 --> not ped recall (b/c no walk)
# tdat <- dat[dat$A21==0 & !is.na(dat$A21),]
dat$RECALL[dat$A21==0 & !is.na(dat$A21) & is.na(dat$RECALL)] <- FALSE

# if A90 == 0 --> no ped
# and if A21 > 1 --> yes ped recall (b/c walk with no ped)
# tdat <- dat[dat$A90==0 & !is.na(dat$A90) & dat$A21>1 & is.na(dat$RECALL),]
dat$RECALL[dat$A90==0 & !is.na(dat$A90) & dat$A21>1 & is.na(dat$RECALL)] <- TRUE
# and if A21 == 1 and A00 == 1 --> yes ped recall (b/c walk = phase but no ped)
# tdat <- dat[dat$A90==0 & !is.na(dat$A90) & dat$A21==1 & dat$A00==1 & is.na(dat$RECALL),]
dat$RECALL[dat$A90==0 & !is.na(dat$A90) & dat$A21==1 & dat$A00==1 & is.na(dat$RECALL)] <- TRUE
# and if A21 == 1 and A00 > 1 --> no ped recall (assume ped at end of previous hour)
# tdat <- dat[dat$A90==0 & !is.na(dat$A90) & dat$A21==1 & dat$A00>1 & is.na(dat$RECALL),]
dat$RECALL[dat$A90==0 & !is.na(dat$A90) & dat$A21==1 & dat$A00>1 & is.na(dat$RECALL)] <- FALSE

# if (A00 - A21) <= 0 --> equal or more walk than phase
# and if (A21 - A45) > 1 --> yes ped recall (b/c fewer pedact than walk)
# tdat <- dat[dat$PR1==0 & !is.na(dat$PR1) & dat$PR2>1 & !is.na(dat$PR2) & is.na(dat$RECALL),]
dat$RECALL[dat$PR1==0 & !is.na(dat$PR1) & dat$PR2>1 & !is.na(dat$PR2) & is.na(dat$RECALL)] <- TRUE

# if (A21 - A45) %in% c(0,1) --> nearly equal walk and pedact
# and if (A00 - A21) > 1 --> no ped recall (b/c fewer walk than phase)
# tdat <- dat[dat$PR2 %in% c(0,1) & !is.na(dat$PR2) & dat$PR1>1 & !is.na(dat$PR1) & is.na(dat$RECALL),]
dat$RECALL[dat$PR2 %in% c(0,1) & !is.na(dat$PR2) & dat$PR1>1 & !is.na(dat$PR1) & is.na(dat$RECALL)] <- FALSE

# if PR1 >= 1 and if PR2 > 1
# and if PREC > 0.25 for all three --> ped recall (b/c )
# tdat <- dat[dat$PR1>=1 & dat$PR2>1 & dat$PREC1>0.25 & dat$PREC2>0.25 & dat$PREC3>0.25 & dat$SIGNAL!=8302 & !is.na(dat$A90) & is.na(dat$RECALL),]
dat$RECALL[dat$PR1>=1 & dat$PR2>1 & dat$PREC1>0.25 & dat$PREC2>0.25 & dat$PREC3>0.25 & dat$SIGNAL!=8302 & !is.na(dat$A90) & is.na(dat$RECALL)] <- TRUE

# if PR1 <= 1 and if PR2 <= 1
# and if TDIFF == 60 and if A00 < 20 and if A21 < 20 --> no ped recall (b/c few phases)
# tdat <- dat[dat$PR1 %in% c(0,1) & dat$PR2 %in% c(0,1) & dat$TDIFF==60 & dat$A00<20 & dat$A21<20 & !is.na(dat$A90) & is.na(dat$RECALL),]
dat$RECALL[dat$PR1 %in% c(0,1) & dat$PR2 %in% c(0,1) & dat$TDIFF==60 & dat$A00<20 & dat$A21<20 & !is.na(dat$A90) & is.na(dat$RECALL)] <- FALSE

# for remainder, use model
mod <- glm(RECALL ~ A00 + A21 + A45B, family="binomial", data=dat) # ; summary(mod)
dat$PROB <- predict(mod, dat, type="response")
dat$RECALL[is.na(dat$RECALL)] <- dat$PROB[is.na(dat$RECALL)] >= 0.50
rm(mod)

# # inspect one-by-one
# for (i in unique(dat$FOLDER)) {
#   tdat <- dat[dat$FOLDER==i,]
#   for (j in unique(tdat$P)) {
#     tdata <- tdat[tdat$P==j,]
#     print(tdata[,c("FOLDER", "TIME1", "P", "A00", "A21", "A45", "A90", "RECALL")])
#     readline(prompt="Press [enter] to continue")
#     rm(tdata)
#   }; rm(j)
#   rm(tdat)
# }; rm(i)

# # inspect for common issues
# for (i in unique(dat$FOLDER)) {
#   tdat <- dat[dat$FOLDER==i,]
#   for (j in unique(tdat$P)) {
#     tdata <- tdat[tdat$P==j & !is.na(tdat$P),]
#     if (length(table(tdata$RECALL)) == 2) {
#       tdata$P1 <- c(tdata$RECALL[2:nrow(tdata)], NA)
#       tdata$P2 <- c(tdata$RECALL[3:nrow(tdata)], NA, NA)
#       tdata$N1 <- c(NA, tdata$RECALL[1:(nrow(tdata)-1)])
#       tdata$N2 <- c(NA, NA, tdata$RECALL[1:(nrow(tdata)-2)])
#       tdata$PN1 <- 2*rowSums(data.frame(tdata$RECALL==tdata$P1, tdata$RECALL==tdata$N1), na.rm=T) + rowSums(data.frame(tdata$RECALL==tdata$P2, tdata$RECALL==tdata$N2), na.rm=T)
#       tdata$PN2 <- 2*rowSums(data.frame(!is.na(tdata$P1), !is.na(tdata$N1))) + rowSums(data.frame(!is.na(tdata$P2), !is.na(tdata$N2)))
#       tdata$PN <- tdata$PN1 / tdata$PN2
#       if (min(tdata$PN, na.rm=T) < 0.50) {
#         print(tdata[,c("FOLDER", "TIME1", "TIME2", "P", "A00", "A21", "A45", "A90", "RECALL")])
#         readline(prompt="Press [enter] to continue")
#       }
#     }
#     rm(tdata)
#   }; rm(j)
#   rm(tdat)
# }; rm(i)

# fix errors
dat$RECALL[which(dat$FOLDER=="2019-01-29 5306" & dat$TIME1=="2019-01-29 11:00:00" & dat$P==2L)] <- TRUE
dat$RECALL[which(dat$FOLDER=="2019-01-29 5306" & dat$TIME1=="2019-01-30 11:00:00" & dat$P==2L)] <- TRUE
dat$RECALL[which(dat$FOLDER=="2019-01-29 5306" & dat$TIME1=="2019-01-30 13:00:00" & dat$P==2L)] <- TRUE
dat$RECALL[which(dat$FOLDER=="2019-01-29 5306" & dat$TIME1=="2019-01-30 15:00:00" & dat$P==2L)] <- TRUE
dat$RECALL[which(dat$FOLDER=="2019-01-29 5306" & dat$TIME1=="2019-02-01 12:00:00" & dat$P==2L)] <- TRUE
dat$RECALL[which(dat$FOLDER=="2019-01-30 8119" & dat$TIME1=="2019-01-31 06:00:00" & dat$P==4L)] <- FALSE
dat$RECALL[which(dat$FOLDER=="2019-02-08 5305" & dat$TIME1=="2019-02-08 17:00:00" & dat$P==6L)] <- TRUE
dat$RECALL[which(dat$FOLDER=="2019-02-14 7184" & dat$TIME1=="2019-02-14 05:58:06" & dat$P==6L)] <- TRUE
dat$RECALL[which(dat$FOLDER=="2019-03-08 7086 c" & dat$TIME1=="2019-03-08 15:58:38" & dat$P==2L)] <- FALSE
dat$RECALL[which(dat$FOLDER=="2019-03-08 7086 c" & dat$TIME1=="2019-03-08 15:58:38" & dat$P==8L)] <- FALSE
dat$RECALL[which(dat$FOLDER=="2019-03-22 1229" & dat$TIME1=="2019-03-25 09:00:00" & dat$P==2L)] <- TRUE
dat$RECALL[which(dat$FOLDER=="2019-03-25 7218" & dat$TIME1=="2019-03-25 09:56:02" & dat$P==6L)] <- TRUE
dat$RECALL[which(dat$FOLDER=="2019-03-25 8113" & dat$TIME1=="2019-03-25 09:57:07" & dat$P==8L)] <- FALSE
dat$RECALL[which(dat$FOLDER=="2019-03-29 6407" & dat$TIME1=="2019-04-01 13:00:00" & dat$P==6L)] <- TRUE
dat$RECALL[which(dat$FOLDER=="2019-03-29 6446" & dat$TIME1=="2019-03-31 18:00:00" & dat$P==8L)] <- FALSE
dat$RECALL[which(dat$FOLDER=="2019-03-29 6446" & dat$TIME1=="2019-04-01 13:00:00" & dat$P==6L)] <- TRUE
dat$RECALL[which(dat$FOLDER=="2019-04-01 6146" & dat$TIME1=="2019-04-03 12:00:00" & dat$P==6L)] <- FALSE
dat$RECALL[which(dat$FOLDER=="2019-04-05 5363" & dat$TIME1=="2019-04-05 17:00:00" & dat$P==6L)] <- TRUE
dat$RECALL[which(dat$FOLDER=="2019-04-16 7041" & dat$TIME1=="2019-04-18 15:00:00" & dat$P==8L)] <- FALSE
dat$RECALL[which(dat$FOLDER=="2019-06-13 4024" & dat$TIME1=="2019-06-13 08:57:14" & dat$P==2L)] <- FALSE
dat$RECALL[which(dat$FOLDER=="2019-06-13 4024" & dat$TIME1=="2019-06-13 08:57:14" & dat$P==8L)] <- FALSE
dat$RECALL[which(dat$FOLDER=="2019-06-24 7110" & dat$TIME1=="2019-06-25 17:34:00" & dat$P==8L)] <- FALSE
dat$RECALL[which(dat$FOLDER=="2019-07-08 5024" & dat$TIME1=="2019-07-08 08:56:39" & dat$P==2L)] <- TRUE
dat$RECALL[which(dat$FOLDER=="2019-07-08 5030" & dat$TIME1=="2019-07-08 08:57:14" & dat$P==8L)] <- FALSE
dat$RECALL[which(dat$FOLDER=="2019-07-08 6038" & dat$TIME1=="2019-07-08 08:57:45" & dat$P==6L)] <- TRUE
dat$RECALL[which(dat$FOLDER=="2019-07-17 7126" & dat$TIME1=="2019-07-19 16:00:00" & dat$P==6L)] <- TRUE
dat$RECALL[which(dat$FOLDER=="2019-07-19 7126" & dat$TIME1=="2019-07-19 21:56:00" & dat$P==2L)] <- TRUE
dat$RECALL[which(dat$FOLDER=="2019-07-19 7126" & dat$TIME1=="2019-07-20 16:53:00" & dat$P==2L)] <- TRUE
dat$RECALL[which(dat$FOLDER=="2019-07-23 5393" & dat$TIME1=="2019-07-25 10:00:00" & dat$P==4L)] <- FALSE
dat$RECALL[which(dat$FOLDER=="2019-07-23 5393" & dat$TIME1=="2019-07-25 12:00:00" & dat$P==4L)] <- FALSE
dat$RECALL[which(dat$FOLDER=="2019-07-23 5393" & dat$TIME1=="2019-07-25 13:00:00" & dat$P==4L)] <- FALSE
dat$RECALL[which(dat$FOLDER=="2019-07-23 5393" & dat$TIME1=="2019-07-25 14:00:00" & dat$P==4L)] <- FALSE
dat$RECALL[which(dat$FOLDER=="2019-07-23 7464" & dat$TIME1=="2019-07-24 10:00:00" & dat$P==4L)] <- FALSE
dat$RECALL[which(dat$FOLDER=="2019-07-23 7464" & dat$TIME1=="2019-07-24 11:00:00" & dat$P==4L)] <- FALSE
dat$RECALL[which(dat$FOLDER=="2019-07-23 7464" & dat$TIME1=="2019-07-24 15:00:00" & dat$P==4L)] <- FALSE
dat$RECALL[which(dat$FOLDER=="2019-07-23 7464" & dat$TIME1=="2019-07-24 19:00:00" & dat$P==4L)] <- FALSE
dat$RECALL[which(dat$FOLDER=="2019-07-26 8627" & dat$TIME1=="2019-07-26 15:53:38" & dat$P==9L)] <- FALSE
dat$RECALL[which(dat$FOLDER=="2019-07-26 8627" & dat$TIME1=="2019-07-26 15:53:38" & dat$P==9L)] <- FALSE
dat$RECALL[which(dat$FOLDER=="2019-08-05a 5093" & dat$TIME1=="2019-08-05 10:00:00" & dat$P==2L)] <- TRUE
dat$RECALL[which(dat$FOLDER=="2019-08-09 5093" & dat$TIME1=="2019-08-09 09:59:54" & dat$P==6L)] <- TRUE
dat$RECALL[which(dat$FOLDER=="2019-09-11 5260" & dat$TIME1=="2019-09-13 16:00:00" & dat$P==4L)] <- FALSE
dat$RECALL[which(dat$FOLDER=="2019-09-17 5306" & dat$TIME1=="2019-09-19 15:00:00" & dat$P==6L)] <- TRUE
dat$RECALL[which(dat$FOLDER=="2019-10-23 5108" & dat$TIME1=="2019-10-23 13:55:54" & dat$P==8L)] <- TRUE
dat$RECALL[which(dat$FOLDER=="2019-11-18 5349" & dat$TIME1=="2019-11-18 12:00:00" & dat$P==8L)] <- FALSE
dat$RECALL[which(dat$FOLDER=="2019-11-18 5349" & dat$TIME1=="2019-11-18 13:00:00" & dat$P==8L)] <- FALSE
dat$RECALL[which(dat$FOLDER=="2019-11-18 5349" & dat$TIME1=="2019-11-18 13:00:00" & dat$P==2L)] <- FALSE
dat$RECALL[which(dat$FOLDER=="2019-11-19 7126" & dat$TIME1=="2019-11-20 18:00:00" & dat$P==2L)] <- TRUE
dat$RECALL[which(dat$FOLDER=="2019-11-22 7126" & dat$TIME1=="2019-11-22 14:55:10" & dat$P==6L)] <- TRUE
dat$RECALL[which(dat$FOLDER=="2019-12-05 7126" & dat$TIME1=="2019-12-06 08:00:00" & dat$P==6L)] <- TRUE
# various errors with 8302
dat$RECALL[which(dat$FOLDER=="2019-02-27 8302 a" & dat$TIME1=="2019-02-27 12:00:00" & dat$P==8L)] <- FALSE
dat$RECALL[which(dat$FOLDER=="2019-02-28 8302" & dat$TIME1=="2019-02-28 17:00:00" & dat$P==8L)] <- TRUE
dat$RECALL[which(dat$FOLDER=="2019-02-28 8302" & dat$TIME1=="2019-02-28 21:00:00" & dat$P==8L)] <- FALSE
dat$RECALL[which(dat$FOLDER=="2019-02-28 8302" & dat$TIME1=="2019-02-28 23:00:00" & dat$P==8L)] <- FALSE
dat$RECALL[which(dat$FOLDER=="2019-02-28 8302" & dat$TIME1=="2019-03-01 07:00:00" & dat$P==8L)] <- FALSE
dat$RECALL[which(dat$FOLDER=="2019-02-28 8302" & dat$TIME1=="2019-03-01 11:00:00" & dat$P==8L)] <- TRUE
dat$RECALL[which(dat$FOLDER=="2019-03-01 8302" & dat$TIME1=="2019-03-02 02:00:00" & dat$P==8L)] <- FALSE
dat$RECALL[which(dat$FOLDER=="2019-03-01 8302" & dat$TIME1=="2019-03-03 14:00:00" & dat$P==8L)] <- TRUE
dat$RECALL[which(dat$FOLDER=="2019-03-01 8302" & dat$TIME1=="2019-03-01 13:43:10" & dat$P==4L)] <- TRUE
dat$RECALL[which(dat$FOLDER=="2019-03-01 8302" & dat$TIME1=="2019-03-02 12:00:00" & dat$P==4L)] <- TRUE
dat$RECALL[which(dat$FOLDER=="2019-03-01 8302" & dat$TIME1=="2019-03-03 13:00:00" & dat$P==4L)] <- TRUE
dat$RECALL[which(dat$FOLDER=="2019-03-01 8302" & dat$TIME1=="2019-03-03 19:00:00" & dat$P==4L)] <- TRUE
dat$RECALL[which(dat$FOLDER=="2019-07-08 8302" & dat$TIME1=="2019-07-09 07:00:00" & dat$P==8L)] <- FALSE
dat$RECALL[which(dat$FOLDER=="2019-07-10 8302" & dat$TIME1=="2019-07-12 00:00:00" & dat$P==8L)] <- FALSE
dat$RECALL[which(dat$FOLDER=="2019-07-10 8302" & dat$TIME1=="2019-07-12 07:00:00" & dat$P==8L)] <- FALSE
dat$RECALL[which(dat$FOLDER=="2019-07-10 8302" & dat$TIME1=="2019-07-12 00:00:00" & dat$P==2L)] <- FALSE
dat$RECALL[which(dat$FOLDER=="2019-07-10 8302" & dat$TIME1=="2019-07-11 06:00:00" & dat$P==4L)] <- FALSE
dat$RECALL[which(dat$FOLDER=="2019-10-23 8302" & dat$TIME1=="2019-10-23 13:57:04" & dat$P==8L)] <- TRUE
dat$RECALL[which(dat$FOLDER=="2019-10-23 8302" & dat$TIME1=="2019-10-24 07:00:00" & dat$P==8L)] <- FALSE
dat$RECALL[which(dat$FOLDER=="2019-10-23 8302" & dat$TIME1=="2019-10-23 13:57:04" & dat$P==2L)] <- TRUE
dat$RECALL[which(dat$FOLDER=="2019-10-23 8302" & dat$TIME1=="2019-10-23 13:57:04" & dat$P==6L)] <- TRUE
dat$RECALL[which(dat$FOLDER=="2019-10-25 8302" & dat$TIME1=="2019-10-27 22:00:00" & dat$P==2L)] <- FALSE
dat$RECALL[which(dat$FOLDER=="2019-10-25 8302" & dat$TIME1=="2019-10-26 07:00:00" & dat$P==4L)] <- FALSE
dat$RECALL[which(dat$FOLDER=="2019-10-25 8302" & dat$TIME1=="2019-10-27 23:00:00" & dat$P==4L)] <- FALSE
dat$RECALL[which(dat$FOLDER=="2019-10-25 8302" & dat$TIME1=="2019-10-28 06:00:00" & dat$P==4L)] <- FALSE

# remove columns
dat[,c("PR0", "PR1", "PR2", "PREC1", "PREC2", "PREC3", "PREC", "PROB")] <- NULL

########################################
# END
########################################