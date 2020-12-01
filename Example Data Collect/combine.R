########################################
# Project:  UDOT-18.602 Traffic signal pedestrians
# Authors:  Patrick Singleton (patrick.singleton@usu.edu)
# File:     combine.R
# Date:     2020 Summer
# About:    Combine video data and ATSPM data
########################################

########################################
# Initialize

# Prepare processing functions
source(file.path("funs.R"))

# Get list
myfolder <- getwd() # or other location with folders
myfiles <- list.dirs(myfolder, full.names=F, recursive=F)
myfiles

# Load empty dataframe - videos
load(file=file.path("..", "DataEntry", "videos.RData"))
videos_all <- videos; rm(videos)
videos_all$FOLDER <- character()

# Load empty dataframe - events
load(file=file.path("..", "DataEntry", "events.RData"))
events_all <- events; rm(events)
events_all$FOLDER <- character()

# Create empty dataframe - combined
mydat <- data.frame()
mydat$SIGNAL <- integer()
mydat$TIME1 <- videos_all$TIME1
mydat$TIME2 <- videos_all$TIME2
mydat$TDIFF <- mydat$TIME2 - mydat$TIME1
mydat$X <- integer()
mydat$DIR <- character()
mydat$P <- integer()
mydat$PED <- integer()
mydat$BIKE <- integer()
mydat$SCOOT <- integer()
mydat$SKATE <- integer()
mydat$WHEEL <- integer()
mydat$OTHER <- integer()
mydat$PEDOUT <- integer()
mydat$A00 <- integer()
mydat$A21 <- integer()
mydat$A45 <- integer()
mydat$A90 <- integer()
mydat$A45A <- integer()
mydat$A45B <- integer()
mydat$A45C <- integer()
mydat$A90A <- integer()
mydat$A90B <- integer()
mydat$A90C <- integer()
mydat$A00_2 <- integer()
mydat$A45_26 <- integer()
mydat$A45_48 <- integer()
mydat$A90_26 <- integer()
mydat$A90_48 <- integer()
mydat$FOLDER <- character()
dat <- mydat

########################################
# Assemble data

# For loop across folders
for (i in 1:length(myfiles)) {
  
  ### Load data
  print(myfiles[i])
  # Load data - videos
  load(file=file.path(myfolder, myfiles[i], "DataEntry", "videos.RData"))
  # Load data - events
  load(file=file.path(myfolder, myfiles[i], "DataEntry", "events.RData"))
  # Load data - ATSPM
  # atspm <- read.csv(file=file.path(myfolder, myfiles[i], "ATSPM", "ControllerEventLogs.csv"))
  atspm <- getsig(fpath=file.path(myfolder, myfiles[i], "ATSPM", "ControllerEventLogs.csv"), printsum=T)
  atspm <- atspm[order(atspm$TIME),]
  atspm <- atspm[atspm$EVENT %in% c(0,21,22,23,45,89,90),]
  row.names(atspm) <- NULL
  print(table(atspm$PARAM, atspm$EVENT))
  # checks
  print(min(atspm$TIME) < min(videos$TIME1)); print(max(atspm$TIME) > max(videos$TIME2))
  ttime <- data.frame(TIME1=atspm$TIME[1:(nrow(atspm)-1)], TIME2=atspm$TIME[2:nrow(atspm)])
  ttime$TDIFF <- difftime(ttime$TIME2, ttime$TIME1, units="min")
  print(ttime[ttime$TDIFF>15,])
  rm(ttime)
  # Print results
  print(videos)
  # print(summary(events))
  
  ### Get time sequence
  # Get time sequence - videos
  tseq <- vector(mode = "list", length = nrow(videos))
  for (j in unique(videos$VIDEOID)) {
    tmin <- trunc(min(videos[videos$VIDEOID==j,"TIME1"]), units="hours")
    tmax <- trunc(max(videos[videos$VIDEOID==j,"TIME2"]), units="hours")+60*60
    tseq[[j]] <- seq(tmin, tmax, 60*60)
    rm(tmin, tmax)
  }; rm(j)
  for (j in unique(videos$VIDEOID)) { print(tseq[[j]]) }; rm(j)

  ### Get data
  mytemp <- mydat
  for (j in unique(videos$VIDEOID)) {
    # Initialize
    temp <- data.frame(SIGNAL=unique(videos$SIGNALID), 
                       TIME1=tseq[[j]][1:(length(tseq[[j]]) - 1)], 
                       TIME2=tseq[[j]][2:(length(tseq[[j]]))])
    temp$TDIFF <- difftime(temp$TIME2, temp$TIME1, units="min")
    temp$X <- 0L
    temp$DIR <- ""
    temp$P <- 0L
    temp[,c("PED", "BIKE", "SCOOT", "SKATE", "WHEEL", "OTHER", "PEDOUT")] <- 0L
    temp[,c("A00", "A21", "A45", "A90")] <- 0L
    temp[,c("A45A", "A45B", "A45C", "A90A", "A90B", "A90C")] <- 0L
    temp[,c("A00_2", "A45_26", "A45_48", "A90_26", "A90_48")] <- 0L
    t0 <- temp
    temp <- mydat
    # Pre-process data - ATSPM
    tempa <- atspm[atspm$TIME >= videos[videos$VIDEOID==j,"TIME1"] & atspm$TIME <= videos[videos$VIDEOID==j,"TIME2"],]
    # Pre-process data - events
    tevents <- events[events$VIDEOID==j,]
    tbin <- as.POSIXct(cut(tevents$TIME, tseq[[j]], right=F), tz="America/Denver")
    tsig <- t0[,c("SIGNAL", "TIME1", "TIME2", "TDIFF")]
    tsig[,c("X1PED", "X2PED", "X3PED", "X4PED")] <- NA
    tdirs <- unique(c(t(videos[videos$VIDEOID==j,c("X1", "X2", "X3", "X4")])))
    tdirs <- tdirs[tdirs!=""]
    tsig[,paste("BIKE", tdirs, sep="_")] <- NA
    tsig[,paste("SCOOT", tdirs, sep="_")] <- NA
    tsig[,paste("SKATE", tdirs, sep="_")] <- NA
    tsig[,paste("WHEEL", tdirs, sep="_")] <- NA
    tsig[,paste("OTHER", tdirs, sep="_")] <- NA
    tsig[,paste("PEDOUT", tdirs, sep="_")] <- NA
    for (m in 1:length(tsig$TIME1)) {
      te1 <- tevents[tbin==tsig$TIME1[m],]
      tsig$X1PED[m] <- sum(te1$X1PED, na.rm=T)
      tsig$X2PED[m] <- sum(te1$X2PED, na.rm=T)
      tsig$X3PED[m] <- sum(te1$X3PED, na.rm=T)
      tsig$X4PED[m] <- sum(te1$X4PED, na.rm=T)
      for (l in tdirs) {
        te0 <- te1[te1$BODIR==l,]
        tsig[m,paste("BIKE", l, sep="_")] <- sum(te0[te0$BOWHAT=="BIKE","BONUM"], na.rm=T)
        tsig[m,paste("SCOOT", l, sep="_")] <- sum(te0[te0$BOWHAT=="SCOOT","BONUM"], na.rm=T)
        tsig[m,paste("SKATE", l, sep="_")] <- sum(te0[te0$BOWHAT=="SKATE","BONUM"], na.rm=T)
        tsig[m,paste("WHEEL", l, sep="_")] <- sum(te0[te0$BOWHAT=="WHEEL","BONUM"], na.rm=T)
        tsig[m,paste("OTHER", l, sep="_")] <- sum(te0[te0$BOWHAT=="OTHER","BONUM"], na.rm=T)
        tsig[m,paste("PEDOUT", l, sep="_")] <- sum(te0[te0$BOWHAT=="PEDOUT","BONUM"], na.rm=T)
        rm(te0)
      }; rm(l)
      rm(te1)
    }; rm(m)
    # Process for each crossing
    for (k in 1:4) {
      if(videos[videos$VIDEOID==j,paste0("X",k)]!="") {
        t1 <- t0
        t1$X <- k
        t1$DIR <- videos[videos$VIDEOID==j,paste0("X",k)]
        tphase <- as.integer(videos[videos$VIDEOID==j,paste0("P", k)])
        t1$P <- tphase
        t1$PED <- tsig[,paste0("X", k, "PED")]
        t1$BIKE <- tsig[,paste("BIKE", unique(t1$DIR), sep="_")]
        t1$SCOOT <- tsig[,paste("SCOOT", unique(t1$DIR), sep="_")]
        t1$SKATE <- tsig[,paste("SKATE", unique(t1$DIR), sep="_")]
        t1$WHEEL <- tsig[,paste("WHEEL", unique(t1$DIR), sep="_")]
        t1$OTHER <- tsig[,paste("OTHER", unique(t1$DIR), sep="_")]
        t1$PEDOUT <- tsig[,paste("PEDOUT", unique(t1$DIR), sep="_")]
        if (is.na(tphase)) {
          t1$A00 <- NA; t1$A21 <- NA; t1$A45 <- NA; t1$A90 <- NA
          t1$A45A <- NA; t1$A45B <- NA; t1$A45C <- NA; t1$A90A <- NA; t1$A90B <- NA; t1$A90C <- NA
          t1$A00_2 <- NA; t1$A45_26 <- NA; t1$A45_48 <- NA; t1$A90_26 <- NA; t1$A90_48 <- NA
        } else {
          t1$A00 <- tabsig(dat=subsig(dat=tempa, events=c(00), params=tphase), tseq[[j]])$COUNT
          t1$A21 <- tabsig(dat=subsig(dat=tempa, events=c(21), params=tphase), tseq[[j]])$COUNT
          t1$A45 <- tabsig(dat=subsig(dat=tempa, events=c(45), params=tphase), tseq[[j]])$COUNT
          t1$A90 <- tabsig(dat=subsig(dat=tempa, events=c(90), params=tphase), tseq[[j]])$COUNT
          t1$A45A <- tabsigseq(dat=subsig(dat=tempa, events=c(0,21,22,90), params=tphase), tseq[[j]], eseq=c("0 90", "22 90"))$COUNT
          t1$A45B <- tabsigseq(dat=subsig(dat=tempa, events=c(0,21,90), params=tphase), tseq[[j]], eseq=c("0 90", "21 90"))$COUNT
          t1$A45C <- tabsigseq(dat=subsig(dat=tempa, events=c(0,90), params=tphase), tseq[[j]], eseq=c("0 90"))$COUNT
          t1$A90A <- tabsigtdif(dat=subsig(dat=tempa, events=c(90), params=tphase), tseq[[j]], tdif=5)$COUNT
          t1$A90B <- tabsigtdif(dat=subsig(dat=tempa, events=c(90), params=tphase), tseq[[j]], tdif=10)$COUNT
          t1$A90C <- tabsigtdif(dat=subsig(dat=tempa, events=c(90), params=tphase), tseq[[j]], tdif=15)$COUNT
          t1$A00_2 <- tabsig(dat=subsig(dat=tempa, events=c(00), params=2), tseq[[j]])$COUNT
          t1$A45_26 <- tabsig(dat=subsig(dat=tempa, events=c(45), params=c(2,6)), tseq[[j]])$COUNT
          t1$A45_48 <- tabsig(dat=subsig(dat=tempa, events=c(45), params=c(4,8)), tseq[[j]])$COUNT
          t1$A90_26 <- tabsig(dat=subsig(dat=tempa, events=c(90), params=c(2,6)), tseq[[j]])$COUNT
          t1$A90_48 <- tabsig(dat=subsig(dat=tempa, events=c(90), params=c(4,8)), tseq[[j]])$COUNT
        }
        temp <- rbind(temp, t1)
        rm(t1, tphase)
      }
    }; rm(k)
    mytemp <- rbind(mytemp, temp)
    rm(t0, temp, tempa, tevents, tbin, tsig, tdirs)
  }; rm(j)
  
  ### Check results
  View(mytemp)
  print(summary(mytemp))
  plot(mytemp$A45B, mytemp$PED, col = rgb(0,0,0,alpha=0.1), pch=16, las=1, main=myfiles[i], xlab="A45B", ylab="PED")
  # Pause to check
  readline(prompt="Press [enter] to continue")
  
  ### Adjust video and event ids
  vmax <- max(videos_all$VIDEOID, 0L)
  emax <- max(events_all$EVENTID, 0L)
  videos$VIDEOID <- videos$VIDEOID + vmax
  events$VIDEOID <- events$VIDEOID + vmax
  events$EVENTID <- events$EVENTID + emax
  videos$FOLDER <- myfiles[i]
  if (nrow(events)>0) { events$FOLDER <- myfiles[i] }
  mytemp$FOLDER <- myfiles[i]
  
  ### Merge files together
  # videos
  videos_all <- rbind(videos_all, videos)
  # events
  events_all <- rbind(events_all, events)
  # dat
  dat <- rbind(dat, mytemp)
  
  ### Cleanup
  rm(videos, events, atspm, mytemp, tseq)
  
}; rm(i)

# Inspect
summary(dat)

########################################
# Save data

### Save files
# videos
# save(videos_all, "videos_all.RData")
# write.csv(videos_all, "videos_all.csv", row.names=F)
# events
# save(events_all, "events_all.RData")
# write.csv(events_all, "events_all.csv", row.names=F)
# combined
# save(dat, "dat.RData")
write.csv(dat, "dat.csv", row.names=F)

### Cleanup
rm(videos_all, events_all, dat, mydat)
rm(myfiles, myfolder, emax, vmax)
rm(getsig, plotsig4typ, plotsig4typwk, seqsig, sig4typ, sig4typwk, subsig, tabsig, tabsigseq, tabsigtdif, avgsig, timesigmiss)
gc()

########################################
# END
########################################