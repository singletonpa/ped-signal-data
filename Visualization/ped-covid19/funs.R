########################################
# Project:  UDOT-18.602 Traffic signal pedestrians
# Authors:  Patrick Singleton (patrick.singleton@usu.edu)
# File:     funs.R
# Date:     2018 Fall, 2019 Spring, 2020 Spring, Summer
# About:    Functions to process traffic signal data
########################################

########################################
# Notes

# Open R project first, then open this R script

# Major edits
# 2018-11-08
# 2019-01-24
# 2020-02-08
# 2020-02-13
# 2020-02-16
# 2020-06-05 
# 2020-06-25 added timesigmiss

# Format time with decimals
options(digits.secs=6)

########################################
# Functions

# Function to read and process traffic signal controller log data
getsig <- function(fpath=NA, printsum=F) {
  # Inputs
  #   fpath: a character vector of length one containing a file path (within directory)
  #   printsum: a logical vector of length one, for printing summary or not
  # Output
  #   a dataframe containing variables {SIGNAL, TIME, EVENT, PARAM}
  
  # Checks
  if(is.character(fpath)==F) { stop("Format of fpath is not a character.") }
  if(is.logical(printsum)==F) { stop("Format of printsum is not a logical") }
  if(length(fpath)!=1) { stop("Character fpath must be of length 1.") }
  if(length(printsum)!=1) { stop("Logical printsum must be of length 1.") }
  
  # Load data
  temp <- read.csv(file=file.path(fpath))
  
  # Recode variable types
  temp$SIGNAL <- temp$Signal.Id
  # temp$TIME <- as.POSIXlt(temp$Timestamp, format="%m/%d/%Y %H:%M:%OS", tz="America/Denver")
  temp$TIME <- as.POSIXct(strptime(temp$Timestamp, format="%m/%d/%Y %H:%M:%OS", tz="America/Denver"))
  temp$EVENT <- temp$Event.Code
  temp$PARAM <- temp$Event.Parameter
  
  # Select variables
  temp <- temp[,c("SIGNAL", "TIME", "EVENT", "PARAM")]
  
  # Sort by time
  temp <- temp[order(temp$TIME),]
  row.names(temp) <- NULL
  
  # Print summary
  if (printsum) {
    print(paste0("SIGNAL: ", paste0(sort(unique(temp$SIGNAL)), collapse=",")), quote=F)
    print(paste0("TIME 1: ", min(temp$TIME)), quote=F)
    print(paste0("TIME 2: ", max(temp$TIME)), quote=F)
    print(paste0("EVENT:  ", paste0(sort(unique(temp$EVENT)), collapse=",")), quote=F)
    print(paste0("PARAM:  ", paste0(sort(unique(temp$PARAM)), collapse=",")), quote=F)
    pedevents <- c(c(90,89), c(45), c(21,22,23))
    pedphases <- sort(unique(unique(subsig(dat=temp, events=pedevents)$PARAM)))
    print(paste0("PED PH: ", paste0(sort(unique(pedphases)), collapse=",")), quote=F)
  }
  
  # Return
  return(temp)
}

# Function to subset relevant rows from traffic signal controller log data frame
subsig <- function(dat=NA, events=NA, params=NA) {
  # Inputs
  #   dat: a dataframe containing variables {SIGNAL, TIME, EVENT, PARAM, ...}
  #   events: a vector containing one or more event codes to subset
  #   params: a vector containing one or more event parameters to subset
  #     if events or params is c(NA), will skip and keep all
  # Output
  #   a dataframe containing variables {SIGNAL, TIME, EVENT, PARAM, ...}
  
  # Checks
  # if(is.na(dat)) { stop("Please enter a data frame for dat.") }
  if(is.data.frame(dat)==F) { stop("Format of dat is not a data frame.") }
  if(sum(c("SIGNAL", "TIME", "EVENT", "PARAM") %in% names(dat))!=4) { stop("Missing columns in dat.") }
  if(is.vector(events)==F) { stop("Format of events is not a vector.") }
  if(is.vector(params)==F) { stop("Format of params is not a vector.") }
  
  # Subset for event codes
  if(length(events)==1) {
    if(is.na(events)) {
      # do nothing
    } else {
      dat <- dat[which(dat$EVENT %in% events),]
    }
  } else {
    dat <- dat[which(dat$EVENT %in% events),]
  }
  
  # Subset for event parameters
  if(length(params)==1) {
    if(is.na(params)) {
      # do nothing
    } else {
      dat <- dat[which(dat$PARAM %in% params),]
    }
  } else {
    dat <- dat[which(dat$PARAM %in% params),]
  }
  
  # Return
  return(dat)
}

# Function to calculate time bins for summarization
seqsig <- function(dat=NA, tunit="hour") {
  # Inputs
  #   dat: a dataframe containing variables {SIGNAL, TIME, EVENT, PARAM, ...}
  #   tunit: a character describing time units {year, month, day, hour, minute, ...}
  # Output
  #   a vector containing time bins in POSIXct format
  
  # Checks
  # if(is.na(dat)) { stop("Please enter a data frame for dat.") }
  if(is.data.frame(dat)==F) { stop("Format of dat is not a data frame.") }
  if(sum(c("SIGNAL", "TIME", "EVENT", "PARAM") %in% names(dat))!=4) { stop("Missing columns in dat.") }
  if(is.character(tunit)==F) { stop("Format of tunit is not a character.") }
  if(length(tunit)!=1) { stop("Format of tunit is not a single value.")}
  if(!(tunit %in% c("year", "month", "day", "hour", "minute"))) { stop("Format of tunit is not one of: year, month, day, hour, minute, ...") }

  # By year
  if(tunit=="year") {
    # tmin <- as.Date(trunc(min(dat$TIME), units="years"))
    tmin <- trunc(min(dat$TIME, na.rm=T), units="years")
    # tmax <- as.Date(trunc(max(dat$TIME), units="years"))
    tmax <- trunc(max(dat$TIME, na.rm=T), units="years")
    tempseq <- seq(tmin, tmax, "years")
    tseq <- seq(tmin, by="years", length.out=length(tempseq)+1)
    rm(tempseq, tmin, tmax)
  }
  
  # By month
  if(tunit=="month") {
    # tmin <- as.Date(trunc(min(dat$TIME), units="months"))
    tmin <- trunc(min(dat$TIME, na.rm=T), units="months")
    # tmax <- as.Date(trunc(max(dat$TIME), units="months"))
    tmax <- trunc(max(dat$TIME, na.rm=T), units="months")
    tempseq <- seq(tmin, tmax, "month")
    tseq <- seq(tmin, by="month", length.out=length(tempseq)+1)
    rm(tempseq, tmin, tmax)
  }
  
  # By day
  if(tunit=="day") {
    # tmin <- as.Date(trunc(min(dat$TIME), units="days"))
    tmin <- trunc(min(dat$TIME, na.rm=T), units="days")
    # tmax <- as.Date(trunc(max(dat$TIME), units="days"))+1
    # tmax <- trunc(max(dat$TIME), units="days")+24*60*60
    tmax <- trunc(max(dat$TIME, na.rm=T), units="days")
    # tseq <- seq(tmin, tmax, "DSTday")
    tempseq <- seq(tmin, tmax, "DSTday")
    tseq <- seq(tmin, by="DSTday", length.out=length(tempseq)+1)
    rm(tempseq, tmin, tmax)
  }
  
  # By hour
  if(tunit=="hour") {
    tmin <- trunc(min(dat$TIME, na.rm=T), units="hours")
    # tmax <- trunc(max(dat$TIME), units="hours")+60*60
    tmax <- trunc(max(dat$TIME, na.rm=T), units="hours")
    # tseq <- seq(tmin, tmax, 60*60)
    # tseq <- seq(tmin, tmax, "hour")
    # tseq <- seq(tmin, tmax, "hour")
    tempseq <- seq(tmin, tmax, "hour")
    tseq <- seq(tmin, by="hour", length.out=length(tempseq)+1)
    rm(tempseq, tmin, tmax)
  }
  
  # By minutes
  if(tunit=="minute") {
    tmin <- trunc(min(dat$TIME, na.rm=T), units="mins")
    # tmax <- trunc(max(dat$TIME), units="mins")+60
    tmax <- trunc(max(dat$TIME, na.rm=T), units="mins")
    # tseq <- seq(tmin, tmax, 60)
    tempseq <- seq(tmin, tmax, "min")
    tseq <- seq(tmin, by="min", length.out=length(tempseq)+1)
    rm(tempseq, tmin, tmax)
  }
  
  # Return
  return(tseq)
}

# Function to tabulate signal data by time
tabsig <- function(dat=NA, tseq=NA) {
  # Inputs
  #   dat: a dataframe containing variables {SIGNAL, TIME, EVENT, PARAM, ...}
  #   tseq: a vector containing time bins in POSIXct or Date format
  # Output
  #   a dataframe containing variables {SIGNAL, TIME1, TIME2, TDIFF, COUNT}
  
  # Checks
  # if(is.na(dat)) { stop("Please enter a data frame for dat.") }
  if(is.data.frame(dat)==F) { stop("Format of dat is not a data frame.") }
  if(sum(c("SIGNAL", "TIME", "EVENT", "PARAM") %in% names(dat))!=4) { stop("Missing columns in dat.") }
  # if(is.vector(tseq)==F) { stop("Format of tseq is not a vector.") }
  if(inherits(tseq, "POSIXct")==F & inherits(tseq, "Date")==F) { stop("Format of tseq is not POSIXct or Date.")}
  if(length(tseq)<2) { stop("Length of tseq is less than two.")}
  
  # Get signal
  mysig <- sort(unique(dat$SIGNAL))[1]
  
  # Create dataframe
  tdf <- data.frame(SIGNAL=mysig, TIME1=tseq[1:(length(tseq)-1)], TIME2=tseq[2:length(tseq)], TDIFF=0, COUNT=0L)
  tdf$TDIFF <- as.numeric(tdf$TIME2) - as.numeric(tdf$TIME1)
  
  # Populate with counts
  # tdf$COUNT <- as.vector(table(cut(dat$TIME, tseq, right=F)))
  # this change fixes Daylight Saving Time issue, but adds a few seconds of computational time
  # for (i in 1:nrow(tdf)) {
  #   tdf$COUNT[i] <- length(which(dat$TIME >= tdf$TIME1[i] & dat$TIME < tdf$TIME2[i]))
  # }; rm(i)
  if (nrow(dat)==0) { 
    tdf$COUNT <- 0L
  } else {
    # if (inherits(tseq, "Date")) { dat$TIME <- as.Date(dat$TIME) }
    # tdf$COUNT <- hist(dat$TIME, tseq, plot=F)$counts
    tdf$COUNT <- hist(as.numeric(dat$TIME), as.numeric(tseq), plot=F)$counts
  }
  
  # Return
  return(tdf)
}

# Function to tabulate signal data by time and a sequence of events
tabsigseq <- function(dat=NA, tseq=NA, eseq=NA) {
  # Inputs
  #   dat: a dataframe containing variables {SIGNAL, TIME, EVENT, PARAM, ...}
  #   tseq: a vector containing time bins in POSIXct format
  #   eseq: a vector containing event sequences in character format
  # Output
  #   a dataframe containing variables {SIGNAL, TIME1, TIME2, TDIFF, COUNT}
  
  # Checks
  # if(is.na(dat)) { stop("Please enter a data frame for dat.") }
  if(is.data.frame(dat)==F) { stop("Format of dat is not a data frame.") }
  if(sum(c("SIGNAL", "TIME", "EVENT", "PARAM") %in% names(dat))!=4) { stop("Missing columns in dat.") }
  # if(is.vector(tseq)==F) { stop("Format of tseq is not a vector.") }
  if(inherits(tseq, "POSIXct")==F & inherits(tseq, "Date")==F) { stop("Format of tseq is not POSIXct or Date.")}
  if(length(tseq)<2) { stop("Length of tseq is less than two.")}
  # if(is.vector(eseq)==F) { stop("Format of eseq is not a vector.") }
  if(inherits(eseq, "character")==F) { stop("Format of eseq is not character.")}
  
  # Get signal
  mysig <- sort(unique(dat$SIGNAL))[1]
  
  # Create dataframe
  tdf <- data.frame(SIGNAL=mysig, TIME1=tseq[1:(length(tseq)-1)], TIME2=tseq[2:length(tseq)], TDIFF=0, COUNT=0L)
  tdf$TDIFF <- as.numeric(tdf$TIME2) - as.numeric(tdf$TIME1)
  
  for (i in unique(dat$PARAM)) {
    tdat <- subsig(dat, params=i)
    
    if (nrow(tdat)==0) { 
      tdf$COUNT <- tdf$COUNT + 0L
    } else {
      # Process and subset tdat
      tdat$seq <- c(tdat$EVENT[1], paste(tdat$EVENT[1:(nrow(tdat)-1)], tdat$EVENT[2:nrow(tdat)]))[1:nrow(tdat)]
      tdat <- tdat[tdat$seq %in% eseq,]
      
      if(nrow(tdat)==0) {
        tdf$COUNT <- tdf$COUNT + 0L
      } else {
        # Populate with counts
        # if (inherits(tseq, "Date")) { tdat$TIME <- as.Date(tdat$TIME) }
        # tdf$COUNT <- tdf$COUNT + hist(tdat$TIME, tseq, plot=F)$counts
        tdf$COUNT <- tdf$COUNT + hist(as.numeric(tdat$TIME), as.numeric(tseq), plot=F)$counts
      }
    }
    rm(tdat)
  }; rm(i)
  
  
  # Return
  return(tdf)
}

# Function to tabulate signal data by time and a time difference between events
tabsigtdif <- function(dat=NA, tseq=NA, tdif=NA) {
  # Inputs
  #   dat: a dataframe containing variables {SIGNAL, TIME, EVENT, PARAM, ...}
  #   tseq: a vector containing time bins in POSIXct format
  #   tdif: a vector of unit one containing time difference in numeric format
  # Output
  #   a dataframe containing variables {SIGNAL, TIME1, TIME2, TDIFF, COUNT}
  
  # Checks
  # if(is.na(dat)) { stop("Please enter a data frame for dat.") }
  if(is.data.frame(dat)==F) { stop("Format of dat is not a data frame.") }
  if(sum(c("SIGNAL", "TIME", "EVENT", "PARAM") %in% names(dat))!=4) { stop("Missing columns in dat.") }
  # if(is.vector(tseq)==F) { stop("Format of tseq is not a vector.") }
  if(inherits(tseq, "POSIXct")==F & inherits(tseq, "Date")==F) { stop("Format of tseq is not POSIXct or Date.")}
  if(length(tseq)<2) { stop("Length of tseq is less than two.")}
  # if(is.vector(tdif)==F) { stop("Format of tdif is not a vector.") }
  if(length(tdif)!=1) { stop("Length of tdif is not one.")}
  if(inherits(tdif, "numeric")==F & inherits(tdif, "integer")==F) { stop("Format of tdif is not numeric.")}
  
  # Get signal
  mysig <- sort(unique(dat$SIGNAL))[1]
  
  # Create dataframe
  tdf <- data.frame(SIGNAL=mysig, TIME1=tseq[1:(length(tseq)-1)], TIME2=tseq[2:length(tseq)], TDIFF=0, COUNT=0L)
  tdf$TDIFF <- as.numeric(tdf$TIME2) - as.numeric(tdf$TIME1)
  
  for (i in unique(dat$PARAM)) {
    tdat <- subsig(dat, params=i)
  
    if (nrow(tdat)==0) { 
      tdf$COUNT <- tdf$COUNT + 0L
    } else {
      # Process and subset tdat
      tdat$dif <- c(Inf, difftime(tdat$TIME[2:nrow(tdat)], tdat$TIME[1:(nrow(tdat)-1)], units="secs"))[1:nrow(tdat)]
      tdat <- tdat[tdat$dif >= tdif,]
      
      if(nrow(tdat)==0) {
        tdf$COUNT <- tdf$COUNT + 0L
      } else {
        # Populate with counts
        # if (inherits(tseq, "Date")) { tdat$TIME <- as.Date(tdat$TIME) }
        # tdf$COUNT <- tdf$COUNT + hist(tdat$TIME, tseq, plot=F)$counts
        tdf$COUNT <- tdf$COUNT + hist(as.numeric(tdat$TIME), as.numeric(tseq), plot=F)$counts
      }
    }
    rm(tdat)
  }
  # Return
  return(tdf)
}

# Function to average signal data by time unit
avgsig <- function(tdf=NA, tunit="hour") {
  # Inputs
  #   tdf: a dataframe containing variables {SIGNAL, TIME1, TIME2, TDIFF, COUNT}
  #   tunit: a character describing time units {year, month, day, hour, minute, ...}
  # Output
  #   a dataframe containing variables {SIGNAL, TUNIT, COUNT}
  
  # Checks
  # if(is.na(tdf)) { stop("Please enter a data frame for tdf") }
  if(is.data.frame(tdf)==F) { stop("Format of tdf is not a data frame.") }
  if(sum(c("SIGNAL", "TIME1", "TIME2", "TDIFF", "COUNT") %in% names(tdf))!=5) { stop("Missing columns in dat.") }
  if(is.character(tunit)==F) { stop("Format of tunit is not a character.") }
  if(length(tunit)!=1) { stop("Format of tunit is not a single value.")}
  if(!(tunit %in% c("year", "month", "day", "hour", "minute"))) { stop("Format of tunit is not one of: year, month, day, hour, minute, ...") }
  
  # Get signal
  mysig <- sort(unique(tdf$SIGNAL))[1]
  
  # Create time unit variable
  if(tunit=="year") { tdf$TUNIT <- strftime(tdf$TIME1, format="%Y") }
  if(tunit=="month") { tdf$TUNIT <- strftime(tdf$TIME1, format="%m") }
  # if(tunit=="day") { tdf$TUNIT <- strftime(tdf$TIME1, format="%w") }
  if(tunit=="day") { tdf$TUNIT <- strftime(tdf$TIME1, format="%u") }
  if(tunit=="hour") { tdf$TUNIT <- strftime(tdf$TIME1, format="%H") }
  if(tunit=="minute") { tdf$TUNIT <- strftime(tdf$TIME1, format="%M") }
  
  # Aggregate (average)
  tavg <- aggregate(COUNT ~ TUNIT, data=tdf, FUN=mean)
  
  # Format
  tavg$SIGNAL <- mysig
  tavg <- tavg[,c("SIGNAL", "TUNIT", "COUNT")]
  
  # Return
  return(tavg)
}

# Function to determine maximum time difference between successive events within time sequence
timesigmiss <- function(dat = NA, tseq = NA) {
  # Inputs
  #   dat: a dataframe containing variables {SIGNAL, TIME, EVENT, PARAM, ...}
  #   tseq: a vector containing time bins in POSIXct format
  # Output
  #   a dataframe containing variables {SIGNAL, TIME1, TIME2, TDIFF, TDMAX}
  
  # Checks
  # if(is.na(dat)) { stop("Please enter a data frame for dat.") }
  if(is.data.frame(dat)==F) { stop("Format of dat is not a data frame.") }
  if(sum(c("SIGNAL", "TIME", "EVENT", "PARAM") %in% names(dat))!=4) { stop("Missing columns in dat.") }
  # if(is.vector(tseq)==F) { stop("Format of tseq is not a vector.") }
  if(inherits(tseq, "POSIXct")==F & inherits(tseq, "Date")==F) { stop("Format of tseq is not POSIXct or Date.")}
  if(length(tseq)<2) { stop("Length of tseq is less than two.")}

  # Sort by time
  dat <- dat[order(dat$TIME),]
  
  # Get signal
  mysig <- sort(unique(dat$SIGNAL))[1]
  
  # Create dataframe
  tdf <- data.frame(SIGNAL=mysig, TIME1=tseq[1:(length(tseq)-1)], TIME2=tseq[2:length(tseq)])
  tdf$TDIFF <- as.numeric(difftime(tdf$TIME2, tdf$TIME1, units="min"))
  tdf$TIMEINT <- as.integer(tdf$TIME1)
  
  # Process time differences
  dat$TIMEINT <- as.integer(as.POSIXct(trunc(dat$TIME, units="hours")))
  dat$dif1 <- c(difftime(dat$TIME[2:nrow(dat)], dat$TIME[1:(nrow(dat)-1)], units="mins"), 0)[1:nrow(dat)]
  dat$dif2 <- c(0, difftime(dat$TIME[2:nrow(dat)], dat$TIME[1:(nrow(dat)-1)], units="mins"))[1:nrow(dat)]
  dat$dif <- pmax(dat$dif1, dat$dif2)
  
  # Calculate max time difference
  if (sum(!is.na(dat$dif))>0) {
    tagg <- aggregate(dif ~ TIMEINT, data=dat, FUN=max)
    tdf <- merge(tdf, tagg, by.x="TIMEINT", by.y="TIMEINT", all.x=T)
  } else {
    tdf$dif <- NA
  }
  tdf$TDMAX <- as.numeric(tdf$dif)
  tdf$dif <- NULL
  
  # Return
  return(tdf)
}

########################################
# END
########################################