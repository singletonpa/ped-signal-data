########################################
# Project:  UDOT-18.602 Traffic signal pedestrians
# Authors:  Patrick Singleton (patrick.singleton@usu.edu)
# File:     app.R
# Date:     2018 Fall, 2019 Spring, 2019 Summer
# About:    Shiny app to view and process pedestrian videos
########################################

########################################
# Notes
# 2019-02-23 V1
# 2019-03-21 V2
# 2019-04-05 V3
# 2019-06-17 V4

# Open R project first, then open this R script

# Resources
# https://shiny.rstudio.com/tutorial/
# https://deanattali.com/shinyjs/
# https://rstudio.github.io/DT/
# https://dreamrs-vic.shinyapps.io/shinyWidgets/

########################################
# Shiny

# Load libraries
library(shiny)
library(shinyjs)
# library(shinyWidgets)
library(DT)

########################################
# Preparations

# Load files
load(file="videos.Rdata")
load(file="events.Rdata")

# Lists
source(file="lists.R")

# Functions for videos
loadVideosData <- function() {
  load(file="videos.Rdata")
  videos
}
saveVideosData <- function(videos) {
  row.names(videos) <- videos$VIDEOID
  save(videos, file="videos.Rdata")
  write.csv(videos, file="videos.csv", row.names=F)
}
addVideosData <- function(data) {
  videos <- rbind(loadVideosData(), data)
  saveVideosData(videos)
}
deleteVideosData <- function(rows) {
  videos <- loadVideosData()[-rows,]
  saveVideosData(videos)
}
editVideosData <- function(data) {
  videos <- loadVideosData()
  videos[videos$VIDEOID==data$VIDEOID,] <- data
  saveVideosData(videos)
}
updateVideosInputs <- function(session) {
  updateSelectInput(session, "vVIDEOID", choices=max(loadVideosData()$VIDEOID,0)+1L, selected=max(loadVideosData()$VIDEOID,0)+1L)
  updateNumericInput(session, "vSIGNALID", value=0L)
  updateTextInput(session, "vLOCATION", value="")
  updateDateInput(session, "vTIME1date", value=Sys.Date())
  updateSelectInput(session, "vTIME1hr", choices=hours, selected="00")
  updateSelectInput(session, "vTIME1min", choices=minutes, selected="00")
  updateSelectInput(session, "vTIME1sec", choices=seconds, selected="00")
  updateDateInput(session, "vTIME2date", value=Sys.Date())
  updateSelectInput(session, "vTIME2hr", choices=hours, selected="00")
  updateSelectInput(session, "vTIME2min", choices=minutes, selected="00")
  updateSelectInput(session, "vTIME2sec", choices=seconds, selected="00")
  updateSelectInput(session, "vX1", choices=dirX1, selected="")
  updateSelectInput(session, "vX2", choices=dirX2, selected="")
  updateSelectInput(session, "vX3", choices=dirX3, selected="")
  updateSelectInput(session, "vX4", choices=dirX4, selected="")
  updateNumericInput(session, "vP1", value=0L)
  updateNumericInput(session, "vP2", value=0L)
  updateNumericInput(session, "vP3", value=0L)
  updateNumericInput(session, "vP4", value=0L)  
  updateSelectInput(session, "vC1", choices=dirC1, selected="")
  updateSelectInput(session, "vC2", choices=dirC2, selected="")
  updateSelectInput(session, "vC3", choices=dirC3, selected="")
  updateSelectInput(session, "vC4", choices=dirC4, selected="")
  updateTextInput(session, "vPERSON1", value="")
  updateTextInput(session, "vPERSON2", value="")
  updateTextInput(session, "vNOTES", value="")
}
fillVideosInputs <- function(data, session) {
  updateSelectInput(session, "vVIDEOID", choices=data$VIDEOID, selected=data$VIDEOID)
  updateNumericInput(session, "vSIGNALID", value=data$SIGNALID)
  updateTextInput(session, "vLOCATION", value=data$LOCATION)
  updateDateInput(session, "vTIME1date", value=as.Date(data$TIME1, tz="America/Denver"))
  updateSelectInput(session, "vTIME1hr", selected=format(data$TIME1, format="%H"))
  updateSelectInput(session, "vTIME1min", selected=format(data$TIME1, format="%M"))
  updateSelectInput(session, "vTIME1sec", selected=format(data$TIME1, format="%S"))
  updateDateInput(session, "vTIME2date", value=as.Date(data$TIME2, tz="America/Denver"))
  updateSelectInput(session, "vTIME2hr", selected=format(data$TIME2, format="%H"))
  updateSelectInput(session, "vTIME2min", selected=format(data$TIME2, format="%M"))
  updateSelectInput(session, "vTIME2sec", selected=format(data$TIME2, format="%S"))
  updateSelectInput(session, "vX1", selected=data$X1)
  updateSelectInput(session, "vX2", selected=data$X2)
  updateSelectInput(session, "vX3", selected=data$X3)
  updateSelectInput(session, "vX4", selected=data$X4)
  updateNumericInput(session, "vP1", value=data$P1)
  updateNumericInput(session, "vP2", value=data$P2)
  updateNumericInput(session, "vP3", value=data$P3)
  updateNumericInput(session, "vP4", value=data$P4)
  updateSelectInput(session, "vC1", selected=data$C1)
  updateSelectInput(session, "vC2", selected=data$C2)
  updateSelectInput(session, "vC3", selected=data$C3)
  updateSelectInput(session, "vC4", selected=data$C4)
  updateTextInput(session, "vPERSON1", value=data$PERSON1)
  updateTextInput(session, "vPERSON2", value=data$PERSON2)
  updateTextInput(session, "vNOTES", value=data$NOTES)
}

# Functions for events
loadEventsData <- function() {
  load(file="events.Rdata")
  events
}
saveEventsData <- function(events) {
  row.names(events) <- events$COUNTID
  save(events, file="events.Rdata")
  write.csv(events, file="events.csv", row.names=F)
}
addEventsData <- function(data) {
  events <- rbind(loadEventsData(), data)
  saveEventsData(events)
}
deleteEventsData <- function(rows) {
  events <- loadEventsData()[-rows,]
  saveEventsData(events)
}
editEventsData <- function(data) {
  events <- loadEventsData()
  events[events$EVENTID==data$EVENTID,] <- data
  saveEventsData(events)
}
updateEventsInputs <- function(session) {
  videos <- loadVideosData()
  events <- loadEventsData()
  if (nrow(events) > 0) {
    myEVENTID <- max(events$EVENTID)
    myVIDEOID <- events$VIDEOID[which(events$EVENTID==myEVENTID)]
    myTIMEdate <- as.Date(events$TIME[which(events$EVENTID==myEVENTID)], tz="America/Denver")
    myPERSON <- events$PERSON[which(events$EVENTID==myEVENTID)]
    myEVENTID <- max(events$EVENTID)+1L
  } else {
    myEVENTID <- 1
    myVIDEOID <- max(videos$VIDEOID)
    myTIMEdate <- as.Date(videos$TIME1[which(videos$VIDEOID==myVIDEOID)], tz="America/Denver")
    myPERSON <- ""
  }
  updateSelectInput(session, "eEVENTID", choices=myEVENTID, selected=myEVENTID)
  updateSelectInput(session, "eVIDEOID", choices=unique(videos$VIDEOID), selected=myVIDEOID)
  updateNumericInput(session, "eTIMEdate", value=myTIMEdate)
  updateSelectInput(session, "eTIMEhr", choices=hours, selected="00")
  updateSelectInput(session, "eTIMEmin", choices=minutes, selected="00")
  updateSelectInput(session, "eTIMEsec", choices=seconds, selected="00")
  # updateSelectInput(session, "eX1DIR", choices=dirX1, selected="")
  updateNumericInput(session, "eX1PED", value=0L)
  updateNumericInput(session, "eX1DUP", value=0L)
  # updateSelectInput(session, "eX2DIR", choices=dirX2, selected="")
  updateNumericInput(session, "eX2PED", value=0L)
  updateNumericInput(session, "eX2DUP", value=0L)
  # updateSelectInput(session, "eX3DIR", choices=dirX3, selected="")
  updateNumericInput(session, "eX3PED", value=0L)
  updateNumericInput(session, "eX3DUP", value=0L)
  # updateSelectInput(session, "eX4DIR", choices=dirX4, selected="")
  updateNumericInput(session, "eX4PED", value=0L)
  updateNumericInput(session, "eX4DUP", value=0L)
  # updateSelectInput(session, "eC1DIR", choices=dirC1, selected="")
  updateNumericInput(session, "eC1PED", value=0L)
  # updateSelectInput(session, "eC2DIR", choices=dirC2, selected="")
  updateNumericInput(session, "eC2PED", value=0L)
  # updateSelectInput(session, "eC3DIR", choices=dirC3, selected="")
  updateNumericInput(session, "eC3PED", value=0L)
  # updateSelectInput(session, "eC4DIR", choices=dirC4, selected="")
  updateNumericInput(session, "eC4PED", value=0L)
  updateSelectInput(session, "eBOWHAT", choices=BOlist, selected="")
  updateSelectInput(session, "eBODIR", choices=directions, selected="")
  updateNumericInput(session, "eBONUM", value=0L)
  updateTextInput(session, "eBONOTES", value="")
  updateTextInput(session, "ePERSON", value=myPERSON)
  updateTextInput(session, "ePERSON2", value="")
  updateTextInput(session, "eNOTES", value="")
}
fillEventsInputs <- function(data, session) {
  updateSelectInput(session, "e1EVENTID", choices=data$EVENTID, selected=data$EVENTID)
  updateSelectInput(session, "e1VIDEOID", selected=data$VIDEOID)
  updateDateInput(session, "e1TIMEdate", value=as.Date(data$TIME, tz="America/Denver"))
  updateSelectInput(session, "e1TIMEhr", selected=format(data$TIME, format="%H"))
  updateSelectInput(session, "e1TIMEmin", selected=format(data$TIME, format="%M"))
  updateSelectInput(session, "e1TIMEsec", selected=format(data$TIME, format="%S"))
  updateSelectInput(session, "e1X1DIR", selected=data$X1DIR)
  updateNumericInput(session, "e1X1PED", value=data$X1PED)
  updateNumericInput(session, "e1X1DUP", value=data$X1DUP)
  updateSelectInput(session, "e1X2DIR", selected=data$X2DIR)
  updateNumericInput(session, "e1X2PED", value=data$X2PED)
  updateNumericInput(session, "e1X2DUP", value=data$X2DUP)
  updateSelectInput(session, "e1X3DIR", selected=data$X3DIR)
  updateNumericInput(session, "e1X3PED", value=data$X3PED)
  updateNumericInput(session, "e1X3DUP", value=data$X3DUP)
  updateSelectInput(session, "e1X4DIR", selected=data$X4DIR)
  updateNumericInput(session, "e1X4PED", value=data$X4PED)
  updateNumericInput(session, "e1X4DUP", value=data$X4DUP)
  updateSelectInput(session, "e1C1DIR", selected=data$C1DIR)
  updateNumericInput(session, "e1C1PED", value=data$C1PED)
  updateSelectInput(session, "e1C2DIR", selected=data$C2DIR)
  updateNumericInput(session, "e1C2PED", value=data$C2PED)
  updateSelectInput(session, "e1C3DIR", selected=data$C3DIR)
  updateNumericInput(session, "e1C3PED", value=data$C3PED)
  updateSelectInput(session, "e1C4DIR", selected=data$C4DIR)
  updateNumericInput(session, "e1C4PED", value=data$C4PED)
  updateSelectInput(session, "e1BOWHAT", selected=data$BOWHAT)
  updateSelectInput(session, "e1BODIR", selected=data$BODIR)
  updateNumericInput(session, "e1BONUM", value=data$BONUM)
  updateTextInput(session, "e1BONOTES", value=data$BONOTES)
  updateTextInput(session, "e1PERSON", value=data$PERSON)
  updateTextInput(session, "e1PERSON2", value=data$PERSON2)
  updateTextInput(session, "e1NOTES", value=data$NOTES)
}

########################################
# User interface

ui <- navbarPage(title="Pedestrian video data collection", 
  
  # Stuff for videos
  tabPanel(title="Videos", shinyjs::useShinyjs(), 
    checkboxGroupInput("show_vars_video", "Select columns to show:", names(videos), 
                       selected = c("VIDEOID", "SIGNALID", "LOCATION", "TIME1", "TIME2", 
                                    "X1", "X2", "X3", "X4", "P1", "P2", "P3", "P4", "C1", "C2", "C3", "C4", 
                                    "PERSON1", "PERSON2", "SAVED", "NOTES"), 
                       inline=T), 
    hr(), 
    DTOutput("allvideos"), 
    hr(), 
    fluidRow( column(12, h3("Actions")) ), 
    fluidRow(
      column( 2, h4("View or") ), 
      column( 2, uiOutput("add_a_video") ), 
      column( 3, h4("or select a row and") ), 
      column( 2, uiOutput("edit_a_video") ), 
      column( 2, uiOutput("delete_video") ), 
      column( 1 )
    ),
    hr(),
    conditionalPanel(
      condition="input.add_a_video==true | input.edit_a_video==true",
      fluidRow(
        column( 2, shinyjs::disabled(selectInput("vVIDEOID", "Video ID", choices=max(loadVideosData()$VIDEOID,0)+1L, selected=max(loadVideosData()$VIDEOID,0)+1L, width="100%")) ),
        column( 2, numericInput("vSIGNALID", "Signal ID", value=0L, min=0L, max=9999L, step=1L, width="100%") ),
        column( 8, textInput("vLOCATION", "Signal name", value="", width="100%", placeholder="Main St. Center St. (Logan)") )
      ),
      fluidRow(
        column( 4, h4("Video start time:") ),
        column( 2, dateInput("vTIME1date", "Date", value=Sys.Date(), min=as.Date("2001-01-01"), max=as.Date("2020-12-31"), format="mm/dd/yyyy", width="100%") ),
        column( 1, selectInput("vTIME1hr", "Hour", choices=hours, selected="00", width="100%") ),
        column( 1, selectInput("vTIME1min", "Minute", choices=minutes, selected="00", width="100%") ),
        column( 1, selectInput("vTIME1sec", "Second", choices=seconds, selected="00", width="100%") ),
        column( 3, uiOutput("vTIME1") )
      ),
      fluidRow(
        column( 4, h4("Video end time:") ),
        column( 2, dateInput("vTIME2date", "Date", value=Sys.Date(), min=as.Date("2001-01-01"), max=as.Date("2020-12-31"), format="mm/dd/yyyy", width="100%") ),
        column( 1, selectInput("vTIME2hr", "Hour", choices=hours, selected="00", width="100%") ),
        column( 1, selectInput("vTIME2min", "Minute", choices=minutes, selected="00", width="100%") ),
        column( 1, selectInput("vTIME2sec", "Second", choices=seconds, selected="00", width="100%") ),
        column( 3, uiOutput("vTIME2") )
      ),
      fluidRow(
        column( 4, h4("Crosswalks visible:") ),
        column( 2, selectInput("vX1", "1: N / NE", choices=dirX1, selected="", width="100%") ),
        column( 2, selectInput("vX2", "2: E / SE", choices=dirX2, selected="", width="100%") ),
        column( 2, selectInput("vX3", "3: S / SW", choices=dirX3, selected="", width="100%") ),
        column( 2, selectInput("vX4", "4: W / NW", choices=dirX4, selected="", width="100%") )
      ),
      fluidRow(
        column( 4, h4("Signal phase:")), 
        column( 2, numericInput("vP1", "1: N / NE", value=0L, min=0L, max=9999L, step=1L, width="100%") ),
        column( 2, numericInput("vP2", "2: E / SE", value=0L, min=0L, max=9999L, step=1L, width="100%") ),
        column( 2, numericInput("vP3", "3: S / SW", value=0L, min=0L, max=9999L, step=1L, width="100%") ),
        column( 2, numericInput("vP4", "4: W / NW", value=0L, min=0L, max=9999L, step=1L, width="100%") )
      ), 
      fluidRow(
        column( 4, h4("Corners visible:") ),
        column( 2, selectInput("vC1", "1: NE / E", choices=dirC1, selected="", width="100%") ),
        column( 2, selectInput("vC2", "2: SE / S", choices=dirC2, selected="", width="100%") ),
        column( 2, selectInput("vC3", "3: SW / W", choices=dirC3, selected="", width="100%") ),
        column( 2, selectInput("vC4", "4: NW / N", choices=dirC4, selected="", width="100%") )
      ),
      fluidRow(
        column( 2, textInput("vPERSON1", "Created by", value="", width="100%", placeholder="ABC") ),
        column( 8, textInput("vNOTES", "Notes", value="", width="100%") ), 
        column( 2, textInput("vPERSON2", "Checked by", value="", width="100%", placeholder="ABC") )
      ),
      fluidRow(
        column( 12 )
      ),
      fluidRow(
        column( 2, actionButton("submit_video", "Submit") ),
        column( 2, uiOutput("view_video") ), 
        column( 6 ),
        column( 2, uiOutput("new_video") )
      ),
      hr(),
      conditionalPanel(
        condition="input.view_video==true",
        DTOutput("myvideo"),
        hr()
      )
    )
  ),

  # Stuff for events
  tabPanel(title="Events",
    checkboxGroupInput("show_vars_event", "Select columns to show:", names(events),
                       selected = c("EVENTID", "VIDEOID", "SIGNALID", "TIME", 
                                    "PED", "XPED", "XDUP", "CPED", 
                                    "X1DIR", "X1PED", "X1DUP", "X2DIR", "X2PED", "X2DUP", 
                                    "X3DIR", "X3PED", "X3DUP", "X4DIR", "X4PED", "X4DUP", 
                                    "C1DIR", "C1PED", "C2DIR", "C2PED", "C3DIR", "C3PED", "C4DIR", "C4PED", 
                                    "BOWHAT", "BODIR", "BONUM", "BONOTES", 
                                    "PERSON", "PERSON2", "SAVED", "NOTES"),
                       inline=T),
    hr(),
    DTOutput("allevents"),
    hr(),
    fluidRow( column(12, h3("Actions")) ),
    fluidRow(
      column( 2, h4("View or") ),
      column( 2, uiOutput("add_an_event") ),
      column( 3, h4("or select a row and") ),
      column( 2, uiOutput("edit_an_event") ),
      column( 2, uiOutput("delete_event") ),
      column( 1 )
    ),
    hr(),
    conditionalPanel(
      condition="input.edit_an_event==true",
      fluidRow(
        column( 2, shinyjs::disabled(selectInput("e1EVENTID", "Event ID", choices=unique(loadEventsData()$EVENTID), selected=unique(loadEventsData()$EVENTID)[1], width="100%")) ),
        column( 2, selectInput("e1VIDEOID", "Video ID", choices=unique(loadVideosData()$VIDEOID), selected=unique(loadEventsData()$VIDEOID)[1], width="100%") ),
        column( 2, uiOutput("e1SIGNALID") ),
        column( 6, uiOutput("e1LOCATION") )
      ),
      fluidRow(
        column( 4, h4("Event time:") ),
        column( 2, dateInput("e1TIMEdate", "Date", value=as.Date(loadVideosData()$TIME1[which(loadVideosData()$VIDEOID==max(loadVideosData()$VIDEOID))], tz="America/Denver"), min=as.Date("2001-01-01"), max=as.Date("2020-12-31"), format="mm/dd/yyyy", width="100%") ),
        column( 1, selectInput("e1TIMEhr", "Hour", choices=hours, selected="00", width="100%") ),
        column( 1, selectInput("e1TIMEmin", "Minute", choices=minutes, selected="00", width="100%") ),
        column( 1, selectInput("e1TIMEsec", "Second", choices=seconds, selected="00", width="100%") ),
        column( 3, uiOutput("e1TIME") )
      ),
      fluidRow(
        column( 4, h4("Pedestrian totals:") ), 
        column( 2, uiOutput("e1PED") ), column( 2, uiOutput("e1XPED") ), 
        column( 2, uiOutput("e1XDUP") ), column( 2, uiOutput("e1CPED") )
      ), 
      fluidRow(
        column( 2, uiOutput("e1C4DIR") ), column( 1, uiOutput("e1C4PED") ), column( 1 ), 
        column( 2, uiOutput("e1X1DIR") ), column( 1, uiOutput("e1X1PED") ), column( 1, uiOutput("e1X1DUP") ), 
        column( 2, uiOutput("e1C1DIR") ), column( 1, uiOutput("e1C1PED") ), column( 1 )
      ),
      fluidRow(
        column( 2, uiOutput("e1X4DIR") ), column( 1, uiOutput("e1X4PED") ), column( 1, uiOutput("e1X4DUP") ), 
        column( 4 ),
        column( 2, uiOutput("e1X2DIR") ), column( 1, uiOutput("e1X2PED") ), column( 1, uiOutput("e1X2DUP") )
      ),
      fluidRow(
        column( 2, uiOutput("e1C3DIR") ), column( 1, uiOutput("e1C3PED") ), column( 1 ), 
        column( 2, uiOutput("e1X3DIR") ), column( 1, uiOutput("e1X3PED") ), column( 1, uiOutput("e1X3DUP") ), 
        column( 2, uiOutput("e1C2DIR") ), column( 1, uiOutput("e1C2PED") ), column( 1 )
      ),
      fluidRow(
        column( 2, h4("Other:") ),
        column( 2, selectInput("e1BOWHAT", "Mode", choices=BOlist, selected="", width="100%") ), 
        column( 2, selectInput("e1BODIR", "Crossing/Corner", choices=directions, selected="", width="100%") ), 
        column( 2, numericInput("e1BONUM", "Count", value=0L, min=0L, step=1, width="100%") ), 
        column( 4, textInput("e1BONOTES", "Notes", value="", width="100%") )
      ), 
      fluidRow(
        column( 2, textInput("e1PERSON", "Added by", value="", width="100%", placeholder="ABC") ),
        column( 8, textInput("e1NOTES", "Notes", value="", width="100%") ), 
        column( 2, textInput("e1PERSON2", "Checked by", value="", width="100%", placeholder="ABC") )
      ),
      fluidRow(
        column( 2, actionButton("edit_event", "Submit edit") ),
        column( 2, uiOutput("view_event") ), 
        column( 6 ),
        column( 2 )
      ),
      hr(), 
      conditionalPanel(
        condition="input.view_event==true",
        DTOutput("myevent1"), 
        hr()
      )
    )
  ),
  tabPanel(title="Add event",
    fluidRow(
      column( 2, shinyjs::disabled(selectInput("eEVENTID", "Event ID", choices=max(loadEventsData()$EVENTID,0)+1L, selected=max(loadEventsData()$EVENTID,0)+1L, width="100%")) ),
      column( 2, selectInput("eVIDEOID", "Video ID", choices=unique(loadVideosData()$VIDEOID), selected=max(loadVideosData()$VIDEOID), width="100%") ), 
      column( 2, uiOutput("eSIGNALID") ),
      column( 6, uiOutput("eLOCATION") )
    ),
    fluidRow(
      column( 4, h4("Event time:") ),
      column( 2, dateInput("eTIMEdate", "Date", value=as.Date(loadVideosData()$TIME1[which(loadVideosData()$VIDEOID==max(loadVideosData()$VIDEOID))], tz="America/Denver"), min=as.Date("2001-01-01"), max=as.Date("2020-12-31"), format="mm/dd/yyyy", width="100%") ),
      column( 1, selectInput("eTIMEhr", "Hour", choices=hours, selected="00", width="100%") ),
      column( 1, selectInput("eTIMEmin", "Minute", choices=minutes, selected="00", width="100%") ),
      column( 1, selectInput("eTIMEsec", "Second", choices=seconds, selected="00", width="100%") ),
      column( 3, uiOutput("eTIME") )
    ),
    fluidRow(
      column( 4, h4("Pedestrian totals:") ), 
      column( 2, uiOutput("ePED") ), column( 2, uiOutput("eXPED") ), 
      column( 2, uiOutput("eXDUP") ), column( 2, uiOutput("eCPED") )
    ), 
    fluidRow(
      column( 2, uiOutput("eCDDIR") ), column( 1, uiOutput("eCDPED") ), column( 1 ),
      column( 2, uiOutput("eXADIR") ), column( 1, uiOutput("eXAPED") ), column( 1, uiOutput("eXADUP") ), 
      column( 2, uiOutput("eCADIR") ), column( 1, uiOutput("eCAPED") ), column( 1 )
    ),
    fluidRow(
      column( 2, uiOutput("eXDDIR") ), column( 1, uiOutput("eXDPED") ), column( 1, uiOutput("eXDDUP") ), 
      column( 1 ),
      column( 2, uiOutput("rotate_view") ), 
      column( 1 ),
      column( 2, uiOutput("eXBDIR") ), column( 1, uiOutput("eXBPED") ), column( 1, uiOutput("eXBDUP") )
    ),
    fluidRow(
      column( 2, uiOutput("eCCDIR") ), column( 1, uiOutput("eCCPED") ), column( 1 ), 
      column( 2, uiOutput("eXCDIR") ), column( 1, uiOutput("eXCPED") ), column( 1, uiOutput("eXCDUP") ), 
      column( 2, uiOutput("eCBDIR") ), column( 1, uiOutput("eCBPED") ), column( 1 )
    ),
    fluidRow(
      column( 2, h4("Other:") ),
      column( 2, selectInput("eBOWHAT", "Mode", choices=BOlist, selected="", width="100%") ), 
      column( 2, selectInput("eBODIR", "Crossing/Corner", choices=directions, selected="", width="100%") ), 
      column( 2, numericInput("eBONUM", "Count", value=0L, min=0L, step=1, width="100%") ), 
      column( 4, textInput("eBONOTES", "Notes", value="", width="100%") )
    ), 
    fluidRow(
      column( 2, textInput("ePERSON", "Added by", value="", width="100%", placeholder="ABC") ),
      column( 8, textInput("eNOTES", "Notes", value="", width="100%") ), 
      column( 2, textInput("ePERSON2", "Checked by", value="", width="100%", placeholder="ABC") )
    ),
    fluidRow(
      column( 2, actionButton("add_event", "Add event") ),
      column( 2, uiOutput("view_an_event") ),
      column( 6 ),
      column( 2, actionButton("new_event", "Clear") )
    ),
    hr(),
    conditionalPanel(
      condition="input.view_an_event==true",
      DTOutput("myevent"), 
      hr()
    )
  )
)

########################################
# Server logic

server <- function(input, output, session) {
  
  # Stuff for videos
  formVideoData <- reactive({
    temp1 <- paste0(input$vTIME1date, " ", paste(input$vTIME1hr, input$vTIME1min, input$vTIME1sec, sep=":"))
    temp2 <- paste0(input$vTIME2date, " ", paste(input$vTIME2hr, input$vTIME2min, input$vTIME2sec, sep=":"))
    data.frame(VIDEOID=as.integer(input$vVIDEOID), SIGNALID=as.integer(input$vSIGNALID), 
               LOCATION=as.character(input$vLOCATION), 
               TIME1=as.POSIXct(as.character(temp1), tz="America/Denver"), 
               TIME2=as.POSIXct(as.character(temp2), tz="America/Denver"), 
               # TIME1=as.POSIXct(as.character(input$vTIME1), tz="America/Denver"), 
               # TIME2=as.POSIXct(as.character(input$vTIME2), tz="America/Denver"), 
               X1=as.character(input$vX1), X2=as.character(input$vX2), X3=as.character(input$vX3), X4=as.character(input$vX4), 
               P1=as.integer(input$vP1), P2=as.integer(input$vP2), P3=as.integer(input$vP3), P4=as.integer(input$vP4), 
               C1=as.character(input$vC1), C2=as.character(input$vC2), C3=as.character(input$vC3), C4=as.character(input$vC4), 
               PERSON1=as.character(input$vPERSON1), PERSON2=as.character(input$vPERSON2), 
               SAVED=Sys.time(), 
               NOTES=as.character(input$vNOTES), stringsAsFactors=F)
  })
  observeEvent(
    input$add_a_video, 
    {
      updateVideosInputs(session)
    }, priority=1
  )
  observeEvent(
    input$allvideos_rows_selected, 
    {
      if (length(input$allvideos_rows_selected)==1) {
        updateCheckboxInput(session, "edit_a_video", value=T)
      }
    }
  )
  observeEvent(
    input$edit_a_video, 
    {
      if (length(input$allvideos_rows_selected)==1) {
        data <- loadVideosData()[input$allvideos_rows_selected, ]
        fillVideosInputs(data, session)
      }
    }
  )
  observeEvent(
    input$delete_video, 
    {
      deleteVideosData(input$allvideos_rows_selected)
      updateVideosInputs(session)
      updateEventsInputs(session)
    }, priority=1
  )
  observeEvent(
    input$submit_video, 
    {
      if(input$add_a_video==T) {
        addVideosData(formVideoData())
        updateVideosInputs(session)
        updateEventsInputs(session)
        updateCheckboxInput(session, "add_a_video", value=F)
        updateCheckboxInput(session, "view_video", value=F)
      } else if(input$edit_a_video==T) {
        editVideosData(formVideoData())
        updateVideosInputs(session)
        updateEventsInputs(session)
        updateCheckboxInput(session, "edit_a_video", value=F)
        updateCheckboxInput(session, "view_video", value=F)
      } else { }
    }, priority=1
  )
  observeEvent(
    input$new_video, 
    {
      updateVideosInputs(session)
    }, priority=1
  )
  output$add_a_video <- renderUI({
    if (length(input$allvideos_rows_selected)==1) {
      shinyjs::disabled(checkboxInput("add_a_video", "Add new", value=F))
    } else {
      checkboxInput("add_a_video", "Add new", value=F)
    }
  })
  output$edit_a_video <- renderUI({
    if (length(input$allvideos_rows_selected)==1) {
      checkboxInput("edit_a_video", "Edit selected", value=T)
    } else {
      shinyjs::disabled(checkboxInput("edit_a_video", "Edit selected", value=F))
    }
  })
  output$delete_video <- renderUI({
    if (length(input$allvideos_rows_selected)==1) {
      actionButton("delete_video", "Delete selected")
    } else {
      shinyjs::disabled(actionButton("delete_video", "Delete selected"))
    }
  })
  output$new_video <- renderUI({
    if(length(input$add_a_video)!=0) {
      if(input$add_a_video==T) {
        actionButton("new_video", "Clear")
      } else { }
    } else { }
  })
  output$view_video <- renderUI({
    checkboxInput("view_video", "View", value=F)
  })
  output$vTIME1 <- renderUI({
    shinyjs::disabled(textInput("vTIME1", "Time", value=paste0(input$vTIME1date, " ", paste(input$vTIME1hr, input$vTIME1min, input$vTIME1sec, sep=":")), width="100%"))
  })
  output$vTIME2 <- renderUI({
    shinyjs::disabled(textInput("vTIME2", "Time", value=paste0(input$vTIME2date, " ", paste(input$vTIME2hr, input$vTIME2min, input$vTIME2sec, sep=":")), width="100%"))
  })
  output$myvideo <- renderDT({
    temp <- formVideoData()
    temp$TIME1 <- as.character(temp$TIME1)
    temp$TIME2 <- as.character(temp$TIME2)
    temp$SAVED <- as.character(temp$SAVED)
    DT::datatable(temp, options=list(dom="t"), rownames=F, filter="none", selection="none")
  })
  output$allvideos <- renderDT({
    input$delete_video
    input$submit_video
    temp <- loadVideosData()
    temp$SIGNALID <- as.factor(temp$SIGNALID)
    temp$TIME1 <- as.character(temp$TIME1)
    temp$TIME2 <- as.character(temp$TIME2)
    temp$X1 <- as.factor(temp$X1)
    temp$X2 <- as.factor(temp$X2)
    temp$X3 <- as.factor(temp$X3)
    temp$X4 <- as.factor(temp$X4)
    temp$C1 <- as.factor(temp$C1)
    temp$C2 <- as.factor(temp$C2)
    temp$C3 <- as.factor(temp$C3)
    temp$C4 <- as.factor(temp$C4)
    temp$SAVED <- as.character(temp$SAVED)
    DT::datatable(temp[,input$show_vars_video], options = list(lengthMenu=c(5, 10, 25, 50, 100), pageLength=5), rownames=F, filter=list(position="top", clear=F), selection="single")
  })
  
  # Stuff for events
  rot_i <- reactiveVal(0)
  formEventData <- reactive({
    data.frame(EVENTID=as.integer(input$eEVENTID), 
               VIDEOID=as.integer(input$eVIDEOID), SIGNALID=as.integer(input$eSIGNALID), 
               TIME=as.POSIXct(as.character(input$eTIME), tz="America/Denver"), 
               PED=as.integer(input$ePED), XPED=as.integer(input$eXPED), 
               XDUP=as.integer(input$eXDUP), CPED=as.integer(input$eCPED), 
               X1DIR=as.character(input$eX1DIR), X1PED=as.integer(input$eX1PED), X1DUP=as.integer(input$eX1DUP), 
               X2DIR=as.character(input$eX2DIR), X2PED=as.integer(input$eX2PED), X2DUP=as.integer(input$eX2DUP), 
               X3DIR=as.character(input$eX3DIR), X3PED=as.integer(input$eX3PED), X3DUP=as.integer(input$eX3DUP), 
               X4DIR=as.character(input$eX4DIR), X4PED=as.integer(input$eX4PED), X4DUP=as.integer(input$eX4DUP), 
               C1DIR=as.character(input$eC1DIR), C1PED=as.integer(input$eC1PED), 
               C2DIR=as.character(input$eC2DIR), C2PED=as.integer(input$eC2PED), 
               C3DIR=as.character(input$eC3DIR), C3PED=as.integer(input$eC3PED), 
               C4DIR=as.character(input$eC4DIR), C4PED=as.integer(input$eC4PED), 
               BOWHAT=as.character(input$eBOWHAT), BODIR=as.character(input$eBODIR), 
               BONUM=as.integer(input$eBONUM), BONOTES=as.character(input$eBONOTES), 
               PERSON=as.character(input$ePERSON), PERSON2=as.character(input$ePERSON2), SAVED=Sys.time(), 
               NOTES=as.character(input$eNOTES), stringsAsFactors=F)
  })
  formEvent1Data <- reactive({
    data.frame(EVENTID=as.integer(input$e1EVENTID), 
               VIDEOID=as.integer(input$e1VIDEOID), SIGNALID=as.integer(input$e1SIGNALID), 
               TIME=as.POSIXct(as.character(input$e1TIME), tz="America/Denver"), 
               PED=as.integer(input$e1PED), XPED=as.integer(input$e1XPED), 
               XDUP=as.integer(input$e1XDUP), CPED=as.integer(input$e1CPED), 
               X1DIR=as.character(input$e1X1DIR), X1PED=as.integer(input$e1X1PED), X1DUP=as.integer(input$e1X1DUP), 
               X2DIR=as.character(input$e1X2DIR), X2PED=as.integer(input$e1X2PED), X2DUP=as.integer(input$e1X2DUP), 
               X3DIR=as.character(input$e1X3DIR), X3PED=as.integer(input$e1X3PED), X3DUP=as.integer(input$e1X3DUP), 
               X4DIR=as.character(input$e1X4DIR), X4PED=as.integer(input$e1X4PED), X4DUP=as.integer(input$e1X4DUP), 
               C1DIR=as.character(input$e1C1DIR), C1PED=as.integer(input$e1C1PED), 
               C2DIR=as.character(input$e1C2DIR), C2PED=as.integer(input$e1C2PED), 
               C3DIR=as.character(input$e1C3DIR), C3PED=as.integer(input$e1C3PED), 
               C4DIR=as.character(input$e1C4DIR), C4PED=as.integer(input$e1C4PED), 
               BOWHAT=as.character(input$e1BOWHAT), BODIR=as.character(input$e1BODIR), 
               BONUM=as.integer(input$e1BONUM), BONOTES=as.character(input$e1BONOTES), 
               PERSON=as.character(input$e1PERSON), PERSON2=as.character(input$e1PERSON2), SAVED=Sys.time(), 
               NOTES=as.character(input$e1NOTES), stringsAsFactors=F)
  })
  observeEvent(
    input$add_event,
    {
      addEventsData(formEventData())
      updateEventsInputs(session)
    }, priority=1
  )
  observeEvent(
    input$add_an_event,
    {
      updateEventsInputs(session)
    }, priority=1
  )
  observeEvent(
    input$allevents_rows_selected,
    {
      if (length(input$allevents_rows_selected)==1) {
        updateCheckboxInput(session, "edit_an_event", value=T)
      }
    }
  )
  observeEvent(
    input$edit_an_event,
    {
      if (length(input$allevents_rows_selected)==1) {
        data <- loadEventsData()[input$allevents_rows_selected, ]
        fillEventsInputs(data, session)
      }
    }
  )
  observeEvent(
    input$delete_event, 
    {
      deleteEventsData(input$allevents_rows_selected)
      updateEventsInputs(session)
    }, priority=1
  )
  observeEvent(
    input$edit_event, 
    {
      editEventsData(formEvent1Data())
      updateEventsInputs(session)
      updateCheckboxInput(session, "edit_an_event", value=F)
    }, priority=1
  )
  observeEvent(
    input$new_event, 
    {
      updateEventsInputs(session)
    }, priority=1
  )
  observeEvent(
    input$rotate_view, 
    {
      rot_i((rot_i() + 1) %% 4)
    }, priority=1
  )
  output$rotate_view <- renderUI({
    shinyjs::disabled(actionButton("rotate_view", paste0("Rotate Clockwise"), width="100%"))
  })
  output$add_an_event <- renderUI({
    shinyjs::disabled(checkboxInput("add_an_event", "Add new", value=F))
  })
  output$edit_an_event <- renderUI({
    if (length(input$allevents_rows_selected)==1) {
      checkboxInput("edit_an_event", "Edit selected", value=T)
    } else {
      shinyjs::disabled(checkboxInput("edit_an_event", "Edit selected", value=F))
    }
  })
  output$delete_event <- renderUI({
    if (length(input$allevents_rows_selected)==1) {
      actionButton("delete_event", "Delete selected")
    } else {
      shinyjs::disabled(actionButton("delete_event", "Delete selected"))
    }
  })
  output$view_event <- renderUI({
    checkboxInput("view_event", "View", value=F)
  })
  output$view_an_event <- renderUI({
    checkboxInput("view_an_event", "View", value=F)
  })
  output$eSIGNALID <- renderUI({
    shinyjs::disabled(numericInput("eSIGNALID", "Signal ID", value=subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"SIGNALID"], width="100%"))
  })
  output$e1SIGNALID <- renderUI({
    shinyjs::disabled(numericInput("e1SIGNALID", "Signal ID", value=subset(loadVideosData(), VIDEOID==input$e1VIDEOID)[,"SIGNALID"], width="100%"))
  })
  output$eLOCATION <- renderUI({
    shinyjs::disabled(textInput("eLOCATION", "Signal name", value=subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"LOCATION"], width="100%"))
  })
  output$e1LOCATION <- renderUI({
    shinyjs::disabled(textInput("e1LOCATION", "Signal name", value=subset(loadVideosData(), VIDEOID==input$e1VIDEOID)[,"LOCATION"], width="100%"))
  })
  output$eTIME <- renderUI({
    shinyjs::disabled(textInput("eTIME", "Time", value=paste0(input$eTIMEdate, " ", paste(input$eTIMEhr, input$eTIMEmin, input$eTIMEsec, sep=":")), width="100%"))
  })
  output$e1TIME <- renderUI({
    shinyjs::disabled(textInput("e1TIME", "Time", value=paste0(input$e1TIMEdate, " ", paste(input$e1TIMEhr, input$e1TIMEmin, input$e1TIMEsec, sep=":")), width="100%"))
  })
  output$ePED <- renderUI({
    shinyjs::disabled(numericInput("ePED", "Total Peds", value=(input$eXPED - input$eXDUP + input$eCPED), min=0L, step=1, width="100%"))
  })
  output$e1PED <- renderUI({
    shinyjs::disabled(numericInput("e1PED", "Total Peds", value=(input$e1XPED - input$e1XDUP + input$e1CPED), min=0L, step=1, width="100%"))
  })
  output$eXPED <- renderUI({
    shinyjs::disabled(numericInput("eXPED", "Total Crossings", value=(input$eX1PED + input$eX2PED + input$eX3PED + input$eX4PED), min=0L, step=1, width="100%"))
  })
  output$e1XPED <- renderUI({
    shinyjs::disabled(numericInput("e1XPED", "Total Crossings", value=(input$e1X1PED + input$e1X2PED + input$e1X3PED + input$e1X4PED), min=0L, step=1, width="100%"))
  })
  output$eXDUP <- renderUI({
    shinyjs::disabled(numericInput("eXDUP", "Total Duplicates", value=(input$eX1DUP + input$eX2DUP + input$eX3DUP + input$eX4DUP), min=0L, step=1, width="100%"))
  })
  output$e1XDUP <- renderUI({
    shinyjs::disabled(numericInput("e1XDUP", "Total Duplicates", value=(input$e1X1DUP + input$e1X2DUP + input$e1X3DUP + input$e1X4DUP), min=0L, step=1, width="100%"))
  })
  output$eCPED <- renderUI({
    shinyjs::disabled(numericInput("eCPED", "Total Corners", value=(input$eC1PED + input$eC2PED + input$eC3PED + input$eC4PED), min=0L, step=1, width="100%"))
  })
  output$e1CPED <- renderUI({
    shinyjs::disabled(numericInput("e1CPED", "Total Corners", value=(input$e1C1PED + input$e1C2PED + input$e1C3PED + input$e1C4PED), min=0L, step=1, width="100%"))
  })
  output$eXADIR <- renderUI({
    if (rot_i()==0) {
      shinyjs::disabled(selectInput("eX1DIR", "Crosswalk 1", choices=dirX1, selected=subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"X1"], width="100%"))
    } else if (rot_i()==1) {
      shinyjs::disabled(selectInput("eX4DIR", "Crosswalk 4", choices=dirX4, selected=subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"X4"], width="100%"))
    } else if (rot_i()==2) {
      shinyjs::disabled(selectInput("eX3DIR", "Crosswalk 3", choices=dirX3, selected=subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"X3"], width="100%"))
    } else if (rot_i()==3) {
      shinyjs::disabled(selectInput("eX2DIR", "Crosswalk 2", choices=dirX2, selected=subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"X2"], width="100%"))
    }
  })
  output$eXAPED <- renderUI({
    if (rot_i()==0) {
      if (subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"X1"]=="") {
        shinyjs::disabled(numericInput("eX1PED", "Peds", value=0L, min=0L, step=1, width="100%"))
      } else {
        numericInput("eX1PED", "Peds", value=0L, min=0L, step=1, width="100%")
      }
    } else if (rot_i()==1) {
      if (subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"X4"]=="") {
        shinyjs::disabled(numericInput("eX4PED", "Peds", value=0L, min=0L, step=1, width="100%"))
      } else {
        numericInput("eX4PED", "Peds", value=0L, min=0L, step=1, width="100%")
      }
    } else if (rot_i()==2) {
      if (subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"X3"]=="") {
        shinyjs::disabled(numericInput("eX3PED", "Peds", value=0L, min=0L, step=1, width="100%"))
      } else {
        numericInput("eX3PED", "Peds", value=0L, min=0L, step=1, width="100%")
      }
    } else if (rot_i()==3) {
      if (subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"X2"]=="") {
        shinyjs::disabled(numericInput("eX2PED", "Peds", value=0L, min=0L, step=1, width="100%"))
      } else {
        numericInput("eX2PED", "Peds", value=0L, min=0L, step=1, width="100%")
      }
    }
  })
  output$eXADUP <- renderUI({
    if (rot_i()==0) {
      if (subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"X1"]=="") {
        shinyjs::disabled(numericInput("eX1DUP", "Dups", value=0L, min=0L, step=1, width="100%"))
      } else {
        numericInput("eX1DUP", "Dups", value=0L, min=0L, step=1, width="100%")
      }
    } else if (rot_i()==1) {
      if (subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"X4"]=="") {
        shinyjs::disabled(numericInput("eX4DUP", "Dups", value=0L, min=0L, step=1, width="100%"))
      } else {
        numericInput("eX4DUP", "Dups", value=0L, min=0L, step=1, width="100%")
      }
    } else if (rot_i()==2) {
      if (subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"X3"]=="") {
        shinyjs::disabled(numericInput("eX3DUP", "Dups", value=0L, min=0L, step=1, width="100%"))
      } else {
        numericInput("eX3DUP", "Dups", value=0L, min=0L, step=1, width="100%")
      }
    } else if (rot_i()==3) {
      if (subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"X2"]=="") {
        shinyjs::disabled(numericInput("eX2DUP", "Dups", value=0L, min=0L, step=1, width="100%"))
      } else {
        numericInput("eX2DUP", "Dups", value=0L, min=0L, step=1, width="100%")
      }
    }
  })
  output$eXBDIR <- renderUI({
    if (rot_i()==0) {
      shinyjs::disabled(selectInput("eX2DIR", "Crosswalk 2", choices=dirX2, selected=subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"X2"], width="100%"))
    } else if (rot_i()==1) {
      shinyjs::disabled(selectInput("eX1DIR", "Crosswalk 1", choices=dirX1, selected=subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"X1"], width="100%"))
    } else if (rot_i()==2) {
      shinyjs::disabled(selectInput("eX4DIR", "Crosswalk 4", choices=dirX4, selected=subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"X4"], width="100%"))
    } else if (rot_i()==3) {
      shinyjs::disabled(selectInput("eX3DIR", "Crosswalk 3", choices=dirX3, selected=subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"X3"], width="100%"))
    }
  })
  output$eXBPED <- renderUI({
    if (rot_i()==0) {
      if (subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"X2"]=="") {
        shinyjs::disabled(numericInput("eX2PED", "Peds", value=0L, min=0L, step=1, width="100%"))
      } else {
        numericInput("eX2PED", "Peds", value=0L, min=0L, step=1, width="100%")
      }
    } else if (rot_i()==1) {
      if (subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"X1"]=="") {
        shinyjs::disabled(numericInput("eX1PED", "Peds", value=0L, min=0L, step=1, width="100%"))
      } else {
        numericInput("eX1PED", "Peds", value=0L, min=0L, step=1, width="100%")
      }
    } else if (rot_i()==2) {
      if (subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"X4"]=="") {
        shinyjs::disabled(numericInput("eX4PED", "Peds", value=0L, min=0L, step=1, width="100%"))
      } else {
        numericInput("eX4PED", "Peds", value=0L, min=0L, step=1, width="100%")
      }
    } else if (rot_i()==3) {
      if (subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"X3"]=="") {
        shinyjs::disabled(numericInput("eX3PED", "Peds", value=0L, min=0L, step=1, width="100%"))
      } else {
        numericInput("eX3PED", "Peds", value=0L, min=0L, step=1, width="100%")
      }
    }
  })
  output$eXBDUP <- renderUI({
    if (rot_i()==0) {
      if (subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"X2"]=="") {
        shinyjs::disabled(numericInput("eX2DUP", "Dups", value=0L, min=0L, step=1, width="100%"))
      } else {
        numericInput("eX2DUP", "Dups", value=0L, min=0L, step=1, width="100%")
      }
    } else if (rot_i()==1) {
      if (subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"X1"]=="") {
        shinyjs::disabled(numericInput("eX1DUP", "Dups", value=0L, min=0L, step=1, width="100%"))
      } else {
        numericInput("eX1DUP", "Dups", value=0L, min=0L, step=1, width="100%")
      }
    } else if (rot_i()==2) {
      if (subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"X4"]=="") {
        shinyjs::disabled(numericInput("eX4DUP", "Dups", value=0L, min=0L, step=1, width="100%"))
      } else {
        numericInput("eX4DUP", "Dups", value=0L, min=0L, step=1, width="100%")
      }
    } else if (rot_i()==3) {
      if (subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"X3"]=="") {
        shinyjs::disabled(numericInput("eX3DUP", "Dups", value=0L, min=0L, step=1, width="100%"))
      } else {
        numericInput("eX3DUP", "Dups", value=0L, min=0L, step=1, width="100%")
      }
    }
  })
  output$eXCDIR <- renderUI({
    if (rot_i()==0) {
      shinyjs::disabled(selectInput("eX3DIR", "Crosswalk 3", choices=dirX3, selected=subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"X3"], width="100%"))
    } else if (rot_i()==1) {
      shinyjs::disabled(selectInput("eX2DIR", "Crosswalk 2", choices=dirX2, selected=subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"X2"], width="100%"))
    } else if (rot_i()==2) {
      shinyjs::disabled(selectInput("eX1DIR", "Crosswalk 1", choices=dirX1, selected=subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"X1"], width="100%"))
    } else if (rot_i()==3) {
      shinyjs::disabled(selectInput("eX4DIR", "Crosswalk 4", choices=dirX4, selected=subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"X4"], width="100%"))
    }
  })
  output$eXCPED <- renderUI({
    if (rot_i()==0) {
      if (subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"X3"]=="") {
        shinyjs::disabled(numericInput("eX3PED", "Peds", value=0L, min=0L, step=1, width="100%"))
      } else {
        numericInput("eX3PED", "Peds", value=0L, min=0L, step=1, width="100%")
      }
    } else if (rot_i()==1) {
      if (subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"X2"]=="") {
        shinyjs::disabled(numericInput("eX2PED", "Peds", value=0L, min=0L, step=1, width="100%"))
      } else {
        numericInput("eX2PED", "Peds", value=0L, min=0L, step=1, width="100%")
      }
    } else if (rot_i()==2) {
      if (subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"X1"]=="") {
        shinyjs::disabled(numericInput("eX1PED", "Peds", value=0L, min=0L, step=1, width="100%"))
      } else {
        numericInput("eX1PED", "Peds", value=0L, min=0L, step=1, width="100%")
      }
    } else if (rot_i()==3) {
      if (subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"X4"]=="") {
        shinyjs::disabled(numericInput("eX4PED", "Peds", value=0L, min=0L, step=1, width="100%"))
      } else {
        numericInput("eX4PED", "Peds", value=0L, min=0L, step=1, width="100%")
      }
    }
  })
  output$eXCDUP <- renderUI({
    if (rot_i()==0) {
      if (subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"X3"]=="") {
        shinyjs::disabled(numericInput("eX3DUP", "Dups", value=0L, min=0L, step=1, width="100%"))
      } else {
        numericInput("eX3DUP", "Dups", value=0L, min=0L, step=1, width="100%")
      }
    } else if (rot_i()==1) {
      if (subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"X2"]=="") {
        shinyjs::disabled(numericInput("eX2DUP", "Dups", value=0L, min=0L, step=1, width="100%"))
      } else {
        numericInput("eX2DUP", "Dups", value=0L, min=0L, step=1, width="100%")
      }
    } else if (rot_i()==2) {
      if (subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"X1"]=="") {
        shinyjs::disabled(numericInput("eX1DUP", "Dups", value=0L, min=0L, step=1, width="100%"))
      } else {
        numericInput("eX1DUP", "Dups", value=0L, min=0L, step=1, width="100%")
      }
    } else if (rot_i()==3) {
      if (subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"X4"]=="") {
        shinyjs::disabled(numericInput("eX4DUP", "Dups", value=0L, min=0L, step=1, width="100%"))
      } else {
        numericInput("eX4DUP", "Dups", value=0L, min=0L, step=1, width="100%")
      }
    }
  })
  output$eXDDIR <- renderUI({
    if (rot_i()==0) {
      shinyjs::disabled(selectInput("eX4DIR", "Crosswalk 4", choices=dirX4, selected=subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"X4"], width="100%"))
    } else if (rot_i()==1) {
      shinyjs::disabled(selectInput("eX3DIR", "Crosswalk 3", choices=dirX3, selected=subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"X3"], width="100%"))
    } else if (rot_i()==2) {
      shinyjs::disabled(selectInput("eX2DIR", "Crosswalk 2", choices=dirX2, selected=subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"X2"], width="100%"))
    } else if (rot_i()==3) {
      shinyjs::disabled(selectInput("eX1DIR", "Crosswalk 1", choices=dirX1, selected=subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"X1"], width="100%"))
    }
  })
  output$eXDPED <- renderUI({
    if (rot_i()==0) {
      if (subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"X4"]=="") {
        shinyjs::disabled(numericInput("eX4PED", "Peds", value=0L, min=0L, step=1, width="100%"))
      } else {
        numericInput("eX4PED", "Peds", value=0L, min=0L, step=1, width="100%")
      }
    } else if (rot_i()==1) {
      if (subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"X3"]=="") {
        shinyjs::disabled(numericInput("eX3PED", "Peds", value=0L, min=0L, step=1, width="100%"))
      } else {
        numericInput("eX3PED", "Peds", value=0L, min=0L, step=1, width="100%")
      }
    } else if (rot_i()==2) {
      if (subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"X2"]=="") {
        shinyjs::disabled(numericInput("eX2PED", "Peds", value=0L, min=0L, step=1, width="100%"))
      } else {
        numericInput("eX2PED", "Peds", value=0L, min=0L, step=1, width="100%")
      }
    } else if (rot_i()==3) {
      if (subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"X1"]=="") {
        shinyjs::disabled(numericInput("eX1PED", "Peds", value=0L, min=0L, step=1, width="100%"))
      } else {
        numericInput("eX1PED", "Peds", value=0L, min=0L, step=1, width="100%")
      }
    }
  })
  output$eXDDUP <- renderUI({
    if (rot_i()==0) {
      if (subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"X4"]=="") {
        shinyjs::disabled(numericInput("eX4DUP", "Dups", value=0L, min=0L, step=1, width="100%"))
      } else {
        numericInput("eX4DUP", "Dups", value=0L, min=0L, step=1, width="100%")
      }
    } else if (rot_i()==1) {
      if (subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"X3"]=="") {
        shinyjs::disabled(numericInput("eX3DUP", "Dups", value=0L, min=0L, step=1, width="100%"))
      } else {
        numericInput("eX3DUP", "Dups", value=0L, min=0L, step=1, width="100%")
      }
    } else if (rot_i()==2) {
      if (subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"X2"]=="") {
        shinyjs::disabled(numericInput("eX2DUP", "Dups", value=0L, min=0L, step=1, width="100%"))
      } else {
        numericInput("eX2DUP", "Dups", value=0L, min=0L, step=1, width="100%")
      }
    } else if (rot_i()==3) {
      if (subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"X1"]=="") {
        shinyjs::disabled(numericInput("eX1DUP", "Dups", value=0L, min=0L, step=1, width="100%"))
      } else {
        numericInput("eX1DUP", "Dups", value=0L, min=0L, step=1, width="100%")
      }
    }
  })
  output$e1X1DIR <- renderUI({
    shinyjs::disabled(selectInput("e1X1DIR", "Crosswalk 1", choices=dirX1, selected=subset(loadVideosData(), VIDEOID==input$e1VIDEOID)[,"X1"], width="100%"))
  })
  output$e1X1PED <- renderUI({
    if (subset(loadVideosData(), VIDEOID==input$e1VIDEOID)[,"X1"]=="") {
      shinyjs::disabled(numericInput("e1X1PED", "Peds", value=0L, min=0L, step=1, width="100%"))
    } else {
      numericInput("e1X1PED", "Peds", value=0L, min=0L, step=1, width="100%")
    }
  })
  output$e1X1DUP <- renderUI({
    if (subset(loadVideosData(), VIDEOID==input$e1VIDEOID)[,"X1"]=="") {
      shinyjs::disabled(numericInput("e1X1DUP", "Dups", value=0L, min=0L, step=1, width="100%"))
    } else {
      numericInput("e1X1DUP", "Dups", value=0L, min=0L, step=1, width="100%")
    }
  })
  output$e1X2DIR <- renderUI({
    shinyjs::disabled(selectInput("e1X2DIR", "Crosswalk 2", choices=dirX2, selected=subset(loadVideosData(), VIDEOID==input$e1VIDEOID)[,"X2"], width="100%"))
  })
  output$e1X2PED <- renderUI({
    if (subset(loadVideosData(), VIDEOID==input$e1VIDEOID)[,"X2"]=="") {
      shinyjs::disabled(numericInput("e1X2PED", "Peds", value=0L, min=0L, step=1, width="100%"))
    } else {
      numericInput("e1X2PED", "Peds", value=0L, min=0L, step=1, width="100%")
    }
  })
  output$e1X2DUP <- renderUI({
    if (subset(loadVideosData(), VIDEOID==input$e1VIDEOID)[,"X2"]=="") {
      shinyjs::disabled(numericInput("e1X2DUP", "Dups", value=0L, min=0L, step=1, width="100%"))
    } else {
      numericInput("e1X2DUP", "Dups", value=0L, min=0L, step=1, width="100%")
    }
  })
  output$e1X3DIR <- renderUI({
    shinyjs::disabled(selectInput("e1X3DIR", "Crosswalk 3", choices=dirX3, selected=subset(loadVideosData(), VIDEOID==input$e1VIDEOID)[,"X3"], width="100%"))
  })
  output$e1X3PED <- renderUI({
    if (subset(loadVideosData(), VIDEOID==input$e1VIDEOID)[,"X3"]=="") {
      shinyjs::disabled(numericInput("e1X3PED", "Peds", value=0L, min=0L, step=1, width="100%"))
    } else {
      numericInput("e1X3PED", "Peds", value=0L, min=0L, step=1, width="100%")
    }
  })
  output$e1X3DUP <- renderUI({
    if (subset(loadVideosData(), VIDEOID==input$e1VIDEOID)[,"X3"]=="") {
      shinyjs::disabled(numericInput("e1X3DUP", "Dups", value=0L, min=0L, step=1, width="100%"))
    } else {
      numericInput("e1X3DUP", "Dups", value=0L, min=0L, step=1, width="100%")
    }
  })
  output$e1X4DIR <- renderUI({
    shinyjs::disabled(selectInput("e1X4DIR", "Crosswalk 4", choices=dirX4, selected=subset(loadVideosData(), VIDEOID==input$e1VIDEOID)[,"X4"], width="100%"))
  })
  output$e1X4PED <- renderUI({
    if (subset(loadVideosData(), VIDEOID==input$e1VIDEOID)[,"X4"]=="") {
      shinyjs::disabled(numericInput("e1X4PED", "Peds", value=0L, min=0L, step=1, width="100%"))
    } else {
      numericInput("e1X4PED", "Peds", value=0L, min=0L, step=1, width="100%")
    }
  })
  output$e1X4DUP <- renderUI({
    if (subset(loadVideosData(), VIDEOID==input$e1VIDEOID)[,"X4"]=="") {
      shinyjs::disabled(numericInput("e1X4DUP", "Dups", value=0L, min=0L, step=1, width="100%"))
    } else {
      numericInput("e1X4DUP", "Dups", value=0L, min=0L, step=1, width="100%")
    }
  })
  output$eCADIR <- renderUI({
    if (rot_i()==0) {
      shinyjs::disabled(selectInput("eC1DIR", "Corner 1", choices=dirC1, selected=subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"C1"], width="100%"))
    } else if (rot_i()==1) {
      shinyjs::disabled(selectInput("eC4DIR", "Corner 4", choices=dirC4, selected=subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"C4"], width="100%"))
    } else if (rot_i()==2) {
      shinyjs::disabled(selectInput("eC3DIR", "Corner 3", choices=dirC3, selected=subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"C3"], width="100%"))
    } else if (rot_i()==3) {
      shinyjs::disabled(selectInput("eC2DIR", "Corner 2", choices=dirC2, selected=subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"C2"], width="100%"))
    }
  })
  output$eCAPED <- renderUI({
    if (rot_i()==0) {
      if (subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"C1"]=="") {
        shinyjs::disabled(numericInput("eC1PED", "Peds", value=0L, min=0L, step=1, width="100%"))
      } else {
        numericInput("eC1PED", "Peds", value=0L, min=0L, step=1, width="100%")
      }
    } else if (rot_i()==1) { 
      if (subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"C4"]=="") {
        shinyjs::disabled(numericInput("eC4PED", "Peds", value=0L, min=0L, step=1, width="100%"))
      } else {
        numericInput("eC4PED", "Peds", value=0L, min=0L, step=1, width="100%")
      }
    } else if (rot_i()==2) { 
      if (subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"C3"]=="") {
        shinyjs::disabled(numericInput("eC3PED", "Peds", value=0L, min=0L, step=1, width="100%"))
      } else {
        numericInput("eC3PED", "Peds", value=0L, min=0L, step=1, width="100%")
      }
    } else if (rot_i()==3) { 
      if (subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"C2"]=="") {
        shinyjs::disabled(numericInput("eC2PED", "Peds", value=0L, min=0L, step=1, width="100%"))
      } else {
        numericInput("eC2PED", "Peds", value=0L, min=0L, step=1, width="100%")
      }
    }
  })
  output$eCBDIR <- renderUI({
    if (rot_i()==0) {
      shinyjs::disabled(selectInput("eC2DIR", "Corner 2", choices=dirC2, selected=subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"C2"], width="100%"))
    } else if (rot_i()==1) {
      shinyjs::disabled(selectInput("eC1DIR", "Corner 1", choices=dirC1, selected=subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"C1"], width="100%"))
    } else if (rot_i()==2) {
      shinyjs::disabled(selectInput("eC4DIR", "Corner 4", choices=dirC4, selected=subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"C4"], width="100%"))
    } else if (rot_i()==3) {
      shinyjs::disabled(selectInput("eC3DIR", "Corner 3", choices=dirC3, selected=subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"C3"], width="100%"))
    }
  })
  output$eCBPED <- renderUI({
    if (rot_i()==0) {
      if (subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"C2"]=="") {
        shinyjs::disabled(numericInput("eC2PED", "Peds", value=0L, min=0L, step=1, width="100%"))
      } else {
        numericInput("eC2PED", "Peds", value=0L, min=0L, step=1, width="100%")
      }
    } else if (rot_i()==1) { 
      if (subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"C1"]=="") {
        shinyjs::disabled(numericInput("eC1PED", "Peds", value=0L, min=0L, step=1, width="100%"))
      } else {
        numericInput("eC1PED", "Peds", value=0L, min=0L, step=1, width="100%")
      }
    } else if (rot_i()==2) { 
      if (subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"C4"]=="") {
        shinyjs::disabled(numericInput("eC4PED", "Peds", value=0L, min=0L, step=1, width="100%"))
      } else {
        numericInput("eC4PED", "Peds", value=0L, min=0L, step=1, width="100%")
      }
    } else if (rot_i()==3) { 
      if (subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"C3"]=="") {
        shinyjs::disabled(numericInput("eC3PED", "Peds", value=0L, min=0L, step=1, width="100%"))
      } else {
        numericInput("eC3PED", "Peds", value=0L, min=0L, step=1, width="100%")
      }
    }
  })
  output$eCCDIR <- renderUI({
    if (rot_i()==0) {
      shinyjs::disabled(selectInput("eC3DIR", "Corner 3", choices=dirC3, selected=subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"C3"], width="100%"))
    } else if (rot_i()==1) {
      shinyjs::disabled(selectInput("eC2DIR", "Corner 2", choices=dirC2, selected=subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"C2"], width="100%"))
    } else if (rot_i()==2) {
      shinyjs::disabled(selectInput("eC1DIR", "Corner 1", choices=dirC1, selected=subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"C1"], width="100%"))
    } else if (rot_i()==3) {
      shinyjs::disabled(selectInput("eC4DIR", "Corner 4", choices=dirC4, selected=subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"C4"], width="100%"))
    }
  })
  output$eCCPED <- renderUI({
    if (rot_i()==0) {
      if (subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"C3"]=="") {
        shinyjs::disabled(numericInput("eC3PED", "Peds", value=0L, min=0L, step=1, width="100%"))
      } else {
        numericInput("eC3PED", "Peds", value=0L, min=0L, step=1, width="100%")
      }
    } else if (rot_i()==1) { 
      if (subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"C2"]=="") {
        shinyjs::disabled(numericInput("eC2PED", "Peds", value=0L, min=0L, step=1, width="100%"))
      } else {
        numericInput("eC2PED", "Peds", value=0L, min=0L, step=1, width="100%")
      }
    } else if (rot_i()==2) { 
      if (subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"C1"]=="") {
        shinyjs::disabled(numericInput("eC1PED", "Peds", value=0L, min=0L, step=1, width="100%"))
      } else {
        numericInput("eC1PED", "Peds", value=0L, min=0L, step=1, width="100%")
      }
    } else if (rot_i()==3) { 
      if (subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"C4"]=="") {
        shinyjs::disabled(numericInput("eC4PED", "Peds", value=0L, min=0L, step=1, width="100%"))
      } else {
        numericInput("eC4PED", "Peds", value=0L, min=0L, step=1, width="100%")
      }
    }
  })
  output$eCDDIR <- renderUI({
    if (rot_i()==0) {
      shinyjs::disabled(selectInput("eC4DIR", "Corner 4", choices=dirC4, selected=subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"C4"], width="100%"))
    } else if (rot_i()==1) {
      shinyjs::disabled(selectInput("eC3DIR", "Corner 3", choices=dirC3, selected=subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"C3"], width="100%"))
    } else if (rot_i()==2) {
      shinyjs::disabled(selectInput("eC2DIR", "Corner 2", choices=dirC2, selected=subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"C2"], width="100%"))
    } else if (rot_i()==3) {
      shinyjs::disabled(selectInput("eC1DIR", "Corner 1", choices=dirC1, selected=subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"C1"], width="100%"))
    }
  })
  output$eCDPED <- renderUI({
    if (rot_i()==0) {
      if (subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"C4"]=="") {
        shinyjs::disabled(numericInput("eC4PED", "Peds", value=0L, min=0L, step=1, width="100%"))
      } else {
        numericInput("eC4PED", "Peds", value=0L, min=0L, step=1, width="100%")
      }
    } else if (rot_i()==1) { 
      if (subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"C3"]=="") {
        shinyjs::disabled(numericInput("eC3PED", "Peds", value=0L, min=0L, step=1, width="100%"))
      } else {
        numericInput("eC3PED", "Peds", value=0L, min=0L, step=1, width="100%")
      }
    } else if (rot_i()==2) { 
      if (subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"C2"]=="") {
        shinyjs::disabled(numericInput("eC2PED", "Peds", value=0L, min=0L, step=1, width="100%"))
      } else {
        numericInput("eC2PED", "Peds", value=0L, min=0L, step=1, width="100%")
      }
    } else if (rot_i()==3) { 
      if (subset(loadVideosData(), VIDEOID==input$eVIDEOID)[,"C1"]=="") {
        shinyjs::disabled(numericInput("eC1PED", "Peds", value=0L, min=0L, step=1, width="100%"))
      } else {
        numericInput("eC1PED", "Peds", value=0L, min=0L, step=1, width="100%")
      }
    }
  })
  output$e1C1DIR <- renderUI({
    shinyjs::disabled(selectInput("e1C1DIR", "Corner 1", choices=dirC1, selected=subset(loadVideosData(), VIDEOID==input$e1VIDEOID)[,"C1"], width="100%"))
  })
  output$e1C1PED <- renderUI({
    if (subset(loadVideosData(), VIDEOID==input$e1VIDEOID)[,"C1"]=="") {
      shinyjs::disabled(numericInput("e1C1PED", "Peds", value=0L, min=0L, step=1, width="100%"))
    } else {
      numericInput("e1C1PED", "Peds", value=0L, min=0L, step=1, width="100%")
    }
  })
  output$e1C2DIR <- renderUI({
    shinyjs::disabled(selectInput("e1C2DIR", "Corner 2", choices=dirC2, selected=subset(loadVideosData(), VIDEOID==input$e1VIDEOID)[,"C2"], width="100%"))
  })
  output$e1C2PED <- renderUI({
    if (subset(loadVideosData(), VIDEOID==input$e1VIDEOID)[,"C2"]=="") {
      shinyjs::disabled(numericInput("e1C2PED", "Peds", value=0L, min=0L, step=1, width="100%"))
    } else {
      numericInput("e1C2PED", "Peds", value=0L, min=0L, step=1, width="100%")
    }
  })
  output$e1C3DIR <- renderUI({
    shinyjs::disabled(selectInput("e1C3DIR", "Corner 3", choices=dirC3, selected=subset(loadVideosData(), VIDEOID==input$e1VIDEOID)[,"C3"], width="100%"))
  })
  output$e1C3PED <- renderUI({
    if (subset(loadVideosData(), VIDEOID==input$e1VIDEOID)[,"C3"]=="") {
      shinyjs::disabled(numericInput("e1C3PED", "Peds", value=0L, min=0L, step=1, width="100%"))
    } else {
      numericInput("e1C3PED", "Peds", value=0L, min=0L, step=1, width="100%")
    }
  })
  output$e1C4DIR <- renderUI({
    shinyjs::disabled(selectInput("e1C4DIR", "Corner 4", choices=dirC4, selected=subset(loadVideosData(), VIDEOID==input$e1VIDEOID)[,"C4"], width="100%"))
  })
  output$e1C4PED <- renderUI({
    if (subset(loadVideosData(), VIDEOID==input$e1VIDEOID)[,"C4"]=="") {
      shinyjs::disabled(numericInput("e1C4PED", "Peds", value=0L, min=0L, step=1, width="100%"))
    } else {
      numericInput("e1C4PED", "Peds", value=0L, min=0L, step=1, width="100%")
    }
  })
  output$myevent <- renderDT({
    temp <- formEventData()
    temp$TIME <- as.character(temp$TIME)
    temp$SAVED <- as.character(temp$SAVED)
    DT::datatable(temp, options=list(dom="t"), rownames=F, filter="none", selection="none")
  })
  output$myevent1 <- renderDT({
    temp <- formEvent1Data()
    temp$TIME <- as.character(temp$TIME)
    temp$SAVED <- as.character(temp$SAVED)
    DT::datatable(temp, options=list(dom="t"), rownames=F, filter="none", selection="none")
  })
  output$allevents <- renderDT({
    input$delete_video
    input$submit_video
    input$add_event
    input$edit_event
    input$delete_event
    temp <- loadEventsData()
    temp$VIDEOID <- as.factor(temp$VIDEOID)
    temp$TIME <- as.character(temp$TIME)
    temp$X1DIR <- as.factor(temp$X1DIR)
    temp$X2DIR <- as.factor(temp$X2DIR)
    temp$X3DIR <- as.factor(temp$X3DIR)
    temp$X4DIR <- as.factor(temp$X4DIR)
    temp$C1DIR <- as.factor(temp$C1DIR)
    temp$C2DIR <- as.factor(temp$C2DIR)
    temp$C3DIR <- as.factor(temp$C3DIR)
    temp$C4DIR <- as.factor(temp$C4DIR)
    temp$SAVED <- as.character(temp$SAVED)
    DT::datatable(temp[,input$show_vars_event], options = list(lengthMenu=c(5, 10, 25, 50, 100), pageLength=5), rownames=F, filter=list(position="top", clear=F), selection="single")
  })
  
}

########################################
# Run app

shinyApp(ui = ui, server = server)

########################################
# END
########################################