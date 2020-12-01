########################################
# Project:  UDOT-18.602 Traffic signal pedestrians
# Authors:  Patrick Singleton (patrick.singleton@usu.edu)
# File:     app.R
# Date:     2020 Spring, Summer
# About:    Shiny app to visualize ped signal data
########################################

########################################
# Click the "Run App" button above to run the application. 

# Notes
# 2020-02-16 V1
# 2020-03-27 V2
# 2020-08-19 V3

# Resources
# https://shiny.rstudio.com/articles/
# https://rstudio.github.io/DT/
# https://rstudio.github.io/leaflet/shiny.html

########################################
# Shiny

# Load libraries
library(shiny)
library(shinyjs)
library(DT)
library(leaflet)
library(RColorBrewer)

########################################
# Preparations

# Load files and functions
signals <- readRDS(file=paste0("Data", "/", "signals.rds"))
peddata <- readRDS(file=paste0("Data", "/", "peddata.rds"))
# source("funs.r")

# Pre-calculate things
mindate <- as.Date(min(peddata$TIME1))
maxdate <- as.Date(max(peddata$TIME1))
minyear <- as.integer(format(mindate, "%Y"))
maxyear <- as.integer(format(maxdate, "%Y"))
sigs <- sort(unique(peddata$SIGNAL))
signals <- signals[signals$SIGNAL %in% sigs,]

########################################
# User interface

ui <- navbarPage(title="Pedestrian signal data visualization", 
    
    # Map
    tabPanel(title="Map", sidebarLayout(position="left", 
        
        # Sidebar panel
        sidebarPanel(width=3, 
            uiOutput("mdate1"), 
            selectInput("mHOUR1", NULL, choices=c(0L:23L), selected=0L), 
            uiOutput("mdate2"), 
            selectInput("mHOUR2", NULL, choices=c(0L:23L), selected=23L), 
            radioButtons("mPAM", "Pedestrian Activity", choiceNames=c("45", "90", "45B", "90C", "PED"), choiceValues=c("A45", "A90", "A45B", "A90C", "PED"), selected="PED", inline=T), 
            radioButtons("mAVG", "Total or Average", choices=c("Total", "Average"), selected="Total", inline=T), 
            uiOutput("munit"), 
            uiOutput("mslider"), 
            actionButton("mBUTTON", "Update Map")
        ), 
        
        # Main panel
        mainPanel(width=9, 
            leafletOutput("mMAP", width="100%"), 
            hr(), 
            DTOutput("mTABLE"), 
            shinyjs::disabled(uiOutput("mdownload")), 
            hr(), 
            p("Created in 2020 at Utah State University. Contact Patrick Singleton (patrick.singleton@usu.edu) with questions.")
        )
    )), 
                 
    # Figure
    tabPanel(title="Figure", sidebarLayout(position="left", 
        
        # Sidebar panel
        sidebarPanel(width=3, 
            selectInput("fSIGNAL", "Signal ID", choices=sigs), 
            uiOutput("fdate1"), 
            selectInput("fHOUR1", NULL, choices=c(0L:23L), selected=0L), 
            uiOutput("fdate2"), 
            selectInput("fHOUR2", NULL, choices=c(0L:23L), selected=23L), 
            uiOutput("fphase"), 
            radioButtons("fPAM", "Pedestrian Activity", choiceNames=c("45", "90", "45B", "90C", "PED"), choiceValues=c("A45", "A90", "A45B", "A90C", "PED"), selected="PED", inline=T), 
            radioButtons("fUNIT", "Time Unit", choices=c("year", "month", "day", "hour"), selected="hour", inline=T), 
            radioButtons("fAVG", "Total or Average", choices=c("Total", "Average"), selected="Total", inline=T), 
            actionButton("fBUTTON", "Create Figure & Table")
        ),
        
        # Main panel
        mainPanel(width=9, 
            plotOutput("fFIGURE", click = "plot_click"), 
            verbatimTextOutput("fCLICK"), 
            hr(), 
            DTOutput("fTABLE"), 
            uiOutput("fdownload"), 
            hr(), 
            p("Created in 2020 at Utah State University. Contact Patrick Singleton (patrick.singleton@usu.edu) with questions.")
        )
    ))
    
)

########################################
# Server logic

# Define server logic
server <- function(input, output, session) {
    
    # Processes for Map
    # Load and subset all data
    mymapdata <- reactive({
        # Define times
        mytime1 <- as.POSIXct(paste(input$mDATE1, input$mHOUR1), format="%Y-%m-%d %H", tz="America/Denver")
        mytime2 <- as.POSIXct(paste(input$mDATE2, input$mHOUR2), format="%Y-%m-%d %H", tz="America/Denver")
        # Prepare
        mysigs <- signals
        mysigs$COUNT <- NA
        # Load, subset, and merge
        for (i in 1:length(signals$SIGNAL)) {
            # Load data
            mydat <- peddata[peddata$SIGNAL==signals$SIGNAL[i],]
            # Subset data by time
            mydat <- mydat[mydat$TIME1 >= mytime1 & mydat$TIME1 <= mytime2,]
            if (nrow(mydat)==0) {
                mysigs$COUNT[i] <- 0
            } else {
                # Tabulate or average
                mydat$COUNT <- mydat[,input$mPAM]
                if (input$mAVG=="Average") {
                    if(input$mUNIT=="year") { mydat$TUNIT <- strftime(mydat$TIME1, format="%Y") }
                    if(input$mUNIT=="month") { mydat$TUNIT <- strftime(mydat$TIME1, format="%m") }
                    if(input$mUNIT=="day") { mydat$TUNIT <- strftime(mydat$TIME1, format="%u") }
                    if(input$mUNIT=="hour") { mydat$TUNIT <- strftime(mydat$TIME1, format="%H") }
                    tavg <- aggregate(COUNT ~ TUNIT, data=mydat, FUN=mean)
                    mysigs$COUNT[i] <- tavg$COUNT[as.integer(tavg$TUNIT)==input$mSLIDER]
                } else {
                    mysigs$COUNT[i] <- sum(mydat$COUNT)
                }
            }
            rm(mydat)
        }; rm(i)
        mysigs
    })
    
    # Processes for Figure
    # Load data
    mydata <- reactive({
        peddata[peddata$SIGNAL==input$fSIGNAL,]
    })
    # Subset data by time and phase
    mydatasub <- reactive({
        mydat <- mydata()
        mytime1 <- as.POSIXct(paste(input$fDATE1, input$fHOUR1), format="%Y-%m-%d %H", tz="America/Denver")
        mytime2 <- as.POSIXct(paste(input$fDATE2, input$fHOUR2), format="%Y-%m-%d %H", tz="America/Denver")
        mydat[mydat$TIME1 >= mytime1 & mydat$TIME1 <= mytime2 & mydat$P %in% input$fPHASE,]
    })
    # Get time sequence by units
    mydataseq <- reactive({
        mydat <- mydatasub()
        if(input$fUNIT=="year") {
            tmin <- trunc(min(mydat$TIME1, na.rm=T), units="years")
            tmax <- trunc(max(mydat$TIME1, na.rm=T), units="years")
            tempseq <- seq(tmin, tmax, "years")
            myseq <- seq(tmin, by="years", length.out=length(tempseq)+1)
            rm(tempseq, tmin, tmax)
        }
        if(input$fUNIT=="month") {
            tmin <- trunc(min(mydat$TIME1, na.rm=T), units="months")
            tmax <- trunc(max(mydat$TIME1, na.rm=T), units="months")
            tempseq <- seq(tmin, tmax, "month")
            myseq <- seq(tmin, by="month", length.out=length(tempseq)+1)
            rm(tempseq, tmin, tmax)
        }
        if(input$fUNIT=="day") {
            tmin <- trunc(min(mydat$TIME1, na.rm=T), units="days")
            tmax <- trunc(max(mydat$TIME1, na.rm=T), units="days")
            tempseq <- seq(tmin, tmax, "DSTday")
            myseq <- seq(tmin, by="DSTday", length.out=length(tempseq)+1)
            rm(tempseq, tmin, tmax)
        }
        if(input$fUNIT=="hour") {
            tmin <- trunc(min(mydat$TIME1, na.rm=T), units="hours")
            tmax <- trunc(max(mydat$TIME1, na.rm=T), units="hours")
            tempseq <- seq(tmin, tmax, "hour")
            myseq <- seq(tmin, by="hour", length.out=length(tempseq)+1)
            rm(tempseq, tmin, tmax)
        }
        myseq
    })
    # Tabulate by time unit
    mydatasum <- reactive({
        mydat <- mydatasub()
        myseq <- mydataseq()
        mydat$COUNT <- mydat[,input$fPAM]
        if(input$fUNIT=="year") { mydat$TUNIT <- strftime(mydat$TIME1, format="%Y") }
        if(input$fUNIT=="month") { mydat$TUNIT <- strftime(mydat$TIME1, format="%Y-%m") }
        if(input$fUNIT=="day") { mydat$TUNIT <- strftime(mydat$TIME1, format="%Y-%m-%d") }
        if(input$fUNIT=="hour") { mydat$TUNIT <- strftime(mydat$TIME1, format="%Y-%m-%d %H") }
        ttot <- aggregate(COUNT ~ TUNIT, data=mydat, FUN=sum)
        temp <- data.frame(SIGNAL=input$fSIGNAL, TIME1=myseq[1:(length(myseq)-1)], TIME2=myseq[2:length(myseq)], TDIFF=0, COUNT=0L)
        temp$TDIFF <- as.numeric(temp$TIME2) - as.numeric(temp$TIME1)
        if (input$fUNIT=="hour") {
            temp$COUNT <- NULL
            ttot$TIME1 <- as.POSIXct(ttot$TUNIT, format="%Y-%m-%d %H", tz="America/Denver")
            temp <- merge(temp, ttot, by="TIME1", all.x=T)
            temp <- temp[,c("SIGNAL", "TIME1", "TIME2", "TDIFF", "COUNT")]
        } else {
            temp$COUNT <- ttot$COUNT
        }
        temp
    })
    # Average if necessary
    mydataavg <- reactive({
        temp <- mydatasum()
        if(input$fUNIT=="year") { temp$TUNIT <- strftime(temp$TIME1, format="%Y") }
        if(input$fUNIT=="month") { temp$TUNIT <- strftime(temp$TIME1, format="%m") }
        if(input$fUNIT=="day") { temp$TUNIT <- strftime(temp$TIME1, format="%u") }
        if(input$fUNIT=="hour") { temp$TUNIT <- strftime(temp$TIME1, format="%H") }
        tavg <- aggregate(COUNT ~ TUNIT, data=temp, FUN=mean)
        tavg$SIGNAL <- input$fSIGNAL
        tavg <- tavg[,c("SIGNAL", "TUNIT", "COUNT")]
        tavg
    })
    
    # Map
    output$mMAP <- renderLeaflet({
        if (input$mBUTTON==0) {
            leaflet(signals) %>%
                addProviderTiles("CartoDB.Positron") %>%
                addCircleMarkers(lat=~LAT, lng=~LNG, radius=10, stroke=F, fillColor="black", fillOpacity=0.75,
                                 label=~as.character(SIGNAL), labelOptions=labelOptions(textsize="12px"), popup=~as.character(SIGNAL))
        } else { isolate({
            mysigs <- mymapdata()
            maxcount <- max(sqrt(mysigs$COUNT),1, na.rm=T)
            mysigs$myrad <- 250*sqrt(mysigs$COUNT)/maxcount
            mysigs$myrad[mysigs$myrad<10] <- 10
            mysigs$mypopup <- paste0(mysigs$SIGNAL, " -- ", mysigs$ST_EW, " ", mysigs$ST_NS, ", ", mysigs$CITY)
            mypal <- colorNumeric(palette="YlGnBu", domain=NULL)
            if (input$mAVG=="Average" | input$mPAM=="PED") { mysigs$COUNT <- round(mysigs$COUNT, 2) }
            leaflet(mysigs) %>%
                addProviderTiles("CartoDB.Positron") %>%
                addLegend("bottomright", pal=mypal, values=~COUNT, title = "Legend", opacity = 1) %>%
                addCircles(lat=~LAT, lng=~LNG, radius=~myrad, stroke=T, color="black", weight=1, fillColor=~mypal(COUNT), fillOpacity=0.75,
                                 label=~as.character(COUNT), labelOptions=labelOptions(textsize="12px"), popup=~mypopup
                )
        }) }
    })

    # Table
    output$mTABLE <- renderDT({
        if (input$mBUTTON==0) {
            return()
        } else { isolate({
            temp <- mymapdata()
            temp$SIGNAL <- as.character(temp$SIGNAL)
            temp <- temp[,c("SIGNAL", "ST_EW", "ST_NS", "CITY", "COUNT")]
            if (input$mAVG=="Average" | input$mPAM=="PED") { temp$COUNT <- round(temp$COUNT, 2) }
            DT::datatable(temp, options=list(lengthMenu=c(5, 10, 25, 50, 100), pageLength=5), rownames=F, filter=list(position="top", clear=F), selection="none")
        }) }
    })
    
    # Figure
    output$fFIGURE <- renderPlot({
        if (input$fBUTTON==0) {
            return()
        } else { isolate({
            temp <- mydatasum()
            if (input$fPAM=="PED") {
                myylab <- paste0("Pedestrian crossing volume (estimated)")
            } else {
                myylab <- paste0("Pedestrian activity (", input$fPAM, ")")
            }
            if (input$fAVG=="Average") {
                temp2 <- mydataavg()
                mymain <- paste0("Signal ", input$fSIGNAL, ", Phases ", paste0(input$fPHASE, collapse=" "), "\n", strftime(min(temp$TIME1)), " to ", strftime(max(temp$TIME1)), " (averaged by ", input$fUNIT, ")")
                barplot(temp2$COUNT, width=1, space=0, las=1, names.arg=temp2$TUNIT, main=mymain, ylab=myylab)                
            } else {
                mymain <- paste0("Signal ", input$fSIGNAL, ", Phases ", paste0(input$fPHASE, collapse=" "), "\n", strftime(min(temp$TIME1)), " to ", strftime(max(temp$TIME1)), " (by ", input$fUNIT, ")")
                barplot(temp$COUNT, width=1, space=0, las=1, names.arg=temp$TIME1, main=mymain, ylab=myylab)
            }
        }) }
    })
    
    # Text for figure click
    output$fCLICK <- renderText({
        if (input$fBUTTON==0) {
            return()
        } else { 
            if(is.null(input$plot_click$x)) {
                return("") }
            else { isolate({
                # input$plot_click
                if (input$fAVG=="Average" | input$fPAM=="PED") {
                    temp2 <- mydataavg()
                    paste0(round(temp2$COUNT[ceiling(input$plot_click$x)],2), " for ", input$fUNIT, " ", temp2$TUNIT[ceiling(input$plot_click$x)])
                } else {
                    temp <- mydatasum()
                    paste0(temp$COUNT[ceiling(input$plot_click$x)], " for ", strftime(temp$TIME1[ceiling(input$plot_click$x)]))
                }
            }) }
        }
    })
    
    # Table
    output$fTABLE <- renderDT({
        if (input$fBUTTON==0) {
            return()
        } else { isolate({
            if (input$fAVG=="Average") {
                temp2 <- mydataavg()
                temp2$COUNT <- round(temp2$COUNT, 2)
                DT::datatable(temp2, options=list(lengthMenu=c(5, 10, 25, 50, 100), pageLength=5), rownames=F, filter=list(position="top", clear=F), selection="none")
            } else {
                temp <- mydatasum()
                temp$TIME1 <- as.character(temp$TIME1)
                temp$TIME2 <- as.character(temp$TIME2)
                temp$TDIFF <- NULL
                DT::datatable(temp, options=list(lengthMenu=c(5, 10, 25, 50, 100), pageLength=5), rownames=F, filter=list(position="top", clear=F), selection="none")
            }
        }) }
    })
    
    # Download
    output$fDOWNLOAD <- downloadHandler(
        filename = function() {
            paste(input$fSIGNAL, ".csv", sep = "")
        },
        content = function(file) {
            if (input$fAVG=="Average") {
                write.csv(mydataavg(), file, row.names = FALSE)
            } else {
                write.csv(mydatasum(), file, row.names = FALSE)
            }
        }
    )
    
    # UI elements
    output$mdate1 <- renderUI({
        datemin <- mindate
        datemax <- maxdate
        dateInput("mDATE1", "Start Date & Hour", value=datemin, min=datemin, max=datemax)
    })
    output$mdate2 <- renderUI({
        input$mDATE1
        datemin <- input$mDATE1
        datemax <- maxdate
        dateInput("mDATE2", "End Date & Hour", value=datemin, min=datemin, max=datemax)
    })
    output$munit <- renderUI({
        if(input$mAVG=="Average") {
            radioButtons("mUNIT", "Time Unit", choices=c("year", "month", "day", "hour"), selected="hour", inline=T)
        } else { return() }
    })
    output$mslider <- renderUI({
        if(input$mAVG=="Average") {
            input$mUNIT
            if(is.null(input$mUNIT)) { tmin<-0L; tmax<-1L } else 
            if(input$mUNIT=="year") { tmin<-minyear; tmax<-maxyear } else
            if(input$mUNIT=="month") { tmin<-1L; tmax<-12L } else
            if(input$mUNIT=="day") { tmin<-1L; tmax<-7L } else 
            if(input$mUNIT=="hour") { tmin<-0L; tmax<-23L }
            sliderInput("mSLIDER", NULL, min=tmin, max=tmax, value=tmin, step=1L)
        } else { return() }
    })
    output$fdate1 <- renderUI({
        input$fSIGNAL
        temp <- mydata()
        datemin <- as.Date(min(temp$TIME1))
        datemax <- as.Date(max(temp$TIME1))
        dateInput("fDATE1", "Start Date & Hour", value=datemin, min=datemin, max=datemax)
    })
    output$fdate2 <- renderUI({
        input$fSIGNAL
        input$fDATE1
        temp <- mydata()
        datemin <- input$fDATE1
        datemax <- as.Date(max(temp$TIME1))
        dateInput("fDATE2", "End Date & Hour", value=datemin, min=datemin, max=datemax)
    })
    output$fphase <- renderUI({
        input$fSIGNAL
        temp <- mydata()
        pedphases <- unique(temp$P)
        checkboxGroupInput("fPHASE", "Ped Phases", choices=pedphases, select=pedphases, inline=T)
    })
    output$fdownload <- renderUI({
        if (input$fBUTTON==0) {
            return()
        } else {
            input$fBUTTON
            downloadButton("fDOWNLOAD", "Download Table")
        }
    })
}

########################################
# Run app

shinyApp(ui = ui, server = server)

########################################
# END
########################################