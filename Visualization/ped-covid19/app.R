########################################
# Project:  UDOT-18.602 Traffic signal pedestrians
# Authors:  Patrick Singleton (patrick.singleton@usu.edu)
# File:     app.R
# Date:     2020 Spring
# About:    Shiny app to view ped signal data, related to COVID-19 pandemic
########################################

########################################
# Click the "Run App" button above to run the application. 

# Notes
# based on app.R in Visualizing
# 2020-03-27 V1
# 2020-03-30 V2
# 2020-04-01 V3
# 2020-04-15 V4
# 2020-05-01 V5
# 2020-07-14 V6

# Resources
# https://shiny.rstudio.com/articles/
# https://rstudio.github.io/DT/
# https://rstudio.github.io/leaflet/shiny.html

########################################
# Shiny

# Load libraries
library(shiny)
library(shinyjs)
library(shinyTime)
library(DT)
library(leaflet)
library(RColorBrewer)
library(sf)

########################################
# Preparations

# Load files
dat19 <- readRDS(file=paste0("Data", "/", "dat19.rds"))
dat20 <- readRDS(file=paste0("Data", "/", "dat20.rds"))
signals <- readRDS(file=paste0("Data", "/", "signals.rds"))
counties <- readRDS(file=paste0("Data", "/", "countiesGIS.rds"))
source("sigareas.R")
source("funs.R")

# Process data
dat19[,c("TIME2", "TDIFF")] <- NULL
names(dat19)[1] <- c("DATE")
dat19$DATE <- as.Date(dat19$DATE, tz="America/Denver")
dat20[,c("TIME2", "TDIFF")] <- NULL
names(dat20)[1] <- c("DATE")
dat20$DATE <- as.Date(dat20$DATE, tz="America/Denver")

# Signals
mysigs <- unique(as.integer(gsub("sig", "", names(dat19[2:ncol(dat19)]))), 
                 as.integer(gsub("sig", "", names(dat20[2:ncol(dat20)]))))
mysignals <- signals[signals$SIGNAL %in% mysigs, ]

# Start/end dates
datebeg <- min(dat20$DATE)
dateend <- max(dat20$DATE)
dateone <- as.Date("2020-03-01", tz="America/Denver")

# Counties & Areas
# counties
tempcounties <- counties[,c("NAME")]
temp <- st_as_sf(x=mysignals, coords=c("LNG", "LAT"), dim="XY", crs=4326)
tempsig <- st_join(temp, tempcounties)
tempsig <- tempsig[,c("SIGNAL", "NAME")]
tempsig$geometry <- NULL
names(tempsig)[2] <- "COUNTY"
# areas
tempsig$AREA <- NA
for (i in 1:length(sigareas)) {
    tempsig$AREA <- ifelse(tempsig$SIGNAL %in% sigareas[[i]], names(sigareas)[i], tempsig$AREA)
}; rm(i)
# merge
mysignals <- merge(mysignals, tempsig, by="SIGNAL", all=T)

########################################
# User interface

ui <- navbarPage(title="Utah Pedestrian Activity", 
    
    # Home
    tabPanel(title="Information", 
        fluidPage(fluidRow(column(width=12, 
            
            h1("Monitoring pedestrian activity in Utah in the time of COVID-19"), 
            p(em("Updated 2020-08-04")), 
            p("This website provides data and visualizations of pedestrian activity (and changes in pedestrian activity) at various (signalized) intersections throughout Utah. ", 
              # strong("Currently, around 150 locations are available"), " (mostly clustered near Salt Lake City, Ogden, Provo, Logan, St. George, and Moab), but more are being added weekly upon request.", 
              "We are currently showing ", strong("all locations with available signal data"), " throughout Utah. ", 
              "Data are derived from pedestrian push-button presses at traffic signals, taken from the Utah Department of Transportation's ", a("Automated Traffic Signal Performance Measures System", href="https://udottraffic.utah.gov/atspm/"), "website.",  
              "We hope that this information is useful for public agencies to track changes in walking activity at different (types of) locations, in order to inform crowd management efforts in public spaces and decisions about travel restrictions and stay-at-home orders and directives. "), 
            h3(a(id="toggle_results", "Recent results")), 
            (div(id="results", wellPanel(
                fluidRow(
                    column(width=3, h4("During the week of")),
                    column(width=3, uiOutput("idate1")), 
                    column(width=3, uiOutput("idate2"))
                ),
                h4("pedestrian activity in Utah was..."),
                fluidRow(
                    column(width=1), 
                    column(width=2, uiOutput("iweekagg1")), 
                    column(width=3, uiOutput("iweekaggsig1")), 
                    column(width=3, uiOutput("icounty1"))
                ), 
                fluidRow(
                    column(width=1), 
                    column(width=2, uiOutput("iweekagg2")), 
                    column(width=3, uiOutput("iweekaggsig2")), 
                    column(width=3, uiOutput("icounty2")), 
                    column(width=2, h4("County"))
                ), 
                fluidRow(
                    column(width=1), 
                    column(width=2, uiOutput("iareaagg1")), 
                    column(width=3, uiOutput("iareaaggsig1")), 
                    column(width=3, uiOutput("iarea1")), 
                    column(width=2, h4("areas"))
                    
                ), 
                fluidRow(
                    column(width=1), 
                    column(width=2, uiOutput("iareaagg2")), 
                    column(width=3, uiOutput("iareaaggsig2")), 
                    column(width=3, uiOutput("iarea2")), 
                    column(width=2, h4("areas"))
                ),
                fluidRow(
                    column(width=3, h4("versus last year:")), 
                    column(width=3, uiOutput("idate3")), 
                    column(width=3, uiOutput("idate4"))
                )
            ))), 
            h3(a(id="toggle_howto", "How to use")), 
            shinyjs::hidden(div(id="howto", 
                p("There are two visualizations available in the tabs above. ", 
                  "In both visualizations, you can observe ", strong("daily pedestrian activity"), " at one or more signals (on a map or in a figure). ", 
                  "You can also download a CSV file containing the data that is creating the map or figure. "), 
                p("Importantly, you can also observe ", strong("changes or differences in daily pedestrian activity"), " compared to the same day", em("last year"), ", the same day", em("1+ weeks ago"), ", or ", em("specific date(s)"), " in 2020. ", 
                  "This allows you to see roughly how many fewer or more people are walking in different areas, given COVID-19 related social/physical distancing, travel restrictions, stay-at-home directives, and notices about participating in outdoor physical activities.", 
                  "We recommend looking at data starting in late February or early March; although, data are available back to 2020-01-01."), 
                p("The first visualization -- ", strong("Map of many signals"), " -- shows one day of data for all signals that we have made available. You can zoom in to different areas of the state. You can specify a particular date (in the box or the slider), and/or press play to see things change over time. "), 
                p("The second visualization -- ", strong("Figure of one signal"), " -- shows potentially multiple days of data for one signal. You can search for a particular location or select it from the list. You can specify a particular date range, and see the time series results presented in a figure and a table. "), 
                p("In both visualizations, we recommend clicking the \"compare\" button with \"last year\" selected to see how pedestrian activity has changed year-over-year.")
            )), 
            h3(a(id="toggle_note", "Notes")), 
            shinyjs::hidden(div(id="note", 
                p("Data come from UDOT's ", a("Automated Traffic Signal Performance Measures System (ATSPM)", href="https://udottraffic.utah.gov/atspm/"), ". They have been manually downloaded and automatically processed and aggregated by date. The data have not been cleaned, and errors or missing data may be present. "), 
                p("As shown on this website in maps, figures, and tables, ", strong("pedestrian activity is NOT pedestrian volume"), ". ", 
                  "Instead, \"pedestrian activity\" is measuring the total number of \"unique\" pedestrian push-button presses that were measured at that intersection. ", 
                  "(Unique means that we have eliminated push-button presses that occur within 10 seconds of another push-button press.) ", 
                  "While not a direct measure of pedestrian volume, pedestrian signal activity is a fairly accurate proxy for pedestrian crossing volumes at signalized intersections. ", 
                  "This has been demonstrated in a couple of small studies in Oregon (", a("Blanc et al., 2015", href="https://doi.org/10.3141/2527-08"), ", ", a("Kothuri et al., 2017", href="https://doi.org/10.3141/2644-02"), "), and also at a large scale in Utah through ongoing work by Patrick Singleton, Ferdousy Runa, and other members of the ", a("Singleton Transportation Lab", href="https://engineering.usu.edu/cee/research/labs/patrick-singleton/index"), " at Utah State University (", a("see recent presentation on LinkedIn", href="https://www.linkedin.com/feed/update/urn:li:activity:6653510747034918912/"), ").", 
                  "We are currently studying the extent to which an aversion to high-touch surfaces due to COVID-19 is affecting pedestrian push-button usage. "), 
                p(em("Caveats"), " -- ", 
                  "Data have not been fully cleaned and may contain errors or missing data. ", 
                  "Data may not exclusively reflect pedestrian activity; other sidewalk/crosswalk users may use pedestrian push-buttons, including people using bicycles or scooters. ", 
                  "Some signals and crossings may operate on pedestrian recall at certain times of day, where pedestrians do not have to press the push-button to get the walk indication. ", 
                  "Data may be biased towards pedestrian activity patterns at signalized intersections, and walking activity may be different (higher or lower) at other locations. ", 
                  "Currently, people may be more reluctant to press the pedestrian push-button, so data may be underestimating current walking activity. "), 
                p(em("Events"), " -- ", 
                  "A 5.7 magnitude earthquake struck near Salt Lake City on 2020-03-18. ", 
                  "Spring break for Utah State University (Logan) was 2020-03-02 through 2020-03-06. Spring break for University of Utah (Salt Lake City) was 2020-03-09 through 2020-03-13. ", 
                  "Most university classes were cancelled starting 2020-03-13, with online classes starting 2020-03-18. Most public primary/secondary schools have been closed since 2020-03-16. ", 
                  "Governor Gary Herbert issued a \"Stay Safe, Stay Home\" directive on 2020-03-27. ", 
                  "Utah transitioned from a high (red) risk phase to a moderate (orange) risk phase on 2020-05-01. ", 
                  "Most of Utah transitioned from a moderate (orange) risk phase to a low (yellow) risk phase on 2020-05-16. "), 
                p(em("Updates"), " -- ", 
                  strong("2020-08-04"), "Updated data through 2020-08-01. ", 
                  strong("2020-07-20"), "Updated data through 2020-07-18. ", 
                  strong("2020-07-14"), " Added data for all signals with data (nearly 2,000). Updated data through 2020-07-11. ", 
                  strong("2020-06-18"), " Updated data through 2020-06-13. ", 
                  strong("2020-06-02"), " Updated data through 2020-05-30. ", 
                  strong("2020-05-26"), " Updated data through 2020-05-23. ", 
                  strong("2020-05-18"), " Updated data through 2020-05-16. ", 
                  strong("2020-05-11"), " Updated data through 2020-05-09. ", 
                  strong("2020-05-05"), " Refined result results. Updated data through 2020-05-02. ", 
                  strong("2020-05-01"), " Added recent results by county and area type. Added zoom-to-city functionality to map. Added data for 48 signals (total 149). Updated data through 2020-04-26. ", 
                  strong("2020-04-15"), " Added information page. Added data for 46 signals (total 101). Updated data through 2020-04-12. ", 
                  strong("2020-03-31"), " Added data for 29 signals (total 55). Updated data through 2020-03-29. ", 
                  strong("2020-03-27"), " Launched with data from 26 signals through 2020-03-22. ")
            )), 
            h3(a(id="toggle_acknow", "Acknowledgements")), 
            shinyjs::hidden(div(id="acknow",
                p("Thanks to Angelo Papastamos, Heidi Goedhart, and Stephanie Tomlin of the Utah Department of Transportation for their feedback and support. Special thanks to Paul Jencks for helping to obtain UDOT ATSPM data. Thanks also to Robert Schneider, Chris Monsere, Kelly Clifton, and Steve Gehrke for their suggestions. "), 
                p("This work is supported in part by the Utah Department of Transportation (Research Project 18.602). The authors alone are responsible for the preparation and accuracy of the information, data, analysis, discussions, recommendations, and conclusions presented herein. The contents do not necessarily reflect the views, opinions, endorsements, or policies of the Utah Department of Transportation or the US Department of Transportation. The Utah Department of Transportation makes no representation or warranty of any kind, and assumes no liability therefore.")
            )), 
            hr(), 
            p(strong("Created in 2020 by the ", a("Singleton Transportation Lab", href="https://engineering.usu.edu/cee/research/labs/patrick-singleton/index"), " at Utah State University. Contact Patrick Singleton (patrick.singleton@usu.edu) with questions."))
            
        )))
    ), 
    
    # Map
    tabPanel(title="Map of many signals", 
        
        wellPanel(fluidRow(
            column(width=3, dateInput("mDATE", "Date", value=dateone, min=datebeg, max=dateend), uiOutput("mweekday")), 
            column(width=9, sliderInput("mDATE2", "Date", min=datebeg, max=dateend, value=dateone, width="100%", animate=animationOptions(interval=2000)))
        )), 
        
        sidebarLayout(position="left", 
        
        # Sidebar panel
        sidebarPanel(width=3, 
            selectInput("mCITY", "Zoom to...", choices=c("Entire state", "Salt Lake City", "Ogden", "Provo", "Logan", "St. George", "Moab"), selected="Entire state"), 
            hr(), 
            checkboxInput("mCOMP", "Compare", value=T), 
            uiOutput("mcompare"), 
            uiOutput("mclyear"), 
            uiOutput("mcweek"), 
            uiOutput("mcdate1"), 
            actionButton("mBUTTON", "Show Table")
        ), 
        
        # Main panel
        mainPanel(width=9, 
            leafletOutput("mMAP", width="100%"), 
            hr(), 
            DTOutput("mTABLE"), 
            uiOutput("mdownload"), 
            hr(), 
            p("Created in 2020 by the ", a("Singleton Transportation Lab", href="https://engineering.usu.edu/cee/research/labs/patrick-singleton/index"), " at Utah State University, using data from UDOT's ", a("ATSPM", href="https://udottraffic.utah.gov/atspm/"), ". Contact Patrick Singleton (patrick.singleton@usu.edu) with questions.")
        )
    )), 
                 
    # Figure
    tabPanel(title="Figure of one signal", 
        
        wellPanel(fluidRow(
                column(width=3, selectInput("fSIGNAL", "Signal ID", choices=mysigs)), 
                column(width=9, selectInput("fSIGINFO", "Signal ID & Location", choices=paste0(mysignals$SIGNAL, " -- ", mysignals$ST_EW, " ", mysignals$ST_NS, ", ", mysignals$CITY)))
        )), 
             
        sidebarLayout(position="left", 
        
        # Sidebar panel
        sidebarPanel(width=3, 
            uiOutput("fdate1"), 
            uiOutput("fdate2"), 
            hr(), 
            checkboxInput("fCOMP", "Compare", value=T), 
            uiOutput("fcompare"), 
            uiOutput("fclyear1"), 
            uiOutput("fclyear2"), 
            uiOutput("fcweek"), 
            uiOutput("fcdate1"), 
            uiOutput("fcdate2"), 
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
            p("Created in 2020 by the ", a("Singleton Transportation Lab", href="https://engineering.usu.edu/cee/research/labs/patrick-singleton/index"), " at Utah State University, using data from UDOT's ", a("ATSPM", href="https://udottraffic.utah.gov/atspm/"), ". Contact Patrick Singleton (patrick.singleton@usu.edu) with questions.")
        )
    )), 
    
    useShinyjs()
    
)

########################################
# Server logic

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    ### Intro stuff
    shinyjs::onclick("toggle_howto", shinyjs::toggle(id = "howto", anim = TRUE))
    shinyjs::onclick("toggle_results", shinyjs::toggle(id = "results", anim = TRUE))
    shinyjs::onclick("toggle_acknow", shinyjs::toggle(id = "acknow", anim = TRUE))
    shinyjs::onclick("toggle_note", shinyjs::toggle(id = "note", anim = TRUE))
    observe({
        tdate1 <- input$iDATE1
        updateDateInput(session, "iDATE2", value=(tdate1+6))
        updateDateInput(session, "iDATE3", value=(tdate1-52*7))
        updateDateInput(session, "iDATE4", value=(tdate1+6-52*7))
        
    })
    # Load intro data
    myweeksdata <- reactive({
        datesweek20 <- (input$iDATE1):(input$iDATE2)
        datesweek19 <- datesweek20 - 52*7
        weeksum20 <- as.data.frame(colSums(dat20[dat20$DATE %in% datesweek20,3:ncol(dat20)]))
        names(weeksum20) <- "SUM20"
        weeksum20$SIGNAL <- as.integer(gsub("sig", "", row.names(weeksum20)))
        weeksum19 <- as.data.frame(colSums(dat19[dat19$DATE %in% datesweek19,3:ncol(dat19)]))
        names(weeksum19) <- "SUM19"
        weeksum19$SIGNAL <- as.integer(gsub("sig", "", row.names(weeksum19)))
        weeksum <- merge(weeksum19, weeksum20, by="SIGNAL")
        weeksum <- merge(weeksum, mysignals[,c("SIGNAL", "COUNTY", "AREA")], by="SIGNAL")
        weeksum
    })
    mycountydata <- reactive({
        weeksum <- myweeksdata()
        weeksum$COUNT <- 1
        weekagg <- aggregate(cbind(SUM19, SUM20, COUNT) ~ COUNTY, data=weeksum, FUN=sum)
        weekagg <- weekagg[order(weekagg$COUNT, decreasing=T),]
        weekagg <- rbind(list("ENTIRE STATE", sum(weekagg$SUM19), sum(weekagg$SUM20), sum(weekagg$COUNT)), weekagg)
        weekagg$PROP <- weekagg$SUM20 / weekagg$SUM19
        weekagg$PERC <- 100 * (weekagg$PROP - 1)
        weekagg
    })
    myareadata <- reactive({
        weeksum <- myweeksdata()
        weeksum$COUNT <- 1
        areaagg <- aggregate(cbind(SUM19, SUM20, COUNT) ~ AREA, data=weeksum, FUN=sum)
        areaagg <- areaagg[order(areaagg$COUNT, decreasing=T),]
        areaagg$PROP <- areaagg$SUM20 / areaagg$SUM19
        areaagg$PERC <- 100 * (areaagg$PROP - 1)
        areaagg
    })
    # UI elements
    output$idate1 <- renderUI({
        dateInput("iDATE1", NULL, value=(dateend-6), min=datebeg, max=(dateend-6))
    })
    output$idate2 <- renderUI({
        shinyjs::disabled(dateInput("iDATE2", NULL, value=dateend, min=(datebeg+6), max=dateend))
    })
    output$idate3 <- renderUI({
        shinyjs::disabled(dateInput("iDATE3", NULL, value=(dateend-6-52*7), min=(datebeg+6-52*7), max=(dateend-52*7)))
    })
    output$idate4 <- renderUI({
        shinyjs::disabled(dateInput("iDATE4", NULL, value=(dateend-52*7), min=(datebeg+6-52*7), max=(dateend-52*7)))
    })
    output$icounty1 <- renderUI({
        weekagg <- mycountydata()
        selectInput("iCOUNTY1", label=NULL, choices=weekagg$COUNTY, selected=weekagg$COUNTY[1])
    })
    output$icounty2 <- renderUI({
        weekagg <- mycountydata()
        selectInput("iCOUNTY2", label=NULL, choices=weekagg$COUNTY, selected=weekagg$COUNTY[2])
    })
    output$iarea1 <- renderUI({
        areaagg <- myareadata()
        selectInput("iAREA1", label=NULL, choices=areaagg$AREA, selected=areaagg$AREA[1])
    })
    output$iarea2 <- renderUI({
        areaagg <- myareadata()
        selectInput("iAREA2", label=NULL, choices=areaagg$AREA, selected=areaagg$AREA[nrow(areaagg)])
    })
    output$iweekagg1 <- renderUI({
        input$iCOUNTY1
        weekagg <- mycountydata()
        h4(strong(ifelse(weekagg$PERC[weekagg$COUNTY==input$iCOUNTY1] < 0, "down", "up"), paste0(round(abs(weekagg$PERC[weekagg$COUNTY==input$iCOUNTY1])), "%")))
    })
    output$iweekaggsig1 <- renderUI({
        input$iCOUNTY1
        weekagg <- mycountydata()
        h4("at", weekagg$COUNT[][weekagg$COUNTY==input$iCOUNTY1], "signals in")
    })
    output$iweekagg2 <- renderUI({
        input$iCOUNTY2
        weekagg <- mycountydata()
        h4(strong(ifelse(weekagg$PERC[weekagg$COUNTY==input$iCOUNTY2] < 0, "down", "up"), paste0(round(abs(weekagg$PERC[weekagg$COUNTY==input$iCOUNTY2])), "%")))
    })
    output$iweekaggsig2 <- renderUI({
        input$iCOUNTY2
        weekagg <- mycountydata()
        h4("at", weekagg$COUNT[][weekagg$COUNTY==input$iCOUNTY2], "signals in")
    })
    output$iareaagg1 <- renderUI({
        input$iAREA1
        areaagg <- myareadata()
        h4(strong(ifelse(areaagg$PERC[areaagg$AREA==input$iAREA1] < 0, "down", "up"), paste0(round(abs(areaagg$PERC[areaagg$AREA==input$iAREA1])), "%")))
    })
    output$iareaaggsig1 <- renderUI({
        input$iAREA1
        areaagg <- myareadata()
        h4("at", areaagg$COUNT[][areaagg$AREA==input$iAREA1], "signals in")
    })
    output$iareaagg2 <- renderUI({
        input$iAREA2
        areaagg <- myareadata()
        h4(strong(ifelse(areaagg$PERC[areaagg$AREA==input$iAREA2] < 0, "down", "up"), paste0(round(abs(areaagg$PERC[areaagg$AREA==input$iAREA2])), "%")))
    })
    output$iareaaggsig2 <- renderUI({
        input$iAREA2
        areaagg <- myareadata()
        h4("at", areaagg$COUNT[][areaagg$AREA==input$iAREA2], "signals in")
    })
    
    ### Map stuff
    observe({
        tdate1 <- input$mDATE
        updateSliderInput(session, "mDATE2", value=tdate1)
    })
    observe({
        tdate2 <- input$mDATE2
        updateDateInput(session, "mDATE", value=tdate2)
    })
    # Load map data
    mymapdata <- reactive({
        # Prepare
        mysigdata <- mysignals
        mysigdata$COUNT <- NA
        # Load, subset, and merge
        for (i in 1:length(mysigdata$SIGNAL)) {
            mysigdata$COUNT[i] <- dat20[dat20$DATE==input$mDATE, paste0("sig", mysigdata$SIGNAL[i])]
        }; rm(i)
        mysigdata
    })
    # Load comparison data
    mymapcdata <- reactive({
        req(input$mCOMPARE)
        mysigcdata <- mymapdata()
        mysigcdata$COMPARE <- NA
        mysigcdata$PCTCHANGE <- NA
        if (input$mCOMPARE=="Last year") {
            newdate <- input$mDATE - 7*52
            for (i in 1:length(mysigcdata$SIGNAL)) {
                mysigcdata$COMPARE[i] <- dat19[dat19$DATE==newdate, paste0("sig", mysigcdata$SIGNAL[i])]
            }; rm(i)
        } else if (input$mCOMPARE=="1+ weeks ago") {
            req(input$mcWEEK)
            newdate <- input$mDATE - 7*as.integer(input$mcWEEK)
            for (i in 1:length(mysigcdata$SIGNAL)) {
                mysigcdata$COMPARE[i] <- dat20[dat20$DATE==newdate, paste0("sig", mysigcdata$SIGNAL[i])]
            }; rm(i)
        } else if (input$mCOMPARE=="Specific date") {
            req(input$mcDATE)
            newdate <- input$mcDATE
            for (i in 1:length(mysigcdata$SIGNAL)) {
                mysigcdata$COMPARE[i] <- dat20[dat20$DATE==newdate, paste0("sig", mysigcdata$SIGNAL[i])]
            }; rm(i)
        }
        mysigcdata$PCTCHANGE <- 100*(mysigcdata$COUNT - mysigcdata$COMPARE)/mysigcdata$COMPARE
        mysigcdata
    })
    mywhichmapdata <- reactive({
        # req(input$mCOMP)
        if (input$mCOMP==T) { 
            temp <- mymapcdata() 
        } else { 
            temp <- mymapdata() 
        }
        temp
    })
    # Map
    output$mMAP <- renderLeaflet({
        leaflet(mysignals) %>%
            addProviderTiles("CartoDB.Positron") %>%
            fitBounds(lng1=min(mysignals$LNG), lng2=max(mysignals$LNG), lat1=min(mysignals$LAT), lat2=max(mysignals$LAT))
    })
    colorpal <- reactive({
        if (input$mCOMP==T) {
            mypal <- colorBin(palette="PRGn", domain=c(-100,Inf), bins=c(-100,-75,-50,-25,-10,10,25,50,100,Inf))
        } else {
            mypal <- colorNumeric(palette="YlGnBu", domain=NULL)
        }
        mypal
    })
    myplotdata <- reactive({
        temp <- mywhichmapdata()
        mycolorcol <- if (input$mCOMP==T) { "PCTCHANGE" } else { "COUNT" }
        temp$COLORVAL <- temp[,mycolorcol]
        maxcount <- max(sqrt(temp$COUNT),1, na.rm=T)
        temp$myrad <- 250*sqrt(temp$COUNT)/maxcount
        temp$myrad[temp$myrad<10] <- 10
        temp$mypopup <- paste0(temp$SIGNAL, " -- ", temp$ST_EW, " ", temp$ST_NS, ", ", temp$CITY)
        temp
    })
    observe({
        temp <- myplotdata()
        mypal <- colorpal()
        mylegend <- if (input$mCOMP==T) { "% change" } else { "Ped activity" }
        leafletProxy("mMAP", data=temp) %>%
            clearControls() %>%
            clearShapes() %>%
            addLegend("bottomright", pal=mypal, values=~COLORVAL, title=mylegend, opacity=1) %>%
            addCircles(lat=~LAT, lng=~LNG, radius=~myrad, stroke=T, color="black", weight=1, fillColor=~mypal(COLORVAL), fillOpacity=0.75,
                       label=~as.character(COUNT), labelOptions=labelOptions(textsize="12px"), popup=~mypopup
            )
    })
    mymapcity <- reactive({
        if(input$mCITY=="Entire state") {
            temp <- c(min(mysignals$LNG), max(mysignals$LNG), min(mysignals$LAT), max(mysignals$LAT))
        } else if (input$mCITY=="Salt Lake City") {
            temp <- c(-111.87, 40.75, 13)
        } else if (input$mCITY=="Ogden") {
            temp <- c(-111.97, 41.21, 13)
        } else if (input$mCITY=="Provo") {
            temp <- c(-111.66, 40.25, 13)
        } else if (input$mCITY=="Logan") {
            temp <- c(-111.82, 41.735, 13)
        } else if (input$mCITY=="St. George") {
            temp <- c(-113.58, 37.10, 13)
        } else if (input$mCITY=="Moab") {
            temp <- c(-109.55, 38.57, 13)
        }
        temp
    })
    observe({
        temp <- mymapcity()
        if (length(temp)==4) {
            leafletProxy("mMAP", data=temp) %>%
                flyToBounds(lng1=temp[1], lng2=temp[2], lat1=temp[3], lat2=temp[4])
        } else if (length(temp)==3) {
            leafletProxy("mMAP", data=temp) %>%
                flyTo(lng=temp[1], lat=temp[2], zoom=temp[3])
        }

    })
    # Table
    output$mTABLE <- renderDT({
        if (input$mBUTTON==0) {
            return()
        } else {
            # input$mBUTTON
            temp <- mywhichmapdata()
            temp$SIGNAL <- as.character(temp$SIGNAL)
            if (input$mCOMP==T) {
                temp <- temp[,c("SIGNAL", "ST_EW", "ST_NS", "CITY", "COUNT", "COMPARE", "PCTCHANGE")]
                temp$PCTCHANGE <- round(temp$PCTCHANGE, digits=0)
            } else {
                temp <- temp[,c("SIGNAL", "ST_EW", "ST_NS", "CITY", "COUNT")]
            }
            DT::datatable(temp, options=list(lengthMenu=c(5, 10, 25, 50, 100), pageLength=5), rownames=F, filter=list(position="top", clear=F), selection="none")
        }
    })
    # Download
    output$mDOWNLOAD <- downloadHandler(
        filename = function() {
            paste(input$mDATE, ".csv", sep = "")
        },
        content = function(file) {
            write.csv(mywhichmapdata(), file, row.names = FALSE)
        }
    )
    # UI elements
    output$mweekday <- renderUI({
        input$mDATE
        shinyjs::disabled(textInput("mWEEKDAY", NULL, value=strftime(input$mDATE, "%A")))
    })
    output$mcompare <- renderUI({
        if (input$mCOMP==T) {
            radioButtons("mCOMPARE", "Compare to", choices=c("Last year", "1+ weeks ago", "Specific date"), selected="Last year", inline=F)
        } else { return() }
    })
    output$mclyear <- renderUI({
        if (input$mCOMP==T) {
            if (input$mCOMPARE=="Last year") {
                input$mDATE
                shinyjs::disabled(textInput("mcLYEAR", "Last year's date", value=strftime((input$mDATE - 7*52), "%Y-%m-%d %A")))
            } else { return() }
        } else { return() }
    })
    output$mcweek <- renderUI({
        if (input$mCOMP==T) {
            if (input$mCOMPARE=="1+ weeks ago") {
                selectInput("mcWEEK", "How many weeks ago?", choices=c("Last week"=1L, "2 weeks ago"=2L, "3 weeks ago"=3L, "4 weeks ago"=4L, 
                                                                       "5 weeks ago"=5L, "6 weeks ago"=6L, "7 weeks ago"=7L, "8 weeks ago"=8L), selected=1L)
            } else { return() }
        } else { return() }
    })
    output$mcdate1 <- renderUI({
        if (input$mCOMP==T) {
            if (input$mCOMPARE=="Specific date") {
                dateInput("mcDATE", "Date", value=datebeg, min=datebeg, max=dateend)
            } else { return() }
        } else { return() }
    })
    output$mdownload <- renderUI({
        if (input$mBUTTON==0) {
            return()
        } else {
            input$mBUTTON
            downloadButton("mDOWNLOAD", "Download Table")
        }
    })
    
    ### Figure stuff
    observe({
        tsig1 <- input$fSIGNAL
        tsignal <- mysignals[mysignals$SIGNAL==tsig1,]
        updateSelectInput(session, "fSIGINFO", selected=paste0(tsignal$SIGNAL, " -- ", tsignal$ST_EW, " ", tsignal$ST_NS, ", ", tsignal$CITY))
    })
    observe({
        tsig2 <- input$fSIGINFO
        tsignal <- as.integer(substr(tsig2, 1, 4))
        updateSelectInput(session, "fSIGNAL", selected=tsignal)
    })
    # Load data
    mydata <- reactive({
        temp <- dat20[,c("DATE", paste0("sig", input$fSIGNAL))]
        names(temp)[2] <- "COUNT"
        temp$WEEKDAY <- strftime(temp$DATE, "%A")
        temp <- temp[,c("DATE", "WEEKDAY", "COUNT")]
        temp
    })
    # Subset data by date
    mydatasub <- reactive({
        mydat <- mydata()
        temp <- mydat[mydat$DATE>=input$fDATE1 & mydat$DATE<=input$fDATE2,]
        temp
    })
    # Load comparison data
    mycdata <- reactive({
        mydat <- mydatasub()
        if (input$fCOMPARE=="Last year") {
            temp <- dat19[,c("DATE", paste0("sig", input$fSIGNAL))]
            names(temp)[2] <- "COUNT"
            newdate1 <- input$fDATE1 - 7*52
            newdate2 <- input$fDATE2 - 7*52
            mycdat <- temp[temp$DATE>=newdate1 & temp$DATE<=newdate2,]
            names(mycdat)[2] <- "COMPARE"
            mydat$COMPARE <- mycdat$COMPARE
        } else if (input$fCOMPARE=="1+ weeks ago") {
            temp <- mydata()
            newdate1 <- input$fDATE1 - 7*as.integer(input$fcWEEK)
            newdate2 <- input$fDATE2 - 7*as.integer(input$fcWEEK)
            mycdat <- temp[temp$DATE>=newdate1 & temp$DATE<=newdate2,]
            names(mycdat)[3] <- "COMPARE"
            mydat$COMPARE <- mycdat$COMPARE
        } else if (input$fCOMPARE=="Specific dates") {
            temp <- mydata()
            mycdat <- temp[temp$DATE>=input$fcDATE1 & temp$DATE<=input$fcDATE2,]
            mydat$COMPARE <- NA
            for (i in 1:nrow(mydat)) {
                mydat$COMPARE[i] <- mean(mycdat$COUNT[mycdat$WEEKDAY==mydat$WEEKDAY[i]])
            }; rm(i)
        }
        mydat$PCTCHANGE <- 100*(mydat$COUNT - mydat$COMPARE)/mydat$COMPARE
        # mydat <- mydat[,c("DATE", "WEEKDAY", "COMPARE", "COUNT", "PCTCHANGE")]
        mydat
    })
    mywhichdata <- reactive({
        if (input$fCOMP==T) { mycdata() } else { mydatasub() }
    })
    # Figure
    output$fFIGURE <- renderPlot({
        if (input$fBUTTON==0) {
            return()
        } else { isolate({
            # input$fBUTTON
            mymain <- paste0("Signal ", input$fSIGNAL, ", ", input$fDATE1, " to ", input$fDATE2)
            # myylab <- paste0("Pedestrian activity (", "90A", ")")
            myylab <- paste0("Pedestrian activity")
            temp <- mywhichdata()
            if (input$fCOMP==T) {
                myxlims <- c(min(temp$DATE), max(temp$DATE))
                myylims <- c(0, max(temp$COUNT, temp$COMPARE))
                plot(temp$DATE, temp$COMPARE, type="b", pch=1, lty=2, las=1, cex=1.5, xlim=myxlims, ylim=myylims, xlab="", main=mymain, ylab=myylab)
                points(temp$DATE, temp$COUNT, pch=19, cex=1.5)
                lines(temp$DATE, temp$COUNT, type="c", lty=1, cex=1.5)
                legend("bottom", legend=c("Count", "Compare"), pch=c(19,1), lty=c(1,2), horiz=T)
            } else {
                barplot(temp$COUNT, width=1, space=0, las=1, names.arg=temp$DATE, main=mymain, ylab=myylab)
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
                temp <- mydatasub()
                if (input$fCOMP==T) {
                    tdate  <- as.Date(round(input$plot_click$x), origin="1970-01-01")
                    paste0(temp$COUNT[temp$DATE==tdate], " for ", tdate)
                } else {
                    paste0(temp$COUNT[ceiling(input$plot_click$x)], " for ", strftime(temp$DATE[ceiling(input$plot_click$x)]))
                }
            }) }
        }
    })
    # Table
    output$fTABLE <- renderDT({
        if (input$fBUTTON==0) {
            return()
        } else { isolate({
            # input$fBUTTON
            temp <- mywhichdata()
            if (input$fCOMP==T) {
                temp$COMPARE <- round(temp$COMPARE, digits=0)
                temp$PCTCHANGE <- round(temp$PCTCHANGE, digits=0)
            } else {
            }
            DT::datatable(temp, options=list(lengthMenu=c(7,14,28,56), pageLength=7), rownames=F, filter=list(position="top", clear=F), selection="none")
        }) }
    })
    # Download
    output$fDOWNLOAD <- downloadHandler(
        filename = function() {
            paste(input$fSIGNAL, ".csv", sep = "")
        },
        content = function(file) {
            write.csv(mywhichdata(), file, row.names = FALSE)
        }
    )
    # UI elements
    output$fdate1 <- renderUI({
        dateInput("fDATE1", "Start Date", value=dateone, min=datebeg, max=dateend)
    })
    output$fdate2 <- renderUI({
        input$fDATE1
        datemin <- input$fDATE1
        datemax <- dateend
        dateInput("fDATE2", "End Date", value=datemin, min=datemin, max=datemax)
    })
    output$fdownload <- renderUI({
        if (input$fBUTTON==0) {
            return()
        } else {
            input$fBUTTON
            downloadButton("fDOWNLOAD", "Download Table")
        }
    })
    output$fcompare <- renderUI({
        if (input$fCOMP==T) {
            radioButtons("fCOMPARE", "Compare to", choices=c("Last year", "1+ weeks ago", "Specific dates"), selected="Last year", inline=F)
        } else { return() }
    })
    output$fclyear1 <- renderUI({
        if (input$fCOMP==T) {
            if (input$fCOMPARE=="Last year") {
                input$fDATE1
                shinyjs::disabled(textInput("fcLYEAR1", "Last year's dates", value=strftime((input$fDATE1 - 7*52), "%Y-%m-%d")))
            } else { return() }
        } else { return() }
    })
    output$fclyear2 <- renderUI({
        if (input$fCOMP==T) {
            if (input$fCOMPARE=="Last year") {
                input$fDATE2
                shinyjs::disabled(textInput("fcLYEAR2", NULL, value=strftime((input$fDATE2 - 7*52), "%Y-%m-%d")))
            } else { return() }
        } else { return() }
    })
    output$fcweek <- renderUI({
        if (input$fCOMP==T) {
            if (input$fCOMPARE=="1+ weeks ago") {
                selectInput("fcWEEK", "How many weeks ago?", choices=c("Last week"=1L, "2 weeks ago"=2L, "3 weeks ago"=3L, "4 weeks ago"=4L, 
                                                                       "5 weeks ago"=5L, "6 weeks ago"=6L, "7 weeks ago"=7L, "8 weeks ago"=8L), selected=1L)
            } else { return() }
        } else { return() }
    })
    output$fcdate1 <- renderUI({
        if (input$fCOMP==T) {
            if (input$fCOMPARE=="Specific dates") {
                dateInput("fcDATE1", "Start Date", value=datebeg, min=datebeg, max=dateend)
            } else { return() }
        } else { return() }
    })
    output$fcdate2 <- renderUI({
        if (input$fCOMP==T) {
            if (input$fCOMPARE=="Specific dates") {
                input$fcDATE1
                datemin <- input$fcDATE1
                datemax <- dateend
                dateInput("fcDATE2", "End Date", value=datemin, min=datemin, max=datemax)
            } else { return() }
        } else { return() }
    })
}

########################################
# Run app

shinyApp(ui = ui, server = server)

########################################
# END
########################################