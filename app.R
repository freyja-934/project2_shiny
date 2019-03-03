#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
################################################################################  THESE ARE THE LIBRARIES USED FOR THIS SHINY APP AND PROJECT ##################################################################
library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)
library(reshape) #and melt function from https://www.statmethods.net/management/reshape.html
library(tidyr)
library(readr) #provided by import button for file
library(rsconnect)
library(data.table)

################################################################################ READ IN ALL THE FILES ################################################################################
temp = list.files(pattern="*.csv")
allData2 <- lapply(temp, function(x) fread(x, stringsAsFactors = FALSE))
dailyData <- do.call(rbind, allData2)


################ AQI FILES FROM 1980-2018 ########################
aqi_1980 <- read.table(file= "annual/annual_aqi_by_county_1980.csv",sep = ",", header= TRUE)
aqi_1981 <- read.table(file= "annual/annual_aqi_by_county_1981.csv",sep = ",", header= TRUE)
aqi_1982 <- read.table(file= "annual/annual_aqi_by_county_1982.csv",sep = ",", header= TRUE)
aqi_1983 <- read.table(file= "annual/annual_aqi_by_county_1983.csv",sep = ",", header= TRUE)
aqi_1984 <- read.table(file= "annual/annual_aqi_by_county_1984.csv",sep = ",", header= TRUE)
aqi_1985 <- read.table(file= "annual/annual_aqi_by_county_1985.csv",sep = ",", header= TRUE)
aqi_1986 <- read.table(file= "annual/annual_aqi_by_county_1986.csv",sep = ",", header= TRUE)
aqi_1987 <- read.table(file= "annual/annual_aqi_by_county_1987.csv",sep = ",", header= TRUE)
aqi_1988 <- read.table(file= "annual/annual_aqi_by_county_1988.csv",sep = ",", header= TRUE)
aqi_1989 <- read.table(file= "annual/annual_aqi_by_county_1989.csv",sep = ",", header= TRUE)

aqi_1990 <- read.table(file= "annual/annual_aqi_by_county_1990.csv",sep = ",", header= TRUE)
aqi_1991 <- read.table(file= "annual/annual_aqi_by_county_1991.csv",sep = ",", header= TRUE)
aqi_1992 <- read.table(file= "annual/annual_aqi_by_county_1992.csv",sep = ",", header= TRUE)
aqi_1993 <- read.table(file= "annual/annual_aqi_by_county_1993.csv",sep = ",", header= TRUE)
aqi_1994 <- read.table(file= "annual/annual_aqi_by_county_1994.csv",sep = ",", header= TRUE)
aqi_1995 <- read.table(file= "annual/annual_aqi_by_county_1995.csv",sep = ",", header= TRUE)
aqi_1996 <- read.table(file= "annual/annual_aqi_by_county_1996.csv",sep = ",", header= TRUE)
aqi_1997 <- read.table(file= "annual/annual_aqi_by_county_1997.csv",sep = ",", header= TRUE)
aqi_1998 <- read.table(file= "annual/annual_aqi_by_county_1998.csv",sep = ",", header= TRUE)
aqi_1999 <- read.table(file= "annual/annual_aqi_by_county_1999.csv",sep = ",", header= TRUE)

aqi_2000 <- read.table(file= "annual/annual_aqi_by_county_2000.csv",sep = ",", header= TRUE)
aqi_2001 <- read.table(file= "annual/annual_aqi_by_county_2001.csv",sep = ",", header= TRUE)
aqi_2002 <- read.table(file= "annual/annual_aqi_by_county_2002.csv",sep = ",", header= TRUE)
aqi_2003 <- read.table(file= "annual/annual_aqi_by_county_2003.csv",sep = ",", header= TRUE)
aqi_2004 <- read.table(file= "annual/annual_aqi_by_county_2004.csv",sep = ",", header= TRUE)
aqi_2005 <- read.table(file= "annual/annual_aqi_by_county_2005.csv",sep = ",", header= TRUE)
aqi_2006 <- read.table(file= "annual/annual_aqi_by_county_2006.csv",sep = ",", header= TRUE)
aqi_2007 <- read.table(file= "annual/annual_aqi_by_county_2007.csv",sep = ",", header= TRUE)
aqi_2008 <- read.table(file= "annual/annual_aqi_by_county_2008.csv",sep = ",", header= TRUE)
aqi_2009 <- read.table(file= "annual/annual_aqi_by_county_2009.csv",sep = ",", header= TRUE)

aqi_2010 <- read.table(file= "annual/annual_aqi_by_county_2010.csv",sep = ",", header= TRUE)
aqi_2011 <- read.table(file= "annual/annual_aqi_by_county_2011.csv",sep = ",", header= TRUE)
aqi_2012 <- read.table(file= "annual/annual_aqi_by_county_2012.csv",sep = ",", header= TRUE)
aqi_2013 <- read.table(file= "annual/annual_aqi_by_county_2013.csv",sep = ",", header= TRUE)
aqi_2014 <- read.table(file= "annual/annual_aqi_by_county_2014.csv",sep = ",", header= TRUE)
aqi_2015 <- read.table(file= "annual/annual_aqi_by_county_2015.csv",sep = ",", header= TRUE)
aqi_2016 <- read.table(file= "annual/annual_aqi_by_county_2016.csv",sep = ",", header= TRUE)
aqi_2017 <- read.table(file= "annual/annual_aqi_by_county_2017.csv",sep = ",", header= TRUE)
aqi_2018 <- read.table(file= "annual/annual_aqi_by_county_2018.csv",sep = ",", header= TRUE)

################## AQS FILE OF SITE LAT/LONG FOR MAPPING######################
latlong <- read.table(file= "sites/aqs_sites.csv",sep = ",", header = TRUE)

################################################################################ MERGE ALL AQI DATA INTO ONE TABLE WITH THE SAME COLUMNS AS IN EASH FILE ################################################################################
#adapted from prof code
allData <- rbind(aqi_1980,aqi_1981,aqi_1982,aqi_1983,aqi_1984,aqi_1985,aqi_1986,aqi_1987,aqi_1988,aqi_1989,aqi_1990,aqi_1991,aqi_1992,aqi_1993,aqi_1994,aqi_1995,aqi_1996,aqi_1997,aqi_1998,aqi_1999,aqi_2000,aqi_2001,aqi_2002,aqi_2003,aqi_2004,aqi_2005,aqi_2006,aqi_2007,aqi_2008,aqi_2009,aqi_2010,aqi_2011,aqi_2012,aqi_2013,aqi_2014,aqi_2015,aqi_2016,aqi_2017,aqi_2018)

######################################## THIS IS EXTRA CODE USED TO CREATE THE "B" AND "C" MENU LEVELS - NOT REMOVING SINCE THEY HELPED ME LEARN AND I USE THEM FOR REFERENCE 
#toCounty <- function(x){return (toString(x))}
#listNames <- c(colnames(allData))
#list2 <- unique(allData$County)
#listNames <- toString(unique(allData$County)) #adapted from https://stat.ethz.ch/R-manual/R-devel/library/base/html/toString.html
#listNamesGood <-c(listNames)  #lapply(listNames,toCounty) #listNames[listNames != "Hour" & listNames != "newDate"]
#listNamesGood <- unlist(strsplit(listNames,", "))#adapted from https://stat.ethz.ch/R-manual/R-devel/library/base/html/strsplit.html
#listNamesGood2 <- sort(listNamesGood,decreasing = FALSE) #https://stat.ethz.ch/R-manual/R-devel/library/base/html/sort.html
#listNamesGood2[listNamesGood2 == "Hour" || listNamesGood2 == "newDate"]
#listNames <- unique(allData[1:2])
#listNamesGood <- paste(trimws(listNames$County),",",listNames$State)
##listNamesB2 <- gsub(" , ", ", ", listNamesGood)
#listNamesB <- listNamesGood[listNamesB2 == "Cook, Illinois" | listNamesB2 == "Hawaii, Hawaii" | listNamesB2 == "New York, New York" | listNamesB2 == "Los Angeles, California" | listNamesB2 == "King, Washington" |listNamesB2 == "Harris, Texas" | listNamesB2 == "Harris, Texas" | listNamesB2 == "Miami-Dade, Florida" | listNamesB2 == "San Juan, New Mexico" | listNamesB2 == "Hennepin, Minnesota" | listNamesB2 ==  "Wake, North Carolina" | listNamesB2 == "Craighead, Arkansas" | listNamesB2 == "Covington City, Virginia" ]
#listNamesB  <- sort(listNamesB ,decreasing = FALSE)

######################################################################## CREATE A STARTING DROPDOWN OPTION FOR YEAR ########################################################################################
years<-c(1980:2018)




####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~< THE UI PART OF THE CODE >~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
# Define UI for application that draws a histogram
ui <- dashboardPage(
  ################################################################################ THE COLOR AND LENGTH OF THE TITLE FOR THE SIDEBAR ################################################################################
  skin = "yellow",
  dashboardHeader(title = "CS 424 PROJECT 2"),
  
  ######################################## CREATE DROP DOWN MENUS IN SIDEBAR + NEW TAB CONTAINING RESOURCES ######################################## 
  dashboardSidebar(sidebarMenu(disable = FALSE, collapsed = FALSE, style = "margin-top:500px",
                               selectInput("Year", "Select the year to visualize", years, selected = 2017),
                               selectInput("State", "State", choices = "" , selected = ""),
                               selectInput("Countys", "Countys",choices = "" , selected = ""),
                               #selectInput("County", "Select the county to visualize", listNamesB, selected = " select County"), #### THIS IS THE PART B GRADE MENU ####
                               menuItem("Yearly Data", tabName="yearlydata", icon = icon("dashboard")),
                               menuItem("Daily Data", tabName="dailydata", icon = icon("dashboard")),
                               menuItem("Resources", tabName="resources", icon = icon("bullet"))
  )),
  ######################################## THE MAIN BODDY OF THE WEB APP ########################################
  dashboardBody(
    
    ######################################## TAB THAT WHEN CLICKED DISPLAYES THE LIST OF RESOURCES USED ########################################
    tabItems(
      tabItem(
        tabName = "resources",
        h2("Resources used in this project:"),
        h5("All data used is from here: https://aqs.epa.gov/aqsweb/airdata/download_files.html"),
        h5("Base Code and Code influence from Professor Andy Johnson, https://www.evl.uic.edu/aej/424/ (week 2)"),
        h6("librarys used: shiny, shinydashboard, ggplot2, lubridate, DT, jpeg, grid, leaflet, scales, reshape, tidyr, readr"),
        h5("Techniques and methods adapted from:"),
        h6("  *https://rstudio.github.io/shinydashboard/appearance.html"),
        h6("  *reshape and melt function from https://www.statmethods.net/management/reshape.html"),
        h6("  *https://stat.ethz.ch/R-manual/R-devel/library/base/html/toString.html"),
        h6("  *https://stat.ethz.ch/R-manual/R-devel/library/base/html/strsplit.html "),
        h6("  *https://stat.ethz.ch/R-manual/R-devel/library/base/html/sort.html"),
        h6("  *https://stackoverflow.com/questions/52544228/r-shiny-display-static-text-outside-sidebar-panel"),
        h6("  *https://stackoverflow.com/questions/12280571/how-can-i-remove-rows-containing-0-of-certain-columns-while-keeping-the-rows-i"),
        h6("  *https://www.rdocumentation.org/packages/ggplot2/versions/3.1.0/topics/position_stack"),
        h6("  *http://www.sthda.com/english/wiki/ggplot2-pie-chart-quick-start-guide-r-software-and-data-visualization"),
        h6("  *https://stackoverflow.com/questions/26869141/conditionally-hiding-data-labels-in-ggplot2-graph"),
        h6("  *https://www.displayr.com/how-to-make-a-pie-chart-in-r/"),
        h6("  *https://stackoverflow.com/questions/38126212/how-to-check-if-data-table-has-empty-rows"),
        h6("  *http://www.cookbook-r.com/Graphs/Bar_and_line_graphs_(ggplot2)/"),
        h6("  *https://rpubs.com/euclid/343644"),
        h6("  *http://shiny.rstudio.com/articles/shinyapps.html"),
        h6("  *https://shiny.rstudio.com/reference/shiny/0.14/updateSelectInput.html"),
        h6("  *https://gist.github.com/aagarw30/d08c5fb1794cf9b58fa38342db97b697"),
        h6("  *http://shiny.rstudio.com/articles/shinyapps.html"),
        h6("  *student who was in office hours on Friday - never got your name"),
        h6("  *http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/"),
        h6("  *https://stackoverflow.com/questions/33266157/how-to-add-more-whitespace-to-the-main-panel-in-shiny-dashboard"),
        h6("  *professor Andy Johnson")
        
      ), tabItem(tabName = "yearlydata",
                 
                 ######################################## THE MAIN BODY DISPLAY ########################################
                 fluidRow(
                   ################## FIRST COLUMN ######################
                   column(2,
                          
                          fluidRow(box(title = "% of Day Type", solidHeader = TRUE, status = "primary", width = 12, plotOutput("hist1", height = 400))),
                          fluidRow(box(title = "Location of County", solidHeader = TRUE, status = "primary", width = 12, leafletOutput("map1", height = 400)))
                          
                          #fluidRow(box( title = "% of Pollutant", solidHeader = TRUE, status = "primary", width = 12,plotOutput("hist3", height = 400)))
                          #fluidRow(box(title = "Pollutant Count over Years", solidHeader = TRUE, status = "primary", width = 12, plotOutput("line2", height = 400)))
                          
                   ),
                   ################### SECOND COLUMN #####################
                   column(2,
                          
                          fluidRow(box( title = "Number of Day Types", solidHeader = TRUE, status = "primary", width = 12,plotOutput("hist2", height = 400))),
                          fluidRow(box(title = "Number of Pollutant Days", solidHeader = TRUE, status = "primary", width = 12, plotOutput("hist4", height = 400)))
                          
                          #fluidRow(box( title = "AQI Count over Years", solidHeader = TRUE, status = "primary", width = 12,plotOutput("line1", height = 400)))
                          
                   ),
                   ################### THIRD COLUMN #####################
                   column(2,
                          
                          fluidRow(box(title = "Day Types in Table Format", solidHeader = TRUE, status = "primary", width = 12, dataTableOutput("tab2", height = 400))),
                          fluidRow(box(title = "Pollutant Types in Table Format", solidHeader = TRUE, status = "primary", width = 12, dataTableOutput("tab3", height = 400)))
                          
                          #fluidRow(box(title = "Location of County", solidHeader = TRUE, status = "primary", width = 12, leafletOutput("map1", height = 400)))
                          
                          
                   ),
                   ################### FOURTH COLUMN #####################
                   column(2,
                          fluidRow(box( title = "% of Pollutant CO", solidHeader = TRUE, status = "primary", width = 12,plotOutput("pieCO", height = 400))),
                          fluidRow(box( title = "% of Pollutant NO2", solidHeader = TRUE, status = "primary", width = 12,plotOutput("pieNO2", height = 400)))
                   ),
                   
                   ################### FIFTH COLUMN #####################
                   column(2,
                          fluidRow(box( title = "% of Pollutant Ozone", solidHeader = TRUE, status = "primary", width = 12,plotOutput("pieOzone", height = 400))),
                          fluidRow(box( title = "% of Pollutant SO2", solidHeader = TRUE, status = "primary", width = 12,plotOutput("pieSO2", height = 400)))
                   ),
                   ################### SIXTH COLUMN #####################
                   column(2,
                          fluidRow(box( title = "% of Pollutant PM2.5", solidHeader = TRUE, status = "primary", width = 12,plotOutput("piePM2.5", height = 400))),
                          fluidRow(box( title = "% of Pollutant PM10", solidHeader = TRUE, status = "primary", width = 12,plotOutput("piePM10", height = 400)))
                   ),
                   ################### ROW LINE PLOTS #####################
                   fluidRow(box( title = "AQI Count over Years", solidHeader = TRUE, status = "primary", width = 6, plotOutput("line1", height = 400)),
                            box(title = "Pollutant Count over Years", solidHeader = TRUE, status = "primary", width = 6, plotOutput("line2", height = 400)),
                            box(title = "Pollutant Count over the years", solidHeader = TRUE, status = "primary", width = 4, dataTableOutput("tableBig", height = 400))
                   )
                   
                 )),
      tabItem(tabName = "dailydata",
              
              ######################################## THE MAIN BODY DISPLAY ########################################
              fluidRow(
                ################## FIRST COLUMN ######################
                column(3,
                       
                       fluidRow(
                         box(title = "Daily AQI line chart", solidHeader = TRUE, status = "primary", width = 12, 
                             plotOutput("line_chart", height = 400)))
                ),
                
                column(3,
                       
                       fluidRow(box(title = "this is the bar plot box", solidHeader = TRUE, status = "primary", width = 12, plotOutput("bar", height = 400)))
                ),
                
                column(3, 
                       
                       fluidRow(box(title = "this is the table box", solidHeader = TRUE, status = "primary", width = 12, dataTableOutput("table", height = 400)))
                ),
                
                column(3,
                       
                       fluidRow(box(title = "this is the map box", solidHeader = TRUE, status = "primary", width = 12, leafletOutput("map", height = 400)))
                ) 
                
              ))
      
    ))) ####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~< END OF UI CODE >~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#####


####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~< START OF SERVER CODE >~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
# Define server logic required to draw a histogram
server <- function(session,input, output) {
  theme_set(theme_light(base_size = 18)) #FROM PROFS CODE
  
  ####################CODE USED FOR B PART OF PROJECT ####################
  #mainY <- reactive({subset(allData, allData$Year == input$Year & allData$County == (strsplit(input$County, " ,")[[1]][1]) & allData$State == (strsplit(input$County, ", ")[[1]][2]))})
  #mainC<- reactive({subset(allData, allData$County == (strsplit(input$County, " ,")[[1]][1]) & allData$State == (strsplit(input$County, ", ")[[1]][2]))})
  #mapLL <- reactive({subset(latlong, State.Name== (strsplit(input$County, ", ")[[1]][2]) & County.Name == (strsplit(input$County, " ,")[[1]][1]))})
  
  ####################################################### CODE THAT CONSTANTLY FILTERS ALLDATA TABLE BASED ON USER INPUT ################################################################################
  mainY <- reactive({subset(allData, allData$Year == input$Year & allData$County == input$Countys & allData$State == input$State) })
  mainC<- reactive({subset(allData, allData$County == input$Countys & allData$State == input$State)})
  mapLL <- reactive({subset(latlong, State.Name == input$State & County.Name == input$Countys)})
  
  dailyY <- reactive({subset(dailyData, as.integer(substr(dailyData$Date,1,4)) == input$Year & dailyData$`county Name` == input$Countys & dailyData$`State Name` == input$State) })
  
  
  #################### CHANGES DROP MENU OPTIONS FOR STATE BASED ON YEAR CHOSEN####################
  #observe events adapted from https://gist.github.com/aagarw30/d08c5fb1794cf9b58fa38342db97b697
  observeEvent(
    input$Year,
    updateSelectInput(session, "State", "State", choices = unique(allData$State[allData$Year == input$Year]))
  )
  #################### CHANGES DROP DOWN MENU OPTIONS FOR COUNTY BASED ON STATE AND YEAR CHOSEN####################
  observeEvent(
    input$State,
    updateSelectInput(session, "Countys", "Countys", choices = unique(allData$County[allData$Year == input$Year & allData$State == input$State]))
  )
  
  
  ##################### PIE CHART OF PERCENTAGES OF THE DIFFERENT TYPES OF DAYS ###################
  output$hist1 <- renderPlot({
    main1 <- mainY()
    if(nrow(main1) == 0){
      errorPrint <- "No Data Available"
      errorPrint
    } else { 
      str(main1)
      days<-main1[5:10]
      FullDays <- melt(days) #adapted from https://www.statmethods.net/management/reshape.html
      nonZero <- FullDays[apply(FullDays[c(2)],1,function(x) !any(x==0)),] #adapted from https://stackoverflow.com/questions/12280571/how-can-i-remove-rows-containing-0-of-certain-columns-while-keeping-the-rows-i
      str(FullDays)
      str(sum(FullDays$value))
      DaysPieChart <- ggplot(FullDays,aes(x="",y=value)) + geom_bar(width = 1, stat = "identity", aes("", value, fill = variable), position = position_stack(reverse = TRUE))+coord_polar("y") # adapted from https://www.rdocumentation.org/packages/ggplot2/versions/3.1.0/topics/position_stack and http://www.sthda.com/english/wiki/ggplot2-pie-chart-quick-start-guide-r-software-and-data-visualization
      DaysPieChart +  geom_text(data=subset(FullDays, value>0), aes(x= "",y= value,label=percent(value/sum(value))), size=5, position = position_stack(0.5)) +theme_minimal() + xlab('') + ylab('') # adapted from https://stackoverflow.com/questions/26869141/conditionally-hiding-data-labels-in-ggplot2-graph and http://www.sthda.com/english/wiki/ggplot2-pie-chart-quick-start-guide-r-software-and-data-visualization and https://www.rdocumentation.org/packages/ggplot2/versions/3.1.0/topics/position_stack
    }
  })
  
  
  ##################### BAR CHART ON THE NUMBER OF THE DIFFERENT TYPES OF DAYS ###################
  output$hist2 <- renderPlot({
    main2 <- mainY()
    if(nrow(main2) == 0){
      errorPrint <- "No Data Available"
      errorPrint
    } else {
      str(main2)
      days2<-main2[5:10]
      FullDays2 <- melt(days2) #adapted from https://www.statmethods.net/management/reshape.html
      ggplot(FullDays2,aes(y= value, x=variable, angle = 0)) + geom_bar(stat="identity",aes(fill = variable)) + geom_text(aes(label=value), color="Black", size=5,position = position_dodge(1),vjust = 0)+ theme_minimal()  + xlab('Day Type') +ylab('Days')#http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization  and  https://www.displayr.com/how-to-make-a-pie-chart-in-r/
    }
  })
  
  
  ######################## TABLE OF DATA FOR DIFFERENT DAY TYPES DAY TYPE/NUMBER OF DAYS/AVERAGE ################
  output$tab2 <- DT::renderDataTable({
    main2 <- mainY()
    if(nrow(main2) == 0){
      errorPrint <- "No Data Available"
      errorPrint
    } #adapted from https://stackoverflow.com/questions/38126212/how-to-check-if-data-table-has-empty-rows
    else {
      str(main2)
      days2<-main2[5:10]
      #colorData<- c("brown1","darkolivegreen1","darkorchid1","darkorange1","plum1","yellow") 
      FullDays2 <- melt(days2) #adapted from https://www.statmethods.net/management/reshape.html
      #colnames(FullDays2)[1] <- "Day Type"
      #colnames(FullDays2)[2] <- "Days"
      #names(FullDays2)[names(FullDays2) == 'value'] <- 'Days'
      FullDays2$Avg <- percent(FullDays2$value/sum(FullDays2$value))
      displaydata<- as.data.frame(FullDays2)
    }
    
  })
  
  ######################## PIE CHART OF DIFFERENT Pollutant PERCENAGES ################
  output$hist3 <- renderPlot({
    main1 <- mainY()
    if(nrow(main1) == 0){
      errorPrint <- "No Data Available"
      errorPrint
    } else {
      str(main1)
      days<-main1[14:19]
      FullDays <- melt(days) #adapted from https://www.statmethods.net/management/reshape.html
      nonZero <- FullDays[apply(FullDays[c(2)],1,function(x) !any(x==0)),] #adapted from https://stackoverflow.com/questions/12280571/how-can-i-remove-rows-containing-0-of-certain-columns-while-keeping-the-rows-i
      str(FullDays)
      str(sum(FullDays$value))
      DaysPieChart <- ggplot(FullDays,aes(x="",y=value)) + geom_bar(width = 1, stat = "identity", aes("", value, fill = variable), position = position_stack(reverse = TRUE))+coord_polar("y") # adapted from https://www.rdocumentation.org/packages/ggplot2/versions/3.1.0/topics/position_stack and http://www.sthda.com/english/wiki/ggplot2-pie-chart-quick-start-guide-r-software-and-data-visualization and https://www.rdocumentation.org/packages/ggplot2/versions/3.1.0/topics/position_stack
      DaysPieChart +  geom_text(data=subset(FullDays, value>0), aes(x= "",y= value,label=percent(value/sum(value))), size=5, position = position_stack(0.5)) +theme_minimal()  + xlab('') +ylab('') # adapted from https://stackoverflow.com/questions/26869141/conditionally-hiding-data-labels-in-ggplot2-graph and http://www.sthda.com/english/wiki/ggplot2-pie-chart-quick-start-guide-r-software-and-data-visualization
      
    }
  })
  
  ####################### BAR CHART OF THE NUMBER OF DAYS CONTAINING A Pollutant #################
  output$hist4 <- renderPlot({
    main2 <- mainY()
    if(nrow(main2) == 0){
      errorPrint <- "No Data Available"
      errorPrint
    } else {
      str(main2)
      days2<-main2[14:19]
      colorData<- c("brown1","darkolivegreen1","darkorchid1","darkorange1","plum1","yellow") 
      FullDays2 <- melt(days2) #adapted from https://www.statmethods.net/management/reshape.html
      ggplot(FullDays2,aes(y= value, x=variable)) + geom_bar(stat="identity",aes(fill = variable)) + geom_text(aes(label=value), color="Black", size=5,position = position_dodge(1),vjust = 0)+ theme_minimal()  + xlab('Pollutant') +ylab('Days')#http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization  and  https://www.displayr.com/how-to-make-a-pie-chart-in-r/
      #ggplot(main1,aes(x =Good.Days))
    }
  })
  
  
  ####################### TABLE FOR DIFFERNT PollutantS/ DAYS Pollutant IN AIR/ % OF DAYS  #################
  output$tab3 <- DT::renderDataTable({
    main2 <- mainY()
    if(nrow(main2) == 0){
      errorPrint <- "No Data Available"
      errorPrint
    } else {
      str(main2)
      days2<-main2[14:19]
      #colorData<- c("brown1","darkolivegreen1","darkorchid1","darkorange1","plum1","yellow") 
      FullDays2 <- melt(days2) #adapted from https://www.statmethods.net/management/reshape.html
      #colnames(FullDays2)[1] <- "Day Type"
      #colnames(FullDays2)[2] <- "Days"
      #names(FullDays2)[names(FullDays2) == 'value'] <- 'Days'
      FullDays2$Avg <- percent(FullDays2$value/sum(FullDays2$value))
      #FullDays2$Avg2 <- percent(FullDays2$value/main2$Days.with.AQI)
      displaydata<- as.data.frame(FullDays2)
    }
    
  })
  
  ###################### AN INTERACTIVE MAP THAT DISPLAYS ONE OF THE LOCATIONS OF A SPECIFIED COUNTY (IF ITS NULL OR EMPTY IT CHOOSES ANOTHER POINT)##################
  output$map1 <- renderLeaflet({ # adapted from prof code
    main2 <- mainY()
    if(nrow(main2) == 0){
      errorPrint <- "No Data Available"
      errorPrint
    } else {
      mapLatLong <- mapLL()
      mapit<- mapLatLong[1,]
      str(mapit)
      if(is.null(mapit$Longitude) | is.null(mapit$Latitude)){mapit <- mapLatLong[3,]}
      if(mapit$Longitude == 0 | mapit$Latitude == 0){mapit <- mapLatLong[3,]}
      #adapted from profs code
      str(mapit)
      map <- leaflet()
      map <- addTiles(map)
      map <- setView(map, mapit$Longitude, mapit$Latitude, zoom = 7)
      map <- addMarkers(map, mapit$Longitude, mapit$Latitude, popup = "evl")
      map
    }
  })
  
  ####################### TABLE FOR DIFFERNT PollutantS/ DAYS Pollutant IN AIR/ % OF DAYS  #################
  output$tableBig <- DT::renderDataTable({
    main2 <- mainC()
    if(nrow(main2) == 0){
      errorPrint <- "No Data Available"
      errorPrint
    } else {
      str(main2)
      days2<-main2[14:19]
      days2$year <- main2[3]
      row.names(days2) <- NULL
      str(days2)
      data.frame(days2)
      avg<- function(x) {return(percent(x/main2$Days.with.AQI))}
      final<- as.data.frame(lapply(days2[,1:6], avg))
      final$year <- days2$year
      displaydata<- as.data.frame(final)
    }
    
  })
  
  ######################## A LINE GRAPH TO DISPLAY THE CHANGES OF DAY TYPES BY YEAR ################
  output$line1 <- renderPlot({
    mainLine <- mainC()  # idea for line graph used from https://rpubs.com/euclid/343644
    if(nrow(mainLine) == 0){
      errorPrint <- "No Data Available"
      errorPrint
    } #adapted from https://stackoverflow.com/questions/38126212/how-to-check-if-data-table-has-empty-rows
    else {
      data1<-data.frame(mainLine[3],mainLine[11])
      data2<-data.frame(mainLine[3],mainLine[12])
      data3<-data.frame(mainLine[3],mainLine[13])
      data4 <- merge(data1,data2,by= "Year")
      data5 <- merge(data4,data3, by ="Year")
      reshaped <- melt(data5, id.vars = "Year")
      ggplot(reshaped, aes(Year,value,col=variable))+geom_line()+ xlab('Year') +ylab('AQI')
    }
  })
  
  ######################## A LINE GRAPH TO DISPLAY THE CHANGES OF PollutantS DAYS BY YEAR################
  output$line2 <- renderPlot({
    mainLine <- mainC() # idea for line graph used from https://rpubs.com/euclid/343644
    if(nrow(mainLine) == 0){
      errorPrint <- "No Data Available"
      errorPrint
    } #adapted from https://stackoverflow.com/questions/38126212/how-to-check-if-data-table-has-empty-rows
    else {
      data1<-data.frame(mainLine[3],mainLine[14])
      data2<-data.frame(mainLine[3],mainLine[15])
      data3<-data.frame(mainLine[3],mainLine[16])
      data4<-data.frame(mainLine[3],mainLine[17])
      data5<-data.frame(mainLine[3],mainLine[18])
      data6<-data.frame(mainLine[3],mainLine[19])
      
      data12 <- merge(data1,data2,by= "Year")
      data123 <- merge(data12,data3, by ="Year")
      data1234 <- merge(data123,data4, by ="Year")
      data12345 <- merge(data1234,data5, by ="Year")
      data123456 <- merge(data12345,data6, by ="Year")
      reshaped <- melt(data123456, id.vars = "Year")
      ggplot(reshaped, aes(Year,value,col=variable))+geom_line()+ xlab('Year') +ylab('Days')
    }
  })
  
  
  ######################## PIE CHART OF Pollutant CO ################
  output$pieCO <- renderPlot({
    main1 <- mainY()
    if(nrow(main1) == 0){
      errorPrint <- "No Data Available"
      errorPrint
    } else {
      str(main1)
      daysPie<-main1$Days.CO
      daysPie$Not.pol<- main1$Days.with.AQI - main1$Days.CO
      data.frame(daysPie)
      melted <- melt(daysPie)
      Legend = c("CO","Not CO")
      str(melted)
      pie<-ggplot(melted, aes(x="", y=value))+geom_bar(width = 1, stat = "identity", aes("", value, fill = Legend), position = position_stack())+coord_polar("y")+  geom_text(data =melted, mapping = aes(x= "",y= value,label=percent(value/sum(value))), size=5, position = position_stack(0.5)) + theme_minimal()  + xlab('') +ylab('') 
      pie + scale_fill_manual(values= c("green", "gray"))
    }
  })
  
  
  ######################## PIE CHART OF Pollutant NO2 ################
  output$pieNO2 <- renderPlot({
    main1 <- mainY()
    if(nrow(main1) == 0){
      errorPrint <- "No Data Available"
      errorPrint
    } else {
      str(main1)
      daysPie<-main1$Days.NO2
      daysPie$Not.pol<- main1$Days.with.AQI - main1$Days.NO2
      data.frame(daysPie)
      melted <- melt(daysPie)
      Legend = c("NO2"," Not NO2")
      str(melted)
      pie<-ggplot(melted, aes(x="", y=value))+geom_bar(width = 1, stat = "identity", aes("", value, fill = Legend), position = position_stack())+coord_polar("y")+  geom_text(data =melted, mapping = aes(x= "",y= value,label=percent(value/sum(value))), size=5, position = position_stack(0.5)) + theme_minimal()  + xlab('') +ylab('') 
      pie + scale_fill_manual(values= c("gray", "cadetblue1"))
    }
  })
  
  ######################## PIE CHART OF Pollutant Ozone ################
  output$pieOzone <- renderPlot({
    main1 <- mainY()
    if(nrow(main1) == 0){
      errorPrint <- "No Data Available"
      errorPrint
    } else {
      str(main1)
      daysPie<-main1$Days.Ozone
      daysPie$Not.pol<- main1$Days.with.AQI - main1$Days.Ozone
      data.frame(daysPie)
      melted <- melt(daysPie)
      Legend = c("Ozone","Not Ozone")
      str(melted)
      pie<-ggplot(melted, aes(x="", y=value))+geom_bar(width = 1, stat = "identity", aes("", value, fill = Legend), position = position_stack())+coord_polar("y")+  geom_text(data =melted, mapping = aes(x= "",y= value,label=percent(value/sum(value))), size=5, position = position_stack(0.5)) + theme_minimal()  + xlab('') +ylab('') 
      pie + scale_fill_manual(values= c("gray", "red"))
    }
  })
  
  ######################## PIE CHART OF Pollutant SO2 ################
  output$pieSO2 <- renderPlot({
    main1 <- mainY()
    if(nrow(main1) == 0){
      errorPrint <- "No Data Available"
      errorPrint
    } else {
      str(main1)
      daysPie<-main1$Days.SO2
      daysPie$Not.pol<- main1$Days.with.AQI - main1$Days.SO2
      data.frame(daysPie)
      melted <- melt(daysPie)
      Legend = c("SO2","Not SO2")
      str(melted)
      pie<-ggplot(melted, aes(x="", y=value))+geom_bar(width = 1, stat = "identity", aes("", value, fill = Legend), position = position_stack())+coord_polar("y")+  geom_text(data =melted, mapping = aes(x= "",y= value,label=percent(value/sum(value))), size=5, position = position_stack(0.5)) + theme_minimal()  + xlab('') +ylab('') 
      pie + scale_fill_manual(values= c("gray", "darkorange"))
    }
  })
  
  ######################## PIE CHART OF Pollutant PM2.5 ################
  output$piePM2.5 <- renderPlot({
    main1 <- mainY()
    if(nrow(main1) == 0){
      errorPrint <- "No Data Available"
      errorPrint
    } else {
      str(main1)
      daysPie<-main1$Days.PM2.5
      daysPie$Not.pol<- main1$Days.with.AQI - main1$Days.PM2.5
      data.frame(daysPie)
      melted <- melt(daysPie)
      Legend = c("PM2.5","Not PM2.5")
      pie<-ggplot(melted, aes(x="", y=value))+geom_bar(width = 1, stat = "identity", aes("", value, fill = Legend), position = position_stack())+coord_polar("y")+  geom_text(data =melted, mapping = aes(x= "",y= value,label=percent(value/sum(value))), size=5, position = position_stack(0.5)) + theme_minimal()  + xlab('') +ylab('') 
      pie + scale_fill_manual(values= c("gray", "mediumorchid1"))
    }
  })
  
  ######################## PIE CHART OF Pollutant PM10 ################
  output$piePM10 <- renderPlot({
    main1 <- mainY()
    if(nrow(main1) == 0){
      errorPrint <- "No Data Available"
      errorPrint
    } else {
      str(main1)
      daysPie<-main1$Days.PM10
      daysPie$Not.pol<- main1$Days.with.AQI - main1$Days.PM10
      data.frame(daysPie)
      melted <- melt(daysPie)
      Legend = c("PM10","Not PM10")
      str(melted)
      pie<-ggplot(melted, aes(x="", y=value))+geom_bar(width = 1, stat = "identity", aes("", value, fill = Legend), position = position_stack())+coord_polar("y")+  geom_text(data =melted, mapping = aes(x= "",y= value,label=percent(value/sum(value))), size=5, position = position_stack(0.5)) + theme_minimal()  + xlab('') +ylab('') 
      pie + scale_fill_manual(values= c("gray", "gold"))
    }
  })
  
  
  
  ###################################blank daily code###############
  
  output$line_chart <- renderPlot({
    df <- dailyY()
    if(nrow(df) == 0){
      errorPrint <- "No Data Available"
      errorPrint
    }
    else {
      ggplot(df, aes(Date,AQI,col = `Defining Parameter`)) + 
        geom_line(group=1) +
        ylab('Daily AQI')
      
    }
  })
  
  output$bar <- renderPlot({
    
  })
  
  output$table <- DT::renderDataTable({
    
    
  })
  
  output$map <- renderLeaflet({ # adapted from prof code
    
  })
  
}####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ END OF SERVER CODE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####


######################################### Run the application #########################################
shinyApp(ui = ui, server = server)

