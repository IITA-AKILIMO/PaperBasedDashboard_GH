#' Paper based Annex dashboard
#' Authors : Meklit Chernet, Turry Ouma, IITA
#' Last updated on : November 2021 (to include GH)
#' 
#setwd("C:/Users/User/Documents/ACAI/DASHBOARDS/paper based/PaperbasedDashboard_GH_SP")


library(tidyr)
require(plyr)
library(rgdal)
library(raster)
library(dismo)
library(maptools)
library(rgeos)
require(RColorBrewer)
require(graphics)
require(rasterVis)
library(sp)
library(shinyalert)
library(ggthemes)
require(ggplot2)
library(gridExtra) 
library(hexbin)
library(viridis)
library(sf)
library(ggspatial)
require(ggrepel)
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(plotly)
library(sf)
library(raster)
library(dplyr)
library(spData)
library(tmap)
library(leaflet)
library(cartogram)
library(grid)
library(formattable)
library(shinybusy)
library(DT)
library(shinyjs)

source("SP_functions.R")
### SHINY SERVER ###

server = function(input, output, session) {
  #.............................................................................  
  # Show a modal on initiation of tool
  #.............................................................................
  

      # 
  dataModal <- function(failed = FALSE) {
    list(
    modalDialog(
      #span("AKILIMO Paper Based Annex dashboard"),
      size = c("m"),
      HTML('<img src="pics/akilimo4.jpg" />'),
      
       br(),
      # br(),
      selectInput("use", "Please select the use case",
                  choices = c("Fertilizer Recommendation", "Scheduled Planting"),
                  selected = ""),
 
      
      easyClose = FALSE,
      fade = TRUE,
     
      
      # if (failed)
      #   div(tags$b("Invalid name of data object", style = "color: red;")),
      
      footer = tagList(
        #modalButton("Cancel"),
        useShinyjs(),  # Set up shinyjs
        actionButton("ok", "DONE!")
      )
    ),
    tags$style("
#shiny-modal .modal-dialog {
  position: absolute;
  top: 150px;
  right: 10px;
  left: 200px;
  bottom: 50;
  left: 0;
  z-index: 10040;
 
}")
    )
  }
  

  # Show modal when button is clicked.
  
  # showModal(
  #   list(
  #     modalDialog(title = "Modal2"),
  #     tags$script("$('.modal-backdrop').css('display', 'none');")
  #   )
  # )
 
    showModal(
        dataModal()
    )
    
    observeEvent(input$ok, {
      
      if(input$use == "Fertilizer Recommendation"){
    shinyalert("Fertilizer Recommendation", "This tool contains tables and maps with advice on application rates of urea,
                       NPK fertilizer for cassava, as well as the expected root yield response. Response to fertilizer 
                       depends on soil conditions and the time of planting.

                                 This window will automatically close once country data is loaded.


               ",
               
               type = "info", timer = 7000, size = 'm',
               closeOnClickOutside = FALSE,
               closeOnEsc = FALSE,
               animation = FALSE,
               html = FALSE,
        
               showConfirmButton = FALSE,
               showCancelButton = FALSE,
               confirmButtonText = "OK",
               confirmButtonCol = "#AEDEF4")
      }
  })
    
    # showModal(dataModal())
    
    
    observeEvent(input$ok, {
      
      if(input$use == "Scheduled Planting"){
        shinyalert("Loading planting and harvest schedules...",  "
                                            This tool provides expected cassava yields for different planting and harvest schedules. 
                                            These are yields predicted based on the common rainfall pattern between the month of planting 
                                            and harvest, and assume good agronomic practices are applied. For every State and planting 
                                            month, a table is generated, presenting the expected root yield for different LGAs (in the rows) 
                                            and harvest from 8 up to 15 months after planting (in the columns).

                                            This window will automatically close when the data is loaded.


               ",
                   type = "info", timer = 10000, size = 'm',
                   closeOnClickOutside = FALSE,
                   closeOnEsc = FALSE,
                   animation = FALSE,
                   html = FALSE,
                   
                   showConfirmButton = FALSE,
                   showCancelButton = FALSE,
                   confirmButtonText = "OK",
                   confirmButtonCol = "#AEDEF4")
      }
    })
    
  

  #............................................................................. 
  #spinner before maps are displayed
  #.............................................................................
  observeEvent(input$btn_go, {
  
    shinybusy::show_modal_spinner(
   
      spin = "cube-grid",
      #spin = "fading-circle",
      #spin = "fading-circle",
     
      color = 	"#228B22",
      #00FF00
      text = "Please wait while the map is being generated...",
    )

    Sys.sleep(6)
    remove_modal_spinner()
  })
  

  output$reg_ghana <- renderUI({
    
    pickerInput("reg_ghana", "Select region",
                choices = c("Savannah", "Ashanti","Ahafo", "Volta",  "Central",  "Eastern", "Bono East", "Bono"),
                selected = NULL,
                multiple = TRUE,
                options = pickerOptions(maxOptions = 1))
  })
 

  observeEvent(input$reg_ghana, { 
    if(!is.null(input$reg_ghana))  {
  output$plntmth <- renderUI({
        
      pickerInput("plntmth", "Select planting month",
                  choices = c("January", "February", "March", "April", "May", "June", "July", "August", "September",
                              "October", "November", "December"),
                  selected = NULL,
                  multiple = TRUE,
                  options = pickerOptions(maxOptions = 1))
  })
    }
  })  

  

  
  observeEvent(input$plntmth, {
    if(!is.null(input$plntmth)) {
      output$costs <- renderUI({
        
        pickerInput("costs", "Would you like to specify your prices for cassava and fertilizers?",
                    choices = c("Yes", "No"),
                    selected = NULL,
                    multiple = TRUE,
                    options = pickerOptions(maxOptions = 1))
      })
  
    }
  })
  
  observeEvent(input$plntmth, {
    if(!is.null(input$plntmth))  {
      
      
      output$selection3 <- renderUI({
        
        pickerInput("selection3", "Select variable to view",
                    choices = c("NPK 15:20:20 rate",  "NPK 12:30:17 rate", "Expected yield response", "Urea rate"),
                    selected = NULL,
                    multiple = TRUE,
                    options = pickerOptions(maxOptions = 1))
      })
      
    }
  })
  
 
  observeEvent(input$reg_ghana, {
    if(!is.null(input$reg_ghana))  {
      
      output$unit_loc <- renderUI({
        
        selectInput("unit_loc", "Select unit of land",
                    choices = c("acre", "hectare"))
      })
    }
  }) 
  
  observeEvent(input$reg_ghanaSP, {
    if(!is.null(input$reg_ghanaSP))  {
      
      output$unit_locSP <- renderUI({
        
        selectInput("unit_locSP", "Select unit of land",
                    choices = c("acre", "hectare"))
      })
    }
  }) 
  

  
  observeEvent(input$unit_loc, {
    if(!is.null(input$unit_loc))  {
      output$FCY_ha <- renderUI({

        selectInput("FCY_ha", "Select Your Current Yield (Tonnes)",
                                  choices = c("0-7.5 t/hectare", "7.5-15 t/hectare", "15-22.5 t/hectare", "22.5-30 t/hectare", ">30 t/hectare",  ""),
                    selected = "")            
      })
    

      output$FCY_acre <- renderUI({

        selectInput("FCY_acre", "Select Your Current Yield (Tonnes)",
                    choices = c("0-3 t/acre", "3-6 t/acre", "6-9 t/acre", "9-12 t/acre", ">12 t/acre", ""),
                    selected = "")
      })
    }
      
    })  
  

  observeEvent(input$costs, {
    if(input$costs == "Yes" ) {
   
  output$CassavaPrice <- renderUI({
    
    textInput("CassavaPrice", "Price of cassava per ton")
  })
    }
  })

  observeEvent(input$costs, {
    if(input$costs == "Yes")  {
      
      output$NPK152020Price <- renderUI({
        
        textInput("NPK152020Price", "Cost of NPK:15:20:20 per 50Kg bag")
      })
      
      output$NPK123017Price <- renderUI({
        
        textInput("NPK123017Price", "Cost of NPK:12:30:17 per 50Kg bag")
      })
      
      
    }
  })

    observeEvent(input$costs, {
      if(input$costs == "Yes") {
      output$UreaPrice <- renderUI({
        
        textInput("UreaPrice", "Cost of Urea per 50Kg bag")
      })
    }
    })

  
  observeEvent(input$costs, {
    if(input$costs == "Yes") {  
      output$btn_go <- renderUI({
        actionButton("btn_go", "Get Maps & Tables", icon("map"),
                     style="color: #fff; background-color: green; border-color: #2e6da4")
        
      })
    }else if(input$costs == "No"){
      output$btn_go <- renderUI({
        actionButton("btn_go", "Get Maps & Tables", icon("map"),
                     style="color: #fff; background-color: green; border-color: #2e6da4")
      })
    }
  })
  
  observeEvent(input$use, {
    if (input$use == "Scheduled Planting"){
      showTab(inputId = "nav", target = "Expected yield for different planting and harvest schedules", select = TRUE)
      hideTab(inputId = "nav", target = "Use case mapper")
      hideTab(inputId = "nav", target = "View maps side by side")
      hideTab(inputId = "nav", target = "View Table")
    }
  })

  observeEvent(input$use, {
  if(input$use == "Fertilizer Recommendation"){
    hideTab(inputId = "nav", target = "Expected yield for different planting and harvest schedules")
    showTab(inputId = "nav", target = "Use case mapper", select = TRUE)
    showTab(inputId = "nav", target = "View maps side by side")
    showTab(inputId = "nav", target = "View Table")
     }
  })
 
 #.................................................................................................................
  ## Determine platform type and set working directory accordingly
  # When OK button is pressed, attempt to load the data set. If successful,
  # remove the modal. If not show another modal, but this time with a failure
  # message.
  observeEvent(input$ok, {
    if(!is.null(input$ok)){
      removeModal()
    }
  })
  
  
  observeEvent(input$ok, {
    # Check that data object exists and is data frame.
    if (input$use == "Fertilizer Recommendation"){

  
      #######################################################################################
      ## Read the GIS layers
      #######################################################################################
      
      boundaryGH <- readOGR(dsn=getwd(), layer="gha_admbnda_adm1_gss_20210308")
      ghRegions <- readOGR(dsn=getwd(), layer="gha_admbnda_adm2_gss_20210308")
      
      ###########################################################################
      ## GH fertilizer recom for FCY 1:5
      ###########################################################################
      
      FR_GH_FCY <- readRDS("GH_FR_CassavaPaperBased.RDS")
      FR_GH_FCY1 <- FR_GH_FCY[FR_GH_FCY$FCY == "level1", ]
      FR_GH_FCY2 <- FR_GH_FCY[FR_GH_FCY$FCY == "level2", ]
      FR_GH_FCY3 <- FR_GH_FCY[FR_GH_FCY$FCY == "level3", ]
      FR_GH_FCY4 <- FR_GH_FCY[FR_GH_FCY$FCY == "level4", ]
      FR_GH_FCY5 <- FR_GH_FCY[FR_GH_FCY$FCY == "level5", ]
      
      ###########################################################################
      ##  adding planting month
      ###########################################################################
      addplm <- function(ds, country){
        ds$respY <- ds$TargetY - ds$CurrentY
        ds$groRev <- ds$NR + ds$TC
        ds$plm <- as.factor(ds$plw)
        levels(ds$plm)[levels(ds$plm) %in% 1:4]   <- "January"
        levels(ds$plm)[levels(ds$plm) %in% 5:8]   <- "February"
        levels(ds$plm)[levels(ds$plm) %in% 9:13]  <- "March"
        levels(ds$plm)[levels(ds$plm) %in% 14:17] <- "April"
        levels(ds$plm)[levels(ds$plm) %in% 18:22] <- "May"
        levels(ds$plm)[levels(ds$plm) %in% 23:26] <- "June"
        levels(ds$plm)[levels(ds$plm) %in% 27:30] <- "July"
        levels(ds$plm)[levels(ds$plm) %in% 31:35] <- "August"
        levels(ds$plm)[levels(ds$plm) %in% 36:39] <- "September"
        levels(ds$plm)[levels(ds$plm) %in% 40:43] <- "October"
        levels(ds$plm)[levels(ds$plm) %in% 44:48] <- "November"
        levels(ds$plm)[levels(ds$plm) %in% 49:53] <- "December"
        
        
        ds$rateUrea <- ds$urea
        ds$rateNPK152020 <- ds$NPK152020 
        ds$rateNPK123017 <- ds$NPK123017
        
        return(ds)
      }
      
      FR_GH_FCY1_plm <- addplm(ds=FR_GH_FCY1, country = "GH") ## GH if user current yield is level 1
      FR_GH_FCY2_plm <- addplm(ds=FR_GH_FCY2, country = "GH") ## GH if user current yield is level 2
      FR_GH_FCY3_plm <- addplm(ds=FR_GH_FCY3, country = "GH") ## GH if user current yield is level 3
      FR_GH_FCY4_plm <- addplm(ds=FR_GH_FCY4, country = "GH") ## GH if user current yield is level 4
      FR_GH_FCY5_plm <- addplm(ds=FR_GH_FCY5, country = "GH") ## GH if user current yield is level 5
     
      ###########################################################################
      ## select FCY and read the corresponding file 
      ## NG: Subsetting for the user defined Region and selecting a coordinate to put the state name in the map
      ###########################################################################
      
      
      removeModal()
      #.................................................................................................................
      #Dashboard activity starts here
      #............................................................................. 

      
      
      
    observeEvent(input$btn_go, {
    
    #define reactive values
    
  
      FCY_ha <- input$FCY_ha
      
      FCY_acre <- input$FCY_acre
      
      Selection3 <- input$selection3

      plantMonth <- input$plntmth
     
      reg_ghana <- input$reg_ghana
      plantMonth <- input$plntmth
      cities <- input$city
      unit <- input$unit_loc
      
      UreaPrice <- as.numeric(input$UreaPrice)
      
      NPK152020Price <- as.numeric(input$NPK152020Price)
      NPK123017Price <- as.numeric(input$NPK123017Price)
      
      CassavaPrice <- as.numeric(input$CassavaPrice)
      
      costs <- input$costs

  
    
    #specify yield categories
 
    if(unit == 'hectare'){
      yield_level <- ifelse( FCY_ha == "0-7.5 t/hectare", "a low yield level",
                             ifelse( FCY_ha == "7.5-15 t/hectare","a normal yield level",
                                     ifelse( FCY_ha == "15-22.5 t/hectare","a medium yield level", 
                                             ifelse( FCY_ha == "22.5-30 t/hectare","a high yield level",
                                                     ifelse( FCY_ha == ">30 t/hectare","a very high yield level"
                                                     )))))
    }else if(unit == 'acre'){ 
      yield_level <- ifelse( FCY_acre == "0-3 t/acre","a low yield level",
                             ifelse( FCY_acre == "3-6 t/acre","a normal yield level",
                                     ifelse( FCY_acre == "6-9 t/acre","a medium yield level",
                                             ifelse( FCY_acre == "9-12 t/acre","a high yield level",
                                                     ifelse( FCY_acre == ">12 t/acre","a very high yield level")
                                             ))))
    }
    
    
    lgaGroups <- input$reg_ghana
    lgaGroups2 <- input$reg_ghana
    
    if (unit == "hectare"){
      
      FCY <- FCY_ha
      if(FCY == "7.5-15 t/hectare" ){
        ds=FR_GH_FCY2_plm
      } else if(FCY == "0-7.5 t/hectare" ){
        ds=FR_GH_FCY1_plm
      }else if(FCY == "15-22.5 t/hectare" ){
        ds=FR_GH_FCY3_plm
      }else if(FCY == "22.5-30 t/hectare" ){
        ds <- FR_GH_FCY4_plm
      }else if(FCY == ">30 t/hectare" ){
        ds <- FR_GH_FCY5_plm
      }
    }else if(unit == "acre"){
      FCY <- FCY_acre
      if(FCY == "3-6 t/acre" ){
        ds=FR_GH_FCY2_plm
      } else if(FCY == "0-3 t/acre"  ){
        ds=FR_GH_FCY1_plm
      }else if(FCY == "6-9 t/acre" ){
        ds=FR_GH_FCY3_plm
      }else if(FCY == "9-12 t/acre" ){
        ds <- FR_GH_FCY4_plm
      }else if(FCY == ">30 t/acre" ){
        ds <- FR_GH_FCY5_plm
      }
    }
    
    
    Savannah <- droplevels(ds[ds$Regions %in% c("Savannah"), ])
    Savannahlabel <-  data.frame(Regions= c("Savannah"), lon=c(-0.75), lat=c(9.5))
    
    Bono_East <- droplevels(ds[ds$Regions %in% c("Bono East"), ])
    Bono_Eastlabel <- data.frame(Regions= c("Bono East"), lon=c(-0.65), lat=c(8.5))
    
    Bono <- droplevels(ds[ds$Regions %in% c("Bono"), ])
    Bonolabel <- data.frame(Regions= c("Brong Ahafo"), lon=c(-2.5), lat=c(8.68))
    
    
    Central <- droplevels(ds[ds$Regions %in% c("Central"), ])
    Centrallabel <- data.frame(Regions= c("Central"), lon=c(-1.0), lat=c( 6.1))
    
    Ashanti <- droplevels(ds[ds$Regions %in% c("Ashanti"), ])
    Ashantilabel <- data.frame(Regions= c("Ashanti"), lon=c(-2.5), lat=c(7.53))
    
    Ahafo <- droplevels(ds[ds$Regions %in% c("Ahafo"), ])
    Ahafolabel <- data.frame(Regions= c("Ahafo"), lon=c(-2.8), lat=c(6.5))
    
    
    Eastern <- droplevels(ds[ds$Regions %in% c("Eastern"), ])
    Eastlabel <- data.frame(Regions= c("Eastern"), lon=c(-0.8), lat=c(7.21))
    
    Volta <- droplevels(ds[ds$Regions %in% c("Volta"), ])
    Voltalabel <- data.frame(Regions= c("Volta"), lon=c(0.7), lat=c(7.3))
    
    
    Savannahcity <- data.frame(Regions = c("Savannah"),name=c("Damongo"), lat=c(9.08), lon = c(-1.82))
    
    Centralcity <- data.frame(Regions = c("Central"),name=c("Cape Coast"), lat=c(5.12), lon = c(-1.27))
    
    Bonocity <- data.frame(Regions = c("Brong Ahafo"), name=c("Sunyani"), 
                           lat=c(7.34), lon = c(-2.32))
    
    Bono_Eastcity <- data.frame(Regions = c("Bono East"), name=c("Techiman"), 
                                lat=c(7.58), lon = c(-1.93))
    
    Ashanti_city <- data.frame(Regions = c("Ashanti"), name=c("Kumasi"), 
                               lat=c(6.68), lon = c(-1.63))
    
    Ahafocity <- data.frame(Regions = c("Ahafo"), name=c("Goaso"), 
                            lat=c(6.8), lon = c(-2.52))
    
    
    Eastcity <- data.frame(Regions = c("Eastern"),name=c("Koforidua"), 
                           lat=c(6.1), lon = c(-0.26))
    
    
    Voltacity <- data.frame(Regions = c("Volta"),name=c("Ho"), 
                            lat=c(6.6), lon = c(0.47))
    
    if(lgaGroups =="Ashanti"){
      LGApoints <- Ashanti
      RegionsLabel <- Ashantilabel
      textangle<-0
      cities = c("Ashanti_city")
      couple <- "Two"
      
    }else if(lgaGroups =="Bono"){
      LGApoints <- Bono
      RegionsLabel <- Bonolabel
      textangle<-0
      cities = c("Bonocity")
      couple <- "Two"
      
    }else if(lgaGroups =="Bono East"){
      LGApoints <- Bono_East
      RegionsLabel <- Bono_Eastlabel
      textangle<-0
      cities = c("Bono_Eastcity")
      couple <- "Two"
      
    }else if(lgaGroups =="Savannah"){
      LGApoints <- Savannah
      RegionsLabel <- Savannahlabel
      textangle<-0
      cities = c("Savannahcity")
      couple <- "Two"
      
    }else if(lgaGroups =="Ahafo"){
      LGApoints <- Ahafo     
      RegionsLabel <- Ahafolabel
      textangle<-0 
      cities = c("Ahafocity")
      couple <- "Two"
      
    }else if(lgaGroups =="Central"){
      LGApoints <- Central 
      RegionsLabel <- Centrallabel
      textangle<-0 
      cities = c("Centralcity")
      couple <- "Two"
      
    }else if(lgaGroups =="Volta"){
      LGApoints <- Volta 
      RegionsLabel <- Voltalabel
      textangle<-0 
      cities = c("Voltacity")
      couple <- "Two"
      
    }else if(lgaGroups =="Eastern"){
      LGApoints <- Eastern 
      RegionsLabel <- Eastlabel
      textangle<-0 
      cities = c("Eastcity")
      couple <- "Two"
      
    }
    
    #plantMonth <- "January"
    plotData <- droplevels(LGApoints[LGApoints$plm == plantMonth & LGApoints$Regions %in% lgaGroups , ])
    
    AOI <- lgaGroups
    AOIMapS <- subset(boundaryGH, ADM1_EN %in% AOI ) 
    
    AOIMap <- subset(ghRegions, ADM1_EN %in% AOI )
    AOIMap <- AOIMap[,c("ADM1_EN", "ADM2_EN")]
    LGAnames <- as.data.frame(AOIMap)
    LGAnames <- cbind(LGAnames, coordinates(AOIMap))
    colnames(LGAnames) <- c("REGION","DISTRICT","long","lat")
    crop_ghRegions <- subset(ghRegions, ADM1_EN %in% AOI )
    
    
    ## take REGION average
    LGAaverage <- ddply(plotData, .(Districts, Regions), summarize,
                        LGAUrea = round(mean(Urea), digits=0),
                        LGANPK112221 = round(mean(NPK112221 ), digits=0),
                        LGANPK251010= round(mean(NPK251010), digits=0),
                        LGANPK152020 = round(mean(NPK152020), digits=0),
                        LGANPK123017 = round(mean(NPK123017), digits=0),
                        LGAdY = round(mean(respY), digits=0))
    
    
    LGAaverage$LGAUrea <- ifelse(LGAaverage$LGAUrea <25, 0, LGAaverage$LGAUrea)
    LGAaverage$LGANPK112221 <- ifelse(LGAaverage$LGANPK112221 <25, 0, LGAaverage$LGANPK112221)
    LGAaverage$LGANPK251010 <- ifelse(LGAaverage$LGANPK251010 <25, 0, LGAaverage$LGANPK251010)
    LGAaverage$LGANPK152020 <- ifelse(LGAaverage$LGANPK152020 <25, 0, LGAaverage$LGANPK152020)
    LGAaverage$LGANPK123017 <- ifelse(LGAaverage$LGANPK123017 <25, 0, LGAaverage$LGANPK123017)
    
    
    
    if(unit == 'acre'){
      
      dss <- LGAaverage
      dss$LGAUrea <- dss$LGAUrea / 2.47105
      dss$LGANPK112221 <- dss$LGANPK112221 / 2.47105
      dss$LGANPK251010 <- dss$LGANPK251010 / 2.47105
      dss$LGANPK152020 <- dss$LGANPK152020 / 2.47105
      dss$LGANPK123017 <- dss$LGANPK123017 / 2.47105
      dss$LGAdY <- dss$LGAdY / 2.47105
      
      LGAaverage <- dss
    }
    
    plotData <- merge(plotData, LGAaverage, by=c("Districts", "Regions"))
    
    if(unit == "hectare"){
      plotData$Urea <- round(plotData$LGAUrea/50)*50
      plotData$NPK112221 <- round(plotData$LGANPK112221/50)*50
      plotData$NPK251010 <- round(plotData$LGANPK251010 /25)*25
      plotData$NPK152020 <- round(plotData$LGANPK152020/50)*50
      plotData$NPK123017 <- round(plotData$LGANPK123017 /25)*25
      plotData$dY <- round(plotData$LGAdY/2)*2
    }else{
      plotData$Urea <- round(plotData$LGAUrea/10)*10
      plotData$NPK112221 <- round(plotData$LGANPK112221/10)*10
      plotData$NPK251010 <- round(plotData$LGANPK251010 /10)*10
      plotData$NPK152020 <- round(plotData$LGANPK152020/10)*10
      plotData$NPK123017 <- round(plotData$LGANPK123017 /10)*10
      plotData$dY <- round(plotData$LGAdY/1)*1
    }
    
    fileNameCsv <- paste(plantMonth, "_", paste(lgaGroups, collapse="_"), ".csv", sep="")
    
    AOIMap2 <- merge(AOIMap, unique(plotData[, c("Regions","Districts", "Urea",  "NPK152020", "NPK123017","dY", "LGAdY")]),
                     by.x=c("ADM1_EN","ADM2_EN") ,by.y=c("Regions","Districts"))
    AOIMap2$month <- plantMonth
    AOIMap2 <- AOIMap2[!is.na(AOIMap2$Urea), ]
    plotData$month <- plantMonth
    
    Currency <- "GHC"
    tt_gh <- unique(as.data.frame(plotData[, c("Regions","Districts", "Urea",  "NPK152020", "NPK123017", "LGAdY", "month")]))
    tt_gh$LGAdY <- round(tt_gh$LGAdY, digits = 1)
    tt_gh2 <- dplyr::select(tt_gh, c("Regions","Districts", "Urea",  "NPK152020", "NPK123017", "LGAdY"))
    
    
    colnames(tt_gh2) <- c("Regions","Districts", "Recommended urea rate",  "NPK15_20_20 rate","NPK12_30_17 rate",
                          "Expected yield response"
    )
    
    
    
    #table output based on cost inputs
    
    if(costs == "No"){
      
      output$tabletext_ghc<- renderText({
        
        
        paste("AKILIMO advice for planting in ", plantMonth, ". Your current yield is ", FCY, ".", sep="")
        
      })
      
      output$mytable <- renderDT({tt_gh2},
                                 rownames = FALSE, 
                                 extensions = c('Buttons','FixedColumns'), 
                                 options = list(dom = 'Bfrtip',
                                                pageLength = nrow(tt_gh2),
                                                initComplete = DT::JS(
                                                  "function(settings, json) {",
                                                  "$(this.api().table().header()).css({'background-color': 'black', 'color': '#fff'});",
                                                  "}"),
                                                
                                                buttons = list(
                                                  list(extend = 'excel', 
                                                       filename = paste('AKILIMO advice', '_', lgaGroups2, '_', plantMonth),
                                                       title = paste("AKILIMO advice for planting in ", plantMonth, ". Your current yield is between ", FCY, ".", sep="")),
                                                  list(extend = 'pdf',
                                                       filename = paste('AKILIMO advice', '_', lgaGroups2, '_', plantMonth),
                                                       title = paste("AKILIMO advice for planting in ", plantMonth, ". Your current yield is between ", FCY,  ".", sep=""),
                                                       header = TRUE)
                                                )
                                                
                                 )
      )
      
    }else if (costs == "Yes"){
      #colnames(tt) <- c("Regions","LGA", "Recommended urea rate (kg/ha)", "NPK15_15_15 rate", "Expected yield response", "Planting month")
      tt_dataframe2 <- reactive({
        
        df_gh2 <- data.frame(UreaPrice=as.numeric(input$UreaPrice),
                             NPK1520207Price=as.numeric(input$NPK152020Price),NPK123017Price=as.numeric(input$NPK123017Price),
                             CassavaPrice=as.numeric(input$CassavaPrice),
                             Regions=input$reg_ghana)
        
        return(df_gh2)
      })
      
      
      #df_tt <- data.frame(UreaPrice=UreaPrice,NPK171717Price=NPK171717Price,CassavaPrice=CassavaPrice,REGION=lga_Groups,DAPPrice=DAPPrice)
      #  
      print(CassavaPrice)
      tt_merge_gh <- merge(tt_gh, tt_dataframe2(),by="Regions")
      
      
      #MEKLIT HERE
      tt_merge_gh$totalSalePrice = tt_merge_gh$LGAdY  * tt_merge_gh$CassavaPrice
      tt_merge_gh$totalCost = (tt_merge_gh$UreaPrice/50 * tt_merge_gh$Urea) + 
        (tt_merge_gh$NPK1520207Price/50 * tt_merge_gh$NPK152020)+
        (tt_merge_gh$NPK123017Price/50 * tt_merge_gh$NPK123017)
      
      #tt_merge$NetRevenue = tt_merge$totalSalePrice - tt_merge$totalCost
      #totalCost = (as.numeric(UreaPrice)/50 * 15) + (as.numeric(NPK151515Price)/50 * 300)
      
      tt_merge_gh$NetRevenue = tt_merge_gh$totalSalePrice - tt_merge_gh$totalCost
      
      tt_merge_gh2 <- dplyr::select(tt_merge_gh, c(Regions, Districts, Urea,NPK152020,NPK123017,  LGAdY, CassavaPrice, totalSalePrice, totalCost, NetRevenue))
      colnames(tt_merge_gh2) <- c("Regions","Districts", "Urea (kg/hectare)", "NPK 12:30:17 (kg/hectare)","NPK 25:10:10 (kg/hectare)",
                                  "Expected yield increase (t)",  "Cassava Price",  "Total sale (GHC)", "Fertilizer cost (GHC)", "Profit (GHC)")
      
      
      write.csv(tt_merge_gh2, fileNameCsv, row.names = FALSE)
      
      AOIMap3 <- st_as_sf(AOIMap2)
      
      output$tabletext_ghc<- renderText({
        
        
        paste("AKILIMO advice for planting in ", plantMonth, ". Your current yield is ", FCY,  " and the cassava price is ", CassavaPrice, " ", Currency, ".", sep="")
        
      })
      
      
      output$mytable2 <- renderDT({tt_merge_gh2},
                                  rownames = FALSE, 
                                  extensions = c('Buttons','FixedColumns'), 
                                  options = list(dom = 'Bfrtip',
                                                 pageLength = nrow(tt_merge_gh2),
                                                 initComplete = DT::JS(
                                                   "function(settings, json) {",
                                                   "$(this.api().table().header()).css({'background-color': 'black', 'color': '#fff'});",
                                                   "}"),
                                                 
                                                 buttons = list(
                                                   list(extend = 'excel', 
                                                        filename = paste('AKILIMO advice', '_', lgaGroups2, '_', plantMonth),
                                                        title = paste("AKILIMO advice for planting in ", plantMonth, ". Your current yield is between ", FCY,  " and the cassava price is ", CassavaPrice, " ", Currency, ".", sep="")),
                                                   list(extend = 'pdf',
                                                        filename = paste('AKILIMO advice', '_', lgaGroups2, '_', plantMonth),
                                                        title = paste("AKILIMO advice for planting in ", plantMonth, ". Your current yield is between ", FCY,  " and the cassava price is ", CassavaPrice, " ", Currency, ".", sep=""),
                                                        header = TRUE)
                                                 )
                                                 
                                  )
      )
      
    }
    # --------------------------------------------------------------------------
    #side by side maps
    # --------------------------------------------------------------------------
    AOIMap3 <- st_as_sf(AOIMap2)
    #urea plot
    ################################################
    
    #reactive  title based on unit of land
    tturea <- reactive({
      
      if(unit == "hectare"){
        
        tturea <- paste("Recommended urea rate(kg/hectare)")
      }else {
        
        tturea <- paste("Recommended urea rate(kg/acre)")
      }
    })
    
    
    
    ureasclae <- unique(AOIMap3$Urea)
    keU <- as.character(ureasclae[order(ureasclae)])
    AOIMap3$Urea <- factor(AOIMap3$Urea)
    levels(AOIMap3$Urea) <- keU
    
    require(ggrepel)
    library(tmap)
    
    #tmap output using reactive values  
    #urea
    observeEvent(tturea(),
                 {
                   
                   output$ureaplot2 <- renderTmap({
                     
                     
                     sm1 = tm_shape(AOIMap3) +
                       tm_polygons(
                         col = "Urea",
                         title = tturea(),
                         palette = "Greens",
                         id=paste("ADM2_EN", ":" , "Urea"),
                         popup.vars=c("Urea rate : "="Urea")
                       )+
                       tm_text(text = "ADM2_EN")
                     sm1
                     
                     
                     
                   })
                 })
    
    
    
    ############################################################################   
    #npk NPK12:30:17 plot
    #############################################################################
    #reactive title based on unit of land
    
    ttnpk12 <- reactive({
      
      if(unit == "hectare"){
        
        ttnpk12 <- paste("Recommended NPK 12:30:17 rate (kg/hectare)")
      }else {
        
        ttnpk12 <- paste("Recommended NPK 12:30:17 rate(kg/acre)")
      }
    })
    
    #NPK 12:30:17 plot
    npk12 <- unique(AOIMap3$NPK123017)
    kev <- as.character(npk12[order(npk12)])
    AOIMap3$NPK123017 <- factor(AOIMap3$NPK123017)
    levels(AOIMap3$NPK123017) <- kev
    
    #NPK 12:30:17 plot
    observeEvent(ttnpk12(),
                 {
                   
                   output$npkplot_12 <- renderTmap({
                     
                     
                     
                     nm4 = tm_shape(AOIMap3) +
                       tm_polygons(
                         col = "NPK123017",
                         title = ttnpk12(),
                         palette = "YlGnBu",
                         id="ADM2_EN",
                         popup.vars=c("NPK 12:30:17 rate : "="NPK123017")
                       )+
                       tm_text(text = "ADM2_EN")
                     
                     nm4
                     
                   }) 
                 })
    ############################################################################   
    #NPK 15:20:20 plot
    #############################################################################
    #reactive title based on unit of land
    
    ttnpk15 <- reactive({
      
      if(unit == "hectare"){
        
        ttnpk15 <- paste("Recommended NPK 15:20:20 rate (kg/hectare)")
      }else {
        
        ttnpk15 <- paste("Recommended NPK 15:20:20 rate(kg/acre)")
      }
    })
    
    
    
    npk15 <- unique(AOIMap3$NPK152020)
    kev <- as.character(npk15[order(npk15)])
    AOIMap3$NPK152020 <- factor(AOIMap3$NPK152020)
    levels(AOIMap3$NPK152020) <- kev
    
    #NPK 15:20:20
    observeEvent(ttnpk15(),
                 {
                   
                   output$npkplot_15 <- renderTmap({
                     
                     
                     
                     nm1 = tm_shape(AOIMap3) +
                       tm_polygons(
                         col = "NPK152020",
                         title = ttnpk15(),
                         palette = "Oranges",
                         id="ADM2_EN",
                         popup.vars=c("NPK 15:20:20 rate : "="NPK152020")
                       )+
                       tm_text(text = "ADM2_EN")
                     
                     nm1
                     
                   }) 
                 })
    
    
    
    ############################################################################   
    #Yield plot
    #############################################################################
    #reactive title based on unit of land
    
    ttha <- reactive({
      
      if(unit == "hectare"){
        
        ttha <- paste("Recommended Yield response (t/hectare)")
      }else {
        
        ttha <- paste("Recommended Yield response (t/acre)")
        
      }
    })
    
    
    Ysclae <- unique(AOIMap3$dY)
    keY <- as.factor(Ysclae[order(Ysclae)])
    AOIMap3$dY <- factor(AOIMap3$dY)
    levels(AOIMap3$dY) <- keY
    
    #Yield plot
    observeEvent(ttha(),
                 {
                   
                   
                   output$yieldplot <- renderTmap({
                     
                     
                     sm3 = tm_shape(AOIMap3) +
                       tm_polygons(
                         col = "dY",
                         title = ttha(),
                         #breaks = c(3, 4, 5, 6),
                         #labels = c("Low", "Medium", "High"),
                         palette = "Blues",
                         id="ADM2_EN",
                         popup.vars=c("Yield increase : "="dY")
                       )+
                       tm_text(text = "ADM2_EN")
                     
                     sm3
                     
                   })
                 })
    
    #-------------------------------
    #Generate downloadable maps
    #-------------------------------
    print(plantMonth)
    #generate color pallette
    if(unit == "hectare"){
      ureacols <- c("0" = "#FFFFFF", "25"= "#E5F5E0", "50"= "#C7E9C0", "75"= "#A1D99B", "100"= "#74C476",
                    "125"= "#41AB5D", "150"= "#238B45", "175"="#006D2C", "200"= "#00441B")
      tgh <- "Urea (kg/hectare)"
    }else {
      ureacols <- c("0" = "#FFFFFF", "10"= "#E5F5E0", "20"= "#C7E9C0", "30"= "#A1D99B", "40"= "#74C476",
                    "50"= "#41AB5D", "60"= "#238B45", "70"="#006D2C", "80"= "#00441B")
      tgh <- "Urea (kg/acre)"
    }
    ureasclae <- unique(AOIMap3$Urea)
    keU <- as.character(ureasclae[order(ureasclae)])
    AOIMap3$Urea <- factor(AOIMap3$Urea)
    levels(AOIMap3$Urea) <- keU
    
    require(ggrepel)
    
    #ggplot urea
    
    ggUrea <- ggplot(AOIMap3) +
      geom_sf(aes(fill=Urea), col="darkgrey") +
      scale_fill_manual(values = ureacols, guide = guide_legend(reverse=TRUE))+
      geom_path(data=crop_ghRegions, aes(x=long, y=lat, group=group), size=0.5) +
      geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
      geom_text_repel(data=LGAnames, aes(long, lat, label=DISTRICT, fontface=1, angle=textangle), size=4, segment.size = NA) + 
      geom_text(data=RegionsLabel, aes(lon, lat, label=Regions, fontface=2), col='black', size=6)+
      #geom_point(data=cities, aes(x=lon, y=lat), shape=16,  size=3) +
      annotation_scale(location = "bl", width_hint = 0.3, line_width = 0.4) +
      annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                             style = north_arrow_fancy_orienteering) +
      xlab("") + ylab("") +
      ggtitle(tgh) +
      theme_bw() +
      theme(legend.position="right", legend.title=element_blank(),
            plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
            axis.text = element_text(size=8))  
    
    # 
    # 
    
    
    ### NPK15:20:20 palette <- brewer.pal(9,"YlOrBr")
    if(unit == "hectare"){
      NPKcols15 <- c("0"="#FFFFFF","50"= "#FFF7BC", "100"= "#FEE391", "150"= "#FEC44F", "200"= "#FE9929", 
                     "250"= "#EC7014", "300"= "#CC4C02","350" = "#993404", "400"= "#662506")
      tgh15 <- "NPK 15:20:20 (kg/hectare)"
    }else{
      NPKcols15 <- c("40"="#FFFFFF","50"= "#FFF7BC", "60"= "#FEE391", "70"= "#FEC44F", "80"= "#FE9929", 
                     "90"= "#EC7014", "100"= "#CC4C02", "110" ="#993404","120" = "#662506")
      tgh15 <- "NPK 15:20:20 (kg/acre)"
    }
    
    print(Selection3)
    npk15 <- unique(AOIMap3$NPK152020)
    kev <- as.character(npk15[order(npk15)])
    AOIMap3$NPK152020<- factor(AOIMap3$NPK152020)
    levels(AOIMap3$NPK152020) <- kev
    
    #ggplot NPK
    ggNPK15 <- ggplot(AOIMap3) +
      geom_sf(aes(fill=NPK152020), col="darkgrey") +
      
      scale_fill_manual(values = NPKcols15, guide = guide_legend(reverse=TRUE))+
      geom_path(data=crop_ghRegions, aes(x=long, y=lat, group=group), size=0.5) +
      geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
      geom_text_repel(data=LGAnames, aes(long, lat, label=DISTRICT, fontface=1, angle=textangle), size=2, segment.size = NA) + 
      xlab("") + ylab("") +
      ggtitle(tgh15) +
      theme_bw() +
      theme(legend.position="right", legend.title=element_blank(),
            plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
            axis.text = element_text(size=8))
    
    ### NPK12:30:17 palette <- brewer.pal(9,"YlOrBr")
    if(unit == "hectare"){
      NPKcols12 <- c("0"="#FFFFFF","25"= "#C7E9B4", "50"= "#7FCDBB", "75"= "#41B6C4",
                     "100"= "#1D91C0", "125"= "#225EA8", "150"= "#253494", "175"= "#081D58")
      tgh12 <- "NPK 12:30:17 (kg/hectare)"
    }else{
      NPKcols12 <- c("0"="#FFFFFF","10"= "#C7E9B4", "20"= "#7FCDBB", "30"= "#41B6C4",
                     "40"= "#1D91C0", "50"= "#225EA8", "60"= "#253494", "70"= "#081D58")
      tgh12 <- "NPK 12:30:17 (kg/acre)"
    }
    
    
    npk12 <- unique(AOIMap3$NPK123017)
    kev <- as.character(npk12[order(npk12)])
    AOIMap3$NPK123017  <- factor(AOIMap3$NPK123017)
    levels(AOIMap3$NPK123017 ) <- kev
    
    #ggplot NPK
    ggNPK12 <- ggplot(AOIMap3) +
      geom_sf(aes(fill=NPK123017), col="darkgrey") +
      
      scale_fill_manual(values = NPKcols12, guide = guide_legend(reverse=TRUE))+
      geom_path(data=crop_ghRegions, aes(x=long, y=lat, group=group), size=0.5) +
      geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
      geom_text_repel(data=LGAnames, aes(long, lat, label=DISTRICT, fontface=1, angle=textangle), size=2, segment.size = NA) +
      # geom_point(data=cities, aes(x=lon, y=lat), shape=16,  size=3) +
      xlab("") + ylab("") +
      ggtitle(tgh12) +
      theme_bw() +
      theme(legend.position="right", legend.title=element_blank(),
            plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
            axis.text = element_text(size=8))
    
    #generate color pallette # brewer.pal(9,"heat")
    
    
    if(unit == "hectare"){
      Ydcols <- c( "34"= "#E8FF00FF", "32"="#A2FF00FF", "30"= "#5DFF00FF", "28"= "#17FF00FF", "26"= "#00FF2EFF", "24"= "#00FF74FF",
                   "22"="#00FFB9FF", "20"= "#00FFFFFF", "18"= "#00B9FFFF", "16"= "#0074FFFF", "14"= "#002EFFFF",
                   "12"="#1700FFFF", "10"= "#5D00FFFF", "8"= "#A200FFFF", "6"= "#E800FFFF", "4"= "#FF00D1FF",
                   "2"= "#FF008BFF", "0"= "#FFFFFF")
      tgy <- "Yield increase (t/hectare)"
    }else{
      Ydcols <- c("14"= "#17FF00FF", "13"= "#00FF2EFF", "12"= "#00FF74FF",
                  "11"="#00FFB9FF", "10"= "#00FFFFFF", "9"= "#00B9FFFF", "8"= "#0074FFFF", "7"= "#002EFFFF",
                  "6"="#1700FFFF", "5"= "#5D00FFFF", "4"= "#A200FFFF", "3"= "#E800FFFF", "2"= "#FF00D1FF",
                  "1"= "#FF008BFF", "0"= "#FFFFFF")
      tgy <- "Yield increase (t/acre)"
    }
    
    Ysclae <- unique(AOIMap3$dY)
    keY <- as.factor(Ysclae[order(Ysclae)])
    AOIMap3$dY <- factor(AOIMap3$dY)
    levels(AOIMap3$dY) <- keY
    
    #Yield ggplot
    ggYield <- ggplot(AOIMap3) +
      geom_sf(aes(fill=dY), col="darkgrey") +
      
      scale_fill_manual(values = Ydcols, guide = guide_legend(reverse=TRUE))+
      geom_path(data=crop_ghRegions, aes(x=long, y=lat, group=group), size=0.5) +
      geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
      geom_text_repel(data=LGAnames, aes(long, lat, label=DISTRICT, fontface=1, angle=textangle), size=4, segment.size = NA) +
      xlab("") + ylab("") +
      ggtitle(tgy) +
      theme_bw() +
      theme(legend.position="right", legend.title=element_blank(),
            plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
            axis.text = element_text(size=8))
    
    #Combine plots together in pdf
    fileName <- paste("maps", ".pdf", sep="")
    pdf(fileName, onefile = TRUE, height = 14, width=12)
    #pdf.options(paper = "a4")
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(3, 2, heights = unit(c(0.8, 5, 5, 0.8), "null"))))
    grid.text(paste("Planting in", plantMonth, "at", yield_level, sep=" "), vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
    print(ggUrea, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
    # print(ggNPK11, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
    print(ggNPK15, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
    # print(ggNPK25, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
    print(ggNPK12, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
    
    print(ggYield, vp = viewport(layout.pos.row = 3, layout.pos.col = 2))
    dev.off()
    
    
    # #-------------------------------------------------------------------------
    # #front page dynamic tmap
    # #-------------------------------------------------------------------------
    # 
    
    
    
    #reactive selection of variable to view
    filt_select <- reactive({
      print(Selection3)
      if (Selection3 == "Urea rate"){
        filt_select <- "Urea rate"
      }else if (Selection3 == "Expected yield response"){
        filt_select <- "Expected yield response"
      }else if (Selection3 == "NPK 15:20:20 rate"){
        filt_select <- "NPK 15:20:20 rate"
      }else if (Selection3 == "NPK 12:30:17 rate"){
        filt_select <- "NPK 12:30:17 rate"
        
      }
      
    })
    
    #show map based on selection of variable but retaining single name
    
    #filter by variable selected and unit for color pallette
    observeEvent(filt_select(), {
      if (filt_select() == "Urea rate"){
        
        ureacols <- reactive({
          
          if(unit == "hectare"){
            ureacols <- c("#FFFFFF", "#E5F5E0",  "#C7E9C0",  "#A1D99B",  "#74C476",
                          "#41AB5D",  "#238B45", "#006D2C",  "#00441B")
            tturea <- "Urea (kg/hectare)"
          }else {
            ureacols <- c("#FFFFFF", "#E5F5E0",  "#C7E9C0",  "#A1D99B",  "#74C476",
                          "#41AB5D",  "#238B45", "#006D2C",  "#00441B")
            tturea <- "Urea (kg/acre)"
          }
        })
        
        #reactive legend title
        tturea <- reactive({
          
          if(unit == "hectare"){
            
            tturea <- paste("Recommended urea rate(kg/hectare)")
          }else {
            
            tturea <- paste("Recommended urea rate (kg/acre)")
          }
        })
        
        
        
        ureasclae <- unique(AOIMap3$Urea)
        keU <- as.character(ureasclae[order(ureasclae)])
        AOIMap3$Urea <- factor(AOIMap3$Urea)
        levels(AOIMap3$Urea) <- keU
        
        require(ggrepel)
        library(tmap)
        
        print(country)
        
        #Urea
        observeEvent(tturea(),
                     {
                       
                       output$tmapplot <- renderTmap({
                         
                         
                         sm1 = tm_shape(AOIMap3) +
                           tm_polygons(
                             col = "Urea",
                             title = tturea(),
                             palette = "Greens",
                             id="ADM2_EN",
                             popup.vars=c("Urea rate : "="Urea")
                             
                           )+
                           tm_text(text = "ADM2_EN")
                         sm1
                         
                         
                         
                       })
                     })
      }else if(filt_select() == "NPK 12:30:17 rate"){
        ttnpk12 <- reactive({
          
          if(unit == "hectare"){
            
            ttnpk12 <- paste("Recommended NPK 12:30:17 rate (kg/hectare)")
          }else {
            
            ttnpk12 <- paste("Recommended NPK 12:30:17 rate (kg/acre)")
          }
        })
        
        
        
        npk12 <- unique(AOIMap3$NPK123017 )
        kev <- as.character(npk12[order(npk12)])
        AOIMap3$NPK123017  <- factor(AOIMap3$NPK123017 )
        levels(AOIMap3$NPK123017 ) <- kev
        
        #npk plot
        observeEvent(ttnpk12(),
                     {
                       
                       output$tmapplot <- renderTmap({
                         sm4 = tm_shape(AOIMap3) +
                           tm_polygons(
                             col = "NPK123017",
                             title = ttnpk12(),
                             palette = "YlGnBu",
                             id="ADM2_EN",
                             popup.vars=c("NPK 12:30:17 rate : "="NPK123017")
                           )+
                           tm_text(text = "ADM2_EN")
                         #tm_text(text = "ADM2_EN")
                         
                         sm4
                         
                       }) 
                     })
      }else if(filt_select() == "NPK 15:20:20 rate"){
        ttnpk15 <- reactive({
          
          if(unit == "hectare"){
            
            ttnpk15 <- paste("Recommended NPK 15:20:20 rate (kg/hectare)")
          }else {
            
            ttnpk15 <- paste("Recommended NPK 15:20:20 rate(kg/acre)")
          }
        })
        
        
        npk15 <- unique(AOIMap3$NPK152020)
        kev <- as.character(npk15[order(npk15)])
        AOIMap3$NPK152020<- factor(AOIMap3$NPK152020)
        levels(AOIMap3$NPK152020) <- kev
        
        #npk plot
        observeEvent(ttnpk15(),
                     {
                       
                       output$tmapplot <- renderTmap({
                         sm6 = tm_shape(AOIMap3) +
                           tm_polygons(
                             col = "NPK152020",
                             title = ttnpk15(),
                             palette = "Oranges",
                             id="ADM2_EN",
                             popup.vars=c("NPK 15:20:20 rate : "="NPK152020"))+
                           tm_text(text = "ADM2_EN")
                         
                         
                         sm6
                         
                       }) 
                     })
        
        
        
      }else if(filt_select() == "Expected yield response"){
        ttha <- reactive({
          
          if(unit == "hectare"){
            
            ttha <- paste("Recommended yield increase (t/hectare)")
          }else {
            
            ttha <- paste("Recommended yield increase (t/acre)")
            
          }
        })
        
        
        Ysclae <- unique(AOIMap3$dY)
        keY <- as.factor(Ysclae[order(Ysclae)])
        AOIMap3$dY <- factor(AOIMap3$dY)
        levels(AOIMap3$dY) <- keY
        
        observeEvent(ttha(),
                     {
                       
                       
                       output$tmapplot <- renderTmap({
                         sm3 = tm_shape(AOIMap3) +
                           tm_polygons(
                             col = "dY",
                             title = ttha(),
                             palette = "Blues",
                             id="ADM2_EN",
                             popup.vars=c("Yield increase : "="dY"))+
                           tm_text(text = "ADM2_EN")
                         
                         sm3
                         
                       })
                     })
        
        
        
      }
      
      
    })
    
    
    
    print(Selection3)
    if ( input$use == "Fertilizer Recommendation" & unit == "acre"){
      
      #download acre printable guides
      output$downloadDatafr <- 
        
        downloadHandler(
          filename <- function() {
            paste("FR Printable guides (acre)",  ".pdf", sep="")
          },
          
          content <- function(file) {
            file.copy("data/Tailored fertilizer application recommendations for cassava - Tanzania Acre latest.pdf", file)
          },
          contentType = "application/pdf"
        )
      
    }else if(input$use == "Fertilizer Recommendation" & unit == "hectare"){
      #download hectare printable guides
      output$downloadDatafr <- 
        downloadHandler(
          filename <- function() {
            paste("FR Printable guides (hectare)",  ".pdf", sep="")
          },
          
          content <- function(file) {
            file.copy("data/Tailored fertilizer application recommendations for cassava - Tanzania Hectare latest.pdf", file)
          },
          contentType = "application/pdf"
        ) 
      
      
    }
    
    
    
    output$sidetext <- renderText({
      
      
      # paste0('<span style=\"background-color:', "color", '\ ">',text,' #<span style=\"font-size:8px;font-weight:bold;background-color:white;">',"ent_type",'</span></span>')
      paste("Maps and tables below present fertilizer recommendations for cassava planted in", plantMonth, "in", lgaGroups2, "in a field with", yield_level,
            ". Recommendations are optimized to obtain a maximal return on investment, assuming cassava will be harvested after 12 months.
              ")
      
    })
    
       #download maps
      output$downloadData <- downloadHandler(
        filename <- function() {
          paste(plantMonth, "_", paste(lgaGroups, collapse="_"), ".pdf", sep="")
        },
        
        content <- function(file) {
          file.copy("maps.pdf", file)
        },
        contentType = "application/pdf"
      )
      
      #download tables
      output$downloadcsv <- downloadHandler(
        filename <- function() {
          paste(plantMonth, "_", paste(lgaGroups, collapse="_"), ".csv", sep="")
        },
        
        content <- function(file) {
          file.copy("tables.csv", file)
        },
        contentType = "application/csv"
      )
      
      
  
      })
          
          
        }
  })
    
  observeEvent(input$ok, {
    if(!is.null(input$ok)){
      removeModal()
    }
  })
  
    observeEvent(input$ok, {
    if(input$use == "Scheduled Planting"){
      
      #setwd("C:/Users/User/Documents/ACAI/DASHBOARDS/paper based/PaperbasedDashboard_RW_copyv01")
 
      
      GH_SP <- readRDS("Ghana_SP_annexData.RDS")
      PHdate <- Planting_HarvestDate(country = "GH")
      CY_WLY_coord <- merge(GH_SP, PHdate[, c("st","weekNr")], by.x="plantingDate", by.y="st")
      #head(CY_WLY_coord)

                removeModal()
      #test parameters     
          # countrySP <- "GH"
          # plantMonthSP <- "February"
          # 
          # reg_ghanaSP <- "Ahafo"
          # 
          # unitSP <- "acre"
        observeEvent(input$btn_SP, {
           
                plantMonthSP <- input$plntmthSP
                reg_ghanaSP <- input$reg_ghanaSP
                unitSP <- input$unit_locSP
                print(reg_ghanaSP)
                FCYName <- "FCY_1"
              
                country <- "GH"
        
               #Filter by REGION
             CY_WLY_coord2 <- CY_WLY_coord[CY_WLY_coord$NAME_1 == reg_ghanaSP, ]
             
             #' take the mean of WLY and CY from FCY1 by LGA/District within STATE/REGION and planting month and monthly harvest 8 - 15 MAP 
             #' @param ds : output of function addCoord
             CY_wly_average <- as.data.frame(Agg_plantingMonth_SP(ds=CY_WLY_coord2, country="GH", adminName=reg_ghanaSP, FCYName="FCY_1", unit=unitSP))
 
             SP_plDate <- CY_wly_average[CY_wly_average$plm == plantMonthSP, ]
             SP_plDate <- SP_plDate[order(SP_plDate$NAME_1, SP_plDate$NAME_2),]
             fname <- paste(reg_ghanaSP, "_SP_paperBased_ton_", plantMonthSP, ".csv",sep="")
             SP_plDate <- SP_plDate[order(SP_plDate$NAME_1, SP_plDate$NAME_2), ]
        
            
             SP_plDate2 <- SP_plDate[, c("NAME_1", "NAME_2", "Harvest_8","Harvest_9","Harvest_10", "Harvest_11",
                                       "Harvest_12", "Harvest_13", "Harvest_14","Harvest_15","plm" )]
             SP_plDate3 <- SP_plDate2[order(SP_plDate2$NAME_1, SP_plDate2$NAME_2), ]
           
             if(plantMonthSP == "January"){
              colnames(SP_plDate3) <- c("REGION", "DISTRICT", month.name[c(9:12, 1:4)], "plantingMonth")
            }else if(plantMonthSP == "February"){
              colnames(SP_plDate3) <- c("REGION", "DISTRICT", month.name[c(10:12, 1:5)], "plantingMonth")
            }else if(plantMonthSP == "March"){
              colnames(SP_plDate3) <- c("REGION", "DISTRICT", month.name[c(11:12, 1:6)], "plantingMonth")
            }else if(plantMonthSP == "April"){
              colnames(SP_plDate3) <- c("REGION", "DISTRICT", month.name[c(12, 1:7)], "plantingMonth")
            }else if(plantMonthSP == "May"){
              colnames(SP_plDate3) <- c("REGION", "DISTRICT", month.name[c(1:8)], "plantingMonth")
            }else if(plantMonthSP == "June"){
              colnames(SP_plDate3) <- c("REGION", "DISTRICT", month.name[c(2:9)], "plantingMonth")
            }else if(plantMonthSP == "July"){
              colnames(SP_plDate3) <- c("REGION", "DISTRICT", month.name[c(3:10)], "plantingMonth")
            }else if(plantMonthSP == "August"){
              colnames(SP_plDate3) <- c("REGION", "DISTRICT", month.name[c(4:11)], "plantingMonth")
            }else if(plantMonthSP == "September"){
              colnames(SP_plDate3) <- c("REGION", "DISTRICT", month.name[c(5:12)], "plantingMonth")
            }else if(plantMonthSP == "October"){
              colnames(SP_plDate3) <- c("REGION", "DISTRICT", month.name[c(6:12, 1)], "plantingMonth")
            }else if(plantMonthSP == "November"){
              colnames(SP_plDate3) <- c("REGION", "DISTRICT", month.name[c(7:12, 1:2)], "plantingMonth")
            }else if(plantMonthSP == "December"){
              colnames(SP_plDate3) <- c("REGION", "DISTRICT", month.name[c(8:12, 1:3)], "plantingMonth")
            }
            
            
              SP_plDate3 <- subset(SP_plDate3, select=-c(plantingMonth))
              cols <- names(SP_plDate3)[3:10]
            
              SP_plDate4 <- SP_plDate3 %>% mutate_at(vars(cols), funs(round(., 1)))
 
              #write.csv(SP_plDate4, fname, row.names = FALSE)
              
            #Render table in reactive envi
            output$tabletext_sp <- renderText({
              
              
              paste("Expected yield in tonnes per", unitSP, "when you plant in", plantMonthSP, "and harvest in ...", sep=" ")
              
            })

            Sys.sleep(2)
            output$SPtable <- renderDT({SP_plDate4},
                                       rownames = FALSE, 
                                       extensions = c('Buttons','FixedColumns'), 
                                       options = list(dom = 'Bfrtip',
                                                      pageLength = nrow(SP_plDate4),
                                                      initComplete = DT::JS(
                                                        "function(settings, json) {",
                                                        "$(this.api().table().header()).css({'background-color': 'black', 'color': '#fff'});",
                                                        "}"),
                                                      
                                                      buttons = list(
                                                        list(extend = 'excel', 
                                                             filename = paste('AKILIMO SP advice', '_', reg_ghanaSP, '_', plantMonthSP),
                                                             title = paste("Expected yield in t/acre when you plant in", plantMonthSP, "and harvest in ...", sep=" ")),
                                                        list(extend = 'pdf',
                                                             filename = paste('AKILIMO SP advice', '_', reg_ghanaSP, '_', plantMonthSP),
                                                             title = paste("Expected yield in t/acre when you plant in", plantMonthSP, "and harvest in ...", sep=" "),
                                                             header = TRUE)
                                                      )
                                                      
                                       )
            ) 
            
           
            #Downloadbale guides 
            if (input$use == "Scheduled Planting" & input$unit_locSP == "acre"){
              #download acre printable guides
              output$downloadDatasp <- downloadHandler(
                filename <- function() {
                  paste("SP Printable guides (acre)",  ".pdf", sep="")
                },
                
                content <- function(file) {
                  file.copy("data/Scheduled Planting and Harvest Cassava - Nigeria Acre latest.pdf", file)
                },
                contentType = "application/pdf"
              )
            }else if(input$use == "Scheduled Planting" & input$unit_locSP == "hectare"){
              
              #download hectare printable guides
              output$downloadDatasp <- downloadHandler(
                filename <- function() {
                  paste("SP Printable guides (hectare)",  ".pdf", sep="")
                },
                
                content <- function(file) {
                  file.copy("data/Scheduled Planting and Harvest Cassava - Nigeria Hectare latest.pdf", file)
                },
                contentType = "application/pdf"
              )  
              
            } 
          
          

      
            
          })
          
       #    observeEvent(input$btn_SP, {
       #     
       # 
       #      
       # })
    }
  })
      
   
}



#runApp(shinyApp(ui, server), launch.browser = TRUE)
#shinyApp(ui, server)
#library(rsconnect)
#deployApp(account="vac-lshtm")
