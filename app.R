
header <- "v.25  2020-04-09"
library(ggplot2)
library(DT)
library(leaflet)
library(rgdal)
library(sp)
library(magrittr)
library(RColorBrewer)

######
iYears <- c(0:7)
sYears <-  as.character(2010 + 5 * iYears)
numRzones <- 494
rzoneHeader <- c("total","sf","mf","osf","omf","rsf","rmf")

scenarioList <- c(1916,1851)
descList <- list(
  "#1916 -- 2019 Distributed" = 1916,
  "#1851 -- UGM Decision" = 1851
)

tableList <- list(
  "Demand DU, selected year" = "demand",
  "Demand DU, growth from 2010" = "growth",
  "Price Index (Y2010 = 1)" = "sprice"
)

calcTypeList <- list(
  "Scenario A" = "selA",
  "Scenario B" = "selB",
  "Abs. Diff  (B-A) " = "selAbs",
  "% Rel Diff (B-A)/A" = "selRel"
)


scenario_tablenames <- c("res_du_demand_year","res_du_growth_year","res_supplyprice_year")

lyrRzones <- readOGR("shapes/rzones_simple_wgs84.shp",layer = "rzones_simple_wgs84",GDAL1_integer64_policy = TRUE)
lyrUGB <- readOGR("shapes/ugb_line_simple_wgs84.shp",layer = "ugb_line_simple_wgs84",GDAL1_integer64_policy = TRUE)
lyrRivers <- readOGR("shapes/rivers_simple_wgs84.shp",layer = "rivers_simple_wgs84",GDAL1_integer64_policy = TRUE)


######  Shiny metal app


ui <- 
  pageWithSidebar(
    
    headerPanel("MetroScope Viewer"),
    
    sidebarPanel(width = 3,
                 
                 selectInput("selScenarioA","Scenario A:",choices = descList,selected = 1),
                 selectInput("selScenarioB","Scenario B:",choices = descList,selected = 1),
                 sliderInput("selYear", "Model Year:",min = 2010, max = 2045, value = 2020, step=5, sep="", animate=F, ticks=T),
                 selectInput("selTable","Table:",choices = tableList ,size=4,selectize = F),
                 selectInput("selAttr","Data:",rzoneHeader, size=7,selectize = F),
                 radioButtons("selCalcType", label = "Map Dispay:", choices = calcTypeList),        
                 
                 helpText(header)
    ),
    mainPanel(
      #verbatimTextOutput("calctype"),
      leafletOutput("mymap", height = 500),
      DTOutput("table")
    )
  )

######
server <- function(input, output, session) {
  
  output$calctype <- renderPrint({ input$selCalcType })
  
  # Filter data based on selections
  loadScenarioTables <- function(){
    results_list <- c()
    for (tablename in scenario_tablenames){
      table_inA <- readRDS(file=paste0("scenario_data/scen",input$selScenarioA,"_",tablename,"X",".rds"))
      table_inB <- readRDS(file=paste0("scenario_data/scen",input$selScenarioB,"_",tablename,"X",".rds"))
      
      if (input$selCalcType == "selA"){
        table_in <- table_inA
      }
      if (input$selCalcType == "selB"){
        table_in <- table_inB
      }
      if (input$selCalcType == "selAbs"){
        table_in <- table_inA
        table_in[,4:10] <- table_inB[,4:10] - table_inA[,4:10]
      }
      if (input$selCalcType == "selRel"){
        table_in <- table_inA
        table_in[,4:10] <- 100 * (table_inB[,4:10] - table_inA[,4:10]) / table_inA[,4:10]
        table_in[is.na(table_in)] <- 0
      }
      
      
      results_list[[tablename]] <- table_in
    }  
    
    return(results_list)
  }
  
  setMapData <- function(){
    scenario_tables_in <- loadScenarioTables()
    
    if (input$selTable == "sprice"){
      data_in <- scenario_tables_in$res_supplyprice_year
      dig <- 3
    }
    if (input$selTable == "demand"){
      data_in <- scenario_tables_in$res_du_demand_year
      dig <- 0
    }
    if (input$selTable == "growth"){
      data_in <- scenario_tables_in$res_du_growth_year
      dig <- 0
    }
    
    data_out <- data_in[data_in$syear == input$selYear,3:10] 
    data_out[,2:8] <- round(data_out[2:8],digits = dig)
    #g_table_oneyear <- data_out
    return(data_out)
    
    
  }
  
  output$table <- renderDT(DT::datatable(
    options = list(lengthMenu = c(5, 10, 50),dom = 'itlp'),
    data= setMapData()
    
    
  ) # datatable()
  ) #renderDataTable()
  
  
  getLabels <- function(){
    myData <- setMapData()
    
    if(input$selTable == "demand" | input$selTable == "growth"){
      myLabel <- sprintf(
        "<strong>Rzone %.0f</strong>
    <br>Total: %.0f
    <br/>SF: &nbsp;&nbsp;&nbsp;%.0f
    <br/>MF: &nbsp;&nbsp;&nbsp;%.0f",
        myData$rzone,
        myData$total,
        myData$sf,
        myData$mf
      ) %>% lapply(htmltools::HTML)
    } 
    
    if(input$selTable == "sprice"){
      myLabel <- sprintf(
        "<strong>Rzone %.0f</strong>
    <br/>OSF: &nbsp;&nbsp;&nbsp;%.3f
    <br/>OMF: &nbsp;&nbsp;&nbsp;%.3f     
    <br/>RSF: &nbsp;&nbsp;&nbsp;%.3f
    <br/>RMF: &nbsp;&nbsp;&nbsp%.3f",     
        myData$rzone,
        myData$osf,
        myData$omf,
        myData$rsf,
        myData$rmf
      ) %>% lapply(htmltools::HTML)
    } 
    
    return(myLabel)
  }
  
  getMapParams <- function(){
      if (input$selCalcType == "selA" | input$selCalcType == "selB" ){      

        if (input$selTable == "demand"){
          Xmybins <- c(1, 100, 200, 500, 1000, 2000, 5000, 10000, 20000)
          Xtitle <- "Demand Units"
          Xcolors <- "YlOrRd"
          XmyLabels <- getLabels()
        }
        if (input$selTable == "growth"){
          Xmybins <- c(1, 100, 200, 500, 1000, 2000, 5000, 10000, 20000)
          Xtitle <- "Demand Units"
          Xcolors <- "YlOrBr"
          XmyLabels <- getLabels()
        }
        if (input$selTable == "sprice"){
          Xmybins <- c(0, 1.1, 1.2, 1.5, 1.7, 2, 3, 5, 10)
          Xtitle <- "Price Index"
          Xcolors <- "YlGn"
          XmyLabels <- getLabels()
        }
        
      } # sel AB
   
    if (input$selCalcType == "selAbs" ){      
      
      if (input$selTable == "demand"){
        Xmybins <- c(-20000, -5000, -2000, -1000, -500, -200, 200, 500, 1000, 2000, 5000, 20000)
        Xtitle <- "Demand Units"
        Xcolors <- "RdBu"
        XmyLabels <- getLabels()
      }
      if (input$selTable == "growth"){
        Xmybins <- c(-20000, -5000, -2000, -1000, -500, -200, 200, 500, 1000, 2000, 5000, 20000)
        Xtitle <- "Demand Units"
        Xcolors <- "RdBu"
        XmyLabels <- getLabels()
      }
      if (input$selTable == "sprice"){
        #Xmybins <- c(-Inf, -2, -0.5, -0.2, -0.1, -0.05, 0.05, 0.1, 0.2, 0.5, 2, Inf)
        Xmybins <- c(-Inf, -5, -2, -0.5, -0.2, -0.1,  0.1, 0.2, 0.5, 2, 5, Inf)
        Xtitle <- "Price Index"
        Xcolors <- "RdBu"
        XmyLabels <- getLabels()
      }
      
    } # sel Abs
    
    if (input$selCalcType == "selRel" ){      
      
      if (input$selTable == "demand"){
        Xmybins <- c(-Inf, -100, -50, -20, -10, -5, 5, 10, 20, 50, 100, Inf)
        Xtitle <- "% Difference"
        Xcolors <- "RdBu"
        XmyLabels <- getLabels()
      }
      if (input$selTable == "growth"){
        Xmybins <- c(-Inf, -100, -50, -20, -10, -5, 5, 10, 20, 50, 100, Inf)
        Xtitle <- "% Difference"
        Xcolors <- "RdBu"
        XmyLabels <- getLabels()
      }
      if (input$selTable == "sprice"){
        Xmybins <- c(-Inf, -100, -50, -20, -10, -5, 5, 10, 20, 50, 100, Inf)
        Xtitle <- "% Difference"
        Xcolors <- "RdBu"
        XmyLabels <- getLabels()
      }
      
    } # sel Abs
    
    if (input$selCalcType == "selA" | input$selCalcType == "selB"){
       XisReversed <- F
       Xboxes <- 8
     }
    if ( input$selCalcType == "selAbs" | input$selCalcType == "selRel" ){
       XisReversed <- T
       Xboxes <- 11
    }
    
    outList <- list(Xmybins=Xmybins,Xtitle=Xtitle,Xcolors=Xcolors,XmyLabels=XmyLabels,
                    XisReversed=XisReversed,Xboxes=Xboxes)
    return(outList)
    
  }
  
  
  output$mymap <- renderLeaflet({

    params <- getMapParams()
    
    Xmybins <- params$Xmybins
    Xtitle <- params$Xtitle
    Xcolors <- params$Xcolors
    XmyLabels <- params$XmyLabels
    XisReversed <- params$XisReversed
    Xboxes <- params$Xboxes
 
    pallet <- brewer.pal(name=Xcolors,n=Xboxes)
    if ( input$selCalcType == "selAbs" | input$selCalcType == "selRel" ){
      pallet[6] <- "#FCFCFC"
    }
    Xpallet <- colorBin( palette=pallet, domain=setMapData()[,input$selAttr], 
                           na.color="White", bins=Xmybins, reverse = XisReversed)

    
    data_input <- merge(lyrRzones, setMapData(), by.x = "rzone10", by.y = "rzone")
    data_input@data$map <- data_input@data[,input$selAttr]
    
    leaflet(data_input, options = leafletOptions(minZoom = 8, maxZoom = 13, )) %>%
      setView(-122.7,45.48, zoom = 10) %>% 
      
      addPolygons(color = "#444444", 
                  weight = 0.2, 
                  smoothFactor = 0.5,
                  opacity = 1.0, 
                  fillOpacity = 1.0,
                  fillColor = ~Xpallet(map),
                  label = XmyLabels,
                  highlightOptions = highlightOptions(color = "cyan", weight = 2, bringToFront = TRUE)) %>% 
      
      addPolygons(data= lyrRivers, color = "lightblue",
                  weight = 0.5,
                  opacity = 0.5,
                  fillOpacity = 0.4,
                  fillColor = "lightblue") %>%

    addPolylines(data= lyrUGB, color = "black", weight = 1.5) %>%
    
    addLegend("topright",
              pal = Xpallet,
              values = ~map,
              title = Xtitle,
              labFormat = labelFormat(
                prefix = "", suffix = "", between = " to ", big.mark = ","
              ), 
              opacity = 1) 
  })
  
  
} # server()

# Run the application 
shinyApp(ui = ui, server = server)
