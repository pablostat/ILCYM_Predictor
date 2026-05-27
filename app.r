library(shinydashboard)
library(shiny)
library(raster)
library(RColorBrewer)
library(leaflet)
library(sp)
library(htmltools)
library(plotly)
library(dplyr)
library(nasapower)
library(geodata)
library(rvest)
library(httr)
library(jsonlite)
library(markdown)

ui <- dashboardPage(
  dashboardHeader(title = "ILCYM predictor"), 
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "introd", icon = icon("building")),
      menuItem("Risk Prediction", tabName = "stats", icon = icon("map"),selected=TRUE)
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "introd",
              fluidRow(
                column(12,
                       box(width = 10,
                           includeMarkdown("Introduction.md"),
                           tags$img(src="images/Intro.png",height = 400, width = 600,alt="something went wrong",deleteFile=FALSE)
                       )
                )
              )
      ),
      tabItem(tabName = "stats",
              uiOutput("bodyStats")
      )
    )
  )
)


server <- function(input, output, session) {

  options(shiny.maxRequestSize = 1000*1024^2)# 1000MB
  
  ###############################  
  # Prediction Map
  output$bodyStats <- renderUI({
    borr<-c("potato","eggplant","bell pepper","Cape gooseberry","aubergine","sugar beet")
    names(borr)<-c("Pt","Ep","Bp","Cg","Ag","Sb")
       fluidRow(
        column(12,
               box(width = 3,status = "info", solidHeader = TRUE,
                   selectInput("OpcionMap", h4("Select an option"), choices = c("Add Points"="AddPoint","Load Data"="AddData")),
                   conditionalPanel(
                     'input.OpcionMap == "AddData"',
                       fileInput('DatPresence', h4('Load presence coordinates'), accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'), multiple = TRUE),
                   ),
                   conditionalPanel(
                     'input.OpcionMap == "AddPoint"',
                     h6("*First you must locate and click on the map"),
                     actionButton("use_clik_loc", "Add point",icon=icon("map-marker")) #id=DatPoint
                   ),
                   selectInput("OpcionSource", h4("Data source"), choices = c("Nasa power"="NP","Worldclim (all 2023)"="WC")),
                   selectInput("pest", h4("Main insect list:"),choices = c("P. operculella","B. cockerelli","T. vaporariorum","S. tangolias","T. absoluta",
                                                                    "T. solanivora","C. koehleri","T. triozae")),
                   uiOutput("choicesMOD1"),
                   selectInput("pest2", h4("Optional insect list:"),choices = c("T. solanivora","P. operculella","T. vaporariorum","S. tangolias","T. absoluta",
                                                                                "B. cockerelli","C. koehleri","T. triozae")),
                   uiOutput("choicesMOD2"),
                   br()
               ),
               column(8,tabsetPanel(id="plot_tabs",
                                     tabPanel("Graphics", 
                                          div(class="outer",
                                              leafletOutput("geosimmap0", width = 900, height = 450)
                                          ),
                                          box(width = NULL, solidHeader = TRUE,
                                              h4("   Life Parameters"),plotlyOutput("tabindex", width="95%", height="300px")
                                          )
                                     )
                        )
               )
        )
      )
  })

  output$choicesMOD1 <- renderUI({
    EstSelected<-input$pest
    Modul1<-switch(EstSelected,
                   "B. cockerelli" = "images/BactericeraC.png",
                   "P. operculella" = "images/PhthorimaeaO.png",
                   "T. vaporariorum" = "images/TrialeurodesV.png",
                   "S. tangolias" = "images/SymmetrischemaT.png",
                   "T. absoluta" = "images/TutaA.png",
                   "T. solanivora" = "images/TeciaS.png",
                   "C. koehleri" = "images/CopidosomaK.png",
                   "B. tabaci" = "images/BemisiaT.png",
                   "T. triozae" = "images/TamarixiaT.png"
    )
    print(tags$img(src=Modul1,height="140",width="160"))

  })
  
  output$choicesMOD2 <- renderUI({
    EstSelected<-input$pest2
    Modul1<-switch(EstSelected,
                   "B. cockerelli" = "images/BactericeraC.png",
                   "P. operculella" = "images/PhthorimaeaO.png",
                   "T. vaporariorum" = "images/TrialeurodesV.png",
                   "S. tangolias" = "images/SymmetrischemaT.png",
                   "T. absoluta" = "images/TutaA.png",
                   "T. solanivora" = "images/TeciaS.png",
                   "C. koehleri" = "images/CopidosomaK.png",
                   "B. tabaci" = "images/BemisiaT.png",
                   "T. triozae" = "images/TamarixiaT.png"
    )
    print(tags$img(src=Modul1,height="140",width="160"))
    
  })
  
  observeEvent(input$OpcionMap,{
    if(input$OpcionMap == "AddPoint"){
      clicks <<- data.frame(lat = numeric(), lng = numeric(), .nonce = numeric())
      
      output$geosimmap0 <- renderLeaflet({
        leaflet() %>% setView(lng = 31.88, lat = -25.02, zoom=1) %>% addProviderTiles(providers$Esri.WorldStreetMap)
      })
    }else{
        output$geosimmap0 <- renderLeaflet({
          if (is.null(input$DatPresence)){
            return()
          }else{
            CoordsPath<-input$DatPresence
            suppressWarnings(CoordsI<-read.table(CoordsPath$datapath[1]))
            colnames(CoordsI)<-c("lon","lat","id")
            
            leaflet() %>% setView(lng = 31.88, lat = -25.02, zoom=1) %>% addCircleMarkers(data=CoordsI, ~lon , ~lat, layerId=~id, popup=~id, radius=8 , color="black",  fillColor="red", stroke = TRUE, fillOpacity = 0.8) %>% addProviderTiles(providers$Esri.WorldStreetMap)
          }
        })
    }
  })

  observeEvent(input$use_clik_loc, {
    last_click <- isolate(as.data.frame(input$geosimmap0_click))
    
    leafletProxy("geosimmap0") %>%
      clearShapes() %>% # removeMarker
      addMarkers(last_click$lng[1],last_click$lat[1]  ) # Lon y Lat
    
    clicks <<- clicks |>
      bind_rows(last_click)
  })
  
  ##########################################
  # Executor of the prediction by coordinate
  observeEvent(input$geosimmap0_marker_click,{
    style <- isolate(input$style)
    withProgress(message = 'Uploading the climate dataset', style = style, value = 0.1, {
      data_of_click <- input$geosimmap0_marker_click

      source('lib/geo_sim_M.r')
      if(is.null(data_of_click$id)){data_of_click$id<-"loc"}
      CoordsI<-data.frame(lon=data_of_click$lng[1],lat=data_of_click$lat[1],id=data_of_click$id[1])
      n0<-nrow(CoordsI)
      
      EstSelected<-input$pest;EstSelected2<-input$pest2
      Modul1<-switch(EstSelected,
                     "B. tabaci" = "BemisiaTabaci-AFT-2019/PhenologySims.RData",
                     "B. cockerelli" = "BactericeraCokerelli-AFT-2024/PhenologySims.RData",
                     "P. operculella" = "PhthorimaeaOperculella-AFT-2022/PhenologySims.RData",
                     "T. vaporariorum" = "TrialeurodesVapariorum-AFT-2024/PhenologySims.RData",
                     "S. tangolias" = "SymmestrichemaTangolias-AFT-2022/PhenologySims.RData",
                     "T. absoluta" = "Tuta absoluta-AFT-2022/PhenologySims.RData",
                     "T. solanivora" = "TeciaSolanivora-AFT-2022/PhenologySims.RData",
                     "C. koehleri"="CopidosomaKoehleri-AFT-2019/PhenologySims.RData",
                     "T. triozae"="TamarixiaTriozae-AFT-2023/PhenologySims.RData"
      )
      Modul2<-switch(EstSelected2,
                     "B. tabaci" = "BemisiaTabaci-AFT-2019/PhenologySims.RData",
                     "B. cockerelli" = "BactericeraCokerelli-AFT-2024/PhenologySims.RData",
                     "P. operculella" = "PhthorimaeaOperculella-AFT-2022/PhenologySims.RData",
                     "T. vaporariorum" = "TrialeurodesVapariorum-AFT-2024/PhenologySims.RData",
                     "S. tangolias" = "SymmestrichemaTangolias-AFT-2022/PhenologySims.RData",
                     "T. absoluta" = "Tuta absoluta-AFT-2022/PhenologySims.RData",
                     "T. solanivora" = "TeciaSolanivora-AFT-2022/PhenologySims.RData",
                     "C. koehleri"="CopidosomaKoehleri-AFT-2019/PhenologySims.RData",
                     "T. triozae"="TamarixiaTriozae-AFT-2023/PhenologySims.RData"
      )
      incProgress(0.3,message = "recognizing the phenologies")
      load(Modul1)
      modelim1<-params$modelim
      modelm1<-params$modelm
      estadios1<-params$estadios
      hfeno1<-params$hfeno
      xi1<-params$xi
      steps<-4
      modelim1=c(modelim1,modelm1)
      params1<-params
      
      ID<-1
      
      load(Modul2)
      modelim2<-params$modelim
      modelm2<-params$modelm
      estadios2<-params$estadios
      hfeno2<-params$hfeno
      xi2<-params$xi
      modelim2=c(modelim2,modelm2)
      params2<-params
      
      incProgress(0.5,message = "simulating life parameters")
      if(input$OpcionSource=="NP"){
        
        TempsVec <- get_power(community = "AG",lonlat = c(CoordsI$lon, CoordsI$lat),pars = c("T2M_MIN","T2M_MAX"),
                              dates = c("2023-01-01", "2023-12-31"), temporal_api = "DAILY")
        Table<-data.frame(tmin=TempsVec$T2M_MIN,tmax=TempsVec$T2M_MAX)
        
        IndxByPointLP<-simultemp.dete.fluc(N=100,sexratio=0.5, isFixed=TRUE,Table=Table, xi=params$xi, steps=48,poli=1, params=params1)
        IndxByPointLP<-data.frame(Day=1:nrow(IndxByPointLP),IndxByPointLP[,c("r","Ro","GRR","T","lambda","Dt")])
        
        IndxByPointLP2<-simultemp.dete.fluc(N=100,sexratio=0.5, isFixed=TRUE,Table=Table, xi=params$xi, steps=48,poli=1, params=params2)
        IndxByPointLP2<-data.frame(Day=1:nrow(IndxByPointLP2),IndxByPointLP2[,c("r","Ro","GRR","T","lambda","Dt")])
        
        LPs<-IndxByPointLP$Ro
        LPs2<-IndxByPointLP2$Ro
        DateDly<-seq(as.Date("2025/5/26"), as.Date("2026/5/25"), "days")
      }
      
      if(input$OpcionSource=="WC"){
        dir1 = "World-10 minutes-2023/Tmin/"
        dir2 = "World-10 minutes-2023/Tmax/"
        temp<-raster(list.files(dir1,pattern=".flt",full.names = TRUE)[1])
        ilon<-c(bbox(temp)[1,1],bbox(temp)[1,2])
        ilat<-c(bbox(temp)[2,1],bbox(temp)[2,2])
        rm(temp)
        archivos1=list.files(dir1,pattern="flt");archivos1=paste(dir1,"/",archivos1,sep="")
        archivos2=list.files(dir2,pattern="flt");archivos2=paste(dir2,"/",archivos2,sep="")
        
        IndxByPointLP<-TempGetParms(CoordsI[,-3],archivos1,archivos2,modelim=modelim1,modelm=modelm1,estadios=estadios1,steps=steps,params=params1,ID)
        IndxByPointLP<-data.frame(Month=1:12,IndxByPointLP[,c("r","Ro","GRR","T","lambda","Dt")])
        
        IndxByPointLP2<-TempGetParms(CoordsI[,-3],archivos1,archivos2,modelim=modelim2,modelm=modelm2,estadios=estadios2,steps=steps,params=params2,ID)
        IndxByPointLP2<-data.frame(Month=1:12,IndxByPointLP2[,c("r","Ro","GRR","T","lambda","Dt")])
        
        nmeses<-c(31,28,31,30,31,30,31,31,30,31,30,31)
        LPs<-LPflucImp(IndxByPointLP[,3],nmeses)
        LPs2<-LPflucImp(IndxByPointLP2[,3],nmeses)
        DateDly<-seq(as.Date("2023/1/1"), as.Date("2023/12/31"), "days")
      }
      
      incProgress(0.8,message = "getting the graph")
      
      DatF<-data.frame(Date=DateDly,"Ro1"=LPs,"Ro2"=LPs2)
      output$tabindex <- renderPlotly({
        plot_ly(DatF, x = ~Date, color= I("black")) %>%
          add_lines(y = ~Ro1, name  = EstSelected) %>%
          add_lines(y = ~Ro2, name  = EstSelected2, color= I("red")) %>%
          layout(xaxis = list(title = "day")) %>%
          layout(yaxis = list(title = "Net reproduction rate (Ro)"))
      })
      setProgress(1)
    })

  }) 

}

shinyApp(ui = ui, server = server)