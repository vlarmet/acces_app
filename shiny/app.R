library(shiny)
library(shinyWidgets)
library(shinycssloaders)
library(shinythemes)
library(shinydashboard)
library(sf)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(DT)
library(htmltools)
library(leafgl)
library(shinyjs)
library(jsonify)
library(geojsonsf)
library(ggplot2)
library(scales)
library(data.table)
library(rgdal)
#library(raster)
library(fasterize)

#options(encoding = "UTF-8")


#setwd("D:/users/vlarmet/carroyage/200m/app")
source("utils2.R")


# Define UI ----
ui <- shinyUI(
  navbarPage(
    title = "Accessibilite aux services en France",
    theme = shinytheme("united"),
    tabPanel("Carte",
             div(class = "outer",
                 tags$head(
                   includeCSS("styles.css")
                 ),
                 
                 withSpinner(leafglOutput("map")),
                 absolutePanel(
                   id = "controls", class = "panel panel-default", fixed = TRUE,
                   draggable = TRUE, top = "10%", left = "10%", #125 
                   right = "auto", bottom = "auto",
                   width = 0, height = 0,
                   dropdownButton(
                     icon = icon("search"),
                     selectizeInput(
                       inputId = "search",
                       label = "Rechercher une ville",
                       choices= NULL,
                       selected="France"
                     ),
                     h3(strong("ou"), align="center"),
                     h3(''),
                     selectInput(
                       inputId = "searchdep",
                       label="Rechercher un département",
                       choices=c("",unique(dep$NOM_DEPT)),
                       selected = ""
                     ),
                     checkboxInput(
                       inputId = "carr",
                       label = "Infracommunal",
                       value = FALSE
                     )
                     
                   )
                 ),
                 absolutePanel(
                   id = "controls2", class = "panel panel-default", fixed = TRUE,
                   draggable = TRUE, top = "10%", left = "2%", #125 
                   right = "auto", bottom = "auto",
                   width = 0, height = 0,
                   dropdownButton(
                     icon = icon("gear"),
                     width = 350,
                     radioGroupButtons("indic", "Indicateur",
                                       choices = c("Accessibilite Potentielle", "Temps de trajet"),
                                       selected = "Temps de trajet"),
                     selectInput(
                       inputId = "theme",
                       label = "Choisir un thème",
                       choices = c("Services aux particuliers","Commerces","Education","Santé","Transport","Sport et loisirs"),
                       selected = "Santé"
                     ),
                     
                     conditionalPanel(condition = "input.theme == 'Services aux particuliers'",
                                      selectInput(inputId = "equip1",
                                                  label = "Choisir un équipement",
                                                  choices = c("",libs$libelle[substr(libs$code,1,1) == 'A']),
                                                  selected = "",
                                                  selectize = FALSE)),
                     conditionalPanel(condition = "input.theme == 'Commerces'",
                                      selectInput(inputId ="equip2",
                                                  label = "Choisir un équipement",
                                                  choices = c("",libs$libelle[substr(libs$code,1,1) == 'B']),
                                                  selected = "",
                                                  selectize = FALSE)),
                     conditionalPanel(condition = "input.theme == 'Education'",
                                      selectInput(inputId ="equip3",
                                                  label = "Choisir un équipement",
                                                  choices =c("",libs$libelle[substr(libs$code,1,1) == 'C']),
                                                  selected = "",
                                                  selectize = FALSE)),
                     conditionalPanel(condition = "input.theme == 'Transport'",
                                      selectInput(inputId ="equip5",
                                                  label = "Choisir un équipement",
                                                  choices = c("",libs$libelle[substr(libs$code,1,1) == 'E']),
                                                  selected = "",
                                                  selectize = FALSE)),
                     conditionalPanel(condition = "input.theme == 'Sport et loisirs'",
                                      selectInput(inputId ="equip6",
                                                  label = "Choisir un équipement",
                                                  choices = c("",libs$libelle[substr(libs$code,1,1) == 'F']),
                                                  selected = "",
                                                  selectize = FALSE)),
                     conditionalPanel(condition = "input.theme == 'Santé'",
                                      selectInput(inputId ="equip4",
                                                  label = "Choisir un équipement",
                                                  choices = c("",libs$libelle[substr(libs$code,1,1) == 'D']),
                                                  selected = "Médecin généraliste",
                                                  selectize = FALSE))
                   )
                 ),
                   absolutePanel( # histogrammes
                   
                   id = "hists", class = "panel panel-default",
                   fixed = TRUE, draggable = TRUE,
                   top = 50, left = "auto", right = 0,
                   bottom = "auto",
                   width = "27%", height = "80%",
                   h3(actionButton("reset",label="Réinitialiser",style="color: #fff; background-color: #dd4814"), align="center"),
                   uiOutput("text"),
                   h4(strong("Distribution de l'indicateur pour la population française")),
                   plotOutput("hist1",height = "35%"),
                   h4(strong("Structure de la population par tranches d'âge")),
                   checkboxInput("dep_check","Comparer au département",value = FALSE),
                   plotOutput("hist2",height = "35%")
                 ))
    
  ),
  tabPanel(title="Explorateur",
           dataTableOutput("table")),
  tabPanel(title="A propos",
           withMathJax(includeMD("about.md")))
  ,useShinyjs()
)
)


server <- function(input, output, session) {
  
  # Reactice values
  
  rv <- reactiveValues(equip=NULL,
                       search="France")
  observeEvent(c(input$equip1),{
    rv$equip <-  input$equip1
  })
  observeEvent(c(input$equip2),{
    rv$equip <-  input$equip2
  })
  observeEvent(c(input$equip3),{
    rv$equip <-  input$equip3
  })

  observeEvent(c(input$equip5),{
    rv$equip <-  input$equip5
  })
  observeEvent(c(input$equip6),{
    rv$equip <-  input$equip6
  })

  observeEvent(c(input$equip4),{
    rv$equip <-  input$equip4
  })
  
  observeEvent(input$search,{
    if (!input$search %in% c("")) rv$search <- input$search
  })
  

  #Carte de base 
  output$map<-renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 6)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      leafgl::addGlPolylines(layerId = "contour_c",data=lin, weight = 0.01, color = "black", opacity = 0.6) 

    
  })
  
  # Gérer recherche commune 
  updateSelectizeInput(session, inputId =  'search', choices = c("France",shp$label),selected = "France", server = TRUE)
  
  
  # Zoom sur la commune selectionnee
  observeEvent(rv$search,{
    
    if (rv$search=="France") {
      points<-as.numeric(st_bbox(shp))
      shinyjs::disable("dep_check")
    }
    else {
      points<-as.numeric(st_bbox(shp[shp$label==rv$search,]))
      shinyjs::enable("dep_check")
    }
    
    updateSelectInput(session, inputId =  'searchdep', choices =c("",unique(dep$NOM_DEPT)),selected = "")
    
    leafletProxy("map") %>%
      clearPopups() %>%
      addPopups(data = st_centroid(shp[shp$label== rv$search,]), popup = ~label) %>%
      fitBounds(lng1 = points[3],lat1 = points[4],
                lng2 = points[1],lat2 = points[2])
  
    
  })
  
  # Zoom sur le departement selectionne
  observeEvent(input$searchdep,{
    
    if (input$searchdep=="") {
      
    } else {
      points<-as.numeric(st_bbox(dep[dep$NOM_DEPT == input$searchdep,]))
      #updateSelectizeInput(session, inputId =  'search', choices = c("France",shp$label),selected = NULL, server = TRUE)
      
      leafletProxy("map") %>%
        clearPopups() %>%
        fitBounds(lng1 = points[3],lat1 = points[4],
                  lng2 = points[1],lat2 = points[2])
    } 
  
    

    
    
  })
  
  # Afficher polygones
  observeEvent(c(rv$equip, input$indic, input$carr),{
    
    if (!input$carr){
      var_code <- libs$code[libs$libelle == rv$equip]
      indic_code <- switch(input$indic, `Accessibilite Potentielle`={"sfca"}, `Temps de trajet`={"near"})
      
      if (indic_code == "sfca") lab <- "Pour 10,000 hab."
      if (indic_code == "near") lab <- "Minutes"
      
      variable <- paste(indic_code,var_code,sep="_")
      
      temp <- shp[[variable]]
   
      out <- quantile(temp,probs=0.90,na.rm=TRUE)
   
      qpal <- colorBin(viridis::viridis(10), temp[temp < out], bins = 10)
      bins <- attributes(qpal)$colorArgs$bins
      colors <- viridis::viridis(length(bins) - 1)
      pal_labs <- paste(bins[-length(bins)], bins[-1], sep=" - ")
      pal_labs <- c(pal_labs,paste0("> ", bins[length(bins)]))
      colors <- c(colors,"orange")
      
      my_pal <- function(x){
        colors <- qpal(x)
        colors[x >= out] <- "orange"
        return(colors)
      }
      
      gc()
      
      runjs("L.glify.Shapes.instances.splice(0, 1);")
      runjs("L.glify.Lines.instances.splice(1, 2);")
      
      
      leafletProxy("map") %>%
        clearGroup("legend") %>%
        clearImages() %>%
        leafgl::removeGlPolygons("EPCI") %>%
        leafgl::removeGlPolylines(layerId = "contour_dep")
        gc()
        
        leafletProxy("map") %>%
        leafgl::addGlPolygons(data=shp,
                              layerId ="EPCI",
                              col="black",
                              weight = 0.3,
                              fillOpacity = 0.8,
                              fillColor = my_pal(temp)) %>%
        leafgl::addGlPolylines(layerId = "contour_dep",data=dep, weight = 0.3, color = "black", opacity = 0.6) %>%
        addLegend(layerId = "legend",colors=colors, labels = pal_labs,position = "bottomleft",opacity = 1,title=lab)
      
      
      
    }
  })
  
  observe({
    invalidateLater(1000,session)
    gc()
  })
  
  # Afficher raster infracommunal
  
  observeEvent(c(input$carr, input$searchdep, rv$equip, input$indic),{
    if (input$searchdep != "" & input$carr){
      var_code <- libs$code[libs$libelle == rv$equip]
      indic_code <- switch(input$indic, `Accessibilite Potentielle`={"sfca"},`Temps de trajet`={"near"})
      variable <- paste(indic_code,var_code,sep="_")
      num_dep <- dep$code_dep[dep$NOM_DEPT == input$searchdep]
      
      dat <- readRDS(paste0("raw/",variable,".rds"))
      if (indic_code == "sfca"){
        lab <- "Pour 10,000 hab."
        rast <- generate_raster(variable,dat,num_dep,dep3857=dep3857, nodes=sfca_nodes)
      } 
      if (indic_code == "near"){
        lab <- "Minutes"
        rast <- generate_raster(variable,dat,num_dep, dep3857=dep3857,nodes=near_nodes)
      } 
      
      
      
      temp <- raster::getValues(rast)
      temp <- temp[!is.na(temp)]
      out <- quantile(temp,probs= 0.99, na.rm=T)
      
      qpal <- colorBin(viridis::viridis(10), temp[temp < out],bins = 10, na.color ="#00000000", pretty = TRUE)
      bins <- attributes(qpal)$colorArgs$bins
      colors <- viridis::viridis(length(bins) - 1)
      pal_labs <- paste(bins[-length(bins)], bins[-1], sep=" - ")
      pal_labs <- c(pal_labs,paste0("> ", bins[length(bins)]))
      colors <- c(colors,"orange")

      
      my_pal <- function(x){
        colors <- qpal(x)
        colors[x >= out] <- "orange"
        return(colors)
      }
      
      
      runjs("L.glify.Shapes.instances.splice(0, 1);")
      runjs("L.glify.Lines.instances.splice(1, 2);")
      
      leafletProxy("map") %>%
        clearImages() %>%
        clearGroup("legend") %>%
        leafgl::removeGlPolygons(layerId = "EPCI") %>%
        leafgl::removeGlPolylines(layerId = "contour_dep") 
      
      gc()
      
      leafletProxy("map") %>%
        addRasterImage(rast,project = FALSE, opacity = 0.6,colors = my_pal) %>%
        leafgl::addGlPolylines(layerId = "contour_dep",data=dep, weight = 0.3, color = "black", opacity = 0.6) %>%
        addLegend(layerId = "legend",colors=colors,labels = pal_labs,"bottomleft", opacity = 1,title=lab)
    }
  })
  
  # Griser la case infracommunal quand aucun departement est selectionne
  observeEvent(input$searchdep, {
    if (input$searchdep == "") {
      updateCheckboxInput(session,"carr",value = FALSE)
      shinyjs::disable("carr")
    } else shinyjs::enable("carr")
  })
  
  # Reinitialiser
  observeEvent(input$reset,{
    updateCheckboxInput(session,"carr",value = FALSE)
    updateSelectizeInput(session, inputId =  'search', choices = c("France",shp$label),selected = "France", server = TRUE)
    updateSelectInput(session, inputId =  'searchdep', choices = c("",dep$NOM_DEPT),selected = "")
  })
  
  # Mettre a jour selection commune apres un click sur un polygone
  observeEvent(input$map_glify_click,{
    
    event <- input$map_glify_click
    print(event)
    updateSelectizeInput(session, inputId =  'search', choices = c("France",shp$label),selected = shp$label[event$data$id], server = TRUE)
    updateSelectInput(session, inputId =  'searchdep', choices = c("",unique(dep$NOM_DEPT)),selected = "")
    
    
  })
  
  observe({
    gc()
  })
  
  
  # Histogrammes
  
  observeEvent(c(rv$search, rv$equip, input$dep_check, input$indic),{
    var_code <- libs$code[libs$libelle == rv$equip]
    indic_code <- switch(input$indic, `Accessibilite Potentielle`={"sfca"}, `Temps de trajet`={"near"})
    
    variable <- paste(indic_code,var_code,sep="_")
    
   # dat <- readRDS(paste0("raw/",variable,".rds"))
    
    
    output$hist1 <- renderPlot({
      plot_hist(variable, location = rv$search, label=indic_code)
      })
       
    gc()

    
    output$hist2 <- renderPlot({
      plot_age(rv$search, input$dep_check)
      
    })
    
    gc()
    
    output$text <- renderUI({
      
      text_output(rv$search,variable)
    })
    
    gc()
    
  })
  
  

  
  # Explorer


    output$table<-DT::renderDataTable({
      

      DT::datatable(df[df$Equipement == rv$equip,],  
                    escape = FALSE,
                    filter = "top",
                    options = list(scrollX = TRUE,
                                   scrollY="75vh",
                                   scrollCollapse=TRUE,
                                   "pageLength"=50))
    }, server = TRUE)

  
}

shinyApp(ui = ui, server = server)
