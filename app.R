if (!require(terra)) {
  install.packages("terra")
  library(terra)
}
if (!require(sf)) {
  install.packages("sf")
  library(sf)
}
if (!require(tidyverse)) {
  install.packages("tidyverse")
  library(tidyverse)
}
if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}
if (!require(ggalluvial)) {
  install.packages("ggalluvial")
  library(ggalluvial)
}

if (!require(leaflet)) {
  install.packages("leaflet")
  library(leaflet)
}

if (!require(shiny)) {
  install.packages("shiny")
  library(shiny)
}

if (!require(bslib)) {
  install.packages("bslib")
  library(bslib)
}

if (!require(RColorBrewer)) {
  install.packages("RColorBrewer")
  library(RColorBrewer)
}

if (!require(DT)) {
  install.packages("DT")
  library(DT)
}
if (!require(fs)) {
  install.packages("fs")
  library(fs)
}


# Ich wollte es ermöglichen, dass bei dem klicken eines Polygons, ein entsprechender 
# Plot generiert wird, hierfür benötigte ich Klickevents. Folgernder Post half mir 
# mein Ziel umzusetzen 

# Source - https://stackoverflow.com/a/60286249
# Posted by Ben, modified by community. See post 'Timeline' for change history
# Retrieved 2026-04-19, License - CC BY-SA 4.0



# https://rstudio.github.io/leaflet/articles/shiny.html

# https://github.com/rstudio/shiny-gallery/blob/master/lake-profile-dashboard/app.r 

# https://mastering-shiny.org/

test <- readr::read_csv("test.csv")

r <- list.files("corine_bawu/landcover_stack/", pattern = ".tif$", full.names = T)

landcover.stack <- rast(r)
landcover.stack <- landcover.stack[[c(1,3,5)]]


bawu.kreis <- st_read(
  "Verwaltungsgrenzen_NOrA_Jan_2026_ALKIS-Shape/v_al_kreis.shp"
)

test <- merge(bawu.kreis,test, by = "kreis_name")
test <- st_transform(test, 4326)

#### ui ####


ui = fluidPage(
  theme = bslib::bs_theme(bootswatch = "quartz"),
  # titlePanel("Geodaten Kommunizieren"),
  # fluidRow ermöglicht es mehrere Reihen in die App zu implementieren, 
  # innerhalb der Reihen können wir die Spaltenanzahl mit column einstellen, 
  # Shiny ist dabei in 12 Spalten aufgeteilt. Hier teilen wir den Bildschirm 
  # somit in zwei gleich große Bereiche 
  fluidRow(
    column(3,
           tabsetPanel(
             tabPanel("Map 1",
                      leafletOutput(outputId = "mymap")
             ),
             tabPanel("Map 2",
                      DTOutput(outputId = "tableunderplot1"))
           )),
    column(9,
           plotOutput(outputId = "myplot", height = 800)
    )
  )
  
)


#### server ####

#bawu.kreis_flächen_pro_klasse <- st_transform(bawu.kreis_flächen_pro_klasse, 4326)

# test <- bawu.kreis_flächen_pro_klasse %>%  tidyr::pivot_wider(names_from = lc_year, values_from = prozent)

server = function(input, output){
  
  rv = reactiveVal()
  output$mymap = renderLeaflet(
    leaflet() %>% 
      addPolygons(
        data = test,
        color = "black",
        opacity = 0.1,
        weight = 0.5,
        fillOpacity = 0,
        highlightOptions = highlightOptions(
          color = "white",
          weight = 0.1,
          bringToFront = TRUE
        ),
        layerId = ~id)
  )
  
  
  
  
  observeEvent(input$mymap_shape_click, {
    rv(input$mymap_shape_click$id)
  })
  
  
  output$tableunderplot1 <-renderDT({
    req(rv())
    test2 = test %>% dplyr::filter(
      id == rv()) 
    datatable(test2)
  })
  
  output$myplot = renderPlot({
    req(rv())
    test = test %>% dplyr::filter(
      id == rv()
    )
    bawu.kreis.titel = test[1, ]
    ger.municipality_1 = test
    ger.municipality_1 = vect(ger.municipality_1)
    ger.municipality_1 = project(ger.municipality_1, "epsg:4326")
    landcover.stack_municipality_1 = crop(landcover.stack, ger.municipality_1)
    landcover.stack_municipality_1 = mask(landcover.stack_municipality_1, ger.municipality_1)
    
    
    lc.changes.municipality = crosstab(landcover.stack_municipality_1, long = T)
    names(lc.changes.municipality) = c("lc_1990", "lc_2006", "lc_2018", "n")
    
    lc.changes.municipality = lc.changes.municipality %>% mutate(area = n *
                                                                   100 * 100)
    # lc.changes = lc.changes[, -3]
    
    ggplot(lc.changes.municipality,
           aes(
             axis1 = lc_1990,
             axis2 = lc_2006,
             axis3 = lc_2018,
             y = area
           )) +
      geom_alluvium(aes(fill = lc_2018),
                    width = 1 / 24,
                    aes.bind = T, alpha = 0.4) +
      geom_stratum(width = 1 / 3) +
      ggfittext::geom_fit_text(stat = 'stratum', aes(label = paste(after_stat(stratum),'\n',round(after_stat(prop)*100,1), "%")), min.size = 2.5, width = 1/3)+
      #geom_text(stat = "stratum", aes(label = paste(after_stat(stratum))), size = 2.5) +
      ggtitle(label = bawu.kreis.titel$kreis_name) +
      scale_x_continuous(
        breaks = c(1, 2, 3),
        labels = c('1990', '2006', '2018'),
        position = 'top'
      ) +
      scale_fill_viridis_d()+
      theme_void() +
      theme(axis.text.x = element_text(), legend.position = "none")
  }
  )
}


shinyApp(ui = ui, server = server)