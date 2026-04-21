Sys.setenv(USE_BUNDLED_LIBUV = "1")

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
if (!require(tidyterra)) {
  install.packages("tidyterra")
  library(tidyterra)
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


bawu.kreis <- st_read(
  "Verwaltungsgrenzen_NOrA_Jan_2026_ALKIS-Shape/v_al_kreis.shp"
)


files <- list.files("corine_bawu/", pattern = ".tif$", full.names = T)

names <- basename(files)

r <- list()
for (i in 1:5) {
  r[[i]] <- rast(files[i])
  names(r[[i]]) <- names[i]
}

corine.bawu <- rast(c(unlist(r)))


# levels(corine.bawu[[1]])

m <- rbind(
  c(1, 11, 1),
  # Künstliche Flächen
  c(11, 14, 2),
  # Arable land
  c(14, 17, 3),
  # Permanent Crops
  c(17, 18, 4),
  # Pastures
  c(18, 22, 5),
  # Heterogeneous agricultural areas
  c(22, 34, 6),
  # Forest and seminatural areas
  c(34, 44, 7),
  # Wetlands and Waters
  c(44, 48, 8) 
  # NA
)

cls <- data.frame(
  id = 1:8,
  cover = c(
    "Künstliche Flächen",
    "Ackerland",
    "Dauerkulturen",
    "Weiden",
    paste("Heterogene landwirtschaft-", "\n" ,"liche Gebiete"),
    "Wald & seminatürliche Flächen",
    "Feuchtgebiete & Wasserkörper",
    "NA"
  )
)

corine.bawu.classified <- classify(corine.bawu, m)
corine.bawu.classified
for (i in 1:5) {
  levels(corine.bawu.classified[[i]]) <- cls
  names(corine.bawu.classified[[i]]) <- names[i]
}

landcover.stack <- corine.bawu.classified


bawu.kreis <- st_transform(bawu.kreis, 3035)

corine.bawu.classified.sub <- c(corine.bawu.classified[[1]], corine.bawu.classified[[3]], corine.bawu.classified[[5]])
flächen_pro_klasse <- list()


# Mit der for-Schleife werden die Flächen je Landnuntzungsklasse für jeden 
# Kreis errechnet, sodass diese Infos in Leaflet integriert werden können

for (i in 1:44) {
  
  # corine Rasterfiles auf jeweiligen Kreis zuschneiden und croppen
  corine.bawu.classified.masked <- mask(corine.bawu.classified.sub, bawu.kreis[i,])
  corine.bawu.classified.masked <- crop(corine.bawu.classified.masked, bawu.kreis[i,])
  
  # mit expanse werden die Flächen für die einzelnen Level (also) Klassen bestimmt
  df <- expanse(corine.bawu.classified.masked, byValue = T)
  
  # für Prozentualen Flächenanteil die Gesamtflächen für jedes Jahr errechnen
  sum.area_lc_1990 <- {
    sum.area_lc_1990 <- df$area[df$layer == "1"]
    sum.area_lc_1990 <- sum(sum.area_lc_1990)
  }
  sum.area_lc_2006 <- {
    sum.area_lc_2006 <- df$area[df$layer == "2"]
    sum.area_lc_2006 <- sum(sum.area_lc_2006)
  }
  sum.area_lc_2018 <- {
    sum.area_lc_2018 <- df$area[df$layer == "3"]
    sum.area_lc_2018 <- sum(sum.area_lc_2018)
  }
  
  # Prozentuale Flächenanteile werden in neuen Spalten gespeichert und 
  # Kreisnamen werden ebenfalls in einer neuen Spalte hinterlegt
  df <- df %>% mutate(
    prozent = {
      if(all(df$layer == "1")){
        round(area/sum.area_lc_1990*100,1)} else if(all(df$layer == "2")){
          round(area/sum.area_lc_2006*100,1)} else {
            round(area/sum.area_lc_2018*100,1)  
          }
    },
    prozent_char = paste(as.character(prozent), "%"),
    kreis_name = bawu.kreis[i,]$kreis_name)
  
  # Hier wird die Spalte layer noch umbenannt 
  # Layer entspricht der Nummerierung im SpatRaster  
  df$layer[df$layer == 1] <- "lc_1990"
  df$layer[df$layer == 2] <- "lc_2006"
  df$layer[df$layer == 3] <- "lc_2018"
  
  # Umbenennung der Spalte layer:
  # Source - https://stackoverflow.com/a/22461868
  # Posted by Matheus Abreu, modified by community. See post 'Timeline' for change history
  # Retrieved 2026-04-15, License - CC BY-SA 3.0
  colnames(df)[which(names(df) == "layer")] <- "lc_year"
  
  # Speichern des erstellten Dataframes in in einer Liste, die später 
  # zusammengefügt werden kann
  flächen_pro_klasse[[i]] <- df
}



flächen_pro_klasse_df <- bind_rows(flächen_pro_klasse, .id = "id")

colnames(flächen_pro_klasse_df)[which(names(flächen_pro_klasse_df) == "kreis")] <- "kreis_name"
bawu.kreis_flächen_pro_klasse <- merge(x = bawu.kreis, y = flächen_pro_klasse_df, by = "kreis_name" )









# Ich wollte es ermöglichen, dass bei dem klicken eines Polygons, ein entsprechender 
# Plot generiert wird, hierfür benötigte ich Klickevents. Folgernder Post half mir 
# mein Ziel umzusetzen 

# Source - https://stackoverflow.com/a/60286249
# Posted by Ben, modified by community. See post 'Timeline' for change history
# Retrieved 2026-04-19, License - CC BY-SA 4.0



# https://rstudio.github.io/leaflet/articles/shiny.html

# https://github.com/rstudio/shiny-gallery/blob/master/lake-profile-dashboard/app.r 

# https://mastering-shiny.org/





landcover.stack <- project(landcover.stack, "epsg:4326")
landcover.stack <- landcover.stack[[c(1,3,5)]]
# lc.changes.municipality = crosstab(landcover.stack, long = T)
# names(lc.changes.municipality) = c("lc_1990", "lc_2000", "lc_2006", "lc_2012", "lc_2018", "n")
# 
# lc.changes.municipality = lc.changes.municipality %>% mutate(area = n *
#                                                                 100 * 100)
# bawu.kreis.epsg.4326$id = 1:nrow(bawu.kreis.epsg.4326)
# 




test <- bawu.kreis_flächen_pro_klasse %>%  tidyr::pivot_wider(names_from = lc_year, values_from = prozent)
test <- test[,-c(2,3,4,5,6,7,8)]
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
    ger.municipality_1 = project(ger.municipality_1, test)
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