# 
# 
rm(list = ls(all=TRUE))  

library(shiny)
library(data.table)
library(plyr)
library(ggplot2)
library(geosphere)
library(sp)
library(grid)
library(foreign)
library(shinycssloaders)
library(raster)
library(rasterVis)
library(rgdal)
library(ggrepel)
# library(rJava)
# library(OpenStreetMap)
load("base_file.RData")
choices_area <- c("SCENE Cashel", "SCENE", "SCENE_W","SCENE_E",
                  "SCENE_DL", "SCENE_DL_N", "SCENE_DL_S",
                  "SCENE_FS", "SCENE_P1", "SCENE_P2",
                  "SCENE_SAL","SCENE_SAL_W","SCENE_SAL_M","SCENE_SAL_E",
                  "Cashel", "Cashel_N","Cashel_S",
                  "Kelvingrove","Kelvingrove_W","Kelvingrove_E",
                  "Cochno","Cochno_W","Cochno_E",
                  "Garscube","Garscube_W","Garscube_E")

choices_maptype <- c("bing (Satellite)", "osm (OpenStreetMap)", "default (custom-made map)                    ")


# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
                titlePanel("Nestbox maps"),
                
                # Sidebar with a slider input for number of bins 
                sidebarPanel(
                      fileInput(inputId = "nestboxes",
                                label = "Choose nestbox CSV file:",
                                accept = c(
                                      "text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")),
                      selectInput("lat", "Select latitude column", c('col1', 'col2'), multiple = F),
                      selectInput("long", "Select longitude column", c('col1', 'col2'), multiple = F),
                      selectInput("name", "Select nestbox label column", c('col1', 'col2'), multiple = F),
                      
                      # Horizontal line ----
                      tags$hr(),
                      selectInput(inputId = "area",
                                  label = "Area:",
                                  choices = choices_area),
                      selectInput(inputId = "maptype",
                                  label = "Maptype:",
                                  choices =  choices_maptype),
                      actionButton("go", "Render the plot!", icon = icon("angle-double-right"), width = "auto"),
                      # Horizontal line ----
                      tags$hr()
                      
                ),
                
                # Show a plot of the generated distribution
                mainPanel(
                      plotOutput("nestboxMap")
                )
)

# Define server logic required to draw a histogram
server <- function(session, input, output) {
      nestboxes <- reactive({
            req(input$nestboxes)
            read.csv(input$nestboxes$datapath)
      })
      observeEvent(nestboxes(), {
            updateSelectInput(session, "lat", choices=colnames(nestboxes()))
            updateSelectInput(session, "long", choices=colnames(nestboxes()))
            updateSelectInput(session, "name", choices=colnames(nestboxes()))
            
      })
      observeEvent(input$go, {
            output$nestboxMap <- renderPlot({
                  input$go
                  isolate({
                  # input$file1 will be NULL initially. After the user selects
                  # and uploads a file, head of that data file by default,
                  # or all rows if selected, will be shown.
                  
                        name <- req(input$name)
                        lat <- req(input$lat)
                        long <- req(input$long)
                        req(input$nestboxes)
                        
                        nestboxes <- read.csv(input$nestboxes$datapath)
                        names(nestboxes)[names(nestboxes) == name] <- 'name_ori'
                        names(nestboxes)[names(nestboxes) == lat] <- 'lat'
                        names(nestboxes)[names(nestboxes) == long] <- 'long'
                  
                         area <- gsub("[[:blank:]]", "", area)
                         maptype <- head(strsplit(maptype,split=" ")[[1]],1)
                        
                         if (grepl("SCENECashel", area, ignore.case=TRUE) == TRUE){extent <- data.frame(x=c(-4.6319, -4.5683), y=c(56.0981, 56.1432))}
                         if (grepl("SCENE", area, ignore.case=TRUE) == TRUE){extent <- data.frame(x=c(-4.6286, -4.5914), y=c(56.1122, 56.1385))}
                         if (grepl("SCENE(_|-)W", area, ignore.case=TRUE) == TRUE){extent <- data.frame(x=c(-4.6257, -4.6076), y=c(56.1225, 56.1358))}
                         if (grepl("SCENE(_|-)E", area, ignore.case=TRUE) == TRUE){extent <- data.frame(x=c(-4.6101, -4.5914), y=c(56.1192, 56.1325))}
                        
                         if (grepl("SCENE(_|-)DL", area, ignore.case=TRUE) == TRUE){extent <- data.frame(x=c(-4.6216, -4.6116), y=c(56.1278, 56.1358 ))}
                         if (grepl("SCENE(_|-)DL(_|-)N", area, ignore.case=TRUE) == TRUE){extent <- data.frame(x=c(-4.6216, -4.6116), y=c(56.1288, 56.1358))}
                         if (grepl("SCENE(_|-)DL(_|-)S", area, ignore.case=TRUE) == TRUE){extent <- data.frame(x=c(-4.6176, -4.6116), y=c(56.1278, 56.1328))}
                        
                        
                         if (grepl("SCENE(_|-)FS", area, ignore.case=TRUE) == TRUE){extent <- data.frame(x=c(-4.6148, -4.6077), y=c(56.1268, 56.1315))}
                        
                         if (grepl("SCENE(_|-)P1", area, ignore.case=TRUE) == TRUE){extent <- data.frame(x=c(-4.6216, -4.6158), y=c(56.1245, 56.1290))}
                        
                         if (grepl("SCENE(_|-)P2", area, ignore.case=TRUE) == TRUE){extent <- data.frame(x=c(-4.6257, -4.6196), y=c(56.1229, 56.1269))}
                        
                         if (grepl("SCENE(_|-)SAL", area, ignore.case=TRUE) == TRUE){extent <- data.frame(x=c(-4.6075, -4.5909), y=c(56.1184, 56.1302))}
                         if (grepl("SCENE(_|-)SAL(_|-)W", area, ignore.case=TRUE) == TRUE){extent <- data.frame(x=c(-4.6060, -4.6011), y=c(56.1230, 56.1266))}
                         if (grepl("SCENE(_|-)SAL(_|-)M", area, ignore.case=TRUE) == TRUE){extent <- data.frame(x=c(-4.6011, -4.5963), y=c(56.1210, 56.1246))}
                         if (grepl("SCENE(_|-)SAL(_|-)E", area, ignore.case=TRUE) == TRUE){extent <- data.frame(x=c(-4.5968, -4.5920), y=c(56.1190, 56.1226))}
                        
                         if(grepl("^Cashel", area, ignore.case=TRUE) == TRUE){extent <- data.frame(x=c(-4.5868, -4.5680), y=c(56.1042, 56.1175))}
                         if (grepl("Cashel(_|-)N", area, ignore.case=TRUE) == TRUE){extent <- data.frame(x=c(-4.5816, -4.5707), y=c(56.1078, 56.1168))}
                         if (grepl("Cashel(_|-)S", area, ignore.case=TRUE) == TRUE){extent <- data.frame(x=c(-4.5826, -4.5717), y=c(56.1052, 56.1110))}
                        
                         if(grepl("Kelvingrove", area, ignore.case=TRUE) == TRUE){extent <-data.frame(x=c(-4.2946, -4.2776),y=c(55.8638, 55.8759))}
                         if(grepl("Kelvingrove(_|-)W", area, ignore.case=TRUE) == TRUE){extent <-data.frame(x=c(-4.2942, -4.2862),y=c(55.8645, 55.8756))}
                         if(grepl("Kelvingrove(_|-)E", area, ignore.case=TRUE) == TRUE){extent <-data.frame(x=c(-4.2862, -4.2782),y=c(55.8645, 55.8756))}
                        
                         if(grepl('^Kelvingrove', area, ignore.case=TRUE) == TRUE & maptype == "default" |
                            grepl('^Garscube', area, ignore.case=TRUE) == TRUE  & maptype == "default")
                         {stop("----------No default map for this area (use osm/bing)----------")}
                        
                         if(grepl("Cochno", area, ignore.case=TRUE) == TRUE){extent <-data.frame(x=c(-4.4167, -4.3998),y=c(55.9324, 55.9445))}
                         if (grepl("Cochno(_|-)W", area, ignore.case=TRUE) == TRUE){extent <-data.frame(x=c(-4.4159, -4.4079),y=c(55.9334, 55.9446))}
                         if (grepl("Cochno(_|-)E", area, ignore.case=TRUE) == TRUE){extent <-data.frame(x=c(-4.4082, -4.4001),y=c(55.9323, 55.9434))}
                        
                         if(grepl("Garscube", area, ignore.case=TRUE) == TRUE){extent <-data.frame(x=c(-4.3247, -4.3077),y=c(55.8965, 55.9086))}
                         if(grepl("Garscube(_|-)W", area, ignore.case=TRUE) == TRUE){extent <-data.frame(x=c(-4.3241, -4.3161),y=c(55.8977, 55.9092))}
                         if(grepl("Garscube(_|-)E", area, ignore.case=TRUE) == TRUE){extent <-data.frame(x=c(-4.3163, -4.3082),y=c(55.8977, 55.9089))}
                        
                         colnames(extent) <- c("long", "lat")
                         if(area == "^Cochno"){
                               nestboxes_crop <- nestboxes[grepl(eval(paste("^",substring(area, 1, 1), sep="")), nestboxes$name_ori) == TRUE &
                                                                 grepl("^\\w\\d", nestboxes$name_ori) == FALSE,] 
                         } 
                         if(area == "^Cashel") {
                               nestboxes_crop <- nestboxes[grepl(eval(paste("^",substring(area, 1, 1), sep="")), nestboxes$name_ori) == TRUE &
                                                                 grepl("^\\w\\d", nestboxes$name_ori) == TRUE,] 
                         } 
                         if(area != "^Cashel" & area != "^Cochno"){
                               nestboxes_crop <- nestboxes[grepl(eval(paste("^",substring(area, 1, 1), sep="")), nestboxes$name_ori) == TRUE ,] 
                         }
                         nestboxes_crop <- nestboxes_crop[, !(names(nestboxes_crop) %in% "X")]
                         if("label" %in% names(nestboxes_crop) == FALSE){
                               nestboxes_crop$label <- gsub("[^[:digit:]]", "", nestboxes_crop$name_ori)
                         }
                         nestboxes_crop <- nestboxes_crop[nestboxes_crop$long > extent$long[1] & nestboxes_crop$long < extent$long[2] & 
                                                                nestboxes_crop$lat > extent$lat[1] & nestboxes_crop$lat < extent$lat[2],]
                         
                         nestboxes_crop <- nestboxes_crop[ , -which(names(nestboxes_crop) %in% c("coords.x1", "coords.x2", "optional"))]
                         coords <- cbind(nestboxes_crop$long, nestboxes_crop$lat)
                         sp = SpatialPoints(coords)
                         spdf = SpatialPointsDataFrame(sp, nestboxes_crop)
                         proj4string(spdf) <- CRS("+init=epsg:4326")
                         if (grepl("osm", maptype, ignore.case=TRUE) == TRUE | grepl("bing", maptype, ignore.case=TRUE) == TRUE){
                               nestboxes_crop <- spTransform(spdf, CRS("+init=epsg:3857"))
                         } else {
                               nestboxes_crop <- spTransform(spdf, CRS("+init=epsg:27700"))
                         }
                         nestboxes_crop <- as.data.frame(nestboxes_crop)
                         nestboxes_crop <- nestboxes_crop[ , -which(names(nestboxes_crop) %in% c("lat","long", "X"))]
                         nestboxes_crop <- rename(nestboxes_crop, c("coords.x1" = "long", "coords.x2"="lat"))
                         
                         customcolor <- ifelse(maptype == "bing", "white", "black")
                         if(grepl("\\bSCENE\\b", area, ignore.case=TRUE) == TRUE){
                               plot <- ggplot()
                         }
                         if(grepl('^SCENE', area, ignore.case=TRUE) == TRUE & grepl("\\bSCENE\\b", area, ignore.case=TRUE) == FALSE & maptype == "default" | grepl('^Cashel', area, ignore.case=TRUE) == TRUE  & maptype == "default" | 
                            grepl('^Cochno', area, ignore.case=TRUE) == TRUE  & maptype == "default" ){
                               plot <- gplot(get(paste("DEM_", toupper(gsub( "(_|-).*$", "", area)), sep="")), maxpixels = 30e5) + 
                                     geom_tile(aes(fill = value)) +
                                     scale_fill_gradientn(colours=grey(1:100/100),breaks=get(paste("b.DEM_", toupper(gsub( "(_|-).*$", "", area)), sep="")),guide="none")
                         } 
                         
                         
                         if(grepl('^Kelvingrove', area, ignore.case=TRUE) == TRUE | grepl('^Garscube', area, ignore.case=TRUE) == TRUE |
                            maptype != "default"){
                               map <- openmap(c(extent$lat[2], extent$long[1]), c(extent$lat[1], extent$long[2]), type=maptype, 
                                              minNumTiles = 25)
                               plot <- autoplot(map) 
                         } 
                         
                         extent <- SpatialPoints(extent)
                         proj4string(extent) <- CRS("+init=epsg:4326")
                         if (grepl("osm", maptype, ignore.case=TRUE) == TRUE | grepl("bing", maptype, ignore.case=TRUE) == TRUE){
                               extent <- spTransform(extent, CRS("+init=epsg:3857"))
                               extent <- data.frame(extent)
                         } else {
                               extent <- spTransform(extent, CRS("+init=epsg:27700"))
                               extent <- data.frame(extent)
                         }
                         
                         ###### Add labels to the contour lines ######
                         if (grepl("osm", maptype, ignore.case=TRUE) == TRUE | grepl("bing", maptype, ignore.case=TRUE) == TRUE){
                               elevation <- spTransform(elevation, CRS("+init=epsg:3857"))
                         }
                         elev_df <- fortify(elevation, region = 'id')
                         elev_dt <- data.table(elev_df, key="id")
                         elevation@data$id <- rownames(elevation@data)
                         elev_df2 <- data.frame(elevation)
                         elev_dt2 <- data.table(elev_df2, key="id")
                         dt <- elev_dt2[elev_dt, roll=T]
                         dt <- dt[dt$long > extent$long[1] & dt$long < extent$long[2] 
                                  & dt$lat > extent$lat[1] & dt$lat < extent$lat[2],]
                         contour_labels <- ddply(dt, .(id), function(x) head(x, 1))
                         
                         
                         if(maptype != "osm" | maptype != "bing")
                         {plot <-  plot + 
                               geom_polygon(data= OSwater, aes(long, lat, group=group),
                                            color="black", fill="lightseagreen", lwd=.5) +
                               geom_polygon(data= LochLomond, aes(long, lat, group=group),
                                            color="black", fill="lightseagreen", lwd=.5)+
                               geom_polygon(data= DubhLochan, aes(long, lat, group=group),
                                            color="black", fill="lightseagreen", lwd=.5)} 
                         if (grepl("osm", maptype, ignore.case=TRUE) == TRUE | grepl("bing", maptype, ignore.case=TRUE) == TRUE){
                               WHW <- spTransform(WHW, CRS("+init=epsg:3857"))
                               OSwaterways <- spTransform(OSwaterways, CRS("+init=epsg:3857"))
                               elevation <- spTransform(elevation, CRS("+init=epsg:3857"))
                               elevation_points <- spTransform(elevation_points, CRS("+init=epsg:3857"))
                               All_Sampling_Sectors <- spTransform(All_Sampling_Sectors, CRS("+init=epsg:3857"))
                         }
                         
                         crop.extent <- extent(extent$long[1], extent$long[2], extent$lat[1], extent$lat[2])
                         All_Sampling_Sectors <- crop(All_Sampling_Sectors, crop.extent)
                         
                         if (is.null(All_Sampling_Sectors) == FALSE) {
                               if (grepl("osm", maptype, ignore.case=TRUE) == TRUE | grepl("bing", maptype, ignore.case=TRUE) == TRUE){
                                     shp_df <- fortify(All_Sampling_Sectors)
                                     shp_dt <- data.table(shp_df, key="id")
                                     All_Sampling_Sectors@data$rownr <- rownames(All_Sampling_Sectors@data)
                                     shp_df2 <- data.frame(All_Sampling_Sectors)
                                     shp_dt2 <- data.table(shp_df2, key="rownr")
                                     dt <- shp_dt2[shp_dt, roll=T]
                                     dt$Legend <- as.factor(dt$id)
                                     plot <- plot + geom_polygon(data = dt, aes(long, lat, group= group,fill=Legend), col="black", cex=.3,
                                                                 alpha=.2)
                               } else {
                                     plot <- plot + geom_polygon(data = All_Sampling_Sectors, aes(long, lat, group= group),
                                                                 col= "black", fill="green", cex=.3, alpha=.05)
                               }
                         }
                         
                         plot <- plot +
                               geom_polygon(data= LLbeaches, aes(long, lat, group=group), color="black", fill="darkgoldenrod1", lwd=.5) +
                               geom_path(data= elevation, aes(long, lat, group=group), lwd=.2, color=customcolor, alpha=.5) + #use geom_path (uses order of values in data.frame) instead of geom_line (uses order of x-axis to connect points)
                               geom_point(data= data.frame(elevation_points), aes(coords.x1, coords.x2),  pch=1, cex=1.5) + 
                               geom_path(data= OSwaterways, aes(long, lat, group=group), lwd=.4, col= "lightseagreen") +
                               geom_path(data= OSroads, aes(long, lat, group=group), lwd=1,col= "gray32") +
                               geom_path(data= NFEroads, aes(long, lat, group=group), lwd=1, col= "darkgoldenrod4") + 
                               geom_path(data= RecRoutes, aes(long, lat, group=group), lwd=.4, lty=2, col= "darkgoldenrod4") + 
                               geom_path(data= WHW, aes(long, lat, group=group), lwd=.4, lty=2, col= "darkgoldenrod4") 
                         if(maptype != "bing")
                         {plot <- plot + geom_polygon(data= OSbuildings, aes(long, lat, group=group)
                                                      , color="black", fill="transparent", alpha=.1)} 
                         plot
                         

                  })
            }, width = 650, height = 650)
      })
}

# Run the application 
shinyApp(ui = ui, server = server)

