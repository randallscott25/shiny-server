######################################################################
#                Community Health Status Indicators
#               shinyapp -- Randall Taylor Group 1 Applied Data Science 
######################################################################

#load required packages using a bulk check/install function from professor
options(rsconnect.http = 'curl')
RCurl::curlVersion()$ssl_version

#########################load the packages rather Ging' ################
library(sf)
library(raster)
library(dplyr)
library(stringr)
library(spData)
library(leaflet)
library(sp)
library(readr)
library(tidyr)
library(devtools)
library(ggplot2)
library(tmap)
library(tmaptools)
library(corrplot)
library(RCurl)
library(rgeos)
library(rgdal)
library(maptools)
library(scales)
library(shiny)
library(shinythemes)
library(rnaturalearthdata)
library(maps)


####################TRANSITION INTO THE DATA FILE EXTRACTED FROM THE####### COMBINED CSV FROM THE Community Health Status Indicators GROUP 1 PROJECT 
##HOSTED AT THE FOLLOWING WEB ADDRESS https://query.data.world/s/u7snqip5zd7roqbolnxojg6dvv5gx7
#DATA FILE -  Community Health Status Indicators Extracction
combined <- read.csv("https://query.data.world/s/u7snqip5zd7roqbolnxojg6dvv5gx7", header=TRUE, stringsAsFactors=FALSE)


# We know that negative numbers are really NAs; let's replace them
combined[,-1:-7] <-  data.frame(lapply(combined[,-1:-7], function(x){
  as.numeric(gsub("-1*|-2*|-9*",NA,x))
}))

write_csv(combined,'combined.csv')
######################################MEAN CLEAN THE FUNCTION JUNCTION#######
#replace NAs with mean of column auto
########################################################################
na_mean_swap <- function(x) {
  replace(x, is.na(x),mean(as.numeric(x),na.rm=TRUE))
}
mean_clean <- cbind(combined[,1:7],replace(combined[,-1:-7],TRUE, lapply(combined[,-1:-7], na_mean_swap)))
#########################################################################
#########################################################################
#establishing the DATA frame ############################################
Ale.Contribs.df = mean_clean %>% 
  dplyr::select(CHSI_State_Name,CHSI_County_Name, County_FIPS_Code,No_HS_Diploma, ALE,Population_Size)
#Further Development ######################################################
ale.1 <- Ale.Contribs.df[,c('CHSI_State_Name','CHSI_County_Name','County_FIPS_Code','No_HS_Diploma', 'Population_Size','ALE')] 
ale.1$state <- tolower(ale.1$CHSI_State_Name)
ale.1$county<- tolower(ale.1$CHSI_County_Name)
ale.1$ID <- paste(ale.1$state,ale.1$county,sep=',')
#####THE ABOVE CODE ESTABLISHES THE DATA NEEDED TO FILL IN THE MAP#######
#
#
#ESTABLISHING THE VISUALIZATION BELOW 
#visualization keywords for rendering::world::
#
#
#########################################################################

#############################
# STEP 1 PLOT A WORLD MAP ###
#############################

world <- rnaturalearth::ne_countries(scale='medium',returnclass='sf')

ggplot(data=world) + 
  geom_sf() +
  coord_sf()

##################
# STEP 2 CREATE AND PLOT ST_AS_SF MAP STATES COUNTIES 
##################

#turn map state into a shapefile
states <- st_as_sf(map("state",plot=FALSE,fill=TRUE))
#add coordinates of 'centroid' so we can later plot names
states <- cbind(states, st_coordinates(st_centroid(states)))
#label state names as uppercase
states$name <- toupper(states$ID)

#add us so we can use the limits
us <- map_data("county")[,c('long','lat')]
xlimit <- c(max(us$long)+2,min(us$long)-2)
ylimit <- c(max(us$lat)+2,min(us$lat)-2)


ggplot(data=world) + 
  geom_sf() +
  geom_sf(data=states,fill=NA) + 
  geom_label(data=states,aes(X,Y, label=name), size=3) + 
  coord_sf(xlim=xlimit,ylim=ylimit, expand=FALSE)

#add us so we can use the limits
us <- map_data("county")[,c('long','lat')]
xlimit <- c(max(us$long)+2,min(us$long)-2)
ylimit <- c(max(us$lat)+2,min(us$lat)-2)

counties <- st_as_sf(map("county", plot=FALSE, fill=TRUE))

#create new object that combines the counties sf with the ALE dataset
ale.Map  <- left_join(counties,ale.1)
#plot(ale.Map)

#inherits xlimit and ylimit from above; change as desired 
mMap = ggplot(data = world ) + 
  geom_sf() + 
  geom_sf(data=ale.Map,aes(fill="ALE"),color=gray(0.1)) + 
  geom_sf(data=states,fill=NA,size=0.8,color=gray(0.1)) + 
  coord_sf(xlim=xlimit,ylim=ylimit, expand=FALSE) + labs(title='US life Expectancy By County')
#mMap
ale.mMap = tm_shape(ale.Map) + tm_fill("ALE", legend.show = TRUE,legend.is.portrait = FALSE, legend.hist	= TRUE,legend.hist.title = "ALE UNITED STATES", id = "county", palette = "Greens")  +  tm_polygons("county", "orange", style = "cont",  n = 2, alpha = 1 , stretch.palette = TRUE, popup.vars = NA, convert2density = FALSE, midpoint = FALSE)
tmap_mode("view")
#ale.mMap


###### Community Health Status Indicators 

varlist <- setdiff(names(ale.1), "geometry")

runApp(list(
  ui = fluidPage(
    titlePanel("Community Health Status Indicators "),
    sidebarLayout(
      sidebarPanel(
        selectInput("var", label = "Selected Data", choices = varlist, selected = "pop_est_dens")
      ),
      mainPanel(
        leafletOutput("map")
      )
    )
  ),
  server = function(input, output) {
    output$map = renderLeaflet({
      if (packageVersion("tmap") >= 2.0){
        tm <- tm_basemap(leaflet::providers$Stamen.TerrainBackground) +
          tm_shape(ale.Map)  +
          tm_polygons(input$var) +
          tm_tiles(leaflet::providers$Stamen.TonerLabels, group = "Labels")
        
      } else {
        tm <- tm_shape(ale.Map) +
          tm_polygons(input$var) +
          tm_view(basemaps = "Stamen.TerrainBackground")
      }
      
      tmap_leaflet(tm)
    })
  }
))

#Create Shiny object
shinyApp(ui = ui, server = server)