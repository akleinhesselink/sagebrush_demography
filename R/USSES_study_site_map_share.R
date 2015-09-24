######################## make map of sites ##########################
######## Script written by A.K. to map study sites at the 
######## US Sheep Experiment Station (USSES) near Dubois ID
######## Ten sites are mapped on the station property.  
######## Map shows the dominant species at the sites, the site number 
######## and gives an indication of site elevation. 
######## The sites information are GPS positions taken in the field.  
######## The base map is drawn from google maps terrain 
######## The USSES property boundary is from a shapefile of federal  
######## land units downloaded from The National Atlas. 
######## The script loads the spatial data from an RData file for 
######## easier sharing. 

rm(list = ls())

library(ggplot2)
library(ggmap)
library(ggthemes)
library(plyr)
library(rgdal)
library(rgeos)

####### function is required for generating a scale bar 
distHaversine <- function(long, lat){  
  ##### function for calculating distance between Lat Lon points 
  dlong = (long[2] - long[1])*pi/180
  dlat  = (lat[2] - lat[1])*pi/180
  
  # Haversine formula:
  R = 6371;
  a = sin(dlat/2)*sin(dlat/2) + cos(lat[1])*cos(lat[2])*sin(dlong/2)*sin(dlong/2)
  c = 2 * atan2( sqrt(a), sqrt(1-a) )
  d = R * c
  return(d) # in km
}

##### Three pieces of data need to be loaded for the script 
#### 1. "sitesInfo":    dataframe with the site information.
#### 2. "USSES":        spatial polygon datafram containing the 
####                    USSES perimeter information 
load(file='~/Desktop/AK_map_data.RData') #### load in the data 

mlat = mean(sitesInfo$lat)
mlon = mean(sitesInfo$lon)

crs_for_points = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
usses_crs = CRS("+proj=utm +zone=12 ellps=WGS84")
proj4string(USSES) <- crs_for_points #### use lat long crs

USSESperim = data.frame(USSES@polygons[[2]]@Polygons[[1]]@coords)

USSES@data$id = rownames( USSES@data )

USSES.points = fortify(USSES, region = 'id') 

USSES.df = join( USSES.points, USSES@data, by = 'id')

names( USSESperim) <- c('lon', 'lat')
names( USSES.df )[1:2] <- c('lon', 'lat')

################# Use google for base map layer #########################
mapID <- get_map(location=paste(mlat, mlon), source ="google", maptype="terrain", zoom=12)
bb <- attr(mapID, 'bb') #### bb stands for bounding box and gives extent of map 

basemap = ggmap(mapID, extent="device") 

m1 = basemap + geom_polygon( data = USSES.df, aes(x = lon, y = lat ), linetype = 1, alpha = 0.2) + 
  geom_path( data = USSES.df, aes( x = lon, y = lat ))

m1 ##### view base map, outline is land administered by US Sheep Experiment Station

########## Generate scale bar #################################
sbar <- data.frame(lon.start = c(bb$ll.lon + 0.7*(bb$ur.lon - bb$ll.lon)),
                   lon.end = c(bb$ll.lon + 0.90985*(bb$ur.lon - bb$ll.lon)),
                   lat.start = c(bb$ll.lat + 0.12*(bb$ur.lat - bb$ll.lat)),
                   lat.end = c(bb$ll.lat + 0.12*(bb$ur.lat - bb$ll.lat)))

sbar$distance = distHaversine(long = c(sbar$lon.start,sbar$lon.end),
                              lat = c(sbar$lat.start,sbar$lat.end))

ptspermm <- 2.83464567  # need this because geom_text uses mm, and themes use pts. 

###### add scale bar to the base map 
m1 = m1 + geom_segment(data = sbar,
               aes(x = lon.start,
                   xend = lon.end,
                   y = lat.start,
                   yend = lat.end)) +
  geom_text(data = sbar,
            aes(x = (lon.start + lon.end)/2,
                y = lat.start + 0.025*(bb$ur.lat - bb$ll.lat),
                label = paste(format(distance, 
                                     digits = 2,
                                     nsmall = 1), 'km')),
            hjust = 0.5,
            vjust = 0,
            size = 12/ptspermm)  +
  coord_map(projection="mercator",
            xlim=c(bb$ll.lon, bb$ur.lon),
            ylim=c(bb$ll.lat, bb$ur.lat))


m1 ###### view base map with scale bar 

############ Draw map with site points ##############
m1 + geom_point(data = subset( sitesInfo), aes( x = lon, y = lat, color= ele, shape = comp), size = 4) +
  geom_text(data = sitesInfo, aes(label = site_no, x = lon, y = lat, hjust = -0.7, vjust = 1.7), size = 3) + 
  scale_color_continuous(name = 'Elevation (m)') + scale_shape_discrete(name = 'Species')
