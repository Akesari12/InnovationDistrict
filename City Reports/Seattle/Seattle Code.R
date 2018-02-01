# Set Working Directory and Load Packages
setwd("C:/Users/Owner/Dropbox/Homework/Summer 2017/Google Public Policy Fellowship/Innovation Districts/Data/Seattle")
library("tidyverse")
library("rgdal")
library("sp")
library("maptools")
memory.limit(size = 35000)

# Load + Clean Data

## Zoning Data
seattle_zone <- readOGR(dsn="C:/Users/Owner/Dropbox/Homework/Summer 2017/Google Public Policy Fellowship/Innovation Districts/Data/Seattle/City_of_Seattle_Zoning/WGS84", layer="City_of_Seattle_Zoning")

## Building Permits

### Join Current and Old Permit Data
current_permits <- read.csv("Building_Permits___Current.csv")
old_permits <- read.csv("Building_Permits___Older_than_5_years.csv")
all_permits <- bind_rows(current_permits, old_permits)

### Clean Combined Dataset

all_permits$Application.Date <- as.Date(all_permits$Application.Date, format="%m/%d/%Y")
all_permits$Issue.Date <- as.Date(all_permits$Issue.Date, format="%m/%d/%Y")
all_permits$Final.Date <- as.Date(all_permits$Final.Date, format="%m/%d/%Y")
all_permits$Value <- gsub('\\$', '', all_permits$Value)
all_permits$Value <- as.numeric(all_permits$Value)

## South Lake Union Land Use Permits

sl_union_land_use <- read.csv("South_Lake_Union_Land_Use_Permits.csv")
sl_union_area <- seattle_zone_f %>%
  filter(long >= -122.36, long <= -122.32, lat >= 47.60, lat <=47.65)

sl_union_permits <- all_permits %>%
  filter(Longitude >= -122.36, Longitude <= -122.32, Latitude >= 47.60, Latitude <=47.65, Value > 0)

ggplot() + geom_polygon(data=sl_union_area, aes(long, lat, group=group, fill=CLASS_DESC)) + coord_equal() + geom_point(data = sl_union_permits, aes(Longitude, Latitude, size = Value)) 


# Plotting

## Zoning

### Plain Seattle Zoning
plot(seattle_zone)

### Prep for use in ggplot
seattle_zone_f <- fortify(seattle_zone)
seattle_zone$id <- row.names(seattle_zone)
seattle_zone_f <- left_join(seattle_zone_f, seattle_zone@data)
seattle_zone_f$long <- as.numeric(seattle_zone_f$long)
seattle_zone_f$lat <- as.numeric(seattle_zone_f$lat)

### Ggplot plain zoning
map <- ggplot() + geom_polygon(data = seattle_zone_f, aes(long, lat, group = group, fill = CLASS_DESC)) 

## Permits

### All permits (points plot)
map2 <- map + geom_point(data = all_permits, aes(Longitude, Latitude)) + coord_equal()
all_permits$Year <- format(all_permits$Application.Date, "%Y")
all_permits$Year <- as.numeric(all_permits$Year)

map2 + facet_wrap(~Year)

### Transform Permit Data Into Spatial Points
all_permits <- all_permits %>%
               filter(!is.na(Longitude), !is.na(Latitude)) # Remove NAs

coordinates(all_permits) <- ~ Longitude + Latitude # Turn into spatial points data frame

proj4string(all_permits) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") # Reproject CRS to match "seattle_zone"

permits_zone_info <- over(all_permits, seattle_zone) # Match seattle_zone info to permit data
permits.zone <- spCbind(all_permits, permits_zone_info) # Combine seattle zone info into permit data

## Plotting

# Function to draw a heat map based on number of points in a zone
heatMap <-function(data,shape=NULL,col="blue",main="Sample HeatMap"){
  # Plots a Heat Map of a Polygons Data Frame.  This will 
  # demonstrate density within a finite set of polygons
  #
  # Args:
  #   data:   Spatial Points dataframe
  #   shape:  Polygons Data Frame 
  #
  #
  #   Notes:  This function requires the sp and RColorBrewer
  #           Packages
  #
  #   Beskow: 03/28/11   
  #
  is.installed <- function(mypkg) is.element(mypkg, 
                                             installed.packages()[,1])
  if (is.installed(mypkg="sp")==FALSE)  {
    stop("sp package is not installed")}
  if (is.installed(mypkg="RColorBrewer")==FALSE)  {
    stop("RColorBrewer package is not installed")}
  if (!class(data)=="SpatialPointsDataFrame")  {
    stop("data argument is not SpatialPointsDataFrame")}
  require(sp)
  require(RColorBrewer)
  freq_table<-data.frame(tabulate(over(as(data,"SpatialPoints"),
                                       as(shape,"SpatialPolygons")),nbins=length(shape)))
  names(freq_table)<-"counts"
  
  shape1<-spChFIDs(shape,as.character(1:length(shape)))
  row.names(as(shape1,"data.frame"))
  spdf<-SpatialPolygonsDataFrame(shape1, freq_table, match.ID = TRUE)
  
  rw.colors<-colorRampPalette(c("white",col))
  spplot(spdf, scales = list(draw = TRUE),
         col.regions=rw.colors(max(freq_table)), main=main)
}

### Draw Permit Density Map
heatMap(all_permits, seattle_zone, col="red", main="Seattle Permit Density")

### NC Sample Code

nc <- readOGR(system.file("shapes/", package="maptools"), "sids")

## Working Example that loses Year Info
permits <- as.data.frame(permits.zone)

perms <- permits %>%
         group_by(id, SHAPE_Leng, SHAPE_Area, CLASS_DESC) %>%
         summarise(avg_value = mean(Value))

perms <- merge(seattle_zone, perms, by.x="id", by.y="id")
spplot(perms, z="avg_value")

perms_f <- fortify(perms)
perms_f <- left_join(perms_f, perms@data)
perms_f <- perms_f %>%
           filter(perms_f$avg_value > 0)

ggplot() + geom_polygon(data=perms_f, aes(x=long, y=lat, group=group, fill=avg_value)) 

## Facet with Year  

permits <- as.data.frame(permits.zone)

permsY <-permits %>%
           group_by(id, SHAPE_Leng, SHAPE_Area, CLASS_DESC, Year) %>%
           summarise(avg_value = mean(Value))
permsY <- as.data.frame(permsY)

#permsY <- merge(seattle_zone, permsY, by="id", duplicateGeoms=TRUE)
permsY <- sp::merge(seattle_zone, permsY, by.x="id", by.y="id", duplicateGeoms=TRUE)
#permsY <- merge(permsY, seattle_zone, by.x="id", by.y="id", duplicateGeoms=TRUE)
spplot(permsY, z="avg_value")

permsY_f <- fortify(permsY)
permsY_f <- left_join(permsY_f, permsY@data)
permsY_f <- permsY_f %>%
            filter(permsY_f$avg_value > 0)
ggplot() + geom_polygon(data=permsY_f, aes(x=long, y=lat, group=group, fill=avg_value)) + facet_wrap(~Year)

# Scatterplot

permits <- as.data.frame(permits.zone)

permsYDowntownSouthLake <-permits %>%
                            group_by(id, SHAPE_Leng, SHAPE_Area, CLASS_DESC, Year) %>%
                            filter(CLASS_DESC == "Downtown" | GEO == "SLUC") %>%
                            summarise(avg_value = mean(Value)) 
permsYDowntownSouthLake <- as.data.frame(permsYDowntownSouthLake)

ggplot() + geom_bar(data=permsYDowntownSouthLake, aes(x))

ScatterDownSLU <- permsYDowntownSouthLake %>%
      filter(CLASS_DESC == "Downtown" | CLASS_DESC == "Residential/Commercial") %>%
      group_by(Year, CLASS_DESC) %>%
      summarise(area_avg = sum(avg_value)) %>%
      ggplot() + geom_point(aes(x=Year, y=area_avg)) + geom_smooth(method=lm, aes(x=Year, y=area_avg)) + facet_wrap(~CLASS_DESC) 

