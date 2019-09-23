#This script asssembles maps of mycorhizal vegetation across continents (but not including islands) 
rm(list=ls(all=TRUE))
library(dplyr)
library(tidyr)
library(raster)
require(rgdal)
library(maptools)

#################################################################################################################
## reading data of mycorrhizal biomass fractions per continent per landcover per Bailey ecoregion 
## and transforming wide tables into long ones

#current data
dat_current <- read.csv("Cross_table_final_continents_current.csv",check.names=FALSE)
long_current <- reshape(dat_current, varying = list(names(dat_current[,c(3:40)])), times = names(dat_current[,c(3:40)]), 
                        timevar = "Globcover",
                        direction = "long", sep = ".", v.names = "Mycorrhizae")

longseparated_current <- separate(long_current, Mycorrhizae, sep = ",", into=c("am","ecm","erm","nm"))
longseparated_current <- separate(long_current, Mycorrhizae, sep = ",", into=c("am_old","ecm_old","erm_old","nm_old"))
longseparated_current$id <- with(long_current, paste0(Bailey,"_", Globcover, "_", Continent))

for (i in 1:nrow(longseparated_current))
{
  sum_all_veg<-as.numeric(longseparated_current$am_old[i])+as.numeric(longseparated_current$ecm_old[i])+as.numeric(longseparated_current$erm_old[i])+as.numeric(longseparated_current$nm_old[i])
  if (  (as.numeric(longseparated_current$Globcover[i])<200))
  {
    if ((sum_all_veg<100)&&(sum_all_veg>0)) 
    {
      longseparated_current$am[i]<-(as.numeric(longseparated_current$am_old[i])*100)/sum_all_veg
      longseparated_current$ecm[i]<-(as.numeric(longseparated_current$ecm_old[i])*100)/sum_all_veg
      longseparated_current$erm[i]<-(as.numeric(longseparated_current$erm_old[i])*100)/sum_all_veg
      longseparated_current$nm[i]<-(as.numeric(longseparated_current$nm_old[i])*100)/sum_all_veg
    } 
  } 
}

#data without croplands
dat_without_croplands <- read.csv("Cross_table_final_continents_without_croplands.csv",check.names=FALSE)

long_without_croplands <- reshape(dat_without_croplands, varying = list(names(dat_without_croplands[,c(3:40)])), times = names(dat_current[,c(3:40)]), 
                                  timevar = "Globcover",
                                  direction = "long", sep = ".", v.names = "Mycorrhizae")

longseparated_without_croplands <- separate(long_without_croplands, Mycorrhizae, sep = ",", into=c("am","ecm","erm","nm"))
longseparated_without_croplands <- separate(long_without_croplands, Mycorrhizae, sep = ",", into=c("am_old","ecm_old","erm_old","nm_old"))
longseparated_without_croplands$id <- with(long_without_croplands, paste0(Bailey,"_", Globcover, "_", Continent))


for (i in 1:nrow(longseparated_without_croplands))
{
  sum_all_veg<-as.numeric(longseparated_without_croplands$am_old[i])+as.numeric(longseparated_without_croplands$ecm_old[i])+as.numeric(longseparated_without_croplands$erm_old[i])+as.numeric(longseparated_without_croplands$nm_old[i])
  if (  (as.numeric(longseparated_without_croplands$Globcover[i])<200))
  {
    if ((sum_all_veg<100)&&(sum_all_veg>0)) 
    {
      longseparated_without_croplands$am[i]<-(as.numeric(longseparated_without_croplands$am_old[i])*100)/sum_all_veg
      longseparated_without_croplands$ecm[i]<-(as.numeric(longseparated_without_croplands$ecm_old[i])*100)/sum_all_veg
      longseparated_without_croplands$erm[i]<-(as.numeric(longseparated_without_croplands$erm_old[i])*100)/sum_all_veg
      longseparated_without_croplands$nm[i]<-(as.numeric(longseparated_without_croplands$nm_old[i])*100)/sum_all_veg
    } 
  } 
}

#################################################################################################################
#assembling maps

#Read continents data
c <- readShapePoly("continents_nz")
proj4string(c) <- CRS("+proj=longlat +ellps=WGS84")
c$continents <- 1:nrow(c)

#Read ESA CCL map
glob_new<-raster("ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif") #This map of global landcover at 300m can be downloaded from https://www.esa-landcover-cci.org/

#read re-classified raster of Bailey ecoregions
bai <- raster("wcs_int16_rc6.tif")

#create empty raster for the CCL
rtemp_10min_glob <- raster(resolution=1/6, crs="+proj=longlat +ellps=WGS84")
glob_new<-projectRaster(from=glob_new, to=rtemp_10min_glob, method="ngb")

# rasterize the continents 
rtemp_10min <- raster(resolution=1/6) # this creates a blank 10 min raster
cr <- rasterize(c,rtemp_10min)
levels(cr)[[1]]

# convert aggregated raster to points
memory.limit(size=25000000)
p <- rasterToPoints(bai)
baixy <- as.data.frame(p)
coordinates(baixy)= ~ x + y

# extract data at points from bailey and continents
globcover_2015 <- extract(glob_new,baixy)
continents <- extract(cr,baixy)

# add myc and continents column back to data
epts <- cbind(p,globcover_2015,continents)
Overlay_Bailey10min_ESACCL2015 <- right_join(as.data.frame(c),as.data.frame(epts),by="continents")
Overlay_Bailey10min_ESACCL2015 $id <- paste0(final$wcs_int16_rc6, sep="_", final$globcover, "_", final$CONTINENT)

#Building maps
codes_current <- longseparated_current
codes_without_croplands <- longseparated_without_croplands

codes_current <- codes_current[,c("id","am","ecm","erm","nm")]
codes__without_croplands <- codes_without_croplands[,c("id","am","ecm","erm","nm")]

# create id in df that can be linked with id values from the codes table
df_current <- Overlay_Bailey10min_ESACCL2015 
df_without_croplands <- Overlay_Bailey10min_ESACCL2015 
df_current$X <- NULL
df_without_croplands$X <- NULL

# combine the datasets of Bailey_continent overlay and the mycorrhizal tables
dfm_current <- left_join(df_current,codes_current, by="id")
dfm_without_croplands <- left_join(df_without_croplands,codes_without_croplands, by="id")

l_current<-as.factor(dfm_current$id)
l_without_croplands<-as.factor(dfm_without_croplands$id)

coordinates(dfm_current) <- ~ x + y
coordinates(dfm_without_croplands) <- ~ x + y

# coerce to SpatialPixelsDataFrame
gridded(dfm_current) <- TRUE
gridded(dfm_without_croplands) <- TRUE

# coerce to raster
rasterAM_current <- raster(dfm_current, "am") # AM only
rasterAM_without_croplands <- raster(dfm_without_croplands, "am") # AM only
rasterEM_current <- raster(dfm_current, "ecm") # EM only
rasterEM_without_croplands <- raster(dfm_without_croplands, "ecm") # EM only
rasterER_current <- raster(dfm_current, "erm") # Ericoid only
rasterER_without_croplands <- raster(dfm_without_croplands, "erm") # Ericoid only
rasterNM_current <- raster(dfm_current, "nm") # Non-mycorrhizal only
rasterNM_without_croplands <- raster(dfm_without_croplands, "nm") # Ericoid only

#Save Rasters
writeRaster(rasterAM_current,"Continents_MycDistrAM_current.tif",overwrite=TRUE)
writeRaster(rasterAM_without_croplands,"Continents_MycDistrAM_without_croplands.tif",overwrite=TRUE)
writeRaster(rasterEM_current,"Continents_MycDistrEM_current.tif",overwrite=TRUE)
writeRaster(rasterEM_without_croplands,"Continents_MycDistrEM_without_croplands.tif",overwrite=TRUE)
writeRaster(rasterER_current,"Continents_MycDistrER_current.tif",overwrite=TRUE)
writeRaster(rasterER_without_croplands,"Continents_MycDistrER_without_croplands.tif",overwrite=TRUE)
writeRaster(rasterNM_current,"Continents_MycDistrNM_current.tif",overwrite=TRUE)
writeRaster(rasterNM_without_croplands,"Continents_MycDistrNM_without_croplands.tif",overwrite=TRUE)
