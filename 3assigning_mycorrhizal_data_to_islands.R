# 3_myc_islands
# generates islands raster datasets based on input tables and island shapefile

library(raster)
library(reshape)
library(ggplot2)

rm(list = ls())

# standard CRS
newproj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

# create empty raster
r <- raster(resolution=1/6) # this creates a blank 10 min raster
val = 1:ncell(r)
r <-setValues(r,val)

# convert aggregated raster to points
p <- rasterToPoints(r)
pxy <- as.data.frame(p)
coordinates(pxy)= ~ x + y
crs(pxy) <- newproj

# load islands shapefile
islands <- shapefile("islands.shp")

#load ESA Landcover map 
lc <- raster("ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif") ##This map of global landcover at 300m can be downloaded from https://www.esa-landcover-cci.org/
lc10 <- resample(lc, r,method="ngb") 
writeRaster(lc10,filename = "lc10.tif", overwrite=TRUE) 
lc10 <- raster("lc10.tif")

# overlay pts islands and ESA Landcover map
isl.pts <- pxy[islands,]

isl.over <- over(isl.pts,islands)
lc.extract <- extract(lc10,isl.pts)

islands.final <- cbind(data.frame(isl.pts),isl.over,lc.extract)

# load csv tables of myccorhizal plant biomass on islands: curently and potential, i.e. without croplands 
# The content of these tables is equal to the SUuplementary Tables 6 (for current data) and 8 (for the data without croplands) 
current <- read.csv("data_islands_current_state.csv")
without <- read.csv("data_islands_without_croplands.csv")
varNames <- c("current","without")

# loop current and w/o
for (i in 1:2){
  
  # reshape data for joining
  mdata <- melt(get(varNames[i]), id=c("islands")) 
  mdata <- rename(mdata,c("islands" = "CONTINENT"))
  mdata <- rename(mdata,c("variable" = "lc.extract"))
  mdata$lc.extract <- substring(mdata$lc.extract,2)
  
  dat1 <- data.frame(do.call(rbind, strsplit(as.vector.factor(mdata$value), split = ",")))
  names(dat1) <- c("AM","EcM","ErM","NM")
  
  mdata <- cbind(mdata,dat1)
  
  #join data and polygons
  total <- merge(mdata,islands.final,by=c("CONTINENT","lc.extract")) 
  AM.tab <- data.frame(total$x,total$y,total$AM)
  EcM.tab <- data.frame(total$x,total$y,total$EcM)
  ErM.tab <- data.frame(total$x,total$y,total$ErM)
  NM.tab <- data.frame(total$x,total$y,total$NM)
  
  # convert to raster and write out
  AM <- rasterFromXYZ(AM.tab,digits=2)
  writeRaster(AM,file=paste(varNames[i],"_AM.tif",sep=""),overwrite=TRUE)
  EcM <- rasterFromXYZ(EcM.tab,digits=2)
  writeRaster(EcM,file=paste(varNames[i],"_EcM.tif"),overwrite=TRUE)
  ErM <- rasterFromXYZ(ErM.tab,digits=2)
  writeRaster(ErM,file=paste(varNames[i],"_ErM.tif"),overwrite=TRUE)
  NM <- rasterFromXYZ(NM.tab,digits=2)
  writeRaster(NM,file=paste(varNames[i],"_NM.tif"),overwrite=TRUE)
}
