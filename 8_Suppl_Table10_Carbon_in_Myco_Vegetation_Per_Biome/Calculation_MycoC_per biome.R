
library(raster)
#read rasters carbon in mycorrhizal vegetation (folder "Fig2_Maps_Carbon_in_Myco_vegetation")
C_AM_ras<-raster("C_AM.tif")
C_EM_ras<-raster("C_EM.tif")
C_ER_ras<-raster("C_ER.tif")
C_NM_ras<-raster("C_NM.tif")

biomes<-raster ("biome.tif")

crs(biomes) <-"+proj=longlat +ellps=WGS84"
crs(C_AM_ras) <-"+proj=longlat +ellps=WGS84"
biomes_25 <- projectRaster(biomes,to=C_AM_ras,method='bilinear')

#Biome 1 Tropical and Subtropical Moist Broadleaf Forests
AM_masked_TrSubtr_Moist_Broadleaf_Forests<-mask(C_AM_ras, biomes_25, inverse=TRUE,maskvalue=1,na.rm=T) 
C_AM_masked_TrSubtr_Moist_Broadleaf_Forests<-cellStats(AM_masked_TrSubtr_Moist_Broadleaf_Forests,"sum", na.rm=T)

EM_masked_TrSubtr_Moist_Broadleaf_Forests<-mask(C_EM_ras, biomes_25, inverse=TRUE,maskvalue=1,na.rm=T) 
C_EM_masked_TrSubtr_Moist_Broadleaf_Forests<-cellStats(EM_masked_TrSubtr_Moist_Broadleaf_Forests,"sum", na.rm=T)

ER_masked_TrSubtr_Moist_Broadleaf_Forests<-mask(C_ER_ras, biomes_25, inverse=TRUE,maskvalue=1,na.rm=T) 
C_ER_masked_TrSubtr_Moist_Broadleaf_Forests<-cellStats(ER_masked_TrSubtr_Moist_Broadleaf_Forests,"sum", na.rm=T)

NM_masked_TrSubtr_Moist_Broadleaf_Forests<-mask(C_NM_ras, biomes_25, inverse=TRUE,maskvalue=1,na.rm=T) 
C_NM_masked_TrSubtr_Moist_Broadleaf_Forests<-cellStats(NM_masked_TrSubtr_Moist_Broadleaf_Forests,"sum", na.rm=T)

totalC_myco_TrSubtr_Moist_Broadleaf_Forests=C_AM_masked_TrSubtr_Moist_Broadleaf_Forests+
  C_EM_masked_TrSubtr_Moist_Broadleaf_Forests+
  C_ER_masked_TrSubtr_Moist_Broadleaf_Forests+
  C_NM_masked_TrSubtr_Moist_Broadleaf_Forests

fraction_percAM=100*C_AM_masked_TrSubtr_Moist_Broadleaf_Forests/totalC_myco_TrSubtr_Moist_Broadleaf_Forests
fraction_percEM=100*C_EM_masked_TrSubtr_Moist_Broadleaf_Forests/totalC_myco_TrSubtr_Moist_Broadleaf_Forests
fraction_percER=100*C_ER_masked_TrSubtr_Moist_Broadleaf_Forests/totalC_myco_TrSubtr_Moist_Broadleaf_Forests
fraction_percNM=100*C_NM_masked_TrSubtr_Moist_Broadleaf_Forests/totalC_myco_TrSubtr_Moist_Broadleaf_Forests

###Biome 2 Tropical and subtropical Dry Broad Leaf forests
AM_masked_TrSubtr_Dry_Broadleaf_Forests<-mask(C_AM_ras, biomes_25, inverse=TRUE,maskvalue=2,na.rm=T) 
C_AM_masked_TrSubtr_Dry_Broadleaf_Forests<-cellStats(AM_masked_TrSubtr_Dry_Broadleaf_Forests,"sum", na.rm=T)

EM_masked_TrSubtr_Dry_Broadleaf_Forests<-mask(C_EM_ras, biomes_25, inverse=TRUE,maskvalue=2,na.rm=T) 
C_EM_masked_TrSubtr_Dry_Broadleaf_Forests<-cellStats(EM_masked_TrSubtr_Dry_Broadleaf_Forests,"sum", na.rm=T)

ER_masked_TrSubtr_Dry_Broadleaf_Forests<-mask(C_ER_ras, biomes_25, inverse=TRUE,maskvalue=2,na.rm=T) 
C_ER_masked_TrSubtr_Dry_Broadleaf_Forests<-cellStats(ER_masked_TrSubtr_Dry_Broadleaf_Forests,"sum", na.rm=T)

NM_masked_TrSubtr_Dry_Broadleaf_Forests<-mask(C_NM_ras, biomes_25, inverse=TRUE,maskvalue=2,na.rm=T) 
C_NM_masked_TrSubtr_Dry_Broadleaf_Forests<-cellStats(NM_masked_TrSubtr_Dry_Broadleaf_Forests,"sum", na.rm=T)

totalC_myco_TrSubtr_Dry_Broadleaf_Forests=C_AM_masked_TrSubtr_Dry_Broadleaf_Forests+
  C_EM_masked_TrSubtr_Dry_Broadleaf_Forests+
  C_ER_masked_TrSubtr_Dry_Broadleaf_Forests+
  C_NM_masked_TrSubtr_Dry_Broadleaf_Forests

fraction_percAM=100*C_AM_masked_TrSubtr_Dry_Broadleaf_Forests/totalC_myco_TrSubtr_Dry_Broadleaf_Forests
fraction_percEM=100*C_EM_masked_TrSubtr_Dry_Broadleaf_Forests/totalC_myco_TrSubtr_Dry_Broadleaf_Forests
fraction_percER=100*C_ER_masked_TrSubtr_Dry_Broadleaf_Forests/totalC_myco_TrSubtr_Dry_Broadleaf_Forests
fraction_percNM=100*C_NM_masked_TrSubtr_Dry_Broadleaf_Forests/totalC_myco_TrSubtr_Dry_Broadleaf_Forests

#Biomes 3 and 5 Coniferous forests

biomes_25[(biomes_25 == 3 | biomes_25 == 5)] <- 35

AM_masked_coniferous<-mask(C_AM_ras, biomes_25, inverse=TRUE,maskvalue=35) 
C_AM_masked_coniferous<-cellStats(AM_masked_coniferous,"sum", na.rm=T)

EM_masked_coniferous<-mask(C_EM_ras, biomes_25, inverse=TRUE,maskvalue=35) 
C_EM_masked_coniferous<-cellStats(EM_masked_coniferous,"sum", na.rm=T)

ER_masked_coniferous<-mask(C_ER_ras, biomes_25, inverse=TRUE,maskvalue=35) 
C_ER_masked_coniferous<-cellStats(ER_masked_coniferous,"sum", na.rm=T)

NM_masked_coniferous<-mask(C_NM_ras, biomes_25, inverse=TRUE,maskvalue=35) 
C_NM_masked_coniferous<-cellStats(NM_masked_coniferous,"sum", na.rm=T)

totalC_myco_coniferous=C_AM_masked_coniferous+
  C_EM_masked_coniferous+
  C_ER_masked_coniferous+
  C_NM_masked_coniferous

fraction_percAM=100*C_AM_masked_coniferous/totalC_myco_coniferous
fraction_percEM=100*C_EM_masked_coniferous/totalC_myco_coniferous
fraction_percER=100*C_ER_masked_coniferous/totalC_myco_coniferous
fraction_percNM=100*C_NM_masked_coniferous/totalC_myco_coniferous

###Biome 4 Temperate Broadleaf and Mixed Forests
AM_masked_Temperate<-mask(C_AM_ras, biomes_25, inverse=TRUE,maskvalue=4) 
C_AM_masked_Temperate<-cellStats(AM_masked_Temperate,"sum", na.rm=T)

EM_masked_Temperate<-mask(C_EM_ras, biomes_25, inverse=TRUE,maskvalue=4) 
C_EM_masked_Temperate<-cellStats(EM_masked_Temperate,"sum", na.rm=T)

ER_masked_Temperate<-mask(C_ER_ras, biomes_25, inverse=TRUE,maskvalue=4) 
C_ER_masked_Temperate<-cellStats(ER_masked_Temperate,"sum", na.rm=T)

NM_masked_Temperate<-mask(C_NM_ras, biomes_25, inverse=TRUE,maskvalue=4) 
C_NM_masked_Temperate<-cellStats(NM_masked_Temperate,"sum", na.rm=T)

totalC_myco_Temperate=C_AM_masked_Temperate+
  C_EM_masked_Temperate+
  C_ER_masked_Temperate+
  C_NM_masked_Temperate

fraction_percAM=100*C_AM_masked_Temperate/totalC_myco_Temperate
fraction_percEM=100*C_EM_masked_Temperate/totalC_myco_Temperate
fraction_percER=100*C_ER_masked_Temperate/totalC_myco_Temperate
fraction_percNM=100*C_NM_masked_Temperate/totalC_myco_Temperate

###Biome 5 Boreal forests and Taiga
AM_masked_Boreal<-mask(C_AM_ras, biomes_25, inverse=TRUE,maskvalue=6) 
C_AM_masked_Boreal<-cellStats(AM_masked_Boreal,"sum", na.rm=T)

EM_masked_Boreal<-mask(C_EM_ras, biomes_25, inverse=TRUE,maskvalue=6) 
C_EM_masked_Boreal<-cellStats(EM_masked_Boreal,"sum", na.rm=T)

ER_masked_Boreal<-mask(C_ER_ras, biomes_25, inverse=TRUE,maskvalue=6) 
C_ER_masked_Boreal<-cellStats(ER_masked_Boreal,"sum", na.rm=T)

NM_masked_Boreal<-mask(C_NM_ras, biomes_25, inverse=TRUE,maskvalue=6) 
C_NM_masked_Boreal<-cellStats(NM_masked_Boreal,"sum", na.rm=T)

totalC_myco_Boreal=C_AM_masked_Boreal+
  C_EM_masked_Boreal+
  C_ER_masked_Boreal+
  C_NM_masked_Boreal

fraction_percAM=100*C_AM_masked_Boreal/totalC_myco_Boreal
fraction_percEM=100*C_EM_masked_Boreal/totalC_myco_Boreal
fraction_percER=100*C_ER_masked_Boreal/totalC_myco_Boreal
fraction_percNM=100*C_NM_masked_Boreal/totalC_myco_Boreal

##Biome 10 Montane Grasslands and Shrublands
AM_masked_Mountain<-mask(C_AM_ras, biomes_25, inverse=TRUE,maskvalue=10) 
C_AM_masked_Mountain<-cellStats(AM_masked_Mountain,"sum", na.rm=T)

EM_masked_Mountain<-mask(C_EM_ras, biomes_25, inverse=TRUE,maskvalue=10) 
C_EM_masked_Mountain<-cellStats(EM_masked_Mountain,"sum", na.rm=T)

ER_masked_Mountain<-mask(C_ER_ras, biomes_25, inverse=TRUE,maskvalue=10) 
C_ER_masked_Mountain<-cellStats(ER_masked_Mountain,"sum", na.rm=T)

NM_masked_Mountain<-mask(C_NM_ras, biomes_25, inverse=TRUE,maskvalue=10) 
C_NM_masked_Mountain<-cellStats(NM_masked_Mountain,"sum", na.rm=T)

totalC_myco_Mountain=C_AM_masked_Mountain+
  C_EM_masked_Mountain+
  C_ER_masked_Mountain+
  C_NM_masked_Mountain

fraction_percAM=100*C_AM_masked_Mountain/totalC_myco_Mountain
fraction_percEM=100*C_EM_masked_Mountain/totalC_myco_Mountain
fraction_percER=100*C_ER_masked_Mountain/totalC_myco_Mountain
fraction_percNM=100*C_NM_masked_Mountain/totalC_myco_Mountain


#Biomes 7,8.9 grasslands 

biomes_25[(biomes_25 == 7 | biomes_25 == 8 | biomes_25 == 9)] <- 789

AM_masked_grasslands<-mask(C_AM_ras, biomes_25, inverse=TRUE,maskvalue=789) 
C_AM_masked_grasslands<-cellStats(AM_masked_grasslands,"sum", na.rm=T)

EM_masked_grasslands<-mask(C_EM_ras, biomes_25, inverse=TRUE,maskvalue=789) 
C_EM_masked_grasslands<-cellStats(EM_masked_grasslands,"sum", na.rm=T)

ER_masked_grasslands<-mask(C_ER_ras, biomes_25, inverse=TRUE,maskvalue=789) 
C_ER_masked_grasslands<-cellStats(ER_masked_grasslands,"sum", na.rm=T)

NM_masked_grasslands<-mask(C_NM_ras, biomes_25, inverse=TRUE,maskvalue=789) 
C_NM_masked_grasslands<-cellStats(NM_masked_grasslands,"sum", na.rm=T)

totalC_myco_grasslands=C_AM_masked_grasslands+
  C_EM_masked_grasslands+
  C_ER_masked_grasslands+
  C_NM_masked_grasslands

fraction_percAM=100*C_AM_masked_grasslands/totalC_myco_grasslands
fraction_percEM=100*C_EM_masked_grasslands/totalC_myco_grasslands
fraction_percER=100*C_ER_masked_grasslands/totalC_myco_grasslands
fraction_percNM=100*C_NM_masked_grasslands/totalC_myco_grasslands

#Biome 11 Tundra 

AM_masked_tundra<-mask(C_AM_ras, biomes_25, inverse=TRUE,maskvalue=11) 
C_AM_masked_tundra<-cellStats(AM_masked_tundra,"sum", na.rm=T)

EM_masked_tundra<-mask(C_EM_ras, biomes_25, inverse=TRUE,maskvalue=11) 
C_EM_masked_tundra<-cellStats(EM_masked_tundra,"sum", na.rm=T)

ER_masked_tundra<-mask(C_ER_ras, biomes_25, inverse=TRUE,maskvalue=11) 
C_ER_masked_tundra<-cellStats(ER_masked_tundra,"sum", na.rm=T)

NM_masked_tundra<-mask(C_NM_ras, biomes_25, inverse=TRUE,maskvalue=11) 
C_NM_masked_tundra<-cellStats(NM_masked_tundra,"sum", na.rm=T)

totalC_myco_tundra=C_AM_masked_tundra+
  C_EM_masked_tundra+
  C_ER_masked_tundra+
  C_NM_masked_tundra

fraction_percAM=100*C_AM_masked_tundra/totalC_myco_tundra
fraction_percEM=100*C_EM_masked_tundra/totalC_myco_tundra
fraction_percER=100*C_ER_masked_tundra/totalC_myco_tundra
fraction_percNM=100*C_NM_masked_tundra/totalC_myco_tundra
