#Script 4_merge_isl_cont.R
#merges maps of mycorrhiza on continents and on islands

library(raster)

rm(list = ls())

# current
# islands; These are files created by the script "3_myc_islands.R"
AM <- raster("current_AM.tif")
EcM <- raster("current_EcM.tif")
ErM <- raster("current_ErM.tif")
NM <- raster("current_NM.tif")

# land; These are files created by the script "1_continent_myco_maps.R"
MycDistrAM_current <- raster("Continents_MycDistrAM_current.tif")
MycDistrEM_current <- raster("Continents_MycDistrEM_current.tif")
MycDistrER_current <- raster("Continents_MycDistrER_current.tif")
MycDistrNM_current <- raster("Continents_MycDistrNM_current..tif")

# merged
MycDistrAM_current_all <- merge(MycDistrAM_current,AM)
MycDistrEM_current_all <- merge(MycDistrEM_current,EcM)
MycDistrER_current_all <- merge(MycDistrER_current,ErM)
MycDistrNM_current_all <- merge(MycDistrNM_current,NM)

# export
writeRaster(MycDistrAM_current_all,file="MycDistrAM_current.tif",overwrite=TRUE)
writeRaster(MycDistrEM_current_all,file="MycDistrEM_current.tif",overwrite=TRUE)
writeRaster(MycDistrER_current_all,file="MycDistrER_current.tif",overwrite=TRUE)
writeRaster(MycDistrNM_current_all,file="MycDistrNM_current.tif",overwrite=TRUE)

# without cropland
# islands; These are files created by the script "myc_islands.R"
AMwo <- raster("without_AM.tif")
EcMwo <- raster("without_EcM.tif")
ErMwo <- raster("without_ErM.tif")
NMwo <- raster("without_NM.tif")

# land; These are files created by the script "continent_maps.R"
MycDistrAM_wo <- raster("Continents_MycDistrAM_without_croplands.tif")
MycDistrEM_wo <- raster("Continents_MycDistrEM_without_croplands.tif")
MycDistrER_wo <- raster("Continents_MycDistrER_without_croplands.tif")
MycDistrNM_wo <- raster("Continents_MycDistrNM_without_croplands.tif")

#merged
MycDistrAM_wo_all <- merge(MycDistrAM_wo,AMwo)
MycDistrEM_wo_all <- merge(MycDistrEM_wo,EcMwo)
MycDistrER_wo_all <- merge(MycDistrER_wo,ErMwo)
MycDistrNM_wo_all <- merge(MycDistrNM_wo,NMwo)

writeRaster(MycDistrAM_wo_all,file="MycDistrAM_without_croplands.tif",overwrite=TRUE)
writeRaster(MycDistrEM_wo_all,file="MycDistrEM_without_croplands.tif",overwrite=TRUE)
writeRaster(MycDistrER_wo_all,file="MycDistrER_without_croplands.tif",overwrite=TRUE)
writeRaster(MycDistrNM_wo_all,file="MycDistrNM_without_croplands.tif",overwrite=TRUE)

