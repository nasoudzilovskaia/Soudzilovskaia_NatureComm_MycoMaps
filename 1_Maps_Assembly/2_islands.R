#Script 2_islands.R
# produces a shapefile of islands
library(sp)
library(raster)
library(plyr)

rm(list = ls())

c <- shapefile("continents_nz")

bailey <- raster("wcs_int16_rc6.tif")
lkup <- read.csv("islands.csv")
lkup <- lkup[-3]
lkup <- as.data.frame(lkup)

# load GAUL
#These files are provided by FAO Global Administrative Unit Layers database, and coudl be downloaded at 
#http://www.fao.org/geonetwork/srv/en/
gaul1 <- shapefile('g2015_2014_1.shp')
gaul2 <- shapefile('g2015_2014_2.shp')

# select countries of interest
isl.list <- c("Baltic Sea islands",
              "Gotlands Laen", 
              "Saare maa", 
              "Oland", 
              "Lolland", 
              "Hiiu maa", 
              "Rugen", 
              "Åaland main island", 
              "Bornholm", 
              "Kimitoon", 
              "Falster", 
              "Usedom", 
              "Wolin",
              "Hailuoto",
              "Fehmarn", 
              "Varmdo", 
              "Mediterranean islands",
              "Notio Aigaio",
              "Voreio Aigaio",
              "Ionioi Nisoi",
              "Malta",
              "Euboea", 		 
              "Illes Balears", 	
              "Lesbos",
              "Rhodes",	
              "Chios", 	
              "Minorca", 	
              "Kefalonia", 
              "Corfu",	
              "Ibiza", 	
              "Djerba", 	 
              "Limnos", 		 
              "Samos", 
              "Zakynthos",
              "Subarctic islands",
              "Subantarctic islands",
              "Kuril islands",
              "Aleutians East",
              "Aleutians West",
              "New Caledonia",
              "Bahamas",
              "Barbadoes",
              "Martinique",
              "Grenada",
              "Guadeloupe",
              "Antigua and Barbuda",
              "Bioko",
              "Galapagos",
              "United States Virgin Islands",
              "Canarias",
              "Shetland",
              "Faroe Islands",
              "Saint Helena",
              "Maldives",
              "Micronesia (Federated States of)",
              "Melanesia",
              "Polynesia",
              "Madeira Islands",
              "Andaman",
              "Nicobar",
              "Sundaland islands",
              "Azores",
              "Mascarene",
              "Cape Verde",
              "Seychelles",
              "Samoa",
              "Sao Tome and Principe",
              "Annobon",
              "Comoros",
              "Socotra",
              "Hawaii",
              "Fiji",
              "Solomon Islands",
              "Vanuatu",
              "Antarctica",
              "Falkland",
              "Réunion",
              "Mauritius",
              "South Sandwich",
              "Antarctic Territories",
              "Tonga")
index <- NULL
islands0 <- gaul1[index,]

for (i in 1:length(isl.list)){
  index <- grep(isl.list[i],gaul1$ADM0_NAME)
  temp <- gaul1[index,]
  islands0 <- rbind(islands0,temp, makeUniqueIDs = TRUE)
  temp <- islands0$Shape_Area < 1000
  islands0 <- islands0[temp,]
}

