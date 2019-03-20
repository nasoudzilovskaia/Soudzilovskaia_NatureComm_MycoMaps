## plot global AM EcM ErM NM Difference plots in robinson for publication
## The source data are the maps of current mycorrhizal distributions and mycorrhizal distributions without croplands 
## available in folders "Fig1_Maps_Mycorrhizal_vegetation_current" and "Suppl_Fig4_Maps_Mycorrhizal_vegetation_without_croplands"

library(ggplot2)
library(rgdal)
require(cowplot)
library(RColorBrewer)
library(colorspace)
library(raster)

rm(list = ls(all = TRUE))


c <- shapefile("continents")
index <- c$CONTINENT != "Antartica"
country <- c[index,]
country <- spTransform(country, CRS("+proj=robin"))

# CHANGE BETWEEN "current" AND "wo"
#for (i in c("current","wo")) {

# load rasters ofcurrent mycorrhizal distributions 
AMc <- raster("MycDistrAM_current.tif");#AMc[AMc == 0] <- NA
EMc <- raster("MycDistrEM_current.tif");#EMc[EMc == 0] <- NA
ERc <- raster("MycDistrER_current.tif");#ERc[ERc == 0] <- NA
NMc <- raster("MycDistrNM_current.tif");#ERc[ERc == 0] <- NA

# load rasters ofmycorrhizal distributions without croplands
AMw <- raster("MycDistrAM_without_croplands.tif");#AMw[AMw == 0] <- NA
EMw <- raster("MycDistrEM_without_croplands.tif");#EMw[EMw == 0] <- NA
ERw <- raster("MycDistrER_without_croplands.tif");#ERw[ERw == 0] <- NA
NMw <- raster("MycDistrNM_without_croplands.tif");#ERw[ERw == 0] <- NA


# Difference Maps
AM <- (AMc - AMw)
EM <- (EMc - EMw)
ER <- (ERc - ERw)
NM <- (NMc - NMw)

#reproject to robin
crs(AM) <- "+proj=longlat"
AMp <- projectRaster(AM,crs="+proj=robin",over=T)
AM.df = as.data.frame(AMp,xy=TRUE)
colnames(AM.df) <- c('x','y', 'AM')

crs(EM) <- "+proj=longlat"
EMp <- projectRaster(EM,crs="+proj=robin",over=T)
EM.df = as.data.frame(EMp,xy=TRUE)
colnames(EM.df) <- c('x','y', 'EM')

crs(ER) <- "+proj=longlat"
ERp <- projectRaster(ER,crs="+proj=robin",over=T)
ER.df = as.data.frame(ERp,xy=TRUE)
colnames(ER.df) <- c('x','y', 'ER')

##########CHANGED TO NM !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
crs(NM) <- "+proj=longlat"
NMp <- projectRaster(NM,crs="+proj=robin",over=T)
NM.df = as.data.frame(NMp,xy=TRUE)
colnames(NM.df) <- c('x','y', 'NM')

# map the bbox
bbox <- shapefile("ne_110m_wgs84_bounding_box.shp") 
bbox_df<- fortify(bbox)
bbox_robin <- spTransform(bbox, CRS("+proj=robin"))  # reproject bounding box
bbox_robin_df <- fortify(bbox_robin)

# graticule (Robin)
grat <- shapefile("ne_110m_graticules_30.shp") 
grat_df <- fortify(grat)
grat_robin <- spTransform(grat, CRS("+proj=robin"))  # reproject graticule
grat_df_robin <- fortify(grat_robin)

# create a blank ggplot theme
theme_opts <- list(theme(panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank(),
                         panel.background = element_blank(),
                         plot.background = element_blank(),
                         legend.position="none",
                         panel.border = element_blank(),
                         axis.line = element_blank(),
                         axis.text.x = element_blank(),
                         axis.text.y = element_blank(),
                         axis.ticks = element_blank(),
                         axis.title.x = element_blank(),
                         axis.title.y = element_blank(),
                         plot.title = element_text(size=15)))

#colr <- colorRampPalette( brewer.pal( 9, "Reds" ) )
colg <- colorRampPalette( brewer.pal( 9, "Greens" ) )
colb <- colorRampPalette( brewer.pal( 9, "Blues" ) )

#t.col <- scale_fill_brewer(palette = "greens")

colr <- colorRampPalette(brewer.pal(255, "PRGn"))
#t.colg <- colorRampPalette(brewer.pal(255, "BuGn"))
#t.colb <- colorRampPalette(brewer.pal(255, "PuBu"))

p1 <- ggplot()+ 
  geom_polygon(data=bbox_robin_df, aes(x=long, y=lat), colour="grey50", fill="transparent", size = 0.21) +
  geom_raster(data=AM.df,aes(x,y,fill=AM)) +
  scale_fill_gradientn(colors=colr(255),na.value="transparent",limits = c(-100,100), breaks=c(-100,0,100))+
  geom_path(data=grat_df_robin, aes(long,lat,group=group),linetype="dashed", color="grey50", size = 0.21) +
  geom_path(data=country, aes(long,lat,group=group),linetype="solid", color="grey50", size = 0.05) +
  coord_equal() + 
  ggtitle('a') + theme(plot.title=element_text(hjust=0)) +
  theme_opts 

p2 <- ggplot()+ 
  geom_polygon(data=bbox_robin_df, aes(x=long, y=lat), colour="grey50", fill="transparent", size = 0.21) +
  geom_raster(data=EM.df,aes(x,y,fill=EM)) +
  scale_fill_gradientn(colors=colr(255),na.value="transparent",limits = c(-100,100), breaks=c(-100,0,100))+
  geom_path(data=grat_df_robin, aes(long,lat,group=group),linetype="dashed", color="grey50", size = 0.21) +
  geom_path(data=country, aes(long,lat,group=group),linetype="solid", color="grey50", size = 0.05) +
  coord_equal() + 
  ggtitle('b') + theme(plot.title=element_text(hjust=0)) +
  theme_opts 

p3 <- ggplot()+ 
  geom_polygon(data=bbox_robin_df, aes(x=long, y=lat), colour="grey50", fill="transparent", size = 0.21) +
  geom_raster(data=ER.df,aes(x,y,fill=ER)) +
  scale_fill_gradientn(colors=colr(255),na.value="transparent",limits = c(-100,100), breaks=c(-100,0,100))+
  geom_path(data=grat_df_robin, aes(long,lat,group=group),linetype="dashed", color="grey50", size = 0.21) +
  geom_path(data=country, aes(long,lat,group=group),linetype="solid", color="grey50", size = 0.05) +
  coord_equal() + 
  ggtitle('c') + theme(plot.title=element_text(hjust=0)) +
  theme_opts

p4 <- ggplot()+ 
  geom_polygon(data=bbox_robin_df, aes(x=long, y=lat), colour="grey50", fill="transparent", size = 0.21) +
  geom_raster(data=NM.df,aes(x,y,fill=NM)) +
  scale_fill_gradientn(colors=colr(255),na.value="transparent",limits = c(-100,100), breaks=c(-100,0,100))+
  geom_path(data=grat_df_robin, aes(long,lat,group=group),linetype="dashed", color="grey50", size = 0.21) +
  geom_path(data=country, aes(long,lat,group=group),linetype="solid", color="grey50", size = 0.05) +
  coord_equal() + 
  ggtitle('d') + theme(plot.title=element_text(hjust=0)) +
  theme_opts

prow <- plot_grid( p1, # + theme(legend.position="bottom",legend.title=element_blank()),
                   p2, #"cm")),# + theme(legend.position="bottom",legend.title=element_blank()),
                   p3, # + theme(legend.position="bottom",legend.title=element_blank()),
                   p4 #"cm"))#,

)

legend_b <- get_legend(p3 + theme(legend.position="bottom",legend.title=element_blank(),legend.justification="center",
                                  legend.key.width = unit(1, "cm"),legend.margin=margin(0,0,4,0),legend.box.margin=margin(10,10,10,10),legend.key.height = unit(0.5, "cm")))

plot_grid(prow, rel_heights = c(1, .1))
plot_grid(prow,legend_b,ncol=1,nrow=3,rel_heights = c(1, .05,.01))

# change name from current to wo
ggsave(paste("maps_all_diff.png",sep=""),   dpi=300)
