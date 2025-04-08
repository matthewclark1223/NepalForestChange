library(tidyverse)
library(raster)
library(ggspatial)
library(sf)
Nepal<-st_read("../Data/Nepal_fug.shp")
Slope<-terra::rast("../Data/Covariates/Slope/NepalSlope.tif")

#NepalSub<-Nepal%>%filter(NAME_4=="Banganga")
#ggplot(NepalSub)+geom_sf()
Slope<-terra::crop(Slope,Nepal) #clip the SAing box
Slope<-terra::mask(Slope,Nepal) #NA everything outside .shp


raster::plot(Slope)

#range(Slope[],na.rm=T)

Slope2022<-terra::extract(Slope,Nepal,fun="median",na.rm=T)
Nepal$Slope2022<-Slope2022$NepalSlope


ggplot(Nepal)+
  geom_sf(aes(fill=Slope2022),color=alpha("white",0.1))+
  scale_fill_viridis_c(option="B", direction = -1,
                       name="",labels=scales::comma)+
  ggtitle("Slope2022")+
  #scale_y_continuous(breaks=c(-24,-28,-32))+
  xlab("")+ylab("")+
  #annotation_scale(location = "br", width_hint = 0.5,text_cex = 1.2)+
  #scale_fill_manual(values =c("white","green"))+
  theme_bw()+MapTheme



st_write(Nepal, "../Data/Covariates/Slope/Slope_by_VDC.shp")
