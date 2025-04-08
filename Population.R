library(tidyverse)
library(raster)
library(ggspatial)
library(sf)
Nepal<-st_read("../Data/Nepal_fug.shp")
Pop<-terra::rast("../Data/Covariates/Population/landscan-global-2022.tif")


Pop<-terra::crop(Pop,Nepal) #clip the SAing box
Pop<-terra::mask(Pop,Nepal) #NA everything outside .shp


raster::plot(Pop)

range(Pop[],na.rm=T)

Population2022<-terra::extract(Pop,Nepal,fun="sum",na.rm=T)
Nepal$Population2022<-Population2022$`landscan-global-2022`


ggplot(Nepal)+
  geom_sf(aes(fill=Population2022),color=alpha("white",0.1))+
  scale_fill_viridis_c(option="B", direction = -1,
                       name="",labels=scales::comma)+
  ggtitle("Population2022")+
  #scale_y_continuous(breaks=c(-24,-28,-32))+
  xlab("")+ylab("")+
  #annotation_scale(location = "br", width_hint = 0.5,text_cex = 1.2)+
  #scale_fill_manual(values =c("white","green"))+
  theme_bw()+MapTheme



st_write(Nepal, "../Data/Covariates/Population/Population2022_by_VDC.shp",append=F )


###Now do 2015
Nepal<-st_read("../Data/Nepal_fug.shp")
Pop<-terra::rast("../Data/Covariates/Population/landscan-global-2015.tif")


Pop<-terra::crop(Pop,Nepal) #clip the SAing box
Pop<-terra::mask(Pop,Nepal) #NA everything outside .shp


raster::plot(Pop)

range(Pop[],na.rm=T)

Population2015<-terra::extract(Pop,Nepal,fun="sum",na.rm=T)
Nepal$Population2015<-Population2022$`landscan-global-2015`


ggplot(Nepal)+
  geom_sf(aes(fill=Population2015),color=alpha("white",0.1))+
  scale_fill_viridis_c(option="B", direction = -1,
                       name="",labels=scales::comma)+
  ggtitle("Population2015")+
  #scale_y_continuous(breaks=c(-24,-28,-32))+
  xlab("")+ylab("")+
  #annotation_scale(location = "br", width_hint = 0.5,text_cex = 1.2)+
  #scale_fill_manual(values =c("white","green"))+
  theme_bw()+MapTheme



st_write(Nepal, "../Data/Covariates/Population/Population2015_by_VDC.shp",append=F )
