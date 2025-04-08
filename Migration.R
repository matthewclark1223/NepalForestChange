library(tidyverse)
library(raster)
library(ggspatial)
library(sf)
Nepal<-st_read("../Data/Nepal_fug.shp")
Mig<-terra::rast("../Data/Covariates/Migration/Migration_2000_2019_5yrSum.tif")
Mig<-Mig[[4]]#Just get 2015 to 2019

#NepalSub<-Nepal%>%filter(NAME_4=="Banganga")
#ggplot(NepalSub)+geom_sf()
Mig<-terra::crop(Mig,Nepal) #clip the SAing box
Mig<-terra::mask(Mig,Nepal) #NA everything outside .shp


#Mig[]<-ifelse(Mig[]>=200000,NA,Mig[])#some numeric NA values
raster::plot(Mig)

MapTheme<-  theme(#panel.grid = element_blank(),
  axis.text = element_blank())+
  theme(legend.text=element_text(size=12),
        #legend.position = c(.1,.75),
        title = element_text(color="black",size=14,face="bold"),
        legend.title=element_text(size=14),
        panel.grid = element_blank(),
        legend.key.height = unit(1.5, 'cm'),
        legend.key.width = unit(0.33,"cm"))


Migdf<-raster::as.data.frame(Mig,xy=T)
names(Migdf)[3]<-"NetMigration"
Migdf<-na.omit(Migdf)
#p<-
  ggplot(Nepal)+geom_tile(data=Migdf,aes(x=x,y=y,fill=NetMigration) )+
    scale_fill_distiller(type="div",name="",
                         palette = "PuOr",breaks=c(-32150,0,200000),
                         labels=c("Population loss", "No change", "Population gain"),
                         trans= scales::pseudo_log_trans(sigma = 0.01,base=5000))+
  #scale_fill_viridis_c(option="plasma", direction = -1,
                       #name="",labels=scales::comma)+
                       #trans= scales::pseudo_log_trans(sigma = 0.001))+
  geom_sf(fill=alpha("white",alpha=0.01),color="#525252",alpha=0.2)+
  ggtitle("Net migration (2015-2019)")+
  #scale_y_continuous(breaks=c(-24,-28,-32))+
  xlab("")+ylab("")+
  #annotation_scale(location = "br", width_hint = 0.5,text_cex = 1.2)+
  #scale_fill_manual(values =c("white","green"))+
  theme_void()+MapTheme

  Migration<-terra::extract(Mig,Nepal,fun="sum",na.rm=T)
  Nepal$Migration<-Migration$`2015_2019`
  
  
  ggplot(Nepal)+
    geom_sf(aes(fill=Migration),color=alpha("white",0.1))+
    scale_fill_distiller(type="div",name="",labels=scales::comma,
                         palette = "PuOr",values = )+
    ggtitle("Net migration (2015-2019)")+
    #scale_y_continuous(breaks=c(-24,-28,-32))+
    xlab("")+ylab("")+
    #annotation_scale(location = "br", width_hint = 0.5,text_cex = 1.2)+
    #scale_fill_manual(values =c("white","green"))+
    theme_bw()+MapTheme
  
  names(Nepal)[20]<-"NetMigration_2015_2019"
  
  st_write(Nepal, "../Data/Covariates/Migration/Migration_by_VDC.shp")
  