library(tidyverse)
library(raster)
library(ggspatial)
library(sf)
Nepal<-st_read("../Data/Nepal_fug.shp")
Dep<-terra::rast("../Data/Covariates/Wellbeing/povmap-grdi-v1.tif")

#NepalSub<-Nepal%>%filter(NAME_4=="Banganga")
#ggplot(NepalSub)+geom_sf()
Dep<-terra::crop(Dep,Nepal) #clip the SAing box
Dep<-terra::mask(Dep,Nepal) #NA everything outside .shp


#Dep[]<-ifelse(Dep[]>=200000,NA,Dep[])#some numeric NA values
raster::plot(Dep)

MapTheme<-  theme(#panel.grid = element_blank(),
  axis.text = element_blank())+
  theme(legend.text=element_text(size=12),
        #legend.position = c(.1,.75),
        title = element_text(color="black",size=14,face="bold"),
        legend.title=element_text(size=14),
        panel.grid = element_blank(),
        legend.key.height = unit(1.5, 'cm'),
        legend.key.width = unit(0.33,"cm"))


Depdf<-raster::as.data.frame(Dep,xy=T)
names(Depdf)[3]<-"Deprivation"
Depdf<-na.omit(Depdf)
#p<-
ggplot(Nepal)+geom_tile(data=Depdf,aes(x=x,y=y,fill=Deprivation) )+
  scale_fill_viridis_c(option="B", direction = -1,
  name="",breaks=c(20,85),labels=c("High wellbeing","Low wellbeing"))+
  #trans= scales::pseudo_log_trans(sigma = 0.001))+
  geom_sf(fill=alpha("white",alpha=0.01),color="#525252",alpha=0.2)+
  ggtitle("Community wellbeing")+
  #scale_y_continuous(breaks=c(-24,-28,-32))+
  xlab("")+ylab("")+
  #annotation_scale(location = "br", width_hint = 0.5,text_cex = 1.2)+
  #scale_fill_manual(values =c("white","green"))+
  theme_void()+MapTheme


Deprivation<-terra::extract(Dep,Nepal,fun="median",na.rm=T)
Nepal$Deprivation<-Deprivation$`povmap-grdi-v1`


ggplot(Nepal)+
  geom_sf(aes(fill=Deprivation),color=alpha("white",0.1))+
  scale_fill_viridis_c(option="B", direction = -1,
                       name="",labels=scales::comma)+
  ggtitle("Deprivation")+
  #scale_y_continuous(breaks=c(-24,-28,-32))+
  xlab("")+ylab("")+
  #annotation_scale(location = "br", width_hint = 0.5,text_cex = 1.2)+
  #scale_fill_manual(values =c("white","green"))+
  theme_bw()+MapTheme



st_write(Nepal, "../Data/Covariates/Wellbeing/Deprivation_by_VDC.shp")



