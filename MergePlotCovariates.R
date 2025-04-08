library(tidyverse)
library(raster)
library(ggspatial)
library(sf)

Nepal<-st_read("../Data/Nepal_fug.shp")

Roads<-read_sf("../Data/Covariates/RoadDensity/road_density_by_VDC.shp")
Pop<-read_sf("../Data/Covariates/Population/Population2022_by_VDC.shp")
Dep<-read_sf("../Data/Covariates/Wellbeing/Deprivation_by_VDC.shp")
Mig<-read_sf("../Data/Covariates/Migration/Migration_by_VDC.shp")
Slope<-read_sf("../Data/Covariates/Slope/Slope_by_VDC.shp")

Nepal$RoadDensity_kmPerSqKm<-Roads$rd_____
Nepal$HumanPop2022<-Pop$Ppl2022
Nepal$MedDeprivation<-Dep$Deprvtn
Nepal$MigrationTotal2015_2019<-Mig$NM_2015
Nepal$MedSlope2022<-Slope$Slope2022/100


MapTheme<-  theme(#panel.grid = element_blank(),
  axis.text = element_text(angle = 0,size=14,color="black"))+
  theme(legend.text=element_text(size=12),
        #legend.position="bottom",
        legend.position = c(.25,.125),
        legend.direction = "horizontal",
        title = element_text(color="black",size=10,face="bold"),
        legend.title=element_text(size=7),
        panel.grid = element_blank(),
        legend.key.height = unit(0.2, 'cm'),
        legend.key.width = unit(0.85,"cm"))

limit <- max(abs(Nepal$MigrationTotal2015_2019)) * c(-1, 1)

Kat<-data.frame(Name="Kathmandu",y=27.8503, x=85.3222)

# Migration
p1<-ggplot(Nepal)+
  geom_sf(aes(fill=MigrationTotal2015_2019),color=alpha("white",0.1))+
  geom_text(data=Kat,aes(x=x,y=y,label=Name),color="black",fontface="bold",size=5)+
  scale_fill_distiller(type="div",name="",labels=scales::comma,trans = 'pseudo_log',limit=limit,
                       palette = "PuOr",breaks=c(-75000,0,500000))+
  ggtitle("Net human migration (2015-2019; pseudo-log scale)")+
    scale_y_continuous(breaks=c(28,30))+
  xlab("")+ylab("")+
    scale_x_continuous(breaks=c(80,84,88))+
    annotation_scale(location = "br", width_hint = 0.3,text_cex = 1)+
    theme_bw()+MapTheme+theme(axis.text.x = element_blank())
  
#pop
p2<-ggplot(Nepal)+
  geom_sf(aes(fill=HumanPop2022),color=alpha("white",0.1))+
  scale_fill_viridis_c(option="cividis",name="",labels=scales::comma,trans = 'log',direction = 1,
                       breaks=c(100,5000,1000000))+
  ggtitle("Total human population 2022; log scale)")+
  scale_y_continuous(breaks=c(28,30))+
  xlab("")+ylab("")+
  scale_x_continuous(breaks=c(80,84,88))+
  #annotation_scale(location = "br", width_hint = 0.5,text_cex = 1)+
  theme_bw()+MapTheme+theme(axis.text = element_blank())


#Deprivation
p3<-ggplot(Nepal)+
  geom_sf(aes(fill=MedDeprivation),color=alpha("white",0.1))+
  scale_fill_viridis_c(option="B", direction = -1,
                       name="",labels=c("Better-off","Worse-off"),breaks=c(20,90))+
  ggtitle("Median deprivation index")+
  scale_y_continuous(breaks=c(28,30))+
  xlab("")+ylab("")+
  scale_x_continuous(breaks=c(80,84,88))+
  #annotation_scale(location = "br", width_hint = 0.5,text_cex = 1)+
  theme_bw()+MapTheme+theme(axis.text.x = element_blank())

#RoadDensity

p4<-ggplot(Nepal)+
  geom_sf(aes(fill=RoadDensity_kmPerSqKm),color=alpha("white",0.1))+
  scale_fill_viridis_c(option="mako",name="",direction = -1,breaks=c(2,12,22))+
  ggtitle(bquote(bold('Road density '(km/km^2))) )+
  scale_y_continuous(breaks=c(28,30))+
  xlab("")+ylab("")+
  scale_x_continuous(breaks=c(80,84,88))+
  #annotation_scale(location = "br", width_hint = 0.5,text_cex = 1)+
  theme_bw()+MapTheme+theme(axis.text = element_blank())


#Slope
p5<-ggplot(Nepal)+
  geom_sf(aes(fill=MedSlope2022),color=alpha("white",0.1))+
  scale_fill_distiller(type="seq",name="",direction = 1,
                       palette = "Reds",breaks=c(5,25,50))+
  ggtitle("Median slope (2000; degrees)")+
  scale_y_continuous(breaks=c(28,30))+
  xlab("")+ylab("")+
  scale_x_continuous(breaks=c(80,84,88))+
  #annotation_scale(location = "br", width_hint = 0.5,text_cex = 1)+
  theme_bw()+MapTheme#+theme(axis.text.y = element_blank())

  
  #Combine:
  pc<-cowplot::plot_grid(p1,p2,p3,p4,p5,nrow=3,align="hv",
                         labels = c('a', 'b','c','d','e'),
                         label_size = 22,label_fontface = "bold")
  
  ggsave("../Figures/Covarites.png",pc,dpi=350,units = "in",
         width = 12, height = 12, bg="white")
  