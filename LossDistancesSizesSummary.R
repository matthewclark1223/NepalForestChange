library(sf)
library(tidyverse)
library(progress)
Parks<-read_sf("../Data/Nepal_NationalParks.shp")
Nepal<-read_sf("../Data/Nepal_fug.shp")

#Load in all losses and merge
##############
# Define the folder containing the shapefiles
shapefile_folder <- "../Data/LossDistances/"

# List all shapefiles in the folder
shapefile_list <- list.files(shapefile_folder, pattern = "\\.shp$", full.names = TRUE)

# Initialize a progress bar
pb <- progress_bar$new(
  format = "[:bar] :current/:total (:percent) - ETA: :eta",
  total = length(shapefile_list),
  clear = FALSE,
  width = 60
)

# Read all shapefiles into a list
shapefile_data <- lapply(seq_along(shapefile_list), function(i) {
  pb$tick()  # Update progress bar
  st_read(shapefile_list[i], quiet = TRUE)
})


##

# Combine all shapefiles into a single `sf` object

shapefile_data<-lapply(shapefile_data, st_cast,to="MULTIPOLYGON")
AllLosses <- sf::st_as_sf(data.table::rbindlist(shapefile_data))
rm(shapefile_data)
rm(shapefile_list)


Nepal$CFPres<-ifelse(is.na(Nepal$CF),"NoCF","CF")
Nepal$VDC_Jur<-gsub("/","_",Nepal$VDC_Jur)
AllLosses$VDC_Jur<-gsub("/","_",AllLosses$VDC_Jur)

Nep<-as.data.frame(Nepal)%>%select(VDC_Jur,CFPres,VDC_Jur,NAME_1,NAME_2,NAME_3)
AllLosses<-base::merge(AllLosses,Nep,by="VDC_Jur",all.x=TRUE,all.y=FALSE)

rm(Nep)

ParkIntersects<-st_intersects(AllLosses,Parks,sparse=T)
rm(Parks)
AllLosses$Parks<- lengths(ParkIntersects) > 0
rm(ParkIntersects)


AllLosses<-AllLosses%>%mutate(LandUse = ifelse(Parks == TRUE, "PA", CFPres))

#st_write(AllLosses, "../Data/AllForestLosses.shp", append=FALSE)
AllLosses<-read_sf("../Data/AllForestLosses.shp")

#AllLosses%>%filter(NAME_1=="Mid-Western")%>%
#ggplot(data=., aes(x=DstBn_M,fill=LandUse,color=LandUse))+
 # geom_density( alpha=0.5,linewidth=1.2)+
 # scale_fill_manual(values=c("#cc4c02","#0570b0","#ae017e"),labels=c("Community forest","Unconserved","Protected area"))+
 # scale_colour_manual(values=c("#8c2d04","#034e7b","#7a0177"),labels=c("Community forest","Unconserved","Protected area"))+
 # xlab("Distance to forest edge (M)")+
 # xlim(0,2000)+
 # ylab("Density of observations")+
 ## ggthemes::theme_clean()+
  #facet_wrap(~NAME_3,scales = "free" )
  

L1<-AllLosses%>%#filter(DstBn_M<=1000)%>%
  ggplot(data=., aes(x=DstBn_M,fill=LandUse,color=LandUse))+
    geom_histogram( aes( y=after_stat(c(
      count[group==1]/sum(count[group==1]),
      count[group==2]/sum(count[group==2]),
      count[group==3]/sum(count[group==3])
    ))),alpha=0.5,linewidth=1.2,position="identity",bins = 25)+
    scale_fill_manual(name="Land use",values=c("#cc4c02","#0570b0","#ae017e"),labels=c("Community forest","Unconserved","Protected area"))+
    scale_colour_manual(name="Land use",values=c("#8c2d04","#034e7b","#7a0177"),labels=c("Community forest","Unconserved","Protected area"))+
    xlab("Distance to forest edge (M)")+
    ylab("Density of loss observations")+
  scale_y_continuous(labels=scales::percent)+
  scale_x_continuous(labels=scales::comma, breaks=c(0,1000,2000,3000,4000))+
    ggthemes::theme_clean()+#+
      mytheme+theme(legend.text=element_text(color="black",size=12),
                     legend.key.size = unit(0.3,"in"),
                     legend.position = c(0.5,0.85))
  
L2<-AllLosses%>%filter(Area_M<=2000)%>%
  ggplot(data=., aes(x=Area_M,fill=LandUse,color=LandUse))+
  geom_histogram( aes( y=after_stat(c(
    count[group==1]/sum(count[group==1]),
    count[group==2]/sum(count[group==2]),
    count[group==3]/sum(count[group==3])
  ))),alpha=0.5,linewidth=1.2,position="identity",bins = 25)+
  scale_fill_manual(name="Land use",values=c("#cc4c02","#0570b0","#ae017e"),labels=c("Community forest","Unconserved","Protected area"))+
  scale_colour_manual(name="Land use",values=c("#8c2d04","#034e7b","#7a0177"),labels=c("Community forest","Unconserved","Protected area"))+
  #scale_x_log10()+
  ylab("Density of loss observations")+
  scale_y_continuous(labels=scales::percent)+
  scale_x_continuous(labels=scales::comma, name="Area (M^2)")+
  ggthemes::theme_clean()+
  mytheme+theme(legend.text=element_text(color="black",size=12),
                legend.key.size = unit(0.3,"in"),
                #legend.position = c(0.5,0.85),
                legend.position="none")
 
pc<-cowplot::plot_grid(L1,L2,nrow=1,align="hv",
                       labels = c('A', 'B'),
                       label_size = 22,label_fontface = "bold")

ggsave("../Figures/LossSummary.png",pc,dpi=350,units = "in",
       width = 12, height = 6, bg="white")
  

#Density
L1<-AllLosses%>%filter(DstBn_M<=2000)%>%
  ggplot(data=., aes(x=DstBn_M,fill=LandUse,color=LandUse))+
  geom_density( alpha=0.4,linewidth=1.2)+
  scale_fill_manual(name="Land use",values=c("#cc4c02","#0570b0","#ae017e"),labels=c("Community forest","Unconserved","Protected area"))+
  scale_colour_manual(name="Land use",values=c("#8c2d04","#034e7b","#7a0177"),labels=c("Community forest","Unconserved","Protected area"))+
  xlab("Distance to forest edge (M)")+
  ylab("Density of loss observations")+
  scale_y_continuous()+
  scale_x_continuous(labels=scales::comma, breaks=c(0,500,1000,1500))+
  ggthemes::theme_clean()+#+
  mytheme+theme(legend.text=element_text(color="black",size=18),
                legend.title = element_text(color="black",size=22),
                legend.key.size = unit(0.6,"in"),
                legend.position = c(0.65,0.55))

L2<-AllLosses%>%filter(Area_M<=1200)%>%
  ggplot(data=., aes(x=Area_M,fill=LandUse,color=LandUse))+
  geom_density(alpha=0.4,linewidth=1.2,adjust=7)+
  scale_fill_manual(name="Land use",values=c("#cc4c02","#0570b0","#ae017e"),labels=c("Community forest","Unconserved","Protected area"))+
  scale_colour_manual(name="Land use",values=c("#8c2d04","#034e7b","#7a0177"),labels=c("Community forest","Unconserved","Protected area"))+
  #scale_x_log10()+
  ylab("Density of loss observations")+
  scale_y_continuous()+
  scale_x_continuous(labels=scales::comma, name="Area (M^2)",breaks=c(200,600,1000))+
  ggthemes::theme_clean()+
  mytheme+theme(legend.text=element_text(color="black",size=18),
                legend.key.size = unit(0.3,"in"),
                #legend.position = c(0.5,0.85),
                legend.position="none")

pc<-cowplot::plot_grid(L1,L2,nrow=1,align="hv",
                       labels = c('a', 'b'),
                       label_size = 22,label_fontface = "bold")

ggsave("../Figures/LossSummaryDensity.png",pc,dpi=350,units = "in",
       width = 12, height = 6, bg="white")
  
  
  
  

