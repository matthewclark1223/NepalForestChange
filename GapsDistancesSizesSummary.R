library(sf)
library(tidyverse)
library(progress)
Parks<-read_sf("../Data/Nepal_NationalParks.shp")
Nepal<-read_sf("../Data/Nepal_fug.shp")

#Load in all losses and merge
##############
# Define the folder containing the shapefiles
shapefile_folder <- "../Data/GapsDistances/"

# List all shapefiles in the folder
shapefile_list <- list.files(shapefile_folder, pattern = "\\.shp$", full.names = TRUE)

# Filter shapefiles containing "Bardiya" in their names for testing!
#shapefile_list <- shapefile_list[grepl("Bardiya", shapefile_list)]
#shapefile_list <- shapefile_list[grepl("Achham", shapefile_list)]



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
#AllGaps <- do.call(rbind, shapefile_data2)
#AllGaps<-sf::st_as_sf(as_tibble(AllGaps))
shapefile_data<-lapply(shapefile_data, st_cast,to="MULTIPOLYGON")
AllGaps <- sf::st_as_sf(data.table::rbindlist(shapefile_data))
rm(shapefile_data)
rm(shapefile_list)


Nepal$CFPres<-ifelse(is.na(Nepal$CF),"NoCF","CF")
Nepal$VDC_Jur<-gsub("/","_",Nepal$VDC_Jur)

Nep<-as.data.frame(Nepal)%>%select(VDC_Jur,CFPres,VDC_Jur,NAME_1,NAME_2,NAME_3)
AllGaps<-base::merge(AllGaps,Nep,by="VDC_Jur",all.x=TRUE,all.y=FALSE)

rm(Nep)
#ggplot(AllGaps)+
#  geom_sf(data=Nepal,aes(fill=CFPres),alpha=0.25)+
# geom_sf(data=Parks,color=alpha("green",0.1),fill="green",alpha=0.5)+
#  geom_sf(color=NA,fill="red")

#START HERE. SHOULDN'T NEED TO DO THIS VDC VECTOR THING. ALREADY HAVE NAME.  
#AllGaps$VDC<-st_intersects(AllGaps,Nepal)
ParkIntersects<-st_intersects(AllGaps,Parks,sparse=T)
rm(Parks)
AllGaps$Parks<- lengths(ParkIntersects) > 0
rm(ParkIntersects)



AllGaps<-AllGaps%>%mutate(LandUse = ifelse(Parks == TRUE, "PA", CFPres))

#st_write(AllGaps, "../Data/AllForestGaps.shp", append=FALSE)
AllGaps<-read_sf("../Data/AllForestGaps.shp")


G1<-AllGaps%>%#filter(NAME_3=="Bardiya")%>%
  ggplot(data=., aes(x=DstBn_M,fill=LandUse,color=LandUse))+
  geom_histogram( aes( y=after_stat(c(
    count[group==1]/sum(count[group==1]),
    count[group==2]/sum(count[group==2]),
    count[group==3]/sum(count[group==3])
  ))),alpha=0.5,linewidth=1.2,position="identity",bins=25)+
  scale_fill_manual(name="Land use",values=c("#cc4c02","#0570b0","#ae017e"),labels=c("Community forest","Unconserved","Protected area"))+
  scale_colour_manual(name="Land use",values=c("#8c2d04","#034e7b","#7a0177"),labels=c("Community forest","Unconserved","Protected area"))+
  xlab("Distance to forest edge (M)")+
  #xlim(0,3000)+
  ylab("Density of gap observations")+
  scale_y_continuous(labels=scales::percent)+
  scale_x_continuous(labels=scales::comma, breaks=c(0,1000,2000,3000,4000))+
  ggthemes::theme_clean()+#+
  mytheme+theme(legend.text=element_text(color="black",size=12),
                legend.key.size = unit(0.3,"in"),
                legend.position = "none")

G2<-AllGaps%>%#mutate(LandUse==factor(LandUse,levels=c("PA","CF","NoCF")))%>%
  ggplot(data=., aes(x=Area_M,fill=LandUse,color=LandUse))+
  geom_histogram( aes( y=after_stat(c(
    count[group==1]/sum(count[group==1]),
    count[group==2]/sum(count[group==2]),
    count[group==3]/sum(count[group==3])
  ))),alpha=0.5,linewidth=1.2,position="identity",bins = 25)+
  scale_fill_manual(name="Land use",values=c("#cc4c02","#0570b0","#ae017e"),labels=c("Community forest","Unconserved","Protected area"))+
  scale_colour_manual(name="Land use",values=c("#8c2d04","#034e7b","#7a0177"),labels=c("Community forest","Unconserved","Protected area"))+
  #scale_x_log10()+
  ylab("Density of gap observations")+
  scale_y_continuous(labels=scales::percent)+
  scale_x_continuous(labels=scales::comma, name="Area (M^2)")+
  ggthemes::theme_clean()+
  mytheme+theme(legend.text=element_text(color="black",size=12),
                legend.key.size = unit(0.3,"in"),
                #legend.position = c(0.5,0.85),
                legend.position="none")

pc<-cowplot::plot_grid(G1,G2,nrow=1,align="hv",
                       labels = c('C', 'D'),
                       label_size = 22,label_fontface = "bold")

ggsave("../Figures/GapsSummary.png",pc,dpi=350,units = "in",
       width = 12, height = 6, bg="white")



G1<-AllGaps%>%filter(DstBn_M<=2000)%>%
  ggplot(data=., aes(x=DstBn_M,fill=LandUse,color=LandUse))+
  geom_density( alpha=0.4,linewidth=1.2)+
  scale_fill_manual(name="Land use",values=c("#cc4c02","#0570b0","#ae017e"),labels=c("Community forest","Unconserved","Protected area"))+
  scale_colour_manual(name="Land use",values=c("#8c2d04","#034e7b","#7a0177"),labels=c("Community forest","Unconserved","Protected area"))+
  xlab("Distance to forest edge (M)")+
  ylab("Density of gap observations")+
  scale_y_continuous()+
  scale_x_continuous(labels=scales::comma, breaks=c(0,500,1000,1500))+
  ggthemes::theme_clean()+#+
  mytheme+theme(legend.text=element_text(color="black",size=18),
                legend.title = element_text(color="black",size=22),
                legend.key.size = unit(0.6,"in"),
                legend.position="none")

G2<-AllGaps%>%filter(Area_M<=1200)%>%
  ggplot(data=., aes(x=Area_M,fill=LandUse,color=LandUse))+
  geom_density( alpha=0.4,linewidth=1.2,adjust=7)+
  scale_fill_manual(name="Land use",values=c("#cc4c02","#0570b0","#ae017e"),labels=c("Community forest","Unconserved","Protected area"))+
  scale_colour_manual(name="Land use",values=c("#8c2d04","#034e7b","#7a0177"),labels=c("Community forest","Unconserved","Protected area"))+
  #scale_x_log10()+
  ylab("Density of gap observations")+
  scale_y_continuous()+
  scale_x_continuous(labels=scales::comma, name="Area (M^2)",breaks=c(200,600,1000))+
  ggthemes::theme_clean()+
  mytheme+theme(legend.text=element_text(color="black",size=18),
                legend.key.size = unit(0.3,"in"),
                #legend.position = c(0.5,0.85),
                legend.position="none")

pc<-cowplot::plot_grid(G1,G2,nrow=1,align="hv",
                       labels = c('c', 'd'),
                       label_size = 22,label_fontface = "bold")

ggsave("../Figures/GapsSummaryDensity.png",pc,dpi=350,units = "in",
       width = 12, height = 6, bg="white")

