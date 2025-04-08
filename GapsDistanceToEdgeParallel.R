library(doParallel)
library(foreach)

cl <- 11
registerDoParallel(cl)

foreach(i=rev(DoubleCheck))%dopar%{

library(tidyverse)
library(sf)
library(igraph)
NepalFull<-read_sf("../Data/Nepal_fug.shp")
ParksFull<-read_sf("../Data/Nepal_NationalParks.shp")
SlopeFull<-terra::rast("../Data/Covariates/Slope/NepalSlope.tif")
LCFull<-terra::rast("../Data/BrandtProducts/LC_Classification.tif")
sf_use_s2(FALSE)

#raster::plot(LCFull)

#The goal of this script is to create a data frame with each observation of forest loss 
#as a patch. This should record whether the VDC the loss is in has a CF or not
#Also we want to record whether it is in a protected area.
#Then we want to calculate the distance to forest edge. 

#Let's forest do a small subset of the data
#Nepal<-Nepal%>%filter(NAME_2%in%c("Bheri","Rapti"))
#Nepal<-Nepal%>%filter(NAME_3%in%c("Bardiya"))
#for(i in 65:nrow(NepalFull)){
  
  NAME<-NepalFull[i,]$VDC_Jur
  NAME<-sub("/","_",NAME)
  
  
  Nepal<-NepalFull[i,]
  
  #Nepal<-NepalFull%>%filter(NAME_4%in%c("Devsthal","Bame"))
  #Nepal<-NepalFull%>%filter(NAME_3%in%c("Bardiya"))
  
  Parks<-st_crop(ParksFull,Nepal)
  
  #Could do st_intersection here but this is faster
  sel = unlist(geos::geos_intersects_matrix(Nepal, Parks))
  Parks<-Parks[sel, ]
  
  #ggplot(Nepal)+geom_sf(aes(fill=NAME_4))+geom_sf(data=Parks,fill="green",alpha=0.2)
  
  #Crop/mask land cover to area surrounding AOI
  AOIBuff<-st_buffer( Nepal, 0.02) #about 5km for Nepal
  #ggplot(Nepal)+geom_sf(data=AOIBuff,fill="red")+geom_sf(fill="green")
  LC_Buff<-terra::crop(LCFull,AOIBuff)
  LC_Buff<-terra::mask(LC_Buff,AOIBuff)
  if(0%in%unique(LC_Buff[])==FALSE ) next #If there's no nonforest inside the buffer, skip
  
  #Crop/mask just the vdc
  LC_Inside<-terra::crop(LC_Buff,Nepal)
  LC_Inside<-terra::mask(LC_Inside,Nepal)
  
  
  
  if(max(LC_Inside[],na.rm=T)<=2) next #If there ends up being no gaps there!! #3 is gaps
  if(1%in%unique(LC_Inside[])==FALSE ) next #If there ends up being no forests there!! #3 is gaps
  
  #Filter out losses in slopes >45 degrees
  
  Slope<-terra::crop(SlopeFull,Nepal)
  Slope<-terra::mask(Slope,Nepal)
  #raster::plot(Slope)
  Slope[]<-ifelse(Slope[]<4500,0,1)
  #raster::plot(Slope)
  Slopepoly<-terra::as.polygons(Slope)
  Slopepoly<-sf::st_as_sf(Slopepoly)
  Slopepoly<-Slopepoly%>%dplyr::filter(NepalSlope==0)
  #plot(Slopepoly)
  LC_Inside<-terra::mask(LC_Inside,Slopepoly) #Mask out forest change at slopes>=45 deg
  
  #terra::plot(LC_Inside)
  #terra::plot(LC_Buff)
  
  #Make gaps polygon
  GapsPoly<-terra::as.polygons(LC_Inside)
  names(GapsPoly)[1]<-"Cover"
  GapsPoly<-sf::st_as_sf(GapsPoly)
  #ggplot(GapsPoly)+geom_sf(aes(fill=as.character(Cover)))
  
  #Only keep gaps
  GapsPoly<-GapsPoly%>%dplyr::filter(Cover==3)
  
  #Now get just area outside forests
  OutPoly<-terra::as.polygons(LC_Buff)
  names(OutPoly)[1]<-"Cover"
  OutPoly<-sf::st_as_sf(OutPoly)
  #Only area outside forests
  OutPoly<-OutPoly%>%dplyr::filter(Cover==0)
  
  OutPoly<-rmapshaper::ms_simplify(OutPoly, keep = 0.1, keep_shapes = TRUE)
  
  
  if(sf::st_is_valid(OutPoly) ==FALSE){
    OutPoly<-sf::st_make_valid(OutPoly)
  }
  #OutPoly <- st_simplify(OutPoly, preserveTopology = FALSE, 
  # dTolerance = 0.0005)
  #ggplot(OutPoly)+geom_sf(aes(fill=as.character(Cover)))
  
  # ggplot(OutPoly)+geom_sf(fill="blue",alpha=0.55)+
  #  geom_sf(data=Nepal,fill="grey",alpha=0.2)+
  # geom_sf(data=GapsPoly,color="red",fill="red",linewidth=0.5)
  
  
  
  OutPoly<-st_union(OutPoly)
  
  #Split each pixel into a polygon
  GapsPoly<-st_cast(GapsPoly, "MULTIPOLYGON")
  GapsPoly <- GapsPoly %>%
    st_cast("POLYGON", group_or_split = TRUE) # Ensure all parts are split
  
  
  sf_use_s2(FALSE)
  touching_matrix <- st_touches(GapsPoly, sparse = FALSE)
  
  
  
  
  
  
  graph <- graph_from_adjacency_matrix(touching_matrix, mode = "undirected")
  clusters <- components(graph)$membership
  
  # Step 3: Add cluster IDs to the polygons
  GapsPoly$cluster_id <- clusters
  
  # Step 4: Dissolve polygons by cluster ID
  GapsPoly <- GapsPoly %>%
    group_by(cluster_id) %>%
    summarise(geometry = st_union(geometry)) %>%
    st_as_sf()
  
  
  #dist<-st_distance(patchpoly,LCpoly)
  #patchpoly$DistBound_M<-as.vector(dist)
  
  #This shuld be faster thn the code above. 
  
  nearest_edges <- nngeo::st_nn(GapsPoly, OutPoly, k = 1, returnDist = TRUE)
  
  # Extract the minimum distances from the result
  GapsPoly$DistBound_M<- sapply(nearest_edges$dist, min)
  
  
  GapsPoly$Area_M<-as.vector(st_area(GapsPoly))
  GapsPoly$VDC_Jur<-NAME
  
  #ggplot(AOIBuff)+geom_sf()+
  #  geom_sf(data=Nepal)+
  # geom_sf(data=OutPoly,fill="blue")+
  #  geom_sf(data=GapsPoly,aes(color=DistBound_M))
  
  
  
  print(i)
  print(paste(i/nrow(NepalFull)*100,"%"))
  st_write(GapsPoly, paste0("../Data/GapsDistances/",NAME,".shp"),append = FALSE )}
