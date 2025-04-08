library(tidyverse)
library(sf)
library(igraph)
NepalFull<-read_sf("../Data/Nepal_fug.shp")
ParksFull<-read_sf("../Data/Nepal_NationalParks.shp")
FCFull<-terra::rast("../Data/BrandtProducts/ForestChange.tif")
SlopeFull<-terra::rast("../Data/Covariates/Slope/NepalSlope.tif")
LCFull<-terra::rast("../Data/BrandtProducts/LC_Classification_SIMPLE.tif")
#The goal of this script is to create a data frame with each observation of forest loss 
#as a patch. This should record whether the VDC the loss is in has a CF or not
#Also we want to record whether it is in a protected area.
#Then we want to calculate the distance to forest edge. 

#Let's forest do a small subset of the data
#Nepal<-Nepal%>%filter(NAME_2%in%c("Bheri","Rapti"))
#Nepal<-Nepal%>%filter(NAME_3%in%c("Bardiya"))
for(i in 2000:nrow(NepalFull)){
  NAME<-NepalFull[i,]$VDC_Jur
  NAME<-sub("/","_",NAME)


Nepal<-NepalFull[i,]
Parks<-st_crop(ParksFull,Nepal)

#Could do st_intersection here but this is faster
sel = unlist(geos::geos_intersects_matrix(Nepal, Parks))
Parks<-Parks[sel, ]

#ggplot(Nepal)+geom_sf()+geom_sf(data=Parks,fill="green",alpha=0.5)

AOIBuff<-st_buffer( Nepal, 2000) #about 2km for Nepal
FC<-terra::crop(FCFull,AOIBuff)
FC<-terra::mask(FC,Nepal)

if(min(FC[],na.rm=T)==0) next #If there ends up being no forest loss there!!

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
FC<-terra::mask(FC,Slopepoly) #Mask out forest change at slopes>=45 deg


#Fill forest gaps as forest

LC<-terra::project(LCFull, FC, method="near") #Ge the rasters in the same projection & pixl size
if(max(LC[],na.rm=T)==0) next #If there ends up being no forest there!!

LC<-terra::crop(LC,AOIBuff)
LC<-terra::mask(LC,AOIBuff)

#We just want nonforest
#cls <- data.frame(id=0:3, cover=c("NotForest", NA,"NotForest",NA))
#levels(LC) <- cls
#terra::plot(LC, col=c("green"))
#get rid of random noise Don't need if reprojecting above
#LC <- terra::sieve(LC, 5,directions=8) 
LCpoly<-terra::as.polygons(LC)
#names(LCpoly)[1]<-"Cover"
#LCpoly<-LCpoly[LCpoly$Cover %in%c(0,2),]

LCpoly<-sf::st_as_sf(LCpoly)
names(LCpoly)[1]<-"Cover"

#Only keep area outside
LCpoly<-LCpoly%>%dplyr::filter(Cover==0)

#ggplot(LCpoly)+geom_sf(fill="blue",alpha=0.25)

FC<-terra::crop(FC,Nepal)
FC<-terra::mask(FC,Nepal)
if(min(FC[],na.rm=T)==0) next #If there ends up being no forest loss there!!

patchpoly<-terra::as.polygons(FC)
patchpoly<-sf::st_as_sf(patchpoly)
names(patchpoly)[1]<-"patch"
patchpoly<-patchpoly%>%filter(patch!=0) #any non zero coefficient is loss. Already filtered by signficance, etc

#ggplot(LCpoly)+geom_sf(fill="blue",alpha=0.25)+
#geom_sf(data=patchpoly,color="red",fill="red")

LCpoly<-st_union(LCpoly)

#Split each pixel into a polygon
patchpoly<-st_cast(patchpoly, "MULTIPOLYGON")
patchpoly <- patchpoly %>%
  st_cast("POLYGON", group_or_split = TRUE) # Ensure all parts are split


sel = unlist(geos::geos_intersects_matrix(LCpoly, patchpoly))

if(length(sel)>0){
patchpoly<-patchpoly[-sel, ]#get rid of losses of non-forest trees. Not interested in those in this paper. 
}
#ggplot(LCpoly)+geom_sf(fill="blue",alpha=0.25)+ areas outside forests
#geom_sf(data=patchpoly,color="red",fill="red")

if(nrow(patchpoly)==0) next # if all losses are outside forest area. 


#Now group pixels by which are touching 

###saved INT objects here
#st_write(patchpoly, "../Data/PatchPolyINT.shp",append = FALSE )
#st_write(LCpoly2, "../Data/LCPolyINT.shp",append = FALSE )
#saveRDS(LCpoly, file = "../Data/LCpolyINT.rds")
#Removing unnecessary stuff here bc of memory limitations
#remove FC,Nepal,Parks,Slope,Slopepoly,sel,LC

#patchpoly<-read_sf("../Data/PatchPolyINT.shp")
sf_use_s2(FALSE)
touching_matrix <- st_touches(patchpoly, sparse = FALSE)






graph <- graph_from_adjacency_matrix(touching_matrix, mode = "undirected")
clusters <- components(graph)$membership

# Step 3: Add cluster IDs to the polygons
patchpoly$cluster_id <- clusters

# Step 4: Dissolve polygons by cluster ID
patchpoly <- patchpoly %>%
  group_by(cluster_id) %>%
  summarise(geometry = st_union(geometry)) %>%
  st_as_sf()


#dist<-st_distance(patchpoly,LCpoly)
#patchpoly$DistBound_M<-as.vector(dist)

#This shuld be faster thn the code above. 

nearest_edges <- nngeo::st_nn(patchpoly, LCpoly, k = 1, returnDist = TRUE)

# Extract the minimum distances from the result
patchpoly$DistBound_M<- sapply(nearest_edges$dist, min)


patchpoly$Area_M<-as.vector(st_area(patchpoly))
patchpoly$VDC_Jur<-NAME

#ggplot(AOIBuff)+geom_sf()+
#  geom_sf(data=Nepal)+
#  geom_sf(data=LCpoly,fill="grey")+
#  geom_sf(data=patchpoly,color="red")
print(i)
print(paste(i/nrow(NepalFull)*100,"%"))
st_write(patchpoly, paste0("../Data/LossDistances/",NAME,".shp"),append = FALSE )}
