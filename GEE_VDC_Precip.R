library(tidyverse)
library(sf)
library(rgee)


ee_check()  #this makes sure the machine that's running this has python that is linked to a GEE API

ee_Initialize(user = 'matthewclark989@u.boisestate.edu')



Nepal<-st_read("../Data/Nepal_FUG.shp")

Subs<-unique(Nepal$NAME_2)

NepalSub<-Nepal%>%dplyr::filter(NAME_2==Subs[1])


Years<-2018:2024
Precip<-data.frame("VDC_Jur"=NA,"day"=lubridate::as_date("1999-01-01"),"pr"=NA) #this makes the df container. We delete this row l8r
for(i in Years){
  chirps <- ee$ImageCollection("UCSB-CHG/CHIRPS/DAILY") %>%
    ee$ImageCollection$filterDate(paste0(i,"-01-01"), paste0(i+1,"-01-01") )%>% #get the data through the first of the month in the next year (or else we lose new years)
    ee$ImageCollection$map(function(x) x$select("precipitation")) %>% # Select only precipitation bands
    ee$ImageCollection$toBands() #%>% # from imagecollection to image
  #ee$Image$rename(sprintf("PP_%02d",364)) # rename the bands of an image
  
  
  ee_Nepal_rain <- ee_extract(x = chirps, y = NepalSub["VDC_Jur"], sf = FALSE)
  
  
  
  yearly_precip<-ee_Nepal_rain %>%
    group_by(VDC_Jur)%>%summarise_all(mean,na.rm=TRUE)%>%
    pivot_longer(-VDC_Jur, names_to = "day", values_to = "pr") %>%
    mutate(day=lubridate::as_date(substr(day , 2, 9)))
  
  Precip<-rbind(Precip,yearly_precip)}

Precip<-Precip[-1,]


#####
###Now do the other NAME2 areas


for(s in Subs[-1]){
  #s="Lumbini"
NepalSub<-Nepal%>%dplyr::filter(NAME_2==s)


Years<-2018:2024

for(i in Years){
  chirps <- ee$ImageCollection("UCSB-CHG/CHIRPS/DAILY") %>%
    ee$ImageCollection$filterDate(paste0(i,"-01-01"), paste0(i+1,"-01-01") )%>% #get the data through the first of the month in the next year (or else we lose new years)
    ee$ImageCollection$map(function(x) x$select("precipitation")) %>% # Select only precipitation bands
    ee$ImageCollection$toBands() #%>% # from imagecollection to image
  #ee$Image$rename(sprintf("PP_%02d",364)) # rename the bands of an image
  
  
  ee_Nepal_rain <- ee_extract(x = chirps, y = NepalSub["VDC_Jur"], sf = FALSE)
  
  
  
  yearly_precip<-ee_Nepal_rain %>%
    group_by(VDC_Jur)%>%summarise_all(mean,na.rm=TRUE)%>%
    pivot_longer(-VDC_Jur, names_to = "day", values_to = "pr") %>%
    mutate(day=lubridate::as_date(substr(day , 2, 9)))
  
  Precip<-rbind(Precip,yearly_precip)}

}


nrow(Precip)

length(unique(Nepal$VDC_Jur))*length(unique(Precip$day))

#Precip<-distinct(Precip)


write.csv(Precip,"./VDCPrecipitation.csv")

x<-Precip%>%group_by(VDC_Jur)%>%count()



ggplot(Precip,aes(x = day, y = pr,group=VDC_Jur)) +
  geom_line(aes(color=VDC_Jur),show.legend=F,alpha=0.4) +
  #geom_smooth(se=F,method="loess")+
  xlab("day") +
  ylab("Precipitation (mm)") +
  theme_minimal()






