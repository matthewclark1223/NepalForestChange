library(tidyverse)
library(sf)
Losses<-read_csv("../Data/OutcomeData_AllSlope.csv")%>%dplyr::select(NAME_1,NAME_2,NAME_3,VDC_Jur,Frst_size,Gap_size,CF_area,CF)
Losses$CF<-ifelse(Losses$CF>0,TRUE,FALSE)
Losses$Frst_size<-Losses$Frst_size + Losses$Gap_size 
Losses$PropCommFrst<-Losses$CF_area / Losses$Frst_size
Losses$PropCommFrst<-ifelse(Losses$PropCommFrst >1,1.0,Losses$PropCommFrst)
Losses$VDC_Jur<-gsub("/","_",Losses$VDC_Jur)

Losses <- Losses[!duplicated(Losses$VDC_Jur), ]


AddCovariates<-function(data){
  dat<-sf::read_sf(data) #Load shapefile 
  dat$VDC_Jur<-gsub("/","_",dat$VDC_Jur)
  dat <- dat[!duplicated(dat$VDC_Jur), ]
  dat<-as.data.frame(dat)
  dat<-dat[,1:ncol(dat)-1]
  dat<-dat[,c(1,ncol(dat))]
  Losses<<-merge(Losses,dat,by="VDC_Jur")
  
}

CleanUpVDCs<-function(data){
  data$VDC_Jur<-gsub("/","_",data$VDC_Jur)
  data <- data[!duplicated(data$VDC_Jur), ]
}

AddCovariates("../Data/Covariates/Migration/Migration_by_VDC.shp")
AddCovariates("../Data/Covariates/Population/Population2022_by_VDC.shp")
AddCovariates("../Data/Covariates/Population/Population2015_by_VDC.shp")
AddCovariates("../Data/Covariates/RoadDensity/road_density_by_VDC.shp")
AddCovariates("../Data/Covariates/Wellbeing/Deprivation_by_VDC.shp")
AddCovariates("../Data/Covariates/Slope/Slope_by_VDC.shp")


precipAnom<-read.csv("../Data/Covariates/Precip/VDCPROUTnew2.csv")
precipAnom<-CleanUpVDCs(precipAnom)
Losses<-merge(Losses,precipAnom,by="VDC_Jur")
names(Losses)[16]<-"PrecipAnom"
names(Losses)[13]<-"RoadDensity"
Losses<-Losses%>%dplyr::select(-Gap_size) #Get rid of gaps not masked by slope or parks

#Now get outcome stats masked by slope and parks

AllGaps<-read_sf("../Data/AllForestGaps.shp")
AllGaps<-as.data.frame(AllGaps)%>%dplyr::select(-geometry)


GapSummary<-AllGaps%>%filter(Parks==FALSE)%>%
  group_by(VDC_Jur)%>%
  summarize(MedGapSize_M = median(Area_M),
            MedGapDist_M = median(DstBn_M),
            TotalGapSize_ha = sum(Area_M)/10000,
            NGaps=n())

rm(AllGaps)

GapSummary<-CleanUpVDCs(GapSummary)

PreGaps<-read_sf("../Data/PrecedingGaps.shp")
PreGaps<-as.data.frame(PreGaps)%>%dplyr::select(-geometry)


Gaps_NoLossOverlap_Summary<-PreGaps%>%filter(Parks==FALSE)%>%
  group_by(VDC_Jur)%>%
  summarize( PreGapArea_Ha = sum(Area_M)/10000,
             PreGapN=n())

Gaps_NoLossOverlap_Summary<-CleanUpVDCs(Gaps_NoLossOverlap_Summary)

View(head(Losses))

Losses<-base::merge(Losses,GapSummary,by="VDC_Jur",all.x=T )
Losses<-base::merge(Losses,Gaps_NoLossOverlap_Summary,by="VDC_Jur",all.x=T)
rm(GapSummary)
rm(PreGaps)
rm(Gaps_NoLossOverlap_Summary)

AllLosses<-read_sf("../Data/AllForestLosses.shp")
AllLosses<-as.data.frame(AllLosses)%>%dplyr::select(-geometry)


LossSummary<-AllLosses%>%filter(Parks==FALSE)%>%
  group_by(VDC_Jur)%>%
  summarize(MedLossSize_M = median(Area_M),
            MedLossDist_M = median(DstBn_M),
            TotaLossSize_Ha = sum(Area_M)/10000,
            NLosses=n())

rm(AllLosses)
LossSummary<-CleanUpVDCs(LossSummary)
Losses<-base::merge(Losses,LossSummary,by="VDC_Jur",all.x=T)
#names(Losses)
#Losses<-as.data.frame(Losses)%>%dplyr::select(-c(geometry.x,geometry.y,geometry))

#nrow(Losses)

write.csv(Losses,"../Data/FullSummaryDataset.csv")






