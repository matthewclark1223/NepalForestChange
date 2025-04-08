library(tidyverse)
library(rstan)
library(brms)
library(ggdist)
Losses<-read_csv("../Data/FullSummaryDataset.csv")
Losses<-Losses%>%filter(is.na(Deprvtn )==FALSE)
Losses<-Losses%>%dplyr::filter(Frst_size>0) #Only consider VDCs with forest
Losses$PropFrstLoss<-Losses$TotaLossSize_Ha / Losses$Frst_size
Losses$PropFrstLoss<-ifelse(is.na(Losses$PropFrstLoss),0,Losses$PropFrstLoss)
Losses$PropFrstGap<-Losses$TotalGapSize_ha / Losses$Frst_size
Losses$PropFrstGap<-ifelse(is.na(Losses$PropFrstGap),0,Losses$PropFrstGap)
Losses$PreGapAreaProp<-Losses$PreGapArea_Ha / Losses$Frst_size
Losses$PreGapAreaProp<-ifelse(is.na(Losses$PreGapAreaProp),0,Losses$PreGapAreaProp)

Losses<-Losses[-which(Losses$PropFrstGap>0.4),] #Remove (5) outlier rows with most forest as gaps



#Standardize all covariates
stdize<-function(x){
  (x-mean(x,na.rm=T))/(2*sd(x,na.rm=T))}


Losses$PropCommFrst_STD<-stdize(Losses$PropCommFrst)
Losses$NM_2015_STD<-stdize(Losses$NM_2015)
Losses$Ppl2015_STD<-stdize(Losses$Ppl2015)
Losses$Ppl2022_STD<-stdize(Losses$Ppl2022)
Losses$RoadDensity_STD<-stdize(Losses$RoadDensity)
Losses$Deprvtn_STD<-stdize(Losses$Deprvtn)
Losses$PrecipAnom_STD<-stdize(Losses$PrecipAnom)
Losses$Slope_STD<-stdize(Losses$Slope2022)
Losses$PreGapArea_STD<-stdize(Losses$PreGapAreaProp)




Losses<-Losses%>%dplyr::select(PropFrstLoss,PropFrstGap,PreGapAreaProp,Deprvtn_STD,NM_2015_STD,Ppl2022_STD,RoadDensity_STD,Slope_STD,
                               CF,PropCommFrst_STD,PrecipAnom_STD,NAME_1,NAME_2,NAME_3,PreGapArea_STD,Ppl2015_STD,VDC_Jur )

Losses<-Losses[which(complete.cases(Losses)==T),]

Losses$CF<-ifelse(Losses$CF==TRUE,"CF","NoCF")

fit<-readRDS("../Data/Loss_ZI.rds") #Load fit2_hu


pred <- posterior_predict(fit) #columns are obs, rows are samples
bayesplot::ppc_dens_overlay(y = log1p(Losses$PropFrstLoss), 
                            yrep = log1p(pred[1:50,]))+xlim(0,0.005)



Losses$MedPred<-NA
Losses$LowPred<-NA
Losses$HighPred<-NA

ncol(pred)==nrow(Losses)
for(i in 1:ncol(pred)){
  obs<-pred[,i]
  #Losses[i,]$LowPred<-quantile(obs,0.1)[[1]]
  Losses[i,]$MedPred<-quantile(obs,0.5)[[1]]
  #Losses[i,]$HighPred<-quantile(obs,0.9)[[1]]
}

Losses$LossError<-abs(Losses$PropFrstLoss-Losses$MedPred)                     

mytheme<- theme(axis.title = element_text(color="black",size=16),
                axis.text=element_text(color="black",size=14))

p1<-ggplot(Losses,aes(x=LossError,y=PreGapArea_STD))+
  geom_point(size=2,alpha=0.5)+xlab("Absolute error")+
  annotate("text",x=.15,y=.80,
           label=paste0("cor=",base::round(cor(Losses$LossError,Losses$PreGapArea_STD),digits=2)),
           color="red",fontface="bold",size=10)+ylab("Preexisting gaps")+
  ggthemes::theme_clean()+mytheme

p2<-ggplot(Losses,aes(x=LossError,y=Deprvtn_STD))+
  geom_point(size=2,alpha=0.5)+xlab("Absolute error")+
  annotate("text",x=.15,y=-1,
           label=paste0("cor=",base::round(cor(Losses$LossError,Losses$Deprvtn_STD),digits=2)),
           color="red",fontface="bold",size=10)+ylab("Deprivation")+
  ggthemes::theme_clean()+mytheme

p3<-ggplot(Losses,aes(x=LossError,y=NM_2015_STD))+
  geom_point(size=2,alpha=0.5)+xlab("Absolute error")+
  annotate("text",x=.15,y=.80,
           label=paste0("cor=",base::round(cor(Losses$LossError,Losses$NM_2015_STD),digits=2)),
           color="red",fontface="bold",size=10)+ylab("Net migration")+
  ggthemes::theme_clean()+mytheme

p4<-ggplot(Losses,aes(x=LossError,y=Ppl2022_STD))+
  geom_point(size=2,alpha=0.5)+xlab("Absolute error")+
  annotate("text",x=.15,y=10,
           label=paste0("cor=",base::round(cor(Losses$LossError,Losses$Ppl2022_STD),digits=2)),
           color="red",fontface="bold",size=10)+ylab("Population (2022)")+
  ggthemes::theme_clean()+mytheme

p5<-ggplot(Losses,aes(x=LossError,y=RoadDensity_STD))+
  geom_point(size=2,alpha=0.5)+xlab("Absolute error")+
  annotate("text",x=.15,y=.80,
           label=paste0("cor=",base::round(cor(Losses$LossError,Losses$RoadDensity_STD),digits=2)),
           color="red",fontface="bold",size=10)+ylab("Road density")+
  ggthemes::theme_clean()+mytheme

p6<-ggplot(Losses,aes(x=LossError,y=PropCommFrst_STD))+
  geom_point(size=2,alpha=0.5)+xlab("Absolute error")+
  annotate("text",x=.15,y=.80,
           label=paste0("cor=",base::round(cor(Losses$LossError,Losses$PropCommFrst_STD),digits=2)),
           color="red",fontface="bold",size=10)+ylab("Community forest\nproportion")+
  ggthemes::theme_clean()+mytheme

p7<-ggplot(Losses,aes(x=LossError,y=PrecipAnom_STD))+
  geom_point(size=2,alpha=0.5)+xlab("Absolute error")+
  annotate("text",x=.15,y=.80,
           label=paste0("cor=",base::round(cor(Losses$LossError,Losses$PrecipAnom_STD),digits=2)),
           color="red",fontface="bold",size=10)+ylab("Precipitation anomaly")+
  ggthemes::theme_clean()+mytheme

p8<-ggplot(Losses,aes(x=LossError,y=PrecipAnom_STD*PropCommFrst_STD))+
  geom_point(size=2,alpha=0.5)+xlab("Absolute error")+
  annotate("text",x=.15,y=0,
           label=paste0("cor=",base::round(cor(Losses$LossError,Losses$PrecipAnom_STD*Losses$PropCommFrst_STD),digits=2)),
           color="red",fontface="bold",size=10)+ylab("Precip:Community\nforest interaction")+
  ggthemes::theme_clean()+mytheme

p9<-ggplot(Losses,aes(x=LossError,y=Deprvtn_STD*PropCommFrst_STD))+
  geom_point(size=2,alpha=0.5)+xlab("Absolute error")+ylab("Depriv:Community\nforest interaction")+
  annotate("text",x=.15,y=.80,
           label=paste0("cor=",base::round(cor(Losses$LossError,Losses$Deprvtn_STD*Losses$PropCommFrst_STD),digits=2)),
           color="red",fontface="bold",size=10)+
  ggthemes::theme_clean()+mytheme

pc<-cowplot::plot_grid(p1,p2,p3,p4,p5,p6,p7,p8,p9,nrow=3,align="hv",
                       labels = c('a', 'b','c','d','e','f','g','h','i'),
                       label_size = 22,label_fontface = "bold", label_x = 0.92, label_y = 0.98 )


ggsave("../Figures/LossErrorCorrelations.png",pc,dpi=350,units = "in",width = 14, height = 7, bg="white")


###Gaps
fit<-readRDS("../Data/Gaps_ZI.rds") #Load fit2_hu


pred <- posterior_predict(fit) #columns are obs, rows are samples
#bayesplot::ppc_dens_overlay(y = log1p(Losses$PreGapAreaProp ), 
                    #        yrep = log1p(pred[1:50,]))#+xlim(0,0.01)



Losses$MedPredGap<-NA
Losses$LowPredGap<-NA
Losses$HighPredGap<-NA

ncol(pred)==nrow(Losses)
for(i in 1:ncol(pred)){
  obs<-pred[,i]
  #Losses[i,]$LowPredGap<-quantile(obs,0.1)[[1]]
  Losses[i,]$MedPredGap<-quantile(obs,0.5)[[1]]
  #Losses[i,]$HighPredGap<-quantile(obs,0.9)[[1]]
}

Losses$GapError<-abs(Losses$PreGapAreaProp-Losses$MedPredGap)                     




p1<-ggplot(Losses,aes(x=GapError,y=Deprvtn_STD))+
  geom_point(size=2,alpha=0.5)+xlab("Absolute error")+
  annotate("text",x=.15,y=-1,
           label=paste0("cor=",base::round(cor(Losses$GapError,Losses$Deprvtn_STD),digits=2)),
           color="red",fontface="bold",size=10)+ylab("Deprivation")+
  ggthemes::theme_clean()+mytheme

p2<-ggplot(Losses,aes(x=GapError,y=NM_2015_STD))+
  geom_point(size=2,alpha=0.5)+xlab("Absolute error")+
  annotate("text",x=.15,y=5,
           label=paste0("cor=",base::round(cor(Losses$GapError,Losses$NM_2015_STD),digits=2)),
           color="red",fontface="bold",size=10)+ylab("Net migration")+
  ggthemes::theme_clean()+mytheme

p3<-ggplot(Losses,aes(x=GapError,y=Ppl2015_STD))+
  geom_point(size=2,alpha=0.5)+xlab("Absolute error")+
  annotate("text",x=.15,y=10,
           label=paste0("cor=",base::round(cor(Losses$GapError,Losses$Ppl2022_STD),digits=2)),
           color="red",fontface="bold",size=10)+ylab("Population (2022)")+
  ggthemes::theme_clean()+mytheme

p4<-ggplot(Losses,aes(x=GapError,y=RoadDensity_STD))+
  geom_point(size=2,alpha=0.5)+xlab("Absolute error")+
  annotate("text",x=.15,y=2,
           label=paste0("cor=",base::round(cor(Losses$GapError,Losses$RoadDensity_STD),digits=2)),
           color="red",fontface="bold",size=10)+ylab("Road density")+
  ggthemes::theme_clean()+mytheme

p5<-ggplot(Losses,aes(x=GapError,y=PropCommFrst_STD))+
  geom_point(size=2,alpha=0.5)+xlab("Absolute error")+
  annotate("text",x=.15,y=.5,
           label=paste0("cor=",base::round(cor(Losses$GapError,Losses$PropCommFrst_STD),digits=2)),
           color="red",fontface="bold",size=10)+ylab("Community forest\nproportion")+
  ggthemes::theme_clean()+mytheme





pc<-cowplot::plot_grid(p1,p2,p3,p4,p5,nrow=2,align="hv",
                       labels = c('a', 'b','c','d','e'),
                       label_size = 22,label_fontface = "bold", label_x = 0.92, label_y = 0.98 )


ggsave("../Figures/GapErrorCorrelations.png",pc,dpi=350,units = "in",width = 14, height = 7, bg="white")

###########
#spatial autocorrelation
library(sf)
Nepal<-read_sf("../Data/Nepal_fug.shp")
Nepal<-base::merge(Nepal,Losses,by="VDC_Jur" , all.x=T )

p1<-ggplot(Nepal)+geom_sf(aes(fill=LossError),color=NA)+scale_fill_viridis_c(name="Absolute error")+
  ggtitle("Losses (2018 - 2023)")+
  annotate("text",x=86,y=30,
           label="Moran's I = 0.11",
           color="red",fontface="bold",size=6)+ylab("")+xlab("")+mytheme
p2<-ggplot(Nepal)+geom_sf(aes(fill=LossError),color=NA)+scale_fill_viridis_c(name="Absolute error\n(log10)", trans="log10")+
  ggtitle("Losses (2018 - 2023)")+ylab("")+xlab("")+mytheme
p3<-ggplot(Nepal)+geom_sf(aes(fill=GapError),color=NA)+scale_fill_viridis_c(name="Absolute error")+
  ggtitle("Gaps (<2018)")+
  annotate("text",x=86,y=30,
           label="Moran's I = 0.28",
           color="red",fontface="bold",size=6)+ylab("")+xlab("")+mytheme
p4<-ggplot(Nepal)+geom_sf(aes(fill=GapError),color=NA)+scale_fill_viridis_c(name="Absolute error\n(log10)", trans="log10")+
  ggtitle("Gaps (<2018)")+ylab("")+xlab("")+mytheme

pc<-cowplot::plot_grid(p1,p2,p3,p4,nrow=2,align="hv",
                       labels = c('a', 'b','c','d'),
                       label_size = 22,label_fontface = "bold" )

ggsave("../Figures/ErrorSpatialClustering.png",pc,dpi=350,units = "in",width = 14, height = 7, bg="white")


library(spdep)

Nepal <- Nepal[!is.na(Nepal$LossError), ]

nb <- poly2nb(Nepal, queen = TRUE) # queen shares point or border
connected <- which(card(nb) > 0)  # Keep only polygons with neighbors
Nepal <- Nepal[connected, ]       # Subset only connected polygons
nb <- poly2nb(Nepal, queen = TRUE)  # Recompute neighborhood
nbw <- nb2listw(nb, style = "W")    # Create weight list

# Global Moran's I
gmoran <- moran.test(Nepal$LossError, nbw,
                     alternative = "greater")
gmoran

#Now do spatial autocorrelation of just the predictor so compare with model
Nepal <- Nepal[!is.na(Nepal$PrecipAnom_STD), ]
nb <- poly2nb(Nepal, queen = TRUE) # queen shares point or border
connected <- which(card(nb) > 0)  # Keep only polygons with neighbors
Nepal <- Nepal[connected, ]       # Subset only connected polygons
nb <- poly2nb(Nepal, queen = TRUE)  # Recompute neighborhood
nbw <- nb2listw(nb, style = "W")  
gmoran <- moran.test(Nepal$PrecipAnom_STD, nbw,
                     alternative = "greater")
gmoran



