library(tidyverse)
library(rstan)
library(brms)
library(ggdist)
Losses<-read_csv("../Data/FullSummaryDataset.csv")
Losses<-Losses%>%dplyr::filter(Frst_size>0) #Only consider VDCs with forest
Losses$PropFrstLoss<-Losses$TotaLossSize_Ha / Losses$Frst_size
Losses$PropFrstLoss<-ifelse(is.na(Losses$PropFrstLoss),0,Losses$PropFrstLoss)
Losses$PropFrstGap<-Losses$TotalGapSize_ha / Losses$Frst_size
Losses$PropFrstGap<-ifelse(is.na(Losses$PropFrstGap),0,Losses$PropFrstGap)
Losses$PreGapAreaProp<-Losses$PreGapArea_Ha / Losses$Frst_size
Losses$PreGapAreaProp<-ifelse(is.na(Losses$PreGapAreaProp),0,Losses$PreGapAreaProp)

Losses<-Losses[-which(Losses$PropFrstGap>0.4),] #Remove (5) outlier rows with most forest as gaps

cor(Losses$Deprvtn,Losses$PropCommFrst,use="complete.obs")


mytheme<- theme(axis.title = element_text(color="black",size=22),
                axis.text=element_text(color="black",size=18))

x1<-ggplot(Losses,aes(x=Deprvtn,y=PropCommFrst))+#geom_point(size=2,alpha=0.25)
  geom_hex(binwidth=c(6,.08))+
  scale_fill_viridis_c(name="Observation\ndensity (count)")+
  scale_y_continuous(labels=scales::percent,
                     name="Forest in community management")+
  annotate("text",x=30,y=.80,
           label="cor=0.06",color="red",fontface="bold",size=6)+
  #geom_jitter(size=1.5,alpha=0.5)+
  scale_x_reverse(breaks = c(15,90),
                  labels=c("Better-off","Worse-off"),name="Community wellbeing")+
  #geom_smooth(method="gam",color="red",se=F) 
  ggthemes::theme_clean()+
  mytheme

ggsave("../Figures/DepCFCor.png",x1,dpi=350,units = "in",width = 8.5, height = 6, bg="white")

cor(Losses$Ppl2015,Losses$Frst_size,use="complete.obs")
#exclude Kathmandu which has a pop of nearly 1.2 million
d<-Losses[-which(Losses$Ppl2015==max(Losses$Ppl2015)),]
x2<-ggplot(d,aes(x=Ppl2015,y=Frst_size))+#geom_point(size=2,alpha=0.25)
  geom_hex(bins=40)+
  scale_fill_viridis_c(name="Observation\ndensity (count)")+
  scale_y_continuous(name="Forest size (ha)",labels=scales::comma)+
  scale_x_continuous(labels=scales::comma,breaks=c(0,125000,250000),
                     name="Population 2015")+
  ggthemes::theme_clean()+
  mytheme

ggsave("../Figures/PopForestCor.png",x2,dpi=350,units = "in",width = 8.5, height = 6, bg="white")


ggplot(Losses,aes(x=PropFrstGap,y=PropFrstLoss))+geom_point()+
  geom_smooth(method="lm",color="red",se=F) +ggthemes::theme_clean()
ggplot(Losses,aes(x=PropCommFrst,y=PropFrstLoss))+geom_point()+
  geom_smooth(method="lm",color="red",se=F) +ggthemes::theme_clean()
ggplot(Losses,aes(x=PropCommFrst,y=PropFrstGap))+geom_point()+
  geom_smooth(method="lm",color="red",se=F) +ggthemes::theme_clean()

fit<-readRDS("../Data/LossHurdle.rds") #Load fit2_hu
















