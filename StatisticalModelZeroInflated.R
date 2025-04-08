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
                               CF,PropCommFrst_STD,PrecipAnom_STD,NAME_1,NAME_2,NAME_3,PreGapArea_STD,Ppl2015_STD)

Losses<-Losses[which(complete.cases(Losses)==T),]

Losses$CF<-ifelse(Losses$CF==TRUE,"CF","NoCF")


prior<-get_prior(PropFrstLoss~Deprvtn_STD+NM_2015_STD+Slope_STD+PreGapArea_STD+
  Ppl2022_STD+RoadDensity_STD+PrecipAnom_STD*PropCommFrst_STD+
  PrecipAnom_STD*Deprvtn_STD+
  (1|NAME_1/NAME_2/NAME_3),
family=zero_inflated_beta(),
data=Losses)

prior$prior[1]<-"normal(0,2)" #coefficient default
prior$prior[12]<-"normal(0,2)" #random intercept default
prior$prior[13]<-"normal(1,0.5)" #Over dispersion parameter
prior$prior[21]<-"normal(-2,1)" #zer0 inflation probability


fit2_zi<-brms::brm(
  PropFrstLoss~Deprvtn_STD+NM_2015_STD+Slope_STD+PreGapArea_STD+
       Ppl2022_STD+RoadDensity_STD+PrecipAnom_STD*PropCommFrst_STD+
       PrecipAnom_STD*Deprvtn_STD+
       (1|NAME_1/NAME_2/NAME_3),
  family=zero_inflated_beta(),
  data=Losses,chains=4,cores=4,iter=5000,#prior=prior,
  control = list(adapt_delta = 0.99))

saveRDS(fit2_zi,"../Data/Loss_ZI.rds")
fit2_zi<-readRDS("../Data/Loss_ZI.rds")


LossParsPlot<-mcmc_plot(fit2_zi, variable = c("STD"), regex = TRUE, prob_outer = 0.9)+
  ggthemes::theme_clean()+
  geom_vline(xintercept=0,linetype=1,color="white")+
  geom_vline(xintercept=0,linetype=2,color="black")+
  #scale_y_discrete(labels=c("Percipitation (12mo)","Deprivation","Nature\ndependence",
  #                        "Distance to\n urban center", "Interaction\n(Precipitation*deprivation)"))+xlab("Standardized coefficient estimate")+
  theme(axis.text.y  = element_text(face= "plain"),
        axis.title = element_text(color="black",size=20),
        axis.text=element_text(color="black",size=15))+ylab("Predictor")+
  scale_y_discrete(labels=c("Deprivation\n(wellbeing reverse)","Net migration\n(2015 - 2019)",
                           "Topographic slope\n(control)","Pre-existing gap\narea","Population\n(2022)",
                           "Road density","Precipitation anomaly\n(inverse)","Community forest area\n(proportion)",
                           "Precipitation:community\nforest interaction","Precipitation:deprivation\ninteraction"))+
  mytheme

ggsave("../Figures/LossParsPlotZeroInf.png",LossParsPlot,dpi=350,units = "in",
       width = 9, height = 9, bg="white")


p<-conditional_effects(fit2_zi, effects = "PropCommFrst_STD:PrecipAnom_STD", 
                       #conditions=list(),
                       int_conditions = list(PropCommFrst_STD = range(Losses$PropCommFrst_STD),
                                             PrecipAnom_STD=seq(from=min(Losses$PrecipAnom_STD),to=max(Losses$PrecipAnom_STD),by=0.05)),
                       #spaghetti = T,ndraws=200,
                       prob=c(0.5),robust=TRUE )

p_0.9<-conditional_effects(fit2_zi, effects = "PropCommFrst_STD:PrecipAnom_STD", 
                           #conditions=list(),
                           int_conditions = list(PropCommFrst_STD = range(Losses$PropCommFrst_STD),
                                                 PrecipAnom_STD=seq(from=min(Losses$PrecipAnom_STD),to=max(Losses$PrecipAnom_STD),by=0.05)),
                           #spaghetti = T,ndraws=200,
                           prob=c(0.9),robust=TRUE )




f1<-p[[1]]%>%
  ggplot(.,aes(x=PrecipAnom_STD, y=estimate__))+
  #geom_point(data=dat,aes(x=rollsumSTD,y=Slaughter),
  #          color="black",alpha=0.75)+
  geom_ribbon(data=p_0.9[[1]],aes(x=PrecipAnom_STD,ymin=lower__,ymax=upper__,
                                  fill=as.character(PropCommFrst_STD)),alpha=0.2)+
  geom_lineribbon(aes(ymin=lower__,ymax=upper__,
                      fill=as.character(PropCommFrst_STD)),color="black",alpha=0.9)+
  #geom_line(aes(color=as.character(PropCommFrst_STD)),linewidth=2)+
  scale_fill_manual(values=c("#D55E00","#009E73"),
                    labels=c("No community forest","All community forest"),
                    name="")+
  scale_color_manual(values=c("#D55E00","#009E73"),
                     labels=c("No community forest","All community forest"),
                     name="")+
  #scale_color_viridis_d(name="Deprivation",option="I")+
  #scale_fill_viridis_d(alpha=0.4,name="Deprivation",option="I")+
  scale_y_continuous(labels = scales::percent,limits=c(0.0002,0.0017))+
  xlab("Precipitation anomaly")+
  ylab("Average expected forest loss")+
  scale_x_reverse(breaks=c(0.9,-1),labels = c("Least anomalous","Most anomalous"))+
  ggthemes::theme_clean()+mytheme+theme(legend.title=element_blank(),
                                        legend.text=element_text(color="black",size=16),
                                        legend.key.size = unit(0.5,"in"),
                                        legend.position = c(0.5,0.85))


p5<-conditional_effects(fit2_zi, effects = "PrecipAnom_STD:Deprvtn_STD", 
                        conditions=list(PropCommFrst_STD=1.25),
                        int_conditions = list(Deprvtn_STD = c(-0.5,0.5),
                                              # Deprvtn_STD =range(Losses$Deprvtn_STD,na.rm=T),
                                              PrecipAnom_STD=seq(from=min(Losses$PrecipAnom_STD),to=max(Losses$PrecipAnom_STD),by=0.05)),#spaghetti = T,ndraws=200,
                        prob=0.5,robust=TRUE)

p5_0.9<-conditional_effects(fit2_zi, effects = "PrecipAnom_STD:Deprvtn_STD", 
                            conditions=list(PropCommFrst_STD=1.25),
                            int_conditions = list(Deprvtn_STD = c(-0.5,0.5),
                                                  # Deprvtn_STD =range(Losses$Deprvtn_STD,na.rm=T),
                                                  PrecipAnom_STD=seq(from=min(Losses$PrecipAnom_STD),to=max(Losses$PrecipAnom_STD),by=0.05)),#spaghetti = T,ndraws=200,
                            prob=0.9,robust=TRUE)


f5<-p5[[1]]%>%
  ggplot(.,aes(x=PrecipAnom_STD, y=estimate__))+
  geom_ribbon(data=p5_0.9[[1]],aes(x=PrecipAnom_STD,ymin=lower__,ymax=upper__,
                                   fill=as.character(Deprvtn_STD)),alpha=0.2)+
  geom_lineribbon(aes(ymin=lower__,ymax=upper__,
                      fill=as.character(Deprvtn_STD)),color="black",alpha=0.9)+
  scale_fill_manual(values=c("#0072B2","#E69F00"),
                    labels=c("Better-off","Worse-off"),
                    name="Community wellbeing")+
  scale_color_manual(values=c("#0072B2","#E69F00"),
                     labels=c("Better-off","Worse-off"),
                     name="Community wellbeing")+
  #scale_color_viridis_d(name="Deprivation",option="I")+
  #scale_fill_viridis_d(alpha=0.4,name="Deprivation",option="I")+
  xlab("Precipitation anomaly")+
  ylab("Average expected forest loss")+
  scale_y_continuous(labels=scales::percent,limits=c(0.0002,0.0017))+
  scale_x_reverse(breaks=c(0.9,-1),labels = c("Least anomalous","Most anomalous"))+
  ggthemes::theme_clean()+mytheme+theme(legend.text=element_text(color="black",size=16),
                                        legend.key.size = unit(0.5,"in"),
                                        legend.title=element_text(color="black",size=18),
                                        legend.position = c(0.5,0.85))

pc<-cowplot::plot_grid(f1,f5,nrow=1,align="hv",
                       labels = c('a', 'b'),
                       label_size = 22,label_fontface = "bold")


ggsave("../Figures/ConditionalEffectsCombined_ZeroInf.png",pc,dpi=350,units = "in",
       width = 18, height = 6, bg="white")



##### gaps model
fitGap_zi<-brms::brm(PreGapAreaProp~Deprvtn_STD+NM_2015_STD+Slope_STD+
                 Ppl2015_STD+RoadDensity_STD+PropCommFrst_STD+
                 (1|NAME_1/NAME_2/NAME_3),
               family=zero_inflated_beta(),data=Losses,chains=4,cores=4,
               iter=5000,#prior=prior,
               control = list(adapt_delta = 0.99,max_treedepth=15 ) )

saveRDS(fitGap_zi,"../Data/Gaps_ZI.rds")

fitGap_zi<-readRDS("../Data/Gaps_ZI.rds")

GapPars<-mcmc_plot(fitGap_zi, variable = c("STD"), regex = TRUE, prob_outer = 0.9)+
  ggthemes::theme_clean()+
  geom_vline(xintercept=0,linetype=1,color="white")+
  geom_vline(xintercept=0,linetype=2,color="black")+ylab("Predictor")+
  scale_y_discrete(labels=c("Deprivation\n(wellbeing reverse)","Net migration\n(2015 - 2019)",
                            "Topographic slope\n(control)","Population\n(2022)",
                            "Road density","Community forest area\n(proportion)"))+   
  xlab("Standardized coefficient estimate")+
  mytheme

ggsave("../Figures/GapsParsPlotZeroInf.png",GapPars,dpi=350,units = "in",
       width = 9, height = 9, bg="white")

###CF conditional effect
p12<-conditional_effects(fitGap_zi, effects = "PropCommFrst_STD",
                         prob=0.5,robust=TRUE )
p12_0.9<-conditional_effects(fitGap_zi, effects = "PropCommFrst_STD",
                             prob=0.9,robust=TRUE )

f12<-p12[[1]]%>%
  ggplot(.,aes(x=PropCommFrst_STD, y=estimate__))+
  #geom_point(data=dat,aes(x=rollsumSTD,y=Slaughter),
  #          color="black",alpha=0.75)+
  geom_ribbon(data=p12_0.9[[1]],aes(x=PropCommFrst_STD,ymin=lower__,ymax=upper__),alpha=0.5,fill="#525252")+
  geom_lineribbon(aes(ymin=lower__,ymax=upper__),alpha=0.99,color="white",fill="#525252")+
  #scale_color_viridis_d(name="Deprivation",option="I")+
  #scale_fill_viridis_d(alpha=0.4,name="Deprivation",option="I")+
  xlab("Proportion of forest under community management")+
  ylab("Average expected forest\ngap cover")+
  scale_y_continuous(labels=scales::percent,limits = c(0.01,0.05))+
  scale_x_continuous(breaks = range(Losses$PropCommFrst_STD),
                     labels=c(0,1.0))+
  ggthemes::theme_clean()+mytheme

ggsave("../Figures/CFCondEffectGAPS.png",f12,dpi=350,units = "in",width = 8.5, height = 6, bg="white")



p3<-conditional_effects(fitGap_zi, effects = "Deprvtn_STD",
                        prob=0.5,robust=TRUE )

p3_0.9<-conditional_effects(fitGap_zi, effects = "Deprvtn_STD",
                            prob=0.9,robust=TRUE )

f3<-p3[[1]]%>%
  ggplot(.,aes(x=Deprvtn_STD, y=estimate__))+
  geom_ribbon(data=p3_0.9[[1]],aes(x=Deprvtn_STD,ymin=lower__,ymax=upper__),alpha=0.5,fill="#525252")+
  geom_lineribbon(aes(ymin=lower__,ymax=upper__),alpha=0.99,color="white",fill="#525252")+
  xlab("Community wellbeing")+
  ylab("Average expected forest\ngap cover")+
  scale_y_continuous(labels=scales::percent,limits = c(0.01,0.05))+
  scale_x_reverse(breaks = c(-2.99,0.9),
                  labels=c("Better-off","Worse-off"))+
  ggthemes::theme_clean()+mytheme

ggsave("../Figures/GapsDepCondEffect.png",f3,dpi=350,units = "in",width = 8.5, height = 6, bg="white")

p4<-conditional_effects(fitGap_zi, effects = "Ppl2015_STD",
                        prob=0.5,robust=TRUE )

f4<-p4[[1]]%>%
  ggplot(.,aes(x=Ppl2015_STD, y=estimate__))+
  #geom_point(data=dat,aes(x=rollsumSTD,y=Slaughter),
  #          color="black",alpha=0.75)+
  geom_ribbon(aes(ymin=lower__,ymax=upper__),alpha=0.4)+
  geom_line(linewidth=2)+
  #scale_color_viridis_d(name="Deprivation",option="I")+
  #scale_fill_viridis_d(alpha=0.4,name="Deprivation",option="I")+
  xlab("VDC population")+
  ylab("Average expected forest\ngap cover")+
  scale_y_continuous(labels=scales::percent)+
  scale_x_continuous(breaks = c(-0.14,20),
                     labels=c("Less people","More people"))+
  ggthemes::theme_clean()+mytheme

ggsave("../Figures/GapsPopCondEffect.png",f4,dpi=350,units = "in",width = 8.5, height = 6, bg="white")


#Make road scale
RD_fit<-lm(RoadDensity_STD~RoadDensity,data=Losses)
stats::predict(RD_fit,newdata=data.frame(RoadDensity=c(1,9,18)))

p6<-conditional_effects(fitGap_zi, effects = "RoadDensity_STD",
                        prob=0.5,robust=TRUE )

p6_0.9<-conditional_effects(fitGap_zi, effects = "RoadDensity_STD",
                            prob=0.9,robust=TRUE )

f6<-p6[[1]]%>%
  ggplot(.,aes(x=RoadDensity_STD, y=estimate__))+
  geom_ribbon(data=p6_0.9[[1]],aes(x=RoadDensity_STD,ymin=lower__,ymax=upper__),alpha=0.5,fill="#525252")+
  geom_lineribbon(aes(ymin=lower__,ymax=upper__),alpha=0.99,color="white",fill="#525252")+
  # xlab("Road density (km/km")+
  xlab(bquote('Road density '(km/km^2)))+
  ylab("Average expected forest\ngap cover")+
  scale_y_continuous(labels=scales::percent)+
  scale_x_continuous(breaks = c(-0.533,1.532,3.855),
                     labels=c("1","9","18"))+
  ggthemes::theme_clean()+mytheme


ggsave("../Figures/GapsRoadsCondEffect.png",f6,dpi=350,units = "in",width = 8.5, height = 6, bg="white")


pc<-cowplot::plot_grid(f12,f3,f6,nrow=2,align="hv",
                       labels = c('a', 'b','c'),
                       label_size = 22,label_fontface = "bold")

ggsave("../Figures/GAPSConditionalEffectsCombined.png",pc,dpi=350,units = "in",
       width = 18, height = 12, bg="white")




