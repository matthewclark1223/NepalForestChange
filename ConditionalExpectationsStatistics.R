library(brms)
fit2_zi<-readRDS("../Data/Loss_ZI.rds")

p_0.9<-conditional_effects(fit2_zi, effects = "PropCommFrst_STD:PrecipAnom_STD", 
                           #conditions=list(),
                           int_conditions = list(PropCommFrst_STD = range(Losses$PropCommFrst_STD),
                                                 PrecipAnom_STD=seq(from=min(Losses$PrecipAnom_STD),to=max(Losses$PrecipAnom_STD),by=0.05)),
                           #spaghetti = T,ndraws=200,
                           prob=c(0.9),robust=TRUE )


d<-p_0.9[[1]]%>%
  group_by(PropCommFrst_STD)%>%filter(PrecipAnom_STD==min(PrecipAnom_STD))%>%
  mutate(estimate__=estimate__*100,
         lower__=lower__*100,
         upper__=upper__*100)
View(d)

p5_0.9<-conditional_effects(fit2_zi, effects = "PrecipAnom_STD:Deprvtn_STD", 
                            conditions=list(PropCommFrst_STD=1.25),
                            int_conditions = list(Deprvtn_STD = c(-0.5,0.5),
                                                  # Deprvtn_STD =range(Losses$Deprvtn_STD,na.rm=T),
                                                  PrecipAnom_STD=seq(from=min(Losses$PrecipAnom_STD),to=max(Losses$PrecipAnom_STD),by=0.05)),#spaghetti = T,ndraws=200,
                            prob=0.9,robust=TRUE)

d<-p5_0.9[[1]]%>%
  group_by(Deprvtn_STD)%>%filter(PrecipAnom_STD==max(PrecipAnom_STD))%>%
  mutate(estimate__=estimate__*100,
         lower__=lower__*100,
         upper__=upper__*100)
View(d)


posterior_samples <- as.data.frame(posterior_samples(fit2_zi, pars = "b_RoadDensity_STD"))

length(which(posterior_samples[["b_RoadDensity_STD"]] > 0))/nrow(posterior_samples)


########## gaps

fitGap_zi<-readRDS("../Data/Gaps_ZI.rds")

p12_0.9<-conditional_effects(fitGap_zi, effects = "PropCommFrst_STD",
                             prob=0.9,robust=TRUE )


d<-p12_0.9[[1]]%>%
  filter(PropCommFrst_STD==min(PropCommFrst_STD))%>%
  mutate(estimate__=estimate__*100,
         lower__=lower__*100,
         upper__=upper__*100)
View(d)

p12_0.9<-conditional_effects(fitGap_zi, effects = "RoadDensity_STD",
                             prob=0.9,robust=TRUE )


d<-p12_0.9[[1]]%>%
  filter(RoadDensity_STD==max(RoadDensity_STD))%>%
  mutate(estimate__=estimate__*100,
         lower__=lower__*100,
         upper__=upper__*100)
View(d)


posterior_samples <- as.data.frame(posterior_samples(fitGap_zi, pars = "b_Ppl2015_STD"))

length(which(posterior_samples[["b_Ppl2015_STD"]] < 0))/nrow(posterior_samples)



