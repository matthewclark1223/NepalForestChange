library(tidyverse)
dat<-read_csv("../Data/OutcomeData_AllSlope.csv")

dat$CF<-ifelse(dat$CF==0,FALSE,TRUE)

dat%>%#group_by(CF)%>%
  summarise(ForestArea=sum(Frst_size),
            GapsArea=sum(Gap_size),
            LossArea=sum(Floss_size),
            PercGap=(GapsArea/ForestArea)*100,
            PercLoss=(LossArea/ForestArea)*100)
  