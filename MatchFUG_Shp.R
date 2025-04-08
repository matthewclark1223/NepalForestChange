library(tidyverse)
library(sf)
Nepal<-st_read("../Data/NepalShape/gadm41_NPL_4.shp")
fug<-read_csv("../Data/FUG_Final.csv")




fug<-fug[-c(1,3,4,8,13,14,15,16)] #remove cols we dont need now
names(fug)[3:8]<-c("VDC","Name","Area","HH","ComMemb","Women") #rename cols

#The NAME_4 col in the Nepal shp data 
#Are the VDCs. We need to match these with the VDC col from the fug data

#Some VDCs contain multiple fugs. We need to remove the numbers after these in the fug data
#Also, there are some misepellings, e.g., sapata ve sappata. 
#Also, there are some that have multiple VDC names. 
  #For now let's go with only the 1st

#Remove the numbers first

fug$VDC<-gsub("[-,1,2,3,4,5,6,7,8,9,0].*","",fug$VDC ) #everything after these too
fug$VDC<-gsub("\\&.*","",fug$VDC) #Remove everythng after &. This is to get rid of 
#fugs that are assigned to multiple vdcs. We're taking only the 1st
fug$VDC<-trimws(fug$VDC) #trims leading and trailing white space without removing space
#between two word VDCs
fug$VDC<-str_to_title(fug$VDC) #Capitalize first letter of every word (as in shp data)
fug$VDC<-gsub(" ","",fug$VDC)#remove spaces to create camel case.
fug<-fug%>%filter(!is.na(VDC))#remove NAs
fug<-fug%>%filter(VDC!="")#remove blanks

#Now, some names are shared across different jurisdictions. Let's make a new col with the unique combinations
#First, we need to make sure the Jurisdiction names all match...not that many so just doing manually...
fug[which(fug$Jurisdiction=="Chitwan"),]$Jurisdiction<-"Chitawan"
fug[which(fug$Jurisdiction=="Dabdeldhura"),]$Jurisdiction<-"Dadeldhura"
fug[which(fug$Jurisdiction=="Dhanusha"),]$Jurisdiction<-"Dhanusa"
fug[which(fug$Jurisdiction=="Kabhrepalanchowk"),]$Jurisdiction<-"Kavrepalanchok"
fug[which(fug$Jurisdiction=="Kailikot"),]$Jurisdiction<-"Kalikot"
fug[which(fug$Jurisdiction=="Rauthat"),]$Jurisdiction<-"Rautahat"
fug[which(fug$Jurisdiction=="Sindhu Palchowk"),]$Jurisdiction<-"Sindhupalchok"
fug[which(fug$Jurisdiction=="Terahathum"),]$Jurisdiction<-"Terhathum"
fug[which(fug$Jurisdiction=="Udaypur"),]$Jurisdiction<-"Udayapur"
fug[which(fug$VDC=="PashimPipra"),]$VDC<-"PipraWest"
fug[which(fug$VDC=="Ramnagar"),]$VDC<-"RamnagarMirchaiya"



fug$VDC_Jur<-paste0(fug$Jurisdiction,"_",fug$VDC) #Make the unique combo

#Now do similar processing to shp data
Nepal[which(Nepal$NAME_4=="Pipra(Purba)"),]$NAME_4<-"PipraEast"
Nepal[which(Nepal$NAME_4=="Pipra(West)"),]$NAME_4<-"PipraWest"
Nepal$NAME_4<-gsub(" ","",Nepal$NAME_4)#remove spaces to create camel case.
Nepal$NAME_4<-gsub("N.P.","",Nepal$NAME_4)#remove random N.P.s
Nepal$NAME_5<-gsub("\\(.*","",Nepal$NAME_4) #New column without parenthetical names

Nepal$VDC_Jur<-paste0(Nepal$NAME_3,"_",Nepal$NAME_5)


##There are two categories of spelling errors. 
#1. typos that are row-specific
#2. misspellings of VDCs across all obs. 

#This should deal with type
#Weird loop to catch random misspellings...they are rampant
for(i in 2:nrow(fug)-1){
  
if(!fug[i,]$VDC_Jur %in%Nepal$VDC_Jur  ){ #If it's not in the shp data
  Before<-fug[i-1,]$VDC_Jur #vdc before
  After<-fug[i+1,]$VDC_Jur #vdc after
  if(Before==After ){ #In the one before and after are the same
    
      fug[i,]$VDC_Jur<-After #Make it match the others
    }
  }
  print(i)
}

fug$VDC_Jur2<-fug$VDC_Jur#Make a container for the new names

DIFFLIB<-reticulate::import("difflib") #Load a python package only run once

##Now do the fuzzy matching using 'levenshtein distance'
for(i in 1:nrow(fug)){
  
  if(!fug[i,]$VDC_Jur %in%Nepal$VDC_Jur  ){ #If it's not in the shp data
    
    NepalSub<-Nepal[which(Nepal$NAME_3==fug[i,]$Jurisdiction),] #Only look in the same jurisdiction 
    
    NewNAME<-DIFFLIB$get_close_matches(fug[i,]$VDC_Jur,NepalSub$VDC_Jur,1L,cutoff=0.75)#0.9 match or better. High bc we are including the Juristidtion and we know those match
    
    fug[i,]$VDC_Jur2<- ifelse(length(NewNAME)==0,fug[i,]$VDC_Jur,NewNAME) #If it's NA, keep the old name
    
  }
  print(i)}

length(which(!unique(fug$VDC_Jur2)%in%unique(Nepal$VDC_Jur)))
#Check that there are no duplicate names in VDCs with CFs
#x<-Nepal%>%group_by(VDC_Jur)%>%count() 
#which(x[which(x$n==2),]$VDC_Jur%in%fug$VDC_Jur2)

#all placed except for 39...pretty good!

#DIFFLIB$get_close_matches("Banke_Raptisonari",Nepal$VDC_Jur,1L,cutoff=0.65)


#This summarises CF areas and numbers in each VDC, Keeps NAs, so area will be an udercount for some
fugSum<-fug%>%group_by(VDC_Jur2)%>%summarise(CF_area=sum(Area,na.rm=T),CF=n())  

Nepal<-base::merge(x=Nepal,y=fugSum,by.x="VDC_Jur",by.y="VDC_Jur2",all.x=T,all.y=F)

ggplot(Nepal)+geom_sf(aes(fill=CF_area))


st_write(Nepal,"../Data/Nepal_FUG.shp",append=FALSE)


