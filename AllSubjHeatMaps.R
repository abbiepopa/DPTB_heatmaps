#Import your Data for 22q
HMD277 <-read.table(file.choose(),header=TRUE,sep=",")
HMD283 <-read.table(file.choose(),header=TRUE,sep=",")
HMD357 <-read.table(file.choose(),header=TRUE,sep=",")
HMD368 <-read.table(file.choose(),header=TRUE,sep=",")
HMD385 <-read.table(file.choose(),header=TRUE,sep=",")
HMD677 <-read.table(file.choose(),header=TRUE,sep=",")
HMD687 <-read.table(file.choose(),header=TRUE,sep=",")
HMD690 <-read.table(file.choose(),header=TRUE,sep=",")
HMD731 <-read.table(file.choose(),header=TRUE,sep=",")
HMD733 <-read.table(file.choose(),header=TRUE,sep=",")
#HMD737 <-read.table(file.choose(),header=TRUE,sep=",")
HMD740 <-read.table(file.choose(),header=TRUE,sep=",")
#HMD744 <-read.table(file.choose(),header=TRUE,sep=",")
#HMD745 <-read.table(file.choose(),header=TRUE,sep=",")
HMD754 <-read.table(file.choose(),header=TRUE,sep=",")
HMD755 <-read.table(file.choose(),header=TRUE,sep=",")
HMD761 <-read.table(file.choose(),header=TRUE,sep=",")
HMD763 <-read.table(file.choose(),header=TRUE,sep=",")
HMD768 <-read.table(file.choose(),header=TRUE,sep=",")
HMD170 <-read.table(file.choose(),header=TRUE,sep=",")
HMD300 <-read.table(file.choose(),header=TRUE,sep=",")
HMD280 <-read.table(file.choose(),header=TRUE,sep=",")
HMD354 <-read.table(file.choose(),header=TRUE,sep=",")
HMD783 <-read.table(file.choose(),header=TRUE,sep=",")
HMD784 <-read.table(file.choose(),header=TRUE,sep=",")
HMD788 <-read.table(file.choose(),header=TRUE,sep=",")
HMD792 <-read.table(file.choose(),header=TRUE,sep=",")
HMD793 <-read.table(file.choose(),header=TRUE,sep=",")
HMD798 <-read.table(file.choose(),header=TRUE,sep=",")

#HMD22qFull <-rbind(HMD277,HMD283,HMD357,HMD368,HMD385,HMD677,HMD687,HMD690, HMD731,HMD733,HMD737,HMD740,HMD744,HMD754,HMD755,HMD761,HMD763,HMD768,HMD170,HMD300,HMD280,HMD354,HMD745,HMD783, HMD784, HMD788, HMD792, HMD793)

HMD22qFull <-rbind(HMD277,HMD283,HMD357,HMD368,HMD385,HMD677,HMD687,HMD690, HMD731,HMD733,HMD740,HMD754,HMD755,HMD761,HMD763,HMD768,HMD170,HMD300,HMD280,HMD354,HMD783, HMD784, HMD788, HMD792, HMD793, HMD798)

#Import your Data for TD
HMD779 <-read.table(file.choose(),header=TRUE,sep=",")
HMD676 <-read.table(file.choose(),header=TRUE,sep=",")
HMD688 <-read.table(file.choose(),header=TRUE,sep=",")
#HMD750 <-read.table(file.choose(),header=TRUE,sep=",")
HMD756 <-read.table(file.choose(),header=TRUE,sep=",")
#HMD773 <-read.table(file.choose(),header=TRUE,sep=",")
HMD774 <-read.table(file.choose(),header=TRUE,sep=",")
HMD775 <-read.table(file.choose(),header=TRUE,sep=",")
HMD777 <-read.table(file.choose(),header=TRUE,sep=",")
#HMD303 <-read.table(file.choose(),header=TRUE, sep=",")
#HMD786 <-read.table(file.choose(),header=TRUE,sep=",")
HMD790 <-read.table(file.choose(),header=TRUE,sep=",")
HMD796 <-read.table(file.choose(),header=TRUE,sep=",")
HMD797 <-read.table(file.choose(),header=TRUE,sep=",")
HMD799 <-read.table(file.choose(),header=TRUE,sep=",")
HMD806 <-read.table(file.choose(),header=TRUE,sep=",")

#HMDTDFull<-rbind(HMD779,HMD676,HMD688,HMD750,HMD756,HMD773,HMD774,HMD775, HMD777,HMD303,HMD786,HMD790, 799)

HMDTDFull<-rbind(HMD779,HMD676,HMD688,HMD756,HMD774,HMD775, HMD777,HMD790, HMD796, HMD797, HMD799, HMD806)

#Trim Your Matrices to the Epochs you need for 22q
HMD22qTrim <-subset(HMD22qFull,Display=="FacePair")
HMD22qTrim <-subset(HMD22qTrim, ValidityRightEye== 0)
HMD22qTrim <-subset(HMD22qTrim, XGazePosRightEye>0)
HMD22qTrim <-subset(HMD22qTrim, XGazePosRightEye<1)
HMD22qTrim <-subset(HMD22qTrim, YGazePosRightEye>0)
HMD22qTrim <-subset(HMD22qTrim, YGazePosRightEye<1)

#Trim Your Matrices to the Epochs you need for TD
HMDTDTrim <-subset(HMDTDFull,Display=="FacePair")
HMDTDTrim <-subset(HMDTDTrim, ValidityRightEye== 0)
HMDTDTrim <-subset(HMDTDTrim, XGazePosRightEye>0)
HMDTDTrim <-subset(HMDTDTrim, XGazePosRightEye<1)
HMDTDTrim <-subset(HMDTDTrim, YGazePosRightEye>0)
HMDTDTrim <-subset(HMDTDTrim, YGazePosRightEye<1)

#Create Matrices for each Trial Type

#Neutral-Left Angry-Right 22q
HMD22qTrimNAV<-subset(HMD22qTrim, ProcCode=="NAV")
HMD22qTrimNAI<-subset(HMD22qTrim, ProcCode=="NAI")
HMD22qTrimNA <-rbind(HMD22qTrimNAV,HMD22qTrimNAI)

#Neutral-Left Happy-Right 22q
HMD22qTrimNHV<-subset(HMD22qTrim, ProcCode=="NHV")
HMD22qTrimNHI<-subset(HMD22qTrim, ProcCode=="NHI")
HMD22qTrimNH <-rbind(HMD22qTrimNHV,HMD22qTrimNHI)

#Angry-Left Neutral-Right 22q
HMD22qTrimANV<-subset(HMD22qTrim, ProcCode=="ANV")
HMD22qTrimANI<-subset(HMD22qTrim, ProcCode=="ANI")
HMD22qTrimAN <-rbind(HMD22qTrimANV,HMD22qTrimANI)

#Happy-Left Neutral-Right 22q
HMD22qTrimHNV<-subset(HMD22qTrim, ProcCode=="HNV")
HMD22qTrimHNI<-subset(HMD22qTrim, ProcCode=="HNI")
HMD22qTrimHN <-rbind(HMD22qTrimHNV,HMD22qTrimHNI)

#Neutral-Left Angry-Right TD
HMDTDTrimNAV<-subset(HMDTDTrim, ProcCode=="NAV")
HMDTDTrimNAI<-subset(HMDTDTrim, ProcCode=="NAI")
HMDTDTrimNA <-rbind(HMDTDTrimNAV,HMDTDTrimNAI)

#Neutral-Left Happy-Right TD
HMDTDTrimNHV<-subset(HMDTDTrim, ProcCode=="NHV")
HMDTDTrimNHI<-subset(HMDTDTrim, ProcCode=="NHI")
HMDTDTrimNH <-rbind(HMDTDTrimNHV,HMDTDTrimNHI)

#Angry-Left Neutral-Right TD
HMDTDTrimANV<-subset(HMDTDTrim, ProcCode=="ANV")
HMDTDTrimANI<-subset(HMDTDTrim, ProcCode=="ANI")
HMDTDTrimAN <-rbind(HMDTDTrimANV,HMDTDTrimANI)

#Happy-Left Neutral-Right TD
HMDTDTrimHNV<-subset(HMDTDTrim, ProcCode=="HNV")
HMDTDTrimHNI<-subset(HMDTDTrim, ProcCode=="HNI")
HMDTDTrimHN <-rbind(HMDTDTrimHNV,HMDTDTrimHNI)

#Make Heat Maps for All Trial Types
library(ggplot2)

ggplot(HMD22qTrimNA,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=100, mid="yellow",limits=c(0,200))+labs(title="22q: Neutral-Left, Angry-Right")

ggplot(HMD22qTrimAN,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=75, mid="yellow",limits=c(0,150))+labs(title="22q: Angry-Left, Neutral-Right")

ggplot(HMD22qTrimNH,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=75, mid="yellow",limits=c(0,150))+labs(title="22q: Neutral-Left, Happy-Right")

ggplot(HMD22qTrimHN,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=75, mid="yellow",limits=c(0,150))+labs(title="22q: Happy-Left, Neutral-Right")

ggplot(HMDTDTrimNA,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=75, mid="yellow",limits=c(0,150))+labs(title="TD: Neutral-Left, Angry-Right")

ggplot(HMDTDTrimAN,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=75, mid="yellow",limits=c(0,150))+labs(title="TD: Angry-Left, Neutral-Right")

ggplot(HMDTDTrimNH,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=75, mid="yellow",limits=c(0,150))+labs(title="TD: Neutral-Left, Happy-Right")

ggplot(HMDTDTrimHN,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=75, mid="yellow",limits=c(0,150))+labs(title="TD: Happy-Left, Neutral-Right")

#Make Transparent Heat Maps for all Trial Types

ggplot(HMD22qTrimNA,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=102.5, mid="yellow",limits=c(5,200))+labs(title="22q: Neutral-Left, Angry-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMD22qTrimAN,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=102.5, mid="yellow",limits=c(5,200))+labs(title="22q: Angry-Left, Neutral-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMD22qTrimNH,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=102.5, mid="yellow",limits=c(5,200))+labs(title="22q: Neutral-Left, Happy-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMD22qTrimHN,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=102.5, mid="yellow",limits=c(5,200))+labs(title="22q: Happy-Left, Neutral-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMDTDTrimNA,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=102.5, mid="yellow",limits=c(5,200))+labs(title="TD: Neutral-Left, Angry-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMDTDTrimAN,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=102.5, mid="yellow",limits=c(5,200))+labs(title="TD:  Angry-Left, Neutral-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMDTDTrimNH,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=102.5, mid="yellow",limits=c(5,200))+labs(title="TD: Neutral-Left, Happy-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMDTDTrimHN,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=102.5, mid="yellow",limits=c(5,200))+labs(title="TD: Happy-Left, Neutral-Right")+xlim(0,1)+ylim(0,1)