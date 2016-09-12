 #Single Subj Import
 HMD22qFull<-read.table(file.choose(),header=TRUE,sep=",")
 
#Trim Your Matrices to the Epochs you need for 22q
 HMD22qTrim <-subset(HMD22qFull,Display=="FacePair")
 HMD22qTrim <-subset(HMD22qTrim, ValidityRightEye== 0)
 HMD22qTrim <-subset(HMD22qTrim, XGazePosRightEye>0)
 HMD22qTrim <-subset(HMD22qTrim, XGazePosRightEye<1)
 HMD22qTrim <-subset(HMD22qTrim, YGazePosRightEye>0)
 HMD22qTrim <-subset(HMD22qTrim, YGazePosRightEye<1)
 
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
#Make Heat Maps for All Trial Types
library(ggplot2)
#Value Guidance

ggplot(HMD22qTrimNA,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", mid="yellow") + labs(title="22q: Neutral-Left, Angry-Right")+xlim(0,1)+ylim(0,1)

#Single Subj Values 1 to 1200

ggplot(HMD22qTrimNA,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=600.5, mid="yellow",limits=c(1,1200))+labs(title="22q: Neutral-Left, Angry-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMD22qTrimAN,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=600.5, mid="yellow",limits=c(1,1200))+labs(title="22q: Angry-Left, Neutral-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMD22qTrimNH,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=600.5, mid="yellow",limits=c(1,1200))+labs(title="22q: Neutral-Left, Happy-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMD22qTrimHN,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=600.5, mid="yellow",limits=c(1,1200))+labs(title="22q: Happy-Left, Neutral-Right")+xlim(0,1)+ylim(0,1)

#Single Subj Values 1 to 1100

ggplot(HMD22qTrimNA,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=550.5, mid="yellow",limits=c(1,1100))+labs(title="22q: Neutral-Left, Angry-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMD22qTrimAN,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=550.5, mid="yellow",limits=c(1,1100))+labs(title="22q: Angry-Left, Neutral-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMD22qTrimNH,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=550.5, mid="yellow",limits=c(1,1100))+labs(title="22q: Neutral-Left, Happy-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMD22qTrimHN,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=550.5, mid="yellow",limits=c(1,1100))+labs(title="22q: Happy-Left, Neutral-Right")+xlim(0,1)+ylim(0,1)

#Single Subj Values 1 to 1000

ggplot(HMD22qTrimNA,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=500.5, mid="yellow",limits=c(1,1000))+labs(title="22q: Neutral-Left, Angry-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMD22qTrimAN,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=500.5, mid="yellow",limits=c(1,1000))+labs(title="22q: Angry-Left, Neutral-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMD22qTrimNH,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=500.5, mid="yellow",limits=c(1,1000))+labs(title="22q: Neutral-Left, Happy-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMD22qTrimHN,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=500.5, mid="yellow",limits=c(1,1000))+labs(title="22q: Happy-Left, Neutral-Right")+xlim(0,1)+ylim(0,1)

#Single Subj Values 1 to 900

ggplot(HMD22qTrimNA,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=450.5, mid="yellow",limits=c(1,900))+labs(title="22q: Neutral-Left, Angry-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMD22qTrimAN,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=450.5, mid="yellow",limits=c(1,900))+labs(title="22q: Angry-Left, Neutral-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMD22qTrimNH,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=450.5, mid="yellow",limits=c(1,900))+labs(title="22q: Neutral-Left, Happy-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMD22qTrimHN,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=450.5, mid="yellow",limits=c(1,900))+labs(title="22q: Happy-Left, Neutral-Right")+xlim(0,1)+ylim(0,1)

#Single Subj Values 1 to 800

ggplot(HMD22qTrimNA,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=400.5, mid="yellow",limits=c(1,800))+labs(title="22q: Neutral-Left, Angry-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMD22qTrimAN,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=400.5, mid="yellow",limits=c(1,800))+labs(title="22q: Angry-Left, Neutral-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMD22qTrimNH,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=400.5, mid="yellow",limits=c(1,800))+labs(title="22q: Neutral-Left, Happy-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMD22qTrimHN,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=400.5, mid="yellow",limits=c(1,800))+labs(title="22q: Happy-Left, Neutral-Right")+xlim(0,1)+ylim(0,1)

#Single Subj Values 1 to 700

ggplot(HMD22qTrimNA,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=350.5, mid="yellow",limits=c(1,700))+labs(title="22q: Neutral-Left, Angry-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMD22qTrimAN,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=350.5, mid="yellow",limits=c(1,700))+labs(title="22q: Angry-Left, Neutral-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMD22qTrimNH,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=350.5, mid="yellow",limits=c(1,700))+labs(title="22q: Neutral-Left, Happy-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMD22qTrimHN,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=350.5, mid="yellow",limits=c(1,700))+labs(title="22q: Happy-Left, Neutral-Right")+xlim(0,1)+ylim(0,1)

#Single Subj Values 1 to 600

ggplot(HMD22qTrimNA,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=300.5, mid="yellow",limits=c(1,600))+labs(title="22q: Neutral-Left, Angry-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMD22qTrimAN,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=300.5, mid="yellow",limits=c(1,600))+labs(title="22q: Angry-Left, Neutral-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMD22qTrimNH,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=300.5, mid="yellow",limits=c(1,600))+labs(title="22q: Neutral-Left, Happy-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMD22qTrimHN,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=300.5, mid="yellow",limits=c(1,600))+labs(title="22q: Happy-Left, Neutral-Right")+xlim(0,1)+ylim(0,1)

#Single Subj Values 1 to 500

ggplot(HMD22qTrimNA,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=250.5, mid="yellow",limits=c(1,500))+labs(title="22q: Neutral-Left, Angry-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMD22qTrimAN,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=250.5, mid="yellow",limits=c(1,500))+labs(title="22q: Angry-Left, Neutral-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMD22qTrimNH,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=250.5, mid="yellow",limits=c(1,500))+labs(title="22q: Neutral-Left, Happy-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMD22qTrimHN,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=250.5, mid="yellow",limits=c(1,500))+labs(title="22q: Happy-Left, Neutral-Right")+xlim(0,1)+ylim(0,1)

#Single Subj Values 1 to 450

ggplot(HMD22qTrimNA,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=225.5, mid="yellow",limits=c(1,450))+labs(title="22q: Neutral-Left, Angry-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMD22qTrimAN,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=225.5, mid="yellow",limits=c(1,450))+labs(title="22q: Angry-Left, Neutral-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMD22qTrimNH,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=225.5, mid="yellow",limits=c(1,450))+labs(title="22q: Neutral-Left, Happy-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMD22qTrimHN,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=225.5, mid="yellow",limits=c(1,450))+labs(title="22q: Happy-Left, Neutral-Right")+xlim(0,1)+ylim(0,1)

#Single Subj Values 1 to 400

ggplot(HMD22qTrimNA,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=200.5, mid="yellow",limits=c(1,400))+labs(title="22q: Neutral-Left, Angry-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMD22qTrimAN,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=200.5, mid="yellow",limits=c(1,400))+labs(title="22q: Angry-Left, Neutral-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMD22qTrimNH,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=200.5, mid="yellow",limits=c(1,400))+labs(title="22q: Neutral-Left, Happy-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMD22qTrimHN,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=200.5, mid="yellow",limits=c(1,400))+labs(title="22q: Happy-Left, Neutral-Right")+xlim(0,1)+ylim(0,1)


#Single Subj Values 1 to 350

ggplot(HMD22qTrimNA,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=175.5, mid="yellow",limits=c(1,350))+labs(title="22q: Neutral-Left, Angry-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMD22qTrimAN,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=175.5, mid="yellow",limits=c(1,350))+labs(title="22q: Angry-Left, Neutral-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMD22qTrimNH,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=175.5, mid="yellow",limits=c(1,350))+labs(title="22q: Neutral-Left, Happy-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMD22qTrimHN,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=175.5, mid="yellow",limits=c(1,350))+labs(title="22q: Happy-Left, Neutral-Right")+xlim(0,1)+ylim(0,1)


#Single Subj Values 1 to 300

ggplot(HMD22qTrimNA,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=150.5, mid="yellow",limits=c(1,300))+labs(title="22q: Neutral-Left, Angry-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMD22qTrimAN,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=150.5, mid="yellow",limits=c(1,300))+labs(title="22q: Angry-Left, Neutral-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMD22qTrimNH,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=150.5, mid="yellow",limits=c(1,300))+labs(title="22q: Neutral-Left, Happy-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMD22qTrimHN,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=150.5, mid="yellow",limits=c(1,300))+labs(title="22q: Happy-Left, Neutral-Right")+xlim(0,1)+ylim(0,1)

#Single Subj Values 1 to 250

ggplot(HMD22qTrimNA,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=125.5, mid="yellow",limits=c(1,250))+labs(title="22q: Neutral-Left, Angry-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMD22qTrimAN,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=125.5, mid="yellow",limits=c(1,250))+labs(title="22q: Angry-Left, Neutral-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMD22qTrimNH,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=125.5, mid="yellow",limits=c(1,250))+labs(title="22q: Neutral-Left, Happy-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMD22qTrimHN,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=125.5, mid="yellow",limits=c(1,250))+labs(title="22q: Happy-Left, Neutral-Right")+xlim(0,1)+ylim(0,1)

#Single Subj Values 1 to 200

ggplot(HMD22qTrimNA,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=100.5, mid="yellow",limits=c(1,200))+labs(title="22q: Neutral-Left, Angry-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMD22qTrimAN,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=100.5, mid="yellow",limits=c(1,200))+labs(title="22q: Angry-Left, Neutral-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMD22qTrimNH,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=100.5, mid="yellow",limits=c(1,200))+labs(title="22q: Neutral-Left, Happy-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMD22qTrimHN,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=100.5, mid="yellow",limits=c(1,200))+labs(title="22q: Happy-Left, Neutral-Right")+xlim(0,1)+ylim(0,1)

#Single Subj Values 1 to 150

ggplot(HMD22qTrimNA,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=75.5, mid="yellow",limits=c(1,150))+labs(title="22q: Neutral-Left, Angry-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMD22qTrimAN,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=75.5, mid="yellow",limits=c(1,150))+labs(title="22q: Angry-Left, Neutral-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMD22qTrimNH,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=75.5, mid="yellow",limits=c(1,150))+labs(title="22q: Neutral-Left, Happy-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMD22qTrimHN,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=75.5, mid="yellow",limits=c(1,150))+labs(title="22q: Happy-Left, Neutral-Right")+xlim(0,1)+ylim(0,1)


#Single Subj Values 1 to 100

ggplot(HMD22qTrimNA,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=50.5, mid="yellow",limits=c(1,100))+labs(title="22q: Neutral-Left, Angry-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMD22qTrimAN,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=50.5, mid="yellow",limits=c(1,100))+labs(title="22q: Angry-Left, Neutral-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMD22qTrimNH,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=50.5, mid="yellow",limits=c(1,100))+labs(title="22q: Neutral-Left, Happy-Right")+xlim(0,1)+ylim(0,1)

ggplot(HMD22qTrimHN,aes(x=XGazePosRightEye,y=YGazePosRightEye))+stat_density2d(aes(fill=..density..),contour=FALSE,geom="tile")+scale_fill_gradient2(low="darkgreen", high="darkred", midpoint=50.5, mid="yellow",limits=c(1,100))+labs(title="22q: Happy-Left, Neutral-Right")+xlim(0,1)+ylim(0,1)

