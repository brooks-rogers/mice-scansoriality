install.packages("tidyverse")
library(tidyverse)
install.packages("tidyr")
library(tidyr)
install.packages("ggplot2")
library(ggplot2)


MouseMeasurements<-read.csv("MouseMeasurements.csv",header=TRUE)
MouseMeasurements
table(MouseMeasurements)

#Creating Measurement Objects

Fl<-MouseMeasurements$Fl[!is.na(MouseMeasurements$Fl)]
Fdw<-MouseMeasurements$Fdw[!is.na(MouseMeasurements$Fdw)]
Fsw<-MouseMeasurements$Fsw[!is.na(MouseMeasurements$Fsw)]
Tl<-MouseMeasurements$Tl[!is.na(MouseMeasurements$Tl)]
Tpw<-MouseMeasurements$Tpw[!is.na(MouseMeasurements$Tpw)]
Tmw<-MouseMeasurements$Tmw[!is.na(MouseMeasurements$Tmw)]
Hl<-MouseMeasurements$Hl[!is.na(MouseMeasurements$Hl)]
Hdw<-MouseMeasurements$Hdw[!is.na(MouseMeasurements$Hdw)]
Hpw<-MouseMeasurements$Hpw[!is.na(MouseMeasurements$Hpw)]
Hsw<-MouseMeasurements$Hsw[!is.na(MouseMeasurements$Hsw)]
Ul<-MouseMeasurements$Ul[!is.na(MouseMeasurements$Ul)]
Uol<-MouseMeasurements$Uol[!is.na(MouseMeasurements$Uol)]
Usw<-MouseMeasurements$Usw[!is.na(MouseMeasurements$Usw)]
Rl<-MouseMeasurements$Rl[!is.na(MouseMeasurements$Rl)]
Mcl<-MouseMeasurements$Mcl[!is.na(MouseMeasurements$Mcl)]
Ppl<-MouseMeasurements$Ppl[!is.na(MouseMeasurements$Ppl)]
Mtl<-MouseMeasurements$Mtl[!is.na(MouseMeasurements$Mtl)]
Pppl<-MouseMeasurements$Pppl[!is.na(MouseMeasurements$Pppl)]



log_mass<-log(MouseMeasurements$mass_g)
log_mass                                   #Natural Log of the mice weights


geo_m<-((mean(Fl)*mean(Fdw)*mean(Fsw)*mean(Tl)*mean(Tpw)*mean(Tmw)*mean(Hl)*mean(Hdw)*mean(Hpw)*mean(Hsw)*mean(Ul)*mean(Uol)*mean(Usw)*mean(Rl)*mean(Mcl)*mean(Ppl)*mean(Mtl)*mean(Pppl))^(1/18))
geo_m                                      #Geometric Mean of the 18 measurements, 3.736719



#Indices

MANUS<-Ppl/Mcl      #MANUS Index
HRI<-Hsw/Hl         #Humeral Robustness Index
HPI<-Hpw/Hl         #Humeral Proximal Index
HEB<-Hdw/Hl         #Humeral Epicondyle Breadth
OLI<-Uol/Ul         #Olecranon Process Length Index
BI<-Rl/Hl           #Brachial Index
IM<-(Hl+Ul)/(Fl+Tl) #Intermembral Index
PRTI<-Mcl/(Hl+Rl)   #Palm Robustness Index
FRI<-Fsw/Fl         #Femoral Robustness
FEB<-Fdw/Fl         #Femoral Epicondyle Breadth
CI<-Tl/Fl           #Crural Index
TRI<-Tmw/Tl         #Tibial Robustness Index
PES<-Pppl/Mtl       #PES INdex


#Plot 1 - MANUS by Species

ggplot(data=MouseMeasurements,aes(x=MANUS,y=genus_species))+geom_point()

#Plot 2 - HPI by Species

ggplot(data=MouseMeasurements,aes(x=HPI,y=genus_species))+geom_point()

#Plot 3 - PES by Species

ggplot(data=MouseMeasurements,aes(x=PES,y=genus_species))+geom_point()
