dataFolder <- "~/data"
folderIn <- file.path(dataFolder,"donnees_R")
folderOut <- file.path(dataFolder,"donnees_R","PK")
load(file.path(folderIn,"PK","PK.rda"))
load(file.path(folderIn,"EPHY","EPHY.rda"))
library(plyr)

QPK<-aggregate(quantite_pk~PHYTOPROD+ESPECE+CODE_REG,data= pk, sum)
Surface<-aggregate(Area~ESPECE+CODE_REG, data = pk, sum)
Dose<-join(QPK,Surface)
Dose$DosePK<-Dose$quantite_pk/Dose$Area
SommeDosePK<- aggregate(DosePK~ESPECE, data= Dose, sum)
BasePK<-merge(Dose[,c("ESPECE","DosePK")], SommeDosePK, by="ESPECE")
BasePK$CoefBasePK<-BasePK$DosePK.x/BasePK$DosePK.y
