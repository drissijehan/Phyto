dataFolder <- "~/data"
folderIn <- file.path(dataFolder,"donnees_R")
folderOut <- file.path(dataFolder,"donnees_R","PK")
load(file.path(folderIn,"PK","PK.rda"))
load(file.path(folderIn,"EPHY","EPHY.rda"))
load(file.path(folderIn,"EPHY","CorrespondanceCultureEphyPk.rda"))
library(plyr)
library("DataManagement")
library(plotly)
#Erreurs r�gionales
##CofeBasePK (produit, culture, region)
QPK<-aggregate(quantite_pk~PHYTOPROD+ESPECE+CODE_REG,data= pk, sum)
Surface<-aggregate(Area~ESPECE+CODE_REG, data = pk, sum)
BasePK<-join(QPK,Surface)
BasePK$DosePK<-BasePK$quantite_pk/BasePK$Area
SommeDosePK<- aggregate(DosePK~ESPECE, data= BasePK, sum)
BasePK<-merge(BasePK, SommeDosePK, by="ESPECE")
BasePK$CoefBasePK<-BasePK$DosePK.x/BasePK$DosePK.y
BasePK <- ChangeNameCol(BasePK,"PHYTOPROD","AMM")
##CoefDH (produit, culture)
culture <- sapply(strsplit(as.vector(EPHY$Intitule),"*",fixed = TRUE), function(x) x[1])
EPHY <- cbind(EPHY,culture)
EPHY<- merge(EPHY,CorrespondanceCultureEphyPk, by="culture")
DHCulture<- aggregate(Dose.d.application.retenue~AMM+ESPECE, data= EPHY, median)
SommeDHCulture<-aggregate(Dose.d.application.retenue~ESPECE,data = DHCulture,sum)
BaseDH<-merge(DHCulture, SommeDHCulture, by="ESPECE")
BaseDH$CoefDH<-BaseDH$Dose.d.application.retenue.x/BaseDH$Dose.d.application.retenue.y
##CoefPK (produit, culture, region)
Base<-merge(BasePK,BaseDH,by=c("AMM","ESPECE"))
Base$CoefPK<-Base$CoefBasePK/Base$CoefDH
##Max (produit,region)
MaxCoef<- aggregate(CoefPK~AMM+CODE_REG, data = Base, max)
##Hist
plot_ly(MaxCoef, x = ~ CoefPK, type = "histogram", text = ~paste("AMM:", AMM, "Region" , CODE_REG)) 


#Erreurs nationale
##CofeBasePK (produit, culture)
QPKN<-aggregate(quantite_pk~PHYTOPROD+ESPECE,data= pk, sum)
SurfaceN<-aggregate(Area~ESPECE, data = pk, sum)
BasePKN<-join(QPKN,SurfaceN)
BasePKN$DosePKN<-BasePKN$quantite_pk/BasePKN$Area
SommeDosePKN<- aggregate(DosePKN~ESPECE, data= BasePKN, sum)
BasePKN<-merge(BasePKN, SommeDosePKN, by="ESPECE")
BasePKN$CoefBasePKN<-BasePKN$DosePKN.x/BasePKN$DosePKN.y
BasePKN <- ChangeNameCol(BasePKN,"PHYTOPROD","AMM")
##CoefPK (produit, culture, region)
BaseN<-merge(BasePKN,BaseDH,by=c("AMM","ESPECE"))
BaseN$CoefPKN<-BaseN$CoefBasePKN/BaseN$CoefDH
##Max (produit)
MaxCoefN<- aggregate(CoefPKN~AMM, data = BaseN, max)
##Hist
plot_ly(MaxCoefN, x = ~ CoefPKN, type = "histogram", text = ~paste("AMM:", AMM)) 