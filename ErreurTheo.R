# dataFolder <- "~/data"
dataFolder <- "/media/5AE1EC8814E5040E/" # Corentin
folderIn <- file.path(dataFolder,"donnees_R")
folderOut <- file.path(dataFolder,"donnees_R","PK")
load(file.path(folderIn,"PK","PK.rda"))
load(file.path(folderIn,"EPHY","EPHY.rda"))
load(file.path(folderIn,"EPHY","CorrespondanceCultureEphyPk.rda"))
library(plyr)
library("DataManagement")
library(plotly)
#Dans cette partie on a pas pris compte de la surface
#Erreurs régionales
##CofeBasePK (produit, culture, region)
BasePK<-aggregate(cbind(mean,freq)~PHYTOPROD+ESPECE+CODE_REG, data= pk, sum)
BasePK$DosePK<-BasePK$mean*BasePK$freq
SommeDosePK<- aggregate(DosePK~PHYTOPROD+CODE_REG, data= BasePK, sum)
SommeDosePK<- ChangeNameCol(SommeDosePK, "DosePK","SumDosePK")
BasePK<-merge(BasePK, SommeDosePK, by=c("PHYTOPROD","CODE_REG"))
SommeDosePK[which(SommeDosePK$PHYTOPROD=="2000018"),]
#=> en plus problème de région supplémentaire : 21 qui disparait ensuite dans Base
BasePK[which(BasePK$AMM=="2000018"),]
#=> c'est la betterave qui avait disparu pour 00

BasePK$CoefPK<-BasePK$DosePK/BasePK$SumDosePK
BasePK <- ChangeNameCol(BasePK,"PHYTOPROD","AMM")
##CoefDH (produit, culture)
culture <- sapply(strsplit(as.vector(EPHY$Intitule),"*",fixed = TRUE), function(x) x[1])
EPHY <- cbind(EPHY,culture)
EPHY<- merge(EPHY,CorrespondanceCultureEphyPk, by="culture")
DHCulture<- aggregate(Dose.d.application.retenue~AMM+ESPECE, data= EPHY, median)
DHCulture<- ChangeNameCol(DHCulture,"Dose.d.application.retenue","DH")
SommeDHCulture<-aggregate(DH~AMM,data = DHCulture,sum)
SommeDHCulture<- ChangeNameCol(SommeDHCulture, "DH","SumDH")
BaseDH<-merge(DHCulture, SommeDHCulture, by="AMM")
BaseDH$CoefDH<-BaseDH$DH/BaseDH$SumDH
##CoefPK (produit, culture, region)
Base<-merge(BasePK,BaseDH,by=c("AMM","ESPECE"),all.x=TRUE)
Base$Coef<-Base$CoefPK/Base$CoefDH

# Pb: 
Base[which(Base$AMM=="2000018"),]
#=> somme pour région 00 pour blé et orge n'est pas égale à SumDosePK

##Max (produit,region)
MaxCoef<- aggregate(Coef~AMM+CODE_REG, data = Base, max)
##Hist
p1<-plot_ly(MaxCoef, x = ~ Coef, type = "histogram", text = ~paste("AMM:", AMM, "<br>Region" , CODE_REG),
            name= "Erreurs Regionales à surface égale") %>%
  layout(yaxis = list(type = "log"))


#Erreurs nationale
##CofeBasePK (produit, culture)
BasePKN<-aggregate(cbind(mean,freq)~PHYTOPROD+ESPECE, data= pk, sum)
BasePKN$DosePK<-BasePKN$mean*BasePKN$freq
SommeDosePKN<- aggregate(DosePK~PHYTOPROD, data= BasePKN, sum)
SommeDosePKN<- ChangeNameCol(SommeDosePKN, "DosePK","SumDosePK")
BasePKN<-merge(BasePKN, SommeDosePKN, by="PHYTOPROD")
BasePKN$CoefPK<-BasePKN$DosePK/BasePKN$SumDosePK
BasePKN <- ChangeNameCol(BasePKN,"PHYTOPROD","AMM")
##CoefPK (produit, culture, region)
BaseN<-merge(BasePKN,BaseDH,by=c("AMM","ESPECE"))
BaseN$Coef<-BaseN$CoefPK/BaseN$CoefDH
##Max (produit)
MaxCoefN<- aggregate(Coef~AMM, data = BaseN, max)
##Hist
p2<-plot_ly(MaxCoefN, x = ~ Coef, type = "histogram", text = ~paste("AMM:", AMM),
            name= "Erreurs Nationales à surface égale") %>%
  layout(yaxis = list(type = "log"))


#On inclut la surface
#Erreurs regionales
load(file.path(folderIn,"Agreste","AGRESTE_2014.rda"))
#Erreurs régionales
##CofeBasePK (produit, culture, region)
BasePKS<-aggregate(cbind(mean,freq)~PHYTOPROD+ESPECE+CODE_REG, data= pk, sum)
BasePKS$DosePK<-BasePKS$mean*BasePKS$freq
BasePKS<-merge(BasePKS,AGRESTE_2014, by=c("ESPECE","CODE_REG"))
BasePKS$DoseSurf<- BasePKS$DosePK * BasePKS$Area 
SommeCulture<- aggregate(DoseSurf~PHYTOPROD+CODE_REG, data= BasePKS, sum)
SommeCulture <- ChangeNameCol(SommeCulture,"DoseSurf","SumDoseSurf")
BasePKS<- merge(BasePKS, SommeCulture, by=c("PHYTOPROD","CODE_REG"))
BasePKS$CoefPK<-BasePKS$DosePK/BasePKS$SumDoseSurf
BasePKS <- ChangeNameCol(BasePKS,"PHYTOPROD","AMM")
BaseS<-merge(BasePKS,BaseDH,by=c("AMM","ESPECE"))
BaseS$Coef<-BaseS$CoefPK/BaseS$CoefDH
##Max (produit)
MaxCoefS<- aggregate(Coef~AMM+CODE_REG, data = BaseS, max)
##Hist
p3<-plot_ly(MaxCoefS, x = ~ Coef, type = "histogram", text = ~paste("AMM:", AMM, "<br>Region" , CODE_REG), 
            name= "Erreurs Regionales en tenant compte du surface") %>%
  layout(yaxis = list(type = "log"))


#Erreurs nationale
##CofeBasePK (produit, culture)

BasePKSN<-aggregate(cbind(mean,freq)~PHYTOPROD+ESPECE, data= pk, sum)
BasePKSN$DosePK<-BasePKSN$mean*BasePKSN$freq
AgresteNational<-aggregate(Area~ESPECE, data = AGRESTE_2014, sum) ##Je suis pas sur d'aggreger les surfaces par culture!!!
BasePKSN<-merge(BasePKSN,AgresteNational, by="ESPECE")
BasePKSN$DoseSurf<- BasePKSN$DosePK * BasePKSN$Area 
SommeCultureSN<- aggregate(DoseSurf~PHYTOPROD, data= BasePKSN, sum)
SommeCultureSN <- ChangeNameCol(SommeCultureSN,"DoseSurf","SumDoseSurf")
BasePKSN<- merge(BasePKSN, SommeCultureSN, by="PHYTOPROD")
BasePKSN$CoefPK<-BasePKSN$DosePK/BasePKSN$SumDoseSurf
BasePKSN <- ChangeNameCol(BasePKSN,"PHYTOPROD","AMM")
BaseSN<-merge(BasePKSN,BaseDH,by=c("AMM","ESPECE"))
BaseSN$Coef<-BaseSN$CoefPK/BaseSN$CoefDH
##Max (produit)
MaxCoefSN<- aggregate(Coef~AMM, data = BaseSN, max)
##Hist
p4<-plot_ly(MaxCoefSN, x = ~ Coef, type = "histogram", text = ~paste("AMM:", AMM),
            name= "Erreurs Nationales en tenant compte du surface") %>%
  layout(yaxis = list(type = "log"))

p <- subplot(p1, p2, p3, p4)


