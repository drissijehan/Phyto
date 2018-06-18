source("dataSource.R") # doit contenir quelque chose du genre dataFolder <- "~/data"
folderIn <- file.path(dataFolder,"donnees_R")
folderOut <- file.path(dataFolder,"donnees_R","PK")
load(file.path(folderIn,"PK","PK.rda"))
load(file.path(folderIn,"EPHY","EPHY.rda"))
load(file.path(folderIn,"EPHY","CorrespondanceCultureEphyPk.rda"))
library(plyr)
library("DataManagement")
library(plotly)
library(Hmisc)
#Dans cette partie on a pas pris compte de la surface
#Erreurs régionales
##CofeBasePK (produit, culture, region)
# pk<-pk[pk$CODE_REG%nin%"00",]
BasePK<-aggregate(cbind(mean,freq)~PHYTOPROD+ESPECE+CODE_REG, data= pk, sum)
BasePK$DosePK<-BasePK$mean*BasePK$freq
SommeDosePK<- aggregate(DosePK~PHYTOPROD+CODE_REG, data= BasePK, sum)
SommeDosePK<- ChangeNameCol(SommeDosePK, "DosePK","SumDosePK")
BasePK<-merge(BasePK, SommeDosePK, by=c("PHYTOPROD","CODE_REG"), all= TRUE)
####################################################################################
SommeDosePK[which(SommeDosePK$PHYTOPROD=="2000018"),]
SommeDosePK[which(SommeDosePK$PHYTOPROD=="2000380"),]

# verification no lost phytoprod x code_reg in the merge
expect_equal(nrow(unique(SommeDosePK[,c("PHYTOPROD","CODE_REG")])),
             nrow(unique(BasePK[,c("PHYTOPROD","CODE_REG")])))

####################################################################################
BasePK$CoefPK<-BasePK$DosePK/BasePK$SumDosePK
BasePK <- ChangeNameCol(BasePK,"PHYTOPROD","AMM")
##CoefDH (produit, culture)
EPHY2<- merge(EPHY,CorrespondanceCultureEphyPk, by.x="ESPECE",by.y="culture")

# aggregation sur les différentes années d'homologuation pour les produits (à terme 
# pourrait être remplacé par utilisation de la dose retenue l'année de BNVD donnée) 
# mais aussi aggrégation sur les différents "Intitule", donc les différentes cibles
DHCulture<- aggregate(Dose.d.application.retenue~AMM+ESPECE, data= EPHY, median,na.rm=TRUE)
DHCulture<- ChangeNameCol(DHCulture,"Dose.d.application.retenue","DH")

SommeDHCulture<-aggregate(DH~AMM,data = DHCulture,sum,na.rm=TRUE)

SommeDHCulture<- ChangeNameCol(SommeDHCulture, "DH","SumDH")
BaseDH<-merge(DHCulture, SommeDHCulture, by="AMM")
BaseDH$CoefDH<-BaseDH$DH/BaseDH$SumDH
##CoefPK (produit, culture, region)
Base<-merge(BasePK,BaseDH,by=c("AMM","ESPECE"),all.x=TRUE)

# check no losses since SumDosePK was computed
part1 <- aggregate(Base$DosePK,by=list(Base$AMM,Base$CODE_REG),sum)
part2 <- aggregate(Base$SumDosePK,by=list(Base$AMM,Base$CODE_REG),mean)
expect_equal(part1$x,part2$x)


Base$Coef<-Base$CoefPK/Base$CoefDH

# Pb: 
Base[which(Base$AMM=="2000018"),]
#=> Ok, sumDosePK est bien la somme des DosePK pour la région



table(Base[which(is.na(Base$DH)),"ESPECE"])
#=> ca fait beaucoup, par exemple pour le colza, à étudier

table(Base[which(is.na(Base$DH)&Base$CODE_REG!="00"),"ESPECE"])
#=> même à l'échelle régionale ça fait beaucoup de produits avec au moins 3 applications
#   pour lesquelles il n'y a pas de DH connue


##Max (produit,region)
MaxCoef<- aggregate(Coef~AMM+CODE_REG, data = Base, max,na.rm=TRUE)
MaxCoef <- ChangeNameCol(MaxCoef,"Coef","MaxCoef")
test <- merge(Base,MaxCoef,by=c("AMM","CODE_REG"),all.x=TRUE)
expect_equal(Count(is.na(test$MaxCoef)& !is.na(test$DH)),0)

####################################################################################
MaxCoef[MaxCoef$Coef < 1,] #-> 2 AMM :2030239 (reg 25), 2090057 (reg 41)
Base[Base$AMM%in%"2030239",] #-> 2030239 n'est present que sur 2 especes sur la region 25, avec une coef<1 et l'autre coef NA (CoefDH=NA)
                              #-> 2090057 de meme
##Hist
p1<-plot_ly(MaxCoef, x = ~ Coef, type = "histogram", text = ~paste("AMM:", AMM, "<br>Region" , CODE_REG),
            name="Erreur reg. surf. egual") %>%
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
            name= "National error equal surfaces") %>%
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