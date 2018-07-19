#source("dataSource.R") # doit contenir quelque chose du genre dataFolder <- "~/data"
folderIn <- file.path(dataFolder,"donnees_R")
folderOut <- file.path(dataFolder,"donnees_R","PK")
load(file.path(folderIn,"PK","PK.rda")) # pk
load(file.path(folderIn,"EPHY","EPHY.rda"))
load(file.path(folderIn,"EPHY","CorrespondanceCultureEphyPk.rda"))
library(plyr)
library("DataManagement")
library(plotly)
library(Hmisc)
#Dans cette partie on a pas pris compte de la surface
#Erreurs r?gionales
##CofeBasePK (produit, culture, region)
pk<-pk[pk$CODE_REG%nin%"00",]
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
EPHY<- merge(EPHY,CorrespondanceCultureEphyPk, by.x="intituleCulture",by.y="culture",all.x=TRUE)
#=> ici il faut en fait dupliquer les lignes pour avoir autant de lignes que de cultures dans intituleCulture

# aggregation sur les diff?rentes ann?es d'homologuation pour les produits (? terme 
# pourrait ?tre remplac? par utilisation de la dose retenue l'ann?e de BNVD donn?e) 
# mais aussi aggr?gation sur les diff?rents "Intitule", donc les diff?rentes cibles
DHCulture<- aggregate(Dose.d.application.retenue~AMM+ESPECE, data= EPHY, median,na.rm=TRUE)
DHCulture<- ChangeNameCol(DHCulture,"Dose.d.application.retenue","DH")
SommeDHCulture<-aggregate(DH~AMM,data = DHCulture,sum,na.rm=TRUE)
SommeDHCulture<- ChangeNameCol(SommeDHCulture, "DH","SumDH")
BaseDH<-merge(DHCulture, SommeDHCulture, by="AMM")
BaseDH$CoefDH<-BaseDH$DH/BaseDH$SumDH
##CoefPK (produit, culture, region)
Base<-merge(BasePK,BaseDH,by=c("AMM","ESPECE"),all.x=TRUE)

# est-ce qu'il ne faut pas plut?t utiliser l'esp?ce pk comme unit? d'aggr?gation?
#DHCulture2<- aggregate(Dose.d.application.retenue~AMM+culturePk, data= EPHY2, median,na.rm=TRUE)
#DHCulture2<- ChangeNameCol(DHCulture2,"Dose.d.application.retenue","DH")
#SommeDHCulture2<-aggregate(DH~AMM,data = DHCulture2,sum,na.rm=TRUE)
#SommeDHCulture2<- ChangeNameCol(SommeDHCulture2, "DH","SumDH")
#BaseDH2<-merge(DHCulture2, SommeDHCulture2, by="AMM")
#BaseDH2$CoefDH<-BaseDH2$DH/BaseDH2$SumDH
##CoefPK (produit, culture, region)
#Base2<-merge(BasePK,BaseDH2,by.x=c("AMM","ESPECE"),by.y=c("AMM","culturePk"),all.x=TRUE)
# Base <- Base2


# check no losses since SumDosePK was computed
#part1 <- aggregate(Base$DosePK,by=list(Base$AMM,Base$CODE_REG),sum)
#part2 <- aggregate(Base$SumDosePK,by=list(Base$AMM,Base$CODE_REG),mean)
#expect_equal(part1$x,part2$x)


Base$Coef<-Base$CoefPK/Base$CoefDH

# Pb: 
Base[which(Base$AMM=="2000018"),]
#=> Ok, sumDosePK est bien la somme des DosePK pour la r?gion

Count(is.na(Base$DH))
#=> 1765, mieux qu'un quarr (avant) mais encore beaucoup

table(Base[which(is.na(Base$DH)),"ESPECE"])
#=> ca fait beaucoup, par exemple pour le colza, ? ?tudier
#   nettement mieux avec la nouvelle table, mais encore beaucoup pourle triticale, l'orge et le bl? tendre

table(Base[which(is.na(Base$DH)&Base$CODE_REG!="00"),"ESPECE"])
#=> m?me ? l'?chelle r?gionale ?a fait beaucoup de produits avec au moins 3 applications
#   pour lesquelles il n'y a pas de DH connue

# ?tude de cas
head(Base[which(is.na(Base$DH)),])
#=>  2000045 est du RANMAN, il a ?t? retir? en 2015 mais je ne le trouve pas dans les feuilles excel
#    2014 et 2013. Par contre il y a du RANMAN TOP, avec AMM 2110012 qui est bien autoris? sur pommes de terre
#    encore aujourd'hui. Peut-?tre qu'il faudrait aussi sortir les noms de l'enqu?te PK


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
p3<-plot_ly(MaxCoef[MaxCoef$MaxCoef<20,], x = ~MaxCoef, type = "histogram", text = ~paste("AMM:", AMM),
            name="Erreur Regionale avec Surface Egale")
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
p1<-plot_ly(MaxCoefN[MaxCoefN$Coef<20,], x = ~ Coef, type = "histogram", text = ~paste("AMM:", AMM),
            name= "Erreur Nationale avec Surface Egale") 
##########################################################################################################
##########################################################################################################
##########################################################################################################

#On inclut la surface
#Erreurs regionales
load(file.path(folderIn,"Agreste","AGRESTE_2014.rda"))
AGRESTE_2014<-AGRESTE_2014[AGRESTE_2014$CODE_REG!="00",]
#Erreurs r?gionales
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

#CoefDH

DHCultureS<- aggregate(Dose.d.application.retenue~AMM+ESPECE, data= EPHY, median,na.rm=TRUE)
DHCultureS<- ChangeNameCol(DHCultureS,"Dose.d.application.retenue","DH")
DHCultureS<-merge(unique(DHCultureS),unique(AGRESTE_2014), by="ESPECE")
DHCultureS$DHSurf<-DHCultureS$DH*DHCultureS$Area
SommeDHCultureS<-aggregate(DHSurf~AMM+CODE_REG,data = DHCultureS,sum,na.rm=TRUE)
SommeDHCultureS<- ChangeNameCol(SommeDHCultureS, "DHSurf","SumDHSurf")
BaseDHS<-merge(DHCultureS, SommeDHCultureS, by=c("AMM","CODE_REG"))
BaseDHS<-BaseDHS[BaseDHS$AMM!="",]
BaseDHS$CoefDH<-BaseDHS$DH/BaseDHS$SumDHSurf

BaseS<-merge(BasePKS,BaseDHS,by=c("AMM","ESPECE","CODE_REG"))
BaseS$Coef<-BaseS$CoefPK/BaseS$CoefDH
##Max (produit)
MaxCoefS<- aggregate(Coef~AMM+CODE_REG, data = BaseS, max)
##Hist
p4<-plot_ly(MaxCoefS[MaxCoefS$Coef<20,], x = ~ Coef, type = "histogram", text = ~paste("AMM:", AMM, "<br>Region" , CODE_REG), 
            name= "Erreur Regionale en tenant compte de la surface") 


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
#CoefDH
DHCultureN<- aggregate(Dose.d.application.retenue~AMM+ESPECE, data= EPHY, median,na.rm=TRUE)
DHCultureN<- ChangeNameCol(DHCultureN,"Dose.d.application.retenue","DH")
DHCultureN<-merge(unique(DHCultureN),unique(AGRESTE_2014[,c("Area","ESPECE")]), by="ESPECE")
DHCultureN$DHSurf<-DHCultureN$DH*DHCultureN$Area
SommeDHCultureN<-aggregate(DHSurf~AMM,data = DHCultureN,sum,na.rm=TRUE)
SommeDHCultureN<- ChangeNameCol(SommeDHCultureN, "DHSurf","SumDHSurf")
BaseDHN<-merge(DHCultureN, SommeDHCultureN, by="AMM")
BaseDHN<-BaseDHN[BaseDHN$AMM!="",]
BaseDHN$CoefDH<-BaseDHN$DH/BaseDHN$SumDHSurf

BaseSN<-merge(BasePKSN,BaseDHN,by=c("AMM","ESPECE"))
BaseSN$Coef<-BaseSN$CoefPK/BaseSN$CoefDH
##Max (produit)
MaxCoefSN<- aggregate(Coef~AMM, data = BaseSN, max)
##Hist
p2<-plot_ly(MaxCoefSN[MaxCoefSN$Coef<20,], x = ~ Coef, type = "histogram", 
            name= "Erreur Nationale en tenant compte de la surface") 

#=========================================================================================================
  ###################################################################################################

##♦Aprés le changement de Remy pour la surface
load(file.path(folderIn,"Agreste","AGRESTE_2014.rda"))
AGRESTE_2014<-AGRESTE_2014[AGRESTE_2014$CODE_REG!="00",]
#Erreurs r?gionales
##CofeBasePK (produit, culture, region)
BasePKS<-aggregate(cbind(mean,freq)~PHYTOPROD+ESPECE+CODE_REG, data= pk, sum)
BasePKS$DosePK<-BasePKS$mean*BasePKS$freq
BasePKS<-merge(BasePKS,AGRESTE_2014, by=c("ESPECE","CODE_REG"))
BasePKS$DoseSurf<- BasePKS$DosePK * BasePKS$Area 
SommeCulture<- aggregate(DoseSurf~PHYTOPROD+CODE_REG, data= BasePKS, sum)
SommeCulture <- ChangeNameCol(SommeCulture,"DoseSurf","SumDoseSurf")
BasePKS<- merge(BasePKS, SommeCulture, by=c("PHYTOPROD","CODE_REG"))
#changement
BasePKS$CoefPK<-BasePKS$DoseSurf/BasePKS$SumDoseSurf
BasePKS <- ChangeNameCol(BasePKS,"PHYTOPROD","AMM")

#CoefDH

DHCultureS<- aggregate(Dose.d.application.retenue~AMM+ESPECE, data= EPHY, median,na.rm=TRUE)
DHCultureS<- ChangeNameCol(DHCultureS,"Dose.d.application.retenue","DH")
DHCultureS<-merge(unique(DHCultureS),unique(AGRESTE_2014), by="ESPECE")
DHCultureS$DHSurf<-DHCultureS$DH*DHCultureS$Area
SommeDHCultureS<-aggregate(DHSurf~AMM+CODE_REG,data = DHCultureS,sum,na.rm=TRUE)
SommeDHCultureS<- ChangeNameCol(SommeDHCultureS, "DHSurf","SumDHSurf")
BaseDHS<-merge(DHCultureS, SommeDHCultureS, by=c("AMM","CODE_REG"))
BaseDHS<-BaseDHS[BaseDHS$AMM!="",]
BaseDHS$CoefDH<-BaseDHS$DHSurf/BaseDHS$SumDHSurf

BaseS<-merge(BasePKS,BaseDHS,by=c("AMM","ESPECE","CODE_REG","Area"))
BaseS$Coef<-BaseS$CoefPK/BaseS$CoefDH
##Max (produit)
MaxCoefS<- aggregate(Coef~AMM+CODE_REG, data = BaseS, max)
##Hist
p4<-plot_ly(MaxCoefS[MaxCoefS$Coef<30,], x = ~ Coef, type = "histogram", text = ~paste("AMM:", AMM), 
            name= "Erreur Regionale en tenant compte de la surface") 
