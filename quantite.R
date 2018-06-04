#rm(list=ls())

library(readr)
library(readxl)
library(stringr)
library(DataManagement)

dataFolder <- "~/data"
folderOut <- file.path(dataFolder,"donnees_R","PK")
load(file.path(dataFolder,"donnees_R","PK","pk2014.rda"))
load(file.path(dataFolder,"donnees_R","EPHY","EPHY.rda"))
load(file.path(dataFolder,"donnees_R","bnvdAcheteur","BNVD_2014.rda"))
names(BNVD_2014) <- iconv(names(BNVD_2014),to="ASCII//TRANSLIT")

# CALCUL QUANTITE PK PAR PRODUIT ET REGION (aggrègre les différentes cultures)
quanti <- aggregate(pk$quantite_pk,by=list(pk$PHYTOPROD,pk$CODE_REG),FUN=sum)
colnames(quanti) <- c("PHYTOPROD","CODE_REG","quantite_pk")

# focus sur l'ensemble de la France (retire les détails par région)
quanti <- quanti[quanti$CODE_REG=="00",]
quanti$CODE_REG <- NULL
#stop("tout va bien")

# INTEGRATION DONNEES BNVD
iNotDup <- which(!duplicated(BNVD_2014[,c("Code postal acheteur","AMM")]))
bnvdProd <- BNVD_2014[iNotDup,-c(7:10)]
bnvdProd <- aggregate(bnvdProd[,"Quantite produit"],by=list(bnvdProd$AMM),FUN=sum)
colnames(bnvdProd) <- c("PHYTOPROD","quantite_bnvd")
quanti <- merge(quanti,bnvdProd,all=T,by=c("PHYTOPROD"))

#COMPARAISON PK / BNVD
# CALCUL RAPPORT PK / BNVD
quanti$rapport_bnvd_pk <- quanti$quantite_bnvd/quanti$quantite_pk

# CALCUL DIFFERENCE PK - BNVD
quanti$diff_pk_bnvd <- quanti$quantite_pk-quanti$quantite_bnvd

#Ajouter les categories des produits
quanti<-merge(unique(quanti), unique(EPHY[,c(2,3)]),by.x= "PHYTOPROD", by.y="AMM")
quanti$Fonction<-as.factor(quanti$Fonction)

## On va ajouter la dose homologuee a notre base pour le scatter plot 
DH<-aggregate(Dose.d.application.retenue~AMM, EPHY, median)
quanti<- merge(unique(quanti), unique(DH), by.x="PHYTOPROD", by.y="AMM")
quanti$edp<- quanti$quantite_pk/ quanti$Dose.d.application.retenue
quanti<-merge(quanti,unique(BNVD_2014[,c("AMM","Exemple de nom de produit")]), by.x="PHYTOPROD",by.y="AMM")

saveAs(quanti,"quanti",folderOut)


#####Quantite de substance ##############
#Substance BNVD
SubsBnvd<-aggregate(BNVD_2014$`Quantite substance (Kg)`~BNVD_2014$AMM, data = BNVD_2014, sum)
colnames(SubsBnvd) <- c("AMM","Subs_bnvd")
#Substance PK
load(file.path(dataFolder,"donnees_R","bnvdAcheteur","Composition_par_pdt_subs.rda"))
ConcBnvd<-aggregate(Concentration~AMM, data = Composition_par_pdt_subs, sum)
SubsPk<- merge(quanti[,c("PHYTOPROD","quantite_pk")], ConcBnvd, by.x= "PHYTOPROD", by.y="AMM")
SubsPk$Subs_pk<- SubsPk$quantite_pk*SubsPk$Concentration

quanti_substance<- merge(SubsBnvd, SubsPk, by.x="AMM", by.y="PHYTOPROD")

#Ajouter les categories des produits
quanti_substance<-merge(unique(quanti_substance), unique(EPHY[,c(2,3)]),by="AMM")
quanti_substance$Fonction<-as.factor(quanti_substance$Fonction)

## On va ajouter la dose homologuee a notre base pour le scatter plot EDP
quanti_substance<- merge(unique(quanti_substance), unique(DH), by="AMM")
quanti_substance$edp<- quanti_substance$quantite_pk/ quanti_substance$Dose.d.application.retenue
quanti_substance<-merge(quanti_substance,unique(BNVD_2014[,c("AMM","Exemple de nom de produit")]),by="AMM")

#Ajouter la composition (substance concentration)
load("C:/Users/Utilisateur/Documents/data/donnees_R/bnvdAcheteur/Presence_EBP.rda")
quanti_substance<- merge(quanti_substance,Presence_EBP[,c("AMM","Composition")])

saveAs(quanti_substance,"quanti_substance",folderOut)