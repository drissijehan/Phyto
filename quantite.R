#rm(list=ls())

library(readr)
library(readxl)
library(stringr)

load("pk.rda")
load("EPHY.rda")
load("BNVD_2014.rda")
# CALCUL QUANTITE PK PAR PRODUIT ET REGION
quanti <- aggregate(pk$quantite_pk,by=list(pk$PHYTOPROD,pk$CODE_REG),FUN=sum)
colnames(quanti) <- c("PHYTOPROD","CODE_REG","quantite_pk")
quanti <- quanti[quanti$CODE_REG=="00",]
quanti$CODE_REG <- NULL

# INTEGRATION DONNEES BNVD
bnvdProd <- unique(BNVD_2014[,-c(7:10)])
bnvdProd <- aggregate(bnvdProd[,"Quantité.produit"] ,by=list(bnvdProd$AMM),FUN=sum)
colnames(bnvdProd) <- c("PHYTOPROD","quantite_bnvd")
quanti <- merge(quanti,bnvdProd,all=T)

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


save(quanti,file ="quanti.rda")
