dataFolder <- "~/data"
folderOut <- file.path(dataFolder,"donnees_R","bnvdAcheteur")
load(file.path(dataFolder,"donnees_R","bnvdAcheteur","BNVD.rda"))
library("DataManagement")
library(plotly)
library(testthat)
library(dplyr)
library(plyr)
library(Hmisc)
library(reshape2)

names(BNVD) <- iconv(names(BNVD),to="ASCII//TRANSLIT")

#Table de composition à partir de la BNVD
df<-aggregate(cbind(Qproduit=BNVD[,5], QSA=BNVD[,11]) ~ AMM+Annee_BNVD+BNVD$`Code postal acheteur` +Substance, data = BNVD, sum)
df$Concentration<- df$QSA / df$Qproduit
names(df)[3]<-"CP"

saveAs(df,"concentration_pdt_subs_annee_cp",folderOut)

#Histogramme Rapport sd/mean (concentration)
df_mean<- aggregate(Concentration~AMM+Substance, data = df, mean, na.rm=TRUE)
df_mean <- ChangeNameCol(df_mean,"Concentration","Mean")
df_sd<- aggregate(Concentration~AMM+Substance, data = df, sd, na.rm=TRUE)
df_sd <- ChangeNameCol(df_sd,"Concentration","Sd")
df2<- merge(df_mean,df_sd,by=c("AMM","Substance"))
df2$CoefVar <- df2$Sd / df2$Mean
#-> seulement 18 consituants répartis sur 10 produits avec un problème (coefficient de variation > 0.1), dans tous les cas inférieur à 2
#   problème mineur donc, la médiane devrait être un excellent reflet des concentrations

pbConcentrations <- df2[which(df2$CoefVar>0.1),]
length(unique(pbConcentrations$AMM))

BNVDpb <- BNVD[which(BNVD$AMM %in% pbConcentrations$AMM),]
QpbConcentrations <- aggregate(BNVDpb[,"Quantite produit"],by=list(BNVDpb$AMM),sum)
sum(QpbConcentrations$x/ sum(BNVD$`Quantite produit`))
#=> 5/10000 on verra plus tard

plot_ly(df2, x = ~ CoefVar, type = "histogram", text = ~paste("AMM:", AMM, "Substance:" , Substance))  %>%
  layout(yaxis = list(type = "log"))

###Warning: Ignoring 481 observations###
length(which(is.na(df2$CoefVar))) #->481
length(which(is.na(df2$Sd))) #->438 Observations(AMM+Substance) where we have 1 row, So sd(..)==NA, So CoefVar==NA
length(which(df2$Mean==0)) #->61 Mean==0 So CoefVar=Sd/Mean==NA
length(intersect(which(df2$Mean==0),which(is.na(df2$Sd)))) #->18 Observations where they have Sd==NA and Mean==0 at the same time
expect_equal(length(which(is.na(df2$CoefVar))),length(which(is.na(df2$Sd)))+length(which(df2$Mean==0))-length(intersect(which(df2$Mean==0),which(is.na(df2$Sd)))))

#Composition fixer par produit et substance
df3<-aggregate(Concentration~AMM+Substance, data= df, median)

saveAs(df3,"Composition_par_pdt_subs",folderOut)
#####################################

#Table de correspondances cultures dans Ephy et cultures dans PK
##Fait par Remy script intituleCulture.R

#####################################

# Les bases sont tous de l'année 2014
load(file.path(dataFolder,"donnees_R","bnvdAcheteur","BNVD_2014.rda"))
load(file.path(dataFolder,"donnees_R","PK","pk2014.rda"))
load(file.path(dataFolder,"donnees_R","EPHY","EPHY.rda"))

names(BNVD_2014) <- iconv(names(BNVD_2014),to="ASCII//TRANSLIT")
