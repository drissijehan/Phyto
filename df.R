dataFolder <- "~/data"
folderOut <- file.path(dataFolder,"donnees_R","BNVD")
load(file.path(dataFolder,"donnees_R","BNVD","BNVD.rda"))
library("DataManagement")
library(plotly)
library(testthat)

names(BNVD) <- iconv(names(BNVD),to="ASCII//TRANSLIT")

#Table de correspondance à partir de la BNVD
df<-aggregate(cbind(Qproduit=BNVD[,5], QSA=BNVD[,11]) ~ AMM+Annee_BNVD+BNVD$`Code postal acheteur` +Substance, data = BNVD, sum)
df$Concentration<- df$QSA / df$Qproduit
names(df)[3]<-"CP"

#Histogramme Rapport sd/mean (concentration)
df_mean<- aggregate(Concentration~AMM+Substance, data = df, mean)
df_mean <- ChangeNameCol(df_mean,"Concentration","Mean")
df_sd<- aggregate(Concentration~AMM+Substance, data = df, sd)
df_sd <- ChangeNameCol(df_sd,"Concentration","Sd")
df2<- merge(df_mean,df_sd,by=c("AMM","Substance"))
df2$CoefVar <- df2$Sd / df2$Mean
#-> seulement 18 consituants répartis sur 10 produits avec un problème (coefficient de variation > 0.1), dans tous les cas inférieur à 2
#   problème mineur donc, la médiane devrait être un excellent reflet des concentrations

pbConcentrations <- df2[which(df2$CoefVar>0.1),]


BNVDpb <- BNVD[which(BNVD$AMM %in% pbConcentrations$AMM),]
QpbConcentrations <- aggregate(BNVDpb[,"Quantite produit"],by=list(BNVDpb$AMM),sum)
sum(QpbConcentrations$x/ sum(BNVD$`Quantite produit`))
#=> 5/10000 on verra plus tard

plot_ly(df2, x = ~ CoefVar, type = "histogram", text = ~paste("AMM:", AMM, "Substance:" , Substance)) 

#Composition fixer par produit et substance
df3<-aggregate(Concentration~AMM+Substance, data= df, median)
 

#####################################

#Table de correspondances cultures dans Ephy et cultures dans PK
##Fait par Remy script intituleCulture.R

#####################################

# Les bases sont tous de l'année 2014
load(file.path(dataFolder,"donnees_R","BNVD","BNVD_2014.rda"))
load(file.path(dataFolder,"donnees_R","PK","pk2014.rda"))
load(file.path(dataFolder,"donnees_R","EPHY","EPHY.rda"))

names(BNVD_2014) <- iconv(names(BNVD_2014),to="ASCII//TRANSLIT")

amm<-intersect(BNVD_2014$AMM,pk$PHYTOPROD)
amm<-intersect(amm,EPHY$AMM)

inEPHY<- subset(EPHY, EPHY$AMM %in% amm)
expect_equal(length(amm),length(unique(inEPHY$AMM)))

DH<- aggregate(Dose.d.application.retenue~AMM, data = inEPHY, median)
expect_equal(nrow(DH),length(unique(inEPHY$AMM)))
#=> il y a une difference de 3 AMM 
setdiff(inEPHY$AMM, DH$AMM)

#Concatener les substances du meme AMM
Enregistrement<- df3 %>%
       split(.$AMM) %>%
       lapply(function(d){d=data.frame(d);paste0(levels(as.factor(as.vector(d[,"enrg"]))),collapse=" / ")})

Enregistrement<- data.frame(do.call(rbind,Enregistrement))
Enregistrement$col=rownames(Enregistrement)

names(Enregistrement)<-c("Enregistrement","AMM")

expect_equal(nrow(Enregistrement),length(unique(df3$AMM)))

df4<- merge(DH,Enregistrement, by="AMM")

saveAs(df,"Table_Composition",folderOut)
