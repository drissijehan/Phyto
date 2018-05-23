dataFolder <- "~/data"
folderOut <- file.path(dataFolder,"donnees_R","BNVD")
load(file.path(dataFolder,"donnees_R","BNVD","BNVD.rda"))
library("DataManagement")
library(plotly)
library(testthat)
library(dplyr)
library(plyr)
library(Hmisc)

names(BNVD) <- iconv(names(BNVD),to="ASCII//TRANSLIT")

#Table de composition à partir de la BNVD
df<-aggregate(cbind(Qproduit=BNVD[,5], QSA=BNVD[,11]) ~ AMM+Annee_BNVD+BNVD$`Code postal acheteur` +Substance, data = BNVD, sum)
df$Concentration<- df$QSA / df$Qproduit
names(df)[3]<-"CP"

saveAs(df,"Table_Composition",folderOut)

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
 
saveAs(df3,"Composition_par_pdt_subs",folderOut)
#####################################

#Table de correspondances cultures dans Ephy et cultures dans PK
##Fait par Remy script intituleCulture.R

#####################################

# Les bases sont tous de l'année 2014
load(file.path(dataFolder,"donnees_R","BNVD","BNVD_2014.rda"))
load(file.path(dataFolder,"donnees_R","PK","pk2014.rda"))
load(file.path(dataFolder,"donnees_R","EPHY","EPHY.rda"))

names(BNVD_2014) <- iconv(names(BNVD_2014),to="ASCII//TRANSLIT")



################################### Table de Correspondance
AMMPK<- unique(pk$PHYTOPROD)
AMMBNVD<- unique(BNVD$AMM)
AMMEPHY<- unique(EPHY$AMM)
ALLAMM<- union(AMMEPHY,AMMBNVD)
ALLAMM<- union(ALLAMM,AMMPK)
ALLAMM<-as.data.frame(ALLAMM)
names(ALLAMM)<-"AMM"
ephy=c()
bnvd=c()
pk_=c()

correspondanceEPHY<-ALLAMM %>%
  split(.$AMM) %>%
  lapply(function(d){d=data.frame(d);
  if(d$AMM %in% AMMEPHY){ephy<-TRUE} else if (d$AMM %nin% AMMEPHY){ephy<-FALSE}
  })
correspondanceBNVD<-ALLAMM %>%
  split(.$AMM) %>%
  lapply(function(d){d=data.frame(d);
  if(d$AMM %in% AMMBNVD){bnvd<-TRUE} else if (d$AMM %nin% AMMBNVD){bnvd<-FALSE}
  })
correspondancePK<-ALLAMM %>%
  split(.$AMM) %>%
  lapply(function(d){d=data.frame(d);
  if(d$AMM %in% AMMPK){pk_<-TRUE} else if (d$AMM %nin% AMMPK){pk_<-FALSE}
  })

correspondanceEPHY<- data.frame(do.call(rbind,correspondanceEPHY))
correspondanceEPHY$AMM=rownames(correspondanceEPHY)
correspondanceBNVD<- data.frame(do.call(rbind,correspondanceBNVD))
correspondanceBNVD$AMM=rownames(correspondanceBNVD)
correspondancePK<- data.frame(do.call(rbind,correspondancePK))
correspondancePK$AMM=rownames(correspondancePK)

dfCorrespondance<-merge(correspondanceEPHY,correspondanceBNVD, by="AMM")
dfCorrespondance<-merge(dfCorrespondance,correspondancePK, by="AMM")
names(dfCorrespondance)<- c("AMM","EPHY","BNVD","PK")




################################################Ajout DH et Compostition BNVD a la table de correspondance

DH<- aggregate(Dose.d.application.retenue~AMM, data = EPHY, median)

#Concatener les substances du meme AMM
df3 <- data.frame(df3,newCol=paste(df3$Substance,df3$Concentration,sep=" "))

Enregistrement<- df3 %>%
       split(.$AMM) %>%
       lapply(function(d){d=data.frame(d);paste0(levels(as.factor(as.vector(d[,"newCol"]))),collapse=" / ")})

Enregistrement<- data.frame(do.call(rbind,Enregistrement))
Enregistrement$col=rownames(Enregistrement)

names(Enregistrement)<-c("Enregistrement","AMM")

expect_equal(nrow(Enregistrement),length(unique(df3$AMM)))

df4<-merge(dfCorrespondance,DH,by="AMM", all.x=TRUE)
df4<-merge(df4,Enregistrement,by="AMM", all.x=TRUE)

saveAs(df4,"Table_Correspondance",folderOut)

##On ajoute les Categories
### SAuf que les categories ne se presentent que dans EPHY
### -> 854 AMM sans Categories, ceux qui sont presents dans BNVD ET/OU Pk et non EPHY

df44<-merge(df4,unique(EPHY[,c("AMM","Fonction")]), by= "AMM",all.x=TRUE)

expect_equal(nrow(df4),nrow((df44)))
#5346 - 5440 == -94 -> Les produits qui appartiennent à plus d'une categorie

#On cherche les produits avec plus qu'une categorie
dfDupl<-df44[which(duplicated(df44$AMM)),]
dfDuplicated<- subset(df44, df44$AMM %in% dfDupl$AMM)

Duplicate<-table(dfDuplicated$AMM,dfDuplicated$Fonction)
Duplicate<-D<-as.data.frame.matrix(Duplicate) 
Duplicate$AMM=rownames(Duplicate)

#####################################

n1=length(unique(EPHY$AMM))
n2=length(unique(EPHY[EPHY$Gamme.d.usages%in%"Professionnel",]$AMM))
n3=length(unique(EPHY[EPHY$Filiere%in%"Grandes cultures",]$AMM))
n4=length(unique(EPHY[EPHY$Filiere%in%"Traitements généraux toutes cultures",]$AMM))

df5 <- data.frame(Total = n1, UsagePro = n2, GC = n3 , TC = n4)

#Filiere GC + Toutes Cultures est different de Usage Professionnel (n3+n4!=n2)



######################################

df6<- table(df4$EPHY,df4$BNVD,df4$PK)
df6<-as.data.frame(df6)
df6<- ChangeNameCol(df6,list("Var1","Var2","Var3"),list("EPHY","BNVD","PK"))
df6<-t(df6)
df6<-as.data.frame(df6)


#df6 est pour tt la base EPHY -> Je refais le travail pour EPHY$Filiere %in% c("GC","TC")

GTC<-subset(EPHY, EPHY$Filiere %in% c("Grandes cultures","Traitements généraux toutes cultures"))
AMMGTC<- unique(GTC$AMM)
gtc<-c()
correspondanceGTC<-ALLAMM %>%
  split(.$AMM) %>%
  lapply(function(d){d=data.frame(d);
  if(d$AMM %in% AMMGTC){gtc<-TRUE} else if (d$AMM %nin% AMMGTC){gtc<-FALSE}
  })
correspondanceGTC<- data.frame(do.call(rbind,correspondanceGTC))
correspondanceGTC$AMM=rownames(correspondanceGTC)
dfCorrespondanceGTC<-merge(correspondanceGTC,correspondanceBNVD, by="AMM")
dfCorrespondanceGTC<-merge(dfCorrespondanceGTC,correspondancePK, by="AMM")
names(dfCorrespondanceGTC)<- c("AMM","EPHY","BNVD","PK")

df66<- table(dfCorrespondanceGTC$EPHY,dfCorrespondanceGTC$BNVD,dfCorrespondanceGTC$PK)
df66<-as.data.frame(df66)
df66<- ChangeNameCol(df66,list("Var1","Var2","Var3"),list("EPHY","BNVD","PK"))
df66<-t(df66)
df66<-as.data.frame(df66)

####### table par categorie
df666<- df44 %>%
     split(.$Fonction) %>%
     lapply(function(d){d=data.frame(d);table(d$EPHY,d$BNVD,d$PK)})
df666<-as.data.frame(df666)
df666<-df666[,c(1,2,3,4,8,12,16,20,24,28,32,36,40,44,48,52)]
names(df666)[,c("Acaricide.Var1","Acaricide.Var2","Acaricide.Var3")]<-c("EPHY","BNVD","PK")
df666<- ChangeNameCol(df666,list("Acaricide.Var1","Acaricide.Var2","Acaricide.Var3"),list("EPHY","BNVD","PK"))
#df666<-t(df666)

saveAs(df666,"Table_Distribution",folderOut)

#####################################################
##Barplot BNVD PK
dat<-df666
dat$barplot<-c("TFF","TTF","TFT","TTT")
dat<-dat[dat$barplot%in%c("TTF","TTT"),]

p <- plot_ly(dat, x = ~barplot, y = ~Acaricide.Freq, type = 'bar', name = 'Acaricide') %>%
  add_trace(y = ~Fongicide.Freq, name = 'Fongicide') %>%
  add_trace(y = ~Molluscicide.Freq, name = 'Molluscicide') %>%
  add_trace(y = ~Stimul..Déf..Naturelles.Freq, name = 'Stimul..Déf..Naturelles') %>%
  add_trace(y = ~Virucide.Freq, name = 'Virucide') %>%
  add_trace(y = ~Bactéricide.Freq, name = 'Bactéricide') %>%
  add_trace(y = ~Herbicide.Freq, name = 'Herbicide') %>%
  add_trace(y = ~Nématicide.Freq, name = 'Nématicide') %>%
  add_trace(y = ~Stimulateur.des.défenses.naturelles.Freq, name = 'Stimulateur.des.défenses.naturelles') %>%
  add_trace(y = ~Dévitalisation.Freq, name = 'Dévitalisation') %>%
  add_trace(y = ~Taupicide.Freq, name = 'Taupicide') %>%
  add_trace(y = ~Rodenticide.Freq, name = 'Rodenticide.Freq') %>%
  add_trace(y = ~Insecticide.Freq, name = 'Insecticide') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'stack')



###???Barplot
##Restreint sur produits BNVD2014 et Ephy [filiere %in% c(GC,Toutes cultures)] 
a<-as.data.frame(table(GTC$AMM))
b<-as.data.frame(table(BNVD_2014$AMM))
c<-as.data.frame(table(pk$PHYTOPROD))
dfs <- list(a,b,c)
df7<-join_all(dfs, "Var1")
names(df7)<- c("AMM","EPHY","BNVD","PK")
df7<-merge(unique(df7),unique(GTC[,c("AMM","Fonction")]),by="AMM",all.x = TRUE)
df7<-na.omit(df7)
p <- plot_ly(df7, x = ~Fonction, y = ~EPHY, type = 'bar', name = 'EPHY', marker = list(color = 'rgb(49,130,189)')) %>%
  add_trace(y = ~BNVD, name = 'BNVD', marker = list(color = 'rgb(204,204,204)')) %>%
  add_trace(y = ~PK, name = 'PK', marker = list(color = 'rgb(8,48,107)')) %>%
  layout(xaxis = list(title = "", tickangle = -45),
         yaxis = list(title = ""),
         margin = list(b = 100),
         barmode = 'group')
