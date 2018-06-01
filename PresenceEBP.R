dataFolder <- "~/data"
folderOut <- file.path(dataFolder,"donnees_R","bnvdAcheteur")
load(file.path(dataFolder,"donnees_R","bnvdAcheteur","BNVD.rda"))
load(file.path(dataFolder,"donnees_R","bnvdAcheteur","BNVD_2014.rda"))
load(file.path(dataFolder,"donnees_R","PK","pk.rda"))
load(file.path(dataFolder,"donnees_R","EPHY","EPHY.rda"))
names(BNVD) <- iconv(names(BNVD),to="ASCII//TRANSLIT")
names(BNVD_2014) <- iconv(names(BNVD_2014),to="ASCII//TRANSLIT")

load(file.path(dataFolder,"donnees_R","bnvdAcheteur","Composition_par_pdt_subs.rda"))
library("DataManagement")
library(plotly)
library(testthat)
library(dplyr)
library(plyr)
library(Hmisc)
library(reshape2)
names(BNVD) <- iconv(names(BNVD),to="ASCII//TRANSLIT")


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
df33 <- data.frame(Composition_par_pdt_subs,newCol=paste(Composition_par_pdt_subs$Substance,Composition_par_pdt_subs$Concentration,sep=" "))

Composition<- df33 %>%
  split(.$AMM) %>%
  lapply(function(d){d=data.frame(d);paste0(levels(as.factor(as.vector(d[,"newCol"]))),collapse=" / ")})

Composition<- data.frame(do.call(rbind,Composition))
Composition$col=rownames(Composition)

names(Composition)<-c("Composition","AMM")

expect_equal(nrow(Composition),length(unique(Composition_par_pdt_subs$AMM)))

df4<-merge(dfCorrespondance,DH,by="AMM", all.x=TRUE)
df4<-merge(df4,Composition,by="AMM", all.x=TRUE)

expect_equal(which(!df4$BNVD),which(is.na(df4$Composition)))
#expect_equal(which(!df4$EPHY),which(is.na(df4$Dose.d.application.retenue)))
#170 AMM sans Dose d'application retenue
df4<- ChangeNameCol(df4,"Dose.d.application.retenue","DH")

saveAs(df4,"Presence_EBP",folderOut)

##On ajoute les Categories
### SAuf que les categories ne se presentent que dans EPHY
### -> 854 AMM sans Categories, ceux qui sont presents dans BNVD ET/OU Pk et non EPHY

df44<-merge(df4,unique(EPHY[,c("AMM","Fonction")]), by= "AMM",all.x=TRUE)

#expect_equal(nrow(df4),nrow((df44)))
#5346 - 5440 == -94 -> Les produits qui appartiennent à plus d'une categorie

#On cherche les produits avec plus qu'une categorie
dfDupl<-df44[which(duplicated(df44$AMM)),]
dfDuplicated<- subset(df44, df44$AMM %in% dfDupl$AMM)

Duplicate<-table(dfDuplicated$AMM,dfDuplicated$Fonction)
Duplicate<-as.data.frame.matrix(Duplicate) 
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


#df6 est pour tt la base EPHY -> Je refais le travail pour EPHY$Filiere %in% c("GC","TC") et usage pro

GTC<-subset(EPHY, EPHY$Filiere %in% c("Grandes cultures","Traitements généraux toutes cultures"))
GTC<-subset(GTC, GTC$Gamme.d.usages%in% c("Professionnel"))
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

####### table par categorie GTC Usage PRO#######################
EPHY[EPHY$Fonction%in%"Stimul. Déf. Naturelles",]<-"Stimulateur des défenses naturelles"
dfCorrespondanceGTC<-merge(dfCorrespondanceGTC,unique(EPHY[,c("AMM","Fonction")]), by= "AMM",all.x=TRUE)
df666<- dfCorrespondanceGTC %>%
  split(.$Fonction) %>%
  lapply(function(d){d=data.frame(d);table(d$EPHY,d$BNVD,d$PK)})
df666<-as.data.frame(df666)
df666<-df666[,c(1,2,3,4,8,12,16,20,24,28,32,36,40,44,48)]

df666<- ChangeNameCol(df666,list("Acaricide.Var1","Acaricide.Var2","Acaricide.Var3"),list("EPHY","BNVD","PK"))
#df666<-t(df666)

saveAs(df666,"Presence_EgtcBP",file.path(dataFolder,"donnees_R","bnvdAcheteur"))

#####################################################
##Barplot BNVD PK
dat<-df666
dat$barplot<-c("TFF","TTF","TFT","TTT")
dat<-dat[dat$barplot%in%c("TTF","TTT"),]

p <- plot_ly(dat, x = ~barplot, y = ~Acaricide.Freq, type = 'bar', name = 'Acaricide') %>%
  add_trace(y = ~Fongicide.Freq, name = 'Fongicide') %>%
  add_trace(y = ~Molluscicide.Freq, name = 'Molluscicide') %>%
  
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


############# Barplot Volume #############################
T_F<-dfCorrespondanceGTC %>%
  split(.$AMM) %>%
  lapply(function(d){d=data.frame(d);
  if(all(d[,c("EPHY","BNVD","PK")] == c(TRUE,TRUE,TRUE))==TRUE){t_f<-"EBP"}
  else if (all(d[,c("EPHY","BNVD","PK")] == c(TRUE,TRUE,FALSE))==TRUE){t_f<-"EB-"}
  else if (all(d[,c("EPHY","BNVD","PK")] == c(TRUE,FALSE,FALSE))==TRUE){t_f<-"E--"}
  else if (all(d[,c("EPHY","BNVD","PK")] == c(FALSE,FALSE,FALSE))==TRUE){t_f<-"---"}
  else if (all(d[,c("EPHY","BNVD","PK")] == c(TRUE,FALSE,TRUE))==TRUE){t_f<-"E-P"}
  else if (all(d[,c("EPHY","BNVD","PK")] == c(FALSE,FALSE,TRUE))==TRUE){t_f<-"--P"}
  else if (all(d[,c("EPHY","BNVD","PK")] == c(FALSE,TRUE,TRUE))==TRUE){t_f<-"-BP"}
  else if (all(d[,c("EPHY","BNVD","PK")] == c(FALSE,TRUE,FALSE))==TRUE){t_f<-"-B-"}
  })
T_F<- data.frame(do.call(rbind,T_F))
T_F$AMM=rownames(T_F)
distributionVolume<-merge(T_F, dfCorrespondanceGTC, by="AMM")
distributionVolume<- ChangeNameCol(distributionVolume,"do.call.rbind..T_F.","T_F")

#Volume BNVD
QBNVD<-aggregate(BNVD$`Quantite produit`~AMM, data = BNVD, sum)
QBNVD <- ChangeNameCol(QBNVD,"BNVD$`Quantite produit`","QBNVD")


distributionVolume<-merge(distributionVolume,QBNVD, by="AMM",all.x = TRUE)

Dist<- aggregate(QBNVD~T_F+Fonction, data = distributionVolume, sum)
Dist_Cast<-dcast(Dist, T_F~Fonction)

p <- plot_ly(Dist_Cast, x = ~T_F, y = ~Herbicide, type = 'bar', name = 'Herbicide') %>%
  add_trace(y = ~Fongicide, name = 'Fongicide') %>%
  add_trace(y = ~Insecticide, name = 'Insecticide') %>%
  add_trace(y = ~Molluscicide, name = 'Molluscicide') %>%
  add_trace(y = ~Acaricide, name = 'Acaricide') %>%
  add_trace(y = ~Dévitalisation, name = 'Dévitalisation') %>%
  add_trace(y = ~Bactéricide, name = 'Bactéricide') %>%
  
  layout(yaxis = list(title = 'Volume BNVD'), barmode = 'stack')


saveAs(distributionVolume,"presence_EgtcBP_VolumeBnvd",file.path(dataFolder,"donnees_R","bnvdAcheteur"))

##########################Correspondance Culture EHPHY et PK##############
load(file.path(dataFolder,"donnees_R","EPHY","intituleCulture.rda"))
colnames(intituleCulture)<-c("culture","betterave","ble_dur","ble_tendre","canne_a_s","colza","mais_ens","mais_gr",
                             "orge","pois","pomme_de_t","tournesol","triticale")
s<-c()
ss<-intituleCulture %>%
  split(.$culture) %>%
  lapply(function(d){d=data.frame(d);
  if(d$betterave == 1){s<-"betterave"} else if (d$ble_dur==1){s<-"ble dur"} else if (d$ble_tendre==1){s<-"ble tendre"} 
  else if (d$canne_a_s==1){s<-"canne a s"} else if (d$colza==1){s<-"colza"} else if (d$mais_ens==1){s<-"mais ens"}
  else if (d$mais_gr==1){s<-"mais gr"} else if (d$orge==1){s<-"orge"} else if (d$pois==1){s<-"pois"}
  else if (d$pomme_de_t==1){s<-"pomme de t"} else if (d$tournesol==1){s<-"tournesol"}
  else if (d$triticale==1){s<-"triticale"} 
  })
ss<- data.frame(do.call(rbind,ss))
ss$culture=rownames(ss)
ss <- ChangeNameCol(ss,"do.call.rbind..ss.","ESPECE")

d=c()
dd<-intituleCulture %>%
  split(.$culture) %>%
  lapply(function(d){d=data.frame(d);
  if(d$betterave == 1){d<-"betterave"} else if (d$ble_dur==1){d<-"ble dur"} else if (d$ble_tendre==1){d<-"ble tendre"} 
  else if (d$canne_a_s==1){d<-"canne a s"} else if (d$colza==1){d<-"colza"} else if (d$mais_ens==1){d<-"mais ens"}
  else if (d$mais_gr==1){d<-"mais gr"} else if (d$orge==1){d<-"orge"} else if (d$pois==1){d<-"pois"}
  else if (d$pomme_de_t==1){d<-"pomme de t"} else if (d$tournesol==1){d<-"tournesol"}
  else if (d$triticale==1){d<-"triticale"}else {d<-d$culture}
  })
dd<- data.frame(do.call(rbind,dd))
dd$culture=rownames(dd)
dd <- ChangeNameCol(dd,"do.call.rbind..dd.","ESPECE")

j<-c()
jj<-dd %>%
  split(.$culture) %>%
  lapply(function(d){d=data.frame(d);
  if(d$ESPECE %nin% c("betterave","ble dur","ble tendre","canne a s","colza", "mais gr", "mais ens"
                      , "mais gr","orge","pois","pomme de t","tournesol")){d$ESPECE<-d$culture} 
  })
jj<- data.frame(do.call(rbind,jj))
jj$culture=rownames(jj)
jj <- ChangeNameCol(jj,"do.call.rbind..jj.","ESPECE")

CorrespondanceCultureEphyPk<-rbind(ss,jj)

rownames(CorrespondanceCultureEphyPk) <- 1:nrow(CorrespondanceCultureEphyPk)

library(rgr)
insertRows <- function(dat, newlines, indices){
  lines <- c(1:nrow(dat), indices-0.5)
  dat <- cbind(rbind(dat, newlines), lines=lines)
  dat <- gx.sort.df(~lines, dat)
  subset(dat, select=-ncol(dat))
}
newlines <- rbind( c("ble tendre","Blé"), 
                   
                   c("mais gr","Maïs"),
                   
                   c("ble dur","Traitements généraux"),c("ble tendre","Traitements généraux"), c("canne a s","Traitements généraux"), 
                   c("colza","Traitements généraux"), c("mais ens","Traitements généraux"), c("mais gr","Traitements généraux"), 
                   c("orge","Traitements généraux"),  c("pois","Traitements généraux"), c("pomme de t","Traitements généraux"), 
                   c("tournesol","Traitements généraux"), c("triticale","Traitements généraux"), 
                   
                   c("ble tendre","Céréales"), c("orge","Céréales"), c("triticale","Céréales"),
                   
                   c("ble tendre","Céréales à paille"), c("orge","Céréales à paille"), c("triticale","Céréales à paille"),
                   
                   c("ble dur","Jachères et cultures intermédiaires"),c("pois","Jachères et cultures intermédiaires"),
                   c("pomme de t","Jachères et cultures intermédiaires"), c("ble tendre","Jachères et cultures intermédiaires"),
                   c("canne a s","Jachères et cultures intermédiaires"), c("colza","Jachères et cultures intermédiaires"),
                   c("mais ens","Jachères et cultures intermédiaires"), c("mais gr","Jachères et cultures intermédiaires"), 
                   c("orge","Jachères et cultures intermédiaires"), c("tournesol","Jachères et cultures intermédiaires"), 
                   c("triticale","Jachères et cultures intermédiaires")
                   
)
colnames(newlines)<-c("ESPECE","culture")
indices <- c(4,11,20:30,7:9,8:10,10:20)
CorrespondanceCultureEphyPk<-insertRows(CorrespondanceCultureEphyPk, newlines, indices)
CorrespondanceCultureEphyPk<-CorrespondanceCultureEphyPk[-which(CorrespondanceCultureEphyPk$culture%in%"Adjuvants"),]

saveAs(CorrespondanceCultureEphyPk,"CorrespondanceCultureEphyPk",file.path(dataFolder,"donnees_R","EPHY"))

##############################################################################
############### Table Correspondace EPHY PK #############################
load(file.path(dataFolder,"donnees_R","EPHY","EPHY.rda"))
culture <- sapply(strsplit(as.vector(EPHY$Intitule),"*",fixed = TRUE), function(x) x[1])
EPHY <- cbind(EPHY,culture)
EPHY<- merge(EPHY,CorrespondanceCultureEphyPk, by="culture")

AmmEspecePk<- unique(pk[,c("ESPECE","PHYTOPROD")])
AmmEspecePk<- ChangeNameCol(AmmEspecePk,"PHYTOPROD","AMM")
AmmEspeceEphy<- unique(EPHY[,c("ESPECE","AMM")])
ALL<- rbind(AmmEspecePk,AmmEspeceEphy)
ALL<- unique(ALL)

a<-is.element(ALL$ESPECE,AmmEspeceEphy$ESPECE)
b<-is.element(ALL$AMM,AmmEspeceEphy$AMM)
correspondanceE<-cbind(a,b)
c<-as.data.frame(c)
colnames(correspondanceE)<-c("EspeceEphy","AmmEphy")
inEPHY<-apply(correspondanceE, 1, function(x) {if(all(x==TRUE)){f<-TRUE}else{f<-FALSE}}) 
inEPHY<-as.data.frame(inEPHY)

aa<-is.element(ALL$ESPECE,AmmEspecePk$ESPECE)
bb<-is.element(ALL$AMM,AmmEspecePk$AMM)
correspondanceP<-cbind(aa,bb)
correspondanceP<-as.data.frame(correspondanceP)
colnames(correspondanceP)<-c("EspecePk","AmmPk")
inPK<-apply(correspondanceP, 1, function(x) {if(all(x==TRUE)){ff<-TRUE}else{ff<-FALSE}}) 
inPK<-as.data.frame(inPK)

CorrespondanceEphyPk<-cbind(ALL,inEPHY,inPK)

DHCulture<- aggregate(Dose.d.application.retenue~AMM+ESPECE, data= EPHY, median)

CorrespondanceEphyPk<- merge(CorrespondanceEphyPk, DHCulture, by=c("ESPECE","AMM"),all.x = TRUE)
CorrespondanceEphyPk <- ChangeNameCol(CorrespondanceEphyPk,"Dose.d.application.retenue","DH")
saveAs(CorrespondanceEphyPk,"CorrespondanceEphyPk",file.path(dataFolder,"donnees_R","EPHY"))

#####################Barplot GC Usage Pro ###################
GC<-subset(EPHY, EPHY$Filiere %in% "Grandes cultures")
GC<-subset(GC, GC$Gamme.d.usages%in% c("Professionnel"))
AMMGC<- unique(GC$AMM)
gc<-c()
correspondanceGC<-ALLAMM %>%
  split(.$AMM) %>%
  lapply(function(d){d=data.frame(d);
  if(d$AMM %in% AMMGC){gc<-TRUE} else if (d$AMM %nin% AMMGC){gc<-FALSE}
  })
correspondanceGC<- data.frame(do.call(rbind,correspondanceGC))
correspondanceGC$AMM=rownames(correspondanceGC)
dfCorrespondanceGC<-merge(correspondanceGC,correspondanceBNVD, by="AMM")
dfCorrespondanceGC<-merge(dfCorrespondanceGC,correspondancePK, by="AMM")
names(dfCorrespondanceGC)<- c("AMM","EPHY","BNVD","PK")
dfCorrespondanceGC<-merge(dfCorrespondanceGC,unique(EPHY[,c("AMM","Fonction")]), by= "AMM",all.x=TRUE)

T_F_GC<-dfCorrespondanceGC %>%
  split(.$AMM) %>%
  lapply(function(d){d=data.frame(d);
  if(all(d[,c("EPHY","BNVD","PK")] == c(TRUE,TRUE,TRUE))==TRUE){t_f_<-"EBP"}
  else if (all(d[,c("EPHY","BNVD","PK")] == c(TRUE,TRUE,FALSE))==TRUE){t_f_<-"EB-"}
  else if (all(d[,c("EPHY","BNVD","PK")] == c(TRUE,FALSE,FALSE))==TRUE){t_f_<-"E--"}
  else if (all(d[,c("EPHY","BNVD","PK")] == c(FALSE,FALSE,FALSE))==TRUE){t_f_<-"---"}
  else if (all(d[,c("EPHY","BNVD","PK")] == c(TRUE,FALSE,TRUE))==TRUE){t_f_<-"E-P"}
  else if (all(d[,c("EPHY","BNVD","PK")] == c(FALSE,FALSE,TRUE))==TRUE){t_f_<-"--P"}
  else if (all(d[,c("EPHY","BNVD","PK")] == c(FALSE,TRUE,TRUE))==TRUE){t_f_<-"-BP"}
  else if (all(d[,c("EPHY","BNVD","PK")] == c(FALSE,TRUE,FALSE))==TRUE){t_f_<-"-B-"}
  })
T_F_GC<- data.frame(do.call(rbind,T_F_GC))
T_F_GC$AMM=rownames(T_F_GC)

distributionVolumeGC<-merge(T_F_GC, dfCorrespondanceGC, by="AMM")
distributionVolumeGC<- ChangeNameCol(distributionVolumeGC,"do.call.rbind..T_F_GC.","T_F")


distributionVolumeGC<-merge(distributionVolumeGC,QBNVD, by="AMM",all.x = TRUE)

DistGC<- aggregate(QBNVD~T_F+Fonction, data = distributionVolumeGC, sum)
Dist_Cast_GC<-dcast(DistGC, T_F~Fonction)

saveAs(distributionVolumeGC,"presence_EgtcBP_VolumeBnvdGC",file.path(dataFolder,"donnees_R","bnvdAcheteur"))

p <- plot_ly(Dist_Cast_GC, x = ~T_F, y = ~Herbicide, type = 'bar', name = 'Herbicide') %>%
  add_trace(y = ~Fongicide, name = 'Fongicide') %>%
  add_trace(y = ~Insecticide, name = 'Insecticide') %>%
  add_trace(y = ~Molluscicide, name = 'Molluscicide') %>%
  add_trace(y = ~Acaricide, name = 'Acaricide') %>%
  add_trace(y = ~Dévitalisation, name = 'Dévitalisation') %>%
  add_trace(y = ~Bactéricide, name = 'Bactéricide') %>%
  add_trace(y = ~Nématicide, name = 'Nématicide') %>%
  add_trace(y = ~Taupicide, name = 'Taupicide') %>%
  layout(yaxis = list(title = 'Volume BNVD'), barmode = 'stack')
p
