source("dataSource.R")
load(file.path(dataFolder,"donnees_R","PK","PK.rda"))
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
newlines <- rbind( c("ble tendre","Bl�"), 
                   
                   c("mais gr","Ma�s"),
                   
                   c("ble dur","Traitements g�n�raux"),c("ble tendre","Traitements g�n�raux"), c("canne a s","Traitements g�n�raux"), 
                   c("colza","Traitements g�n�raux"), c("mais ens","Traitements g�n�raux"), c("mais gr","Traitements g�n�raux"), 
                   c("orge","Traitements g�n�raux"),  c("pois","Traitements g�n�raux"), c("pomme de t","Traitements g�n�raux"), 
                   c("tournesol","Traitements g�n�raux"), c("triticale","Traitements g�n�raux"), 
                   
                   c("ble tendre","C�r�ales"), c("orge","C�r�ales"), c("triticale","C�r�ales"),
                   
                   c("ble tendre","C�r�ales � paille"), c("orge","C�r�ales � paille"), c("triticale","C�r�ales � paille"),
                   
                   c("ble dur","Jach�res et cultures interm�diaires"),c("pois","Jach�res et cultures interm�diaires"),
                   c("pomme de t","Jach�res et cultures interm�diaires"), c("ble tendre","Jach�res et cultures interm�diaires"),
                   c("canne a s","Jach�res et cultures interm�diaires"), c("colza","Jach�res et cultures interm�diaires"),
                   c("mais ens","Jach�res et cultures interm�diaires"), c("mais gr","Jach�res et cultures interm�diaires"), 
                   c("orge","Jach�res et cultures interm�diaires"), c("tournesol","Jach�res et cultures interm�diaires"), 
                   c("triticale","Jach�res et cultures interm�diaires")
                   
)
colnames(newlines)<-c("ESPECE","culture")
indices <- c(4,11,20:30,7:9,8:10,10:20)
CorrespondanceCultureEphyPk<-insertRows(CorrespondanceCultureEphyPk, newlines, indices)
CorrespondanceCultureEphyPk<-CorrespondanceCultureEphyPk[-which(CorrespondanceCultureEphyPk$culture%in%"Adjuvants"),]

stop()
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
correspondanceE<-as.data.frame(correspondanceE)
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
