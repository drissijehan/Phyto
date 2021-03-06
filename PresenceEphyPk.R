<<<<<<< HEAD
#source("dataSource.R")
library(magrittr)
=======


options(stringsAsFactors=FALSE)
source("dataSource.R")
>>>>>>> e16e8ad27b5476d887dfc429f62e17097572dcc8
load(file.path(dataFolder,"donnees_R","PK","PK.rda"))
##########################Correspondance Culture EHPHY et PK##############
load(file.path(dataFolder,"donnees_R","EPHY","intituleCulture.rda"))
colnames(intituleCulture) <- gsub(" ","_",colnames(intituleCulture))

s<-c()
ss<-intituleCulture %>%
  split(.$culture) %>%
  lapply(function(d){
             d=data.frame(d);
             if(d$betterave == 1){
                 s<-"betterave"
             } else if (d$ble_dur==1){
                 s<-"ble dur"
             } else if (d$ble_tendre==1){s<-"ble tendre"} 
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
CorrespondanceCultureEphyPk<-CorrespondanceCultureEphyPk[-which(CorrespondanceCultureEphyPk$cultureEphy%in%"Adjuvants"),]

# vérif CorrespondanceCultureEphyPk
## translate intituleCulture in one line per culture and 1 in other columns
AddCultureLine <- function(nom){
    iNewLine <- which(intituleCulture[,nom]==1)
    out <- data.frame(culturePk=nom,cultureEphy=intituleCulture[iNewLine,"culture"])
    return(out)
}
intituleCulture$culture <- as.character(intituleCulture$culture)
culturePkEphy <- data.frame(data.table::rbindlist(lapply(names(intituleCulture)[-1],AddCultureLine)))

# check
expect_equal(sum(intituleCulture[,-1]),nrow(culturePkEphy))

# clean up the values
culturePkEphy$culturePk <- gsub("_"," ",culturePkEphy$culturePk)

# compararison with CorrespondanceCultureEphyPk
# reorder to make it order to compare
culturePkEphy <- culturePkEphy[order(culturePkEphy$culturePk,culturePkEphy$cultureEphy),]
CorrespondanceCultureEphyPk <- CorrespondanceCultureEphyPk[order(CorrespondanceCultureEphyPk$ESPECE,CorrespondanceCultureEphyPk$culture),]

# exhaustive check
set1 <- paste0(CorrespondanceCultureEphyPk$ESPECE,CorrespondanceCultureEphyPk$culture)
set2 <- paste0(culturePkEphy$culturePk,culturePkEphy$cultureEphy)
setdiff(set2,set1)
#=> Manque "adjuvants" pour toutes les cultures dans CorrespondanceCultureEphyPk
setdiff(set1,set2)
#=> CorrespondanceCultureEphyPk a en plus ajouté comme ESPECE, cultureEphy quand il n'y avait pas d'équivalent PK
#   Facile à ajouter
toAdd <- setdiff(EPHY$ESPECE,culturePkEphy$cultureEphy)
toAdd <- data.frame(toAdd,toAdd)
names(toAdd) <- names(culturePkEphy)
culturePkEphy <- rbind(culturePkEphy,toAdd)
#=> discuter avez Rémy lequel est le plus pertinent

stop()

saveAs(CorrespondanceCultureEphyPk,"CorrespondanceCultureEphyPk",file.path(dataFolder,"donnees_R","EPHY"))

##############################################################################

############### Table Correspondace EPHY PK #############################
load(file.path(dataFolder,"donnees_R","EPHY","EPHY.rda"))
EPHY<- merge(EPHY,CorrespondanceCultureEphyPk, by.x="ESPECE",by.y="cultureEphy")
stop()

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
