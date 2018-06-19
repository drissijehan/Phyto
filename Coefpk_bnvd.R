dataFolder ="~/data" #jehan
load(file=file.path(dataFolder,"donnees_R/fichiersOdr/CoefPK_cp.rda"))
load(file=file.path(dataFolder,"donnees_R/bnvdAcheteur/BNVD_2014.rda"))
load(file=file.path(dataFolder,"donnees_R/fichiersOdr/info_ilot.rda"))
library(readr)
laposte_hexasmal <- read_delim("data/carto_init/codePostal/laposte_hexasmal.csv", ";", escape_double = FALSE, trim_ws = TRUE)

#Chercher la quantité produit par CP  en se basant sur CoefPk_cp
iconv.data.frame<-function(df,...){ 
  df.names<-iconv(names(df),...) 
  df.rownames<-iconv(rownames(df),...) 
  names(df)<-df.names 
  rownames(df)<-df.rownames 
  df.list<-lapply(df,function(x){ 
    if(class(x)=="factor"){x<-factor(iconv(as.character(x),...))}else 
      if(class(x)=="character"){x<-iconv(x,...)}else{x} 
  }) 
  df.new<-do.call("data.frame",df.list) 
  return(df.new) 
} 
BNVD_2014 <- iconv.data.frame(BNVD_2014)

library(Hmisc)
library(magrittr)
library(dplyr)
BNVD_2014<- BNVD_2014[BNVD_2014$CODE_REG%nin%"00",]
BNVD_2014<- BNVD_2014[,-c(8:11)]
BNVD_2014<-BNVD_2014[-which(duplicated(BNVD_2014)),]
BNVD_CoefPK_CP <- BNVD_2014 %>%
  select(Code.postal.acheteur,CODE_DEPT,CODE_REG,AMM,Quantité.produit) %>%
  left_join(CoefPK_cp, by=c("AMM"="PHYTOPROD", "Code.postal.acheteur"="Code_postal","CODE_DEPT", "CODE_REG"))


#Chercher ilot par cp
ilot_com <- info_ilot %>%
  select(ilot, com_siege, surf_tot_ilot)
Cp_com<- laposte_hexasmal %>%
  select(Code_postal, Code_commune_INSEE)
ilot_cp <- ilot_com %>%
  left_join(Cp_com, by= c("com_siege"="Code_commune_INSEE"))

ilot_cp<-as.data.frame(ilot_cp)
ilot_cp<- ilot_cp[-which(duplicated(ilot_cp)),]

#Fonction en donnant ilot (amm), retourne qte par cp x espece x amm

GetData=function(i,amm){
  cs=ilot_cp[ilot_cp$ilot%in%i,c("Code_postal","surf_tot_ilot")]
  if(missing(amm)) {
    dat<-subset(BNVD_CoefPK_CP, BNVD_CoefPK_CP$Code.postal.acheteur%in%cs$Code_postal, select = c("Code.postal.acheteur","CODE_DEPT",
                                                                                                  "CODE_REG","Quantité.produit","AMM","ESPECE","Coef"))
  }
  else{
    dat<-subset(BNVD_CoefPK_CP, BNVD_CoefPK_CP$Code.postal.acheteur%in%cs$Code_postal & BNVD_CoefPK_CP$AMM%in%amm, select = c("Code.postal.acheteur","CODE_DEPT",
                                                                                                     "CODE_REG","Quantité.produit","AMM","ESPECE","Coef"))
  }
  dat<-merge(unique(dat),cs, by.x="Code.postal.acheteur" , by.y="Code_postal")
  dat$Quantite_ilot<- dat$Coef * dat$Quantité.produit * dat$surf_tot_ilot
  return(dat)
}
GetData(c("CIMETIER00000000087279832","CIMETIER00000000087280001"),c(9800182,9800244))

