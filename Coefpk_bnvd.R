dataFolder ="~/data" #jehan
load(file=file.path(dataFolder,"donnees_R/fichiersOdr/CoefPK_cp.rda"))
load(file=file.path(dataFolder,"donnees_R/bnvdAcheteur/BNVD_2014.rda"))
load(file=file.path(dataFolder,"donnees_R/fichiersOdr/info_ilot.rda"))
load(file=file.path(dataFolder,"donnees_R/fichiersOdr/ilot_gp_culture.rda"))
library(readr)
laposte_hexasmal <- read_delim("data/carto_init/codePostal/laposte_hexasmal.csv", ";", escape_double = FALSE, trim_ws = TRUE)

CoefPK_cp<-rapply(CoefPK_cp , f=function(x) ifelse(is.nan(x),0,x), how="replace" )
CoefPK_cp<-subset(CoefPK_cp, CoefPK_cp$surface !=0)

#Chercher la Quantite produit par CP  en se basant sur CoefPk_cp
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
names(BNVD_2014)[5]<-"Quantite.produit"
library(Hmisc)
library(magrittr)
library(dplyr)
BNVD_2014<- BNVD_2014[BNVD_2014$CODE_REG%nin%"00",]
BNVD_2014<- BNVD_2014[,-c(8:11)]
BNVD_2014<-BNVD_2014[-which(duplicated(BNVD_2014)),]
BNVD_CoefPK_CP <- BNVD_2014 %>%
  select(Code.postal.acheteur,CODE_DEPT,CODE_REG,AMM,Quantite.produit) %>%
  left_join(CoefPK_cp, by=c("AMM"="PHYTOPROD", "Code.postal.acheteur"="Code_postal","CODE_DEPT", "CODE_REG"))
BNVD_CoefPK_CP<-rapply(BNVD_CoefPK_CP , f=function(x) ifelse(is.na(x),0,x), how="replace" )
BNVD_CoefPK_CP<-subset(BNVD_CoefPK_CP, BNVD_CoefPK_CP$surface !=0)

Cp_com<- laposte_hexasmal %>%
  select(Code_postal, Code_commune_INSEE)

BNVD_CoefPK_CP <- BNVD_CoefPK_CP %>%
  left_join(Cp_com, by= c("Code.postal.acheteur"="Code_postal"))
BNVD_CoefPK_CP<-BNVD_CoefPK_CP[-which(duplicated(BNVD_CoefPK_CP)),]

#Chercher ilot par cp
ilot_com <- info_ilot %>%
  select(ilot, com_siege, surf_tot_ilot)
ilot_occSol<- ilot_gp_culture %>%
  select(ilot, occ_sol)
ilot_com_occSol<- ilot_com %>% 
  left_join(ilot_occSol, by= "ilot")
ilot_cp <- ilot_com_occSol %>%
  left_join(Cp_com, by= c("com_siege"="Code_commune_INSEE"))

ilot_cp<-as.data.frame(ilot_cp)
ilot_cp<- ilot_cp[-which(duplicated(ilot_cp)),]

#Fonction en donnant ilot (amm), retourne qte par cp x espece x amm

GetData<- function(data1, data2, i, amm=NULL,...){
  
  cs<-  data1 %>%
    filter(ilot%in%i) %>% 
    select(com_siege, surf_tot_ilot, occ_sol)
  if(!is.null(amm)) {
    d<- data2 %>%
      filter(Code_commune_INSEE %in% cs$com_siege & AMM %in% amm & occ_sol %in% cs$occ_sol) %>%
      select(Code_commune_INSEE,CODE_DEPT,CODE_REG,Quantite.produit,AMM,ESPECE,occ_sol,Coef)
  }
  else{
    d<- data2 %>%
      filter(Code_commune_INSEE %in% cs$com_siege & occ_sol %in% cs$occ_sol) %>%
      select(Code_commune_INSEE,Code.postal.acheteur,CODE_DEPT,CODE_REG,AMM,ESPECE,occ_sol,Quantite.produit,Coef)
  }
  d<- d%>%
    left_join(cs, by=c("Code_commune_INSEE"="com_siege", "occ_sol")) %>%
    mutate(Quantite.ilot = Quantite.produit * surf_tot_ilot * Coef)
  
  return(d)
}
GetData(ilot_cp,BNVD_CoefPK_CP, c("rpg-001-39463"))
GetData(ilot_cp,BNVD_CoefPK_CP, c("rpg-077-5313877"))
#save(test,file="~/Downloads/test.rda")
GetData(ilot_cp,BNVD_CoefPK_CP, c("rpg-077-5313859"), c(2090171, 2030342))
