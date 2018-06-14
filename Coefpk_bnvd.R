load(file = "~/data/donnees_R/fichiersOdr/CoefPK_cp.rda")
load(file = "~/data/donnees_R/bnvdAcheteur/BNVD_2014.rda")
load(file= "~/data/donnees_R/fichiersOdr/info_ilot.rda")
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
names(BNVD_2014)[c(5,9,11)]<- c("Quantit_produit", "NCas", "Quantite_substance")
library(Hmisc)
BNVD_2014<- BNVD_2014[BNVD_2014$CODE_REG%nin%"00",]
BNVD_CoefPK_CP <- BNVD_2014 %>%
  select(Code.postal.acheteur,CODE_DEPT,CODE_REG,AMM,Quantit_produit) %>%
  left_join(CoefPK_cp, by=c("AMM"="PHYTOPROD", "Code.postal.acheteur"="Code_postal","CODE_DEPT", "CODE_REG"))
BNVD_CoefPK_CP$Qte_Cp<- BNVD_CoefPK_CP$Quantit_produit*BNVD_CoefPK_CP$Coef

#Chercher ilot par cp
ilot_com <- info_ilot %>%
  select(ilot, com_siege)
Cp_com<- laposte_hexasmal %>%
  select(Code_postal, Code_commune_INSEE)
ilot_cp <- ilot_com %>%
  left_join(Cp_com, by= c("com_siege"="Code_commune_INSEE"))

#Fonction en donnant ilot retourne qte par cp x espece x amm
d<- function(i){
  ilot_cp %>%
    filter(ilot=="i") %>% 
    select(Code_postal) -> cp
  return(cp)
    }
