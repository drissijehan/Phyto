library(readr)
library(readxl)
library(stringr)
load("dpt.rda")

bnvd_2013 <- read_delim("carto_init/bnvdAcheteur/BNVD_INERIS_20180313_PRESSIONS_COMMUNE_PRODUIT_DETAILLE_2013_1267_1389_20171229V5.csv", ";", escape_double = FALSE, trim_ws = TRUE)
bnvd_2014<- read_delim("carto_init/bnvdAcheteur/BNVD_INERIS_20180313_PRESSIONS_COMMUNE_PRODUIT_DETAILLE_2014_1267_1389_20171229V5.csv",  ";", escape_double = FALSE, trim_ws = TRUE)
bnvd_2015 <- read_delim("carto_init/bnvdAcheteur/BNVD_INERIS_20180313_PRESSIONS_COMMUNE_PRODUIT_DETAILLE_2015_1267_1389_20171229V5.csv", ";", escape_double = FALSE, trim_ws = TRUE)
bnvd_2016 <- read_delim("carto_init/bnvdAcheteur/BNVD_INERIS_20180313_PRESSIONS_COMMUNE_PRODUIT_DETAILLE_2016_1267_1389_20171229V5.csv", ";", escape_double = FALSE, trim_ws = TRUE)
bnvd_2017 <- read_delim("carto_init/bnvdAcheteur/BNVD_INERIS_20180313_PRESSIONS_COMMUNE_PRODUIT_DETAILLE_2017_1267_1389_20171229V5.csv", ";", escape_double = FALSE, trim_ws = TRUE)

BNVD<- rbind(bnvd_2013,bnvd_2014,bnvd_2015,bnvd_2016,bnvd_2017)


Annee_BNVD<- c(rep("2013",nrow(bnvd_2013)),rep("2014", nrow(bnvd_2014)), rep("2015", nrow(bnvd_2015)),rep("2016", nrow(bnvd_2016)), rep("2017", nrow(bnvd_2017)))

BNVD <- cbind(BNVD, Annee_BNVD)

## Passer du code postal à depart à reg
vect<- BNVD$`Code postal acheteur`
remplacer<- function(vect){str_sub(vect,1,-4)}
v<-lapply(vect, remplacer)
BNVD<- cbind(BNVD, CODE_DEPT= unlist(v))

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
BNVD=iconv.data.frame(BNVD)
BNVD<- merge(BNVD, dpt, by= "CODE_DEPT")
save(BNVD,file ="BNVD.rda")

BNVD_2014<-BNVD[BNVD$Annee_BNVD == "2014",]
save(BNVD_2014,file ="BNVD_2014.rda")
