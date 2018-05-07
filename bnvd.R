library(readr)
library(testthat)
library(readxl)
library(stringr)
library("DataManagement")
library("data.table")

NumToNChar <- function(num,nChar=2){
    fillIn <- as.character(10^nChar)
    num <- paste0(fillIn, num)
    out <- gsub(paste0(".*(.{",nChar,"})$"), "\\1", num)
    return(out)
}

dataFolder <- "/media/5AE1EC8814E5040E" # Corentin
folderIn <- file.path(dataFolder,"carto_init","bnvdAcheteur")
folderOut <- file.path(dataFolder,"donnees_R","BNVD")

bnvd <- list()
for(year in 2013:2017){
    fileName <-paste0("BNVD_INERIS_20180313_PRESSIONS_COMMUNE_PRODUIT_DETAILLE_",year,"_1267_1389_20171229V5.csv")
    bName <- as.character(year)
    bnvd[[bName]] <- read.csv(file.path(folderIn,fileName),
                          fileEncoding="CP1252",sep=";",skip = 2)
    bnvd[[bName]]$Annee_BNVD <- year
}
BNVD <- data.frame(data.table::rbindlist(bnvd))

expect_equal(nrow(BNVD),sum(unlist(lapply(bnvd,nrow))))

## Passer du code postal à departement et région
vect<- BNVD$Code.postal.acheteur
remplacer<- function(vect){str_sub(vect,1,-4)}
v<-NumToNChar(unlist(lapply(vect, remplacer),use.names=FALSE),2)
# il faudrait ajouter la distinction entre corse du nord et corse du sud 
# pour avoir les véritables CODE_DEPT
# pour cela on peut utiliser 
# Documents/recherche/INRA/cartographieGenerale/administrative/communes/2017-09-05_laposte_hexasmal.csv
# en les géolocalisant dans corse du nord/corse du sud

BNVD2 <- cbind(BNVD, CODE_DEPT= v)
expect_equal(nrow(BNVD2),nrow(BNVD))

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

BNVD4 <- iconv.data.frame(BNVD2)
expect_equal(dim(BNVD4),dim(BNVD2))

## retirer le all.x quand on aura réglé le problème corse
BNVD5<- merge(BNVD4, frenchDepartements[,c("CODE_REG","CODE_DEPT","NOM_REG")], by= "CODE_DEPT",all.x=TRUE)
expect_equal(nrow(BNVD5),nrow(BNVD4)) # diff 2 == problème avec la corse

saveAs(BNVD5,"BNVD",folderOut)

BNVD_2014<-BNVD5[BNVD5$Annee_BNVD == "2014",]
expect_equal(nrow(BNVD_2014),nrow(bnvd[["2014"]]))

saveAs(BNVD_2014,"BNVD_2014",folderOut)

