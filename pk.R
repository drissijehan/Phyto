library(readr)
library(readxl)
library(stringr)
library(testthat)
options(stringsAsFactors = FALSE)
load("AGRESTE_2014.rda")
#pk_2014 <- read_delim("carto_init/enquetePhyto/dose2014.csv",    ";", escape_double = FALSE, trim_ws = TRUE)
#pk_2011 Remy
#pk<- rbind(pk_2011, pk_2014)

dataFolder <- "~/data"
pk <- read_delim(file.path(dataFolder,"carto_init/enquetePhyto/dose2014.csv"),
                 ";", escape_double = FALSE, trim_ws = TRUE)

load(file.path(dataFolder,"Rpackages/r-package-frenchlandscape/frenchLandscape/data/frenchDepartements.rda"))

# DONNEES DEPARTEMENT
dpt <- subset(frenchDepartements@data,select = c(CODE_DEPT, CODE_REG, NOM_REG ))
v<-data.frame(CODE_DEPT="00",CODE_REG="00",NOM_REG="FR")
dpt<-rbind(dpt, v)
dpt$NOM_REG<- str_replace_all(dpt$NOM_REG,"-"," ")

# elimination des lignes pour lesquelles la taille d'échantillons est trop restreinte (<3)
pk<- na.omit(pk)

# elimination guadeloupe/a

# INTEGRATION CODE REGION DANS PK
pk$REG<- toupper(pk$REG)
names(pk)[match("REG",names(pk))]<-"NOM_REG"
nRowInit <- nrow(pk)
pk<-merge(pk,unique(dpt[,c("CODE_REG","NOM_REG")]),all.x=TRUE,by="NOM_REG")
expect_equal(nRowInit,nrow(pk))
# note: GUADELOUPE : "01" et REUNION "04" ont un CODE_REG NA
pk$CODE_REG <- as.character(pk$CODE_REG)
pk$CODE_REG[which(pk$NOM_REG=="GUADELOUPE")] <- "01"
pk$CODE_REG[which(pk$NOM_REG=="LA REUNION")] <- "04"
expect_true(all(apply(table(pk$CODE_REG,pk$NOM_REG),2,function(x){length(which(x!=0))})==1))
expect_equal(length(which(is.na(pk$CODE_REG))),0)

pk$REGPAR <- NULL

# INTEGRATION SURFACE DANS PK
expect_true(!any(duplicated(AGRESTE_2014[,c("CODE_REG","ESPECE")])))
nRowInit <- nrow(pk)
pk <- merge(pk,AGRESTE_2014,by=c("ESPECE","CODE_REG"))
expect_equal(nrow(pk),nRowInit)

#Quantite pk
pk$mean<-as.numeric(gsub(",",".",pk$mean))
pk$freq<-as.numeric(gsub(",",".",pk$freq))
expect_equal(length(which(is.na(pk$mean))),0)
expect_equal(length(which(is.na(pk$freq))),0)

pk$quantite_pk <- with(pk,mean*freq*Area)

save(pk,file ="pk.rda")

