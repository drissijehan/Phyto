library(readr)
library(readxl)
library(stringr)

load("AGRESTE_2014.rda")
#pk_2014 <- read_delim("carto_init/enquetePhyto/dose2014.csv",    ";", escape_double = FALSE, trim_ws = TRUE)
#pk_2011 Remy
#pk<- rbind(pk_2011, pk_2014)

pk <- read_delim("carto_init/enquetePhyto/dose2014.csv",    ";", escape_double = FALSE, trim_ws = TRUE)

load("Rpackages/r-package-frenchlandscape/frenchLandscape/data/frenchDepartements.rda")

# DONNEES DEPARTEMENT
dpt <- subset(frenchDepartements@data,select = c(CODE_DEPT, CODE_REG, NOM_REG ))
v=data.frame(CODE_DEPT="00",CODE_REG="00",NOM_REG="FR")
dpt<-rbind(dpt, v)
dpt$NOM_REG<- str_replace_all(dpt$NOM_REG,"-"," ")

# INTEGRATION CODE REGION DANS PK
pk<- na.omit(pk)
pk$REG<- toupper(pk$REG)
names(pk)[16]<-"NOM_REG"
pk<-merge(pk,unique(dpt[,2:3]))
pk$REGPAR <- NULL

# INTEGRATION SURFACE DANS PK
pk <- merge(pk,AGRESTE_2014)

#Quantite pk
pk$mean<-as.numeric(gsub(",",".",pk$mean))
pk$freq<-as.numeric(gsub(",",".",pk$freq))
pk$quantite_pk <- with(pk,mean*freq*Area)


save(pk,file ="pk.rda")
save(dpt,file ="dpt.rda")
