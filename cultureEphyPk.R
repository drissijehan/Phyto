rm(list=ls())

# inport donnees ephy et pk
dataFolder <- "~/data"
load(file.path(dataFolder,"donnees_R/EPHY/EPHY.rda"))
load(file.path(dataFolder,"donnees_R/PK/pk2014.rda"))

# creation d'une table avec chaque intitule culture
intituleCulture <- data.frame(culture=unique(EPHY$intituleCulture))
intituleCulturePk <- table(pk$ESPECE)
for(i in 1:length(intituleCulturePk)){
  intituleCulture[,1+i] <- 0
  colnames(intituleCulture)[1+i] <- names(intituleCulturePk)[i]
}

# renseignement correspondance intitules
intituleCulture[21,3:4] <- 1
intituleCulture[22,9] <- 1
intituleCulture[28,6] <- 1
intituleCulture[29,10] <- 1
intituleCulture[34,8] <- 1
intituleCulture[35,7:8] <- 1
intituleCulture[37,2:13] <- 1
intituleCulture[39,12] <- 1
intituleCulture[40,2] <- 1
intituleCulture[42,11] <- 1
intituleCulture[44,c(3,4,9,13)] <- 1
intituleCulture[64,10] <- 1
intituleCulture[67,c(3,4,9,13)] <- 1
intituleCulture[72,5] <- 1
intituleCulture[81,2:13] <- 1
intituleCulture[99,8] <- 1
intituleCulture[101,2:13] <- 1
intituleCulture[116,6] <- 1
intituleCulture[117,11] <- 1

# enregistrement donnees
save(intituleCulture,file =file.path(dataFolder,"donnees_R","EPHY","intituleCulture.rda"))
