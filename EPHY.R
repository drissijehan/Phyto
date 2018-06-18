library(readr)
library(readxl)
library(stringr)
library(Hmisc)
library(testthat)

source("dataSource.R") # doit contenir quelque chose du genre dataFolder <- "~/data"
nLignesProduitsExcel <- list("2008"=(28386-4),"2009"=(28744-4),"2010"=(28337-4),"2011"=(28337-4),"2012"=(29930-4),
                          "2013"=(31336-4),"2014"=(31483-4),"2015"=(31888-4),
                          "2016"=(34382-4),"2017"=(37111-4))
ImportEPHY <- function(year,nToSkip=4,nCol=20){
    cat("year:",year,";")
    filePath <- file.path(dataFolder,paste0("carto_init/ephy/EPHY/ToutPPP_",year,".csv"))
    testReadCSV <- read.csv(filePath,sep=";",stringsAsFactor=FALSE,header=FALSE)[-(1:nToSkip),]
    for(iCol in 1:ncol(testReadCSV)){
        temp <- iconv(testReadCSV[,iCol],from="CP1252",to="UTF-8")
        temp <- gsub("^ +","",temp)
        temp <- gsub(" +$","",temp)
        testReadCSV[,iCol] <- temp 
    }

    toRemove <- which(apply(testReadCSV,1,function(x){all(x=="")}))
    if(length(toRemove)>0){
        testReadCSV<-testReadCSV[-toRemove,]
    }

    expect_equal(ncol(testReadCSV),nCol)
    expect_equal(nrow(testReadCSV),nLignesProduitsExcel[[as.character(year)]])

    testReadCSV$Annee_EPHY <- year

    return(testReadCSV)
}

EPHY <- data.frame(data.table::rbindlist(lapply(names(nLignesProduitsExcel),ImportEPHY)))

colnames(EPHY)[1:20] <- c("Nom.du.produit","AMM","Fonction","Etat.du.produit","Description.de.l.intrant.Date.de.decision"
                    ,"date.de.retrait","Filiere","Gamme.d.usages","Etat.de.l.usage","Usages.Date.de.decision","Intitule","Condition.d.emploi"
                    ,"Methode.d.application","Dose.d.application.retenue","Unite.de.la.dose.retenue","Type.commercial"
                    ,"nom.SA1","nom.SA2","nom.SA3","nom.SA4")

EPHY$Dose.d.application.retenue <-gsub(",", ".", EPHY$Dose.d.application.retenue)
EPHY$Dose.d.application.retenue<- as.numeric(as.character( EPHY$Dose.d.application.retenue))

# extraction chaine de caractere designant la culture a partir de l'intitule de l'usage
intituleCulture <- sapply(strsplit(as.vector(EPHY$Intitule),"*",fixed = TRUE)
                          ,function(x) x[1])

expect_equal(nrow(EPHY),length(intituleCulture))

EPHY <- cbind(EPHY,ESPECE=intituleCulture)
EPHY$ESPECE <- as.character(EPHY$ESPECE)

save(EPHY,file =file.path(dataFolder,"donnees_R","EPHY","EPHY.rda"))

# for(iCol in 1:ncol(EPHY)){
#     if(class(EPHY[,iCol])=="factor"){
#         EPHY[,iCol] <- as.character(EPHY[,iCol])
#     }
# }
# 
# expect_equal(EPHYnew,EPHY)
# View(rbind(EPHY[28382:28385,],EPHYnew[28382:28385,]))
#=> Il y avait clairement des problèmes, ici des lignes sautées aléatoirement, à priori pas grave 
#   mais les lignes supplémentaires dans EPHY ne semblent pas être des lignes pertinentes

nomsEspecesEPHY <- unique(EPHY$ESPECE)

