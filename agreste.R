library("DataManagement")

dataFolder <- "~/data"
folderOut <- file.path(dataFolder,"donnees_R","Agreste")
AGRESTE<- read_excel(file.path(dataFolder,"carto_init/Agreste/Agreste2000_2016.xlsx"), skip = 5)
AGRESTE<-AGRESTE[,-match(c("Th�me","Sous-th�me"),names(AGRESTE))]

correspondanceAgrestePk <- read_excel(file.path(dataFolder,"carto_init/Agreste/correspondanceAgrestePk.xlsx"))

region<-data.frame(str_split_fixed(AGRESTE$G�ographie," - ", 2))
names(region)<- c("CODE_REG", "NOM_REG")
AGRESTE<-cbind(AGRESTE, region)
AGRESTE<-AGRESTE[,-match("G�ographie",names(AGRESTE))]
AGRESTE <- ChangeNameCol(AGRESTE,"Produit","ESPECE")

initNRow <- nrow(AGRESTE)
AGRESTE <- merge(AGRESTE,correspondanceAgrestePk,by.x="ESPECE",by.y="Agreste")
expect_equal(nrow(AGRESTE),initNRow)
AGRESTE$ESPECE <- NULL
AGRESTE <- ChangeNameCol(AGRESTE,"PK","ESPECE")

colAgg <- as.character(2000:2016)
nColInit <- ncol(AGRESTE)
AGRESTE <- aggregate(AGRESTE[,colAgg],by=list(CODE_REG=AGRESTE$CODE_REG,ESPECE=AGRESTE$ESPECE,NOM_REG=AGRESTE$NOM_REG),sum,na.rm=TRUE)
expect_equal(ncol(AGRESTE),nColInit) # au cas o� on ajoute des ann�es, il faudrat penser � �largir

save(AGRESTE,file =file.path(dataFolder,folderOut))


#Agreste de l'annee 2014
AGRESTE_2014<-subset(AGRESTE, select = c("ESPECE", "CODE_REG","2014"))
AGRESTE_2014 <- ChangeNameCol(AGRESTE_2014,"2014","Area") 

fr<- aggregate(Area~ESPECE, data = AGRESTE_2014, sum)
fr<- cbind(fr, CODE_REG=rep("00", nrow(fr)))

AGRESTE_2014 <- rbind(AGRESTE_2014, fr[,c("CODE_REG","ESPECE","Area")])

save(AGRESTE_2014,file =file.path(dataFolder,folderOut))
