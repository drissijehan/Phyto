AGRESTE<- read_excel("carto_init/Agreste/Agreste2000_2016.xlsx", skip = 5)
AGRESTE<-AGRESTE[,-c(1:2)]

correspondanceAgrestePk <- read_excel("carto_init/Agreste/correspondanceAgrestePk.xlsx")

region<-str_split_fixed(AGRESTE$Géographie," - ", 2)
AGRESTE<-cbind(AGRESTE, region)
AGRESTE<-AGRESTE[,-c(2,5)]
names(AGRESTE)[c(1,18,19)]<- c("ESPECE","CODE_REG", "NOM_REG")


AGRESTE <- merge(AGRESTE,correspondanceAgrestePk,by.x="ESPECE",by.y="Agreste")
AGRESTE$ESPECE <- NULL
names(AGRESTE)[19]<- c("ESPECE")

save(AGRESTE,file ="AGRESTE.rda")

#Agreste de l'annee 2014
AGRESTE_2014<-subset(AGRESTE, select = c("ESPECE", "CODE_REG","2014"))
names(AGRESTE_2014)[3]<- c("Area")


fr<- aggregate(Area~ESPECE, data = AGRESTE_2014, sum)
fr<- cbind(fr, CODE_REG=rep("00", nrow(fr)))

AGRESTE_2014 <- rbind(AGRESTE_2014, fr[,c(3,1,2)])

save(AGRESTE_2014,file ="AGRESTE_2014.rda")
