load("EPHY.rda")
## IFT PK
EPHY_Reduit<- subset(EPHY, EPHY[,15]== c("L/ha","g/ha","kg/ha"))
EPHY_Reduit$Dose.d.application.retenue <-gsub(",", ".", EPHY_Reduit$Dose.d.application.retenue)
EPHY_Reduit$Dose.d.application.retenue<- as.numeric(as.character( EPHY_Reduit$Dose.d.application.retenue))
## Dose Mediane
Dose_median<- aggregate(Dose.d.application.retenue~AMM, data= EPHY_Reduit, median)

Data_IFT_PK<- merge(pk, Dose_median, by.x = "PHYTOPROD", by.y = "AMM")

#######################################
Data_IFT_PK<-merge(unique(Data_IFT_PK),unique(EPHY_Reduit[,c(2,15)]), by.x= "PHYTOPROD", by.y= "AMM")

######################################

Data_IFT_PK$DoseMoy <- Data_IFT_PK$mean * Data_IFT_PK$freq
Data_IFT_PK$IFT<- Data_IFT_PK$DoseMoy / Data_IFT_PK$Dose.d.application.retenue


##########################################

Data_IFT_PK<-merge(unique(Data_IFT_PK), unique(EPHY[,c(1,2,3)]),by.x= "PHYTOPROD", by.y="AMM")

save(Data_IFT_PK,file ="C:/Users/Utilisateur/Documents/Data_IFT_PK.rda")
######################################
IFT_PK<- aggregate(IFT~ESPECE+Fonction+NOM_REG, Data_IFT_PK, sum)

save(IFT_PK,file ="C:/Users/Utilisateur/Documents/IFT_PK.rda")
