#Carte Rapport bnvd pk
library(stringr)
library(sp)
load("BNVD_2014.rda")

#Calculer la quantite utilisée de chaque produit dans chaque region
bnvd_reg<- aggregate(BNVD_2014[,5]~AMM+CODE_REG, data = BNVD_2014, sum)
names(bnvd_reg)[3]<-"quantite_bnvd_reg"

#calculer la quantite pk par produit et par reg
pk_reg<-aggregate(quantite_pk~PHYTOPROD+CODE_REG , data = pk, sum)
names(pk_reg)[1]<-"AMM"

#rapport entre quantite issue de bnvd et celle de pk 
bnvd_reg <- merge(unique(bnvd_reg),unique(pk_reg), by=c("AMM","CODE_REG"),all=T)

bnvd_reg$rapport_bnvd_reg_pk<- bnvd_reg$quantite_bnvd_reg / bnvd_reg$quantite_pk

#median du rapport et la carte
carte<-aggregate(rapport_bnvd_reg_pk~CODE_REG, data = bnvd_reg[bnvd_reg$CODE_REG!="00",], median)
carte<- merge(carte, dpt[,1:2], by= "CODE_REG")

carte<-merge(frenchDepartements, carte, by="CODE_DEPT", duplicateGeoms = FALSE)

save(carte,file ="carte.rda")

#Carte Equivalent Dose Pleine
#CHERCHER EDP
EDP<- merge(bnvd_reg,Dose_median, by="AMM")
EDP$edp <-round( (EDP$quantite_bnvd_reg / EDP$Dose.d.application.retenue)/10)

EDP_Red<- subset(EDP, EDP$rapport_bnvd_reg_pk!="NA")
EDP_Red<- subset(EDP_Red, EDP_Red$edp!=0)

x<-as.vector(rep(EDP_Red$rapport_bnvd_reg_pk, EDP_Red$edp))
reg<-as.vector(rep(EDP_Red$CODE_REG, EDP_Red$edp))
carte2<-cbind.data.frame(x,reg)

carte2<-aggregate(x~reg, data = carte2, median)
carte2<- subset(carte2, reg!= "00")
carte2<- merge(carte2, dpt[,1:2], by.x="reg" , by.y="CODE_REG")

carte2<-merge(frenchDepartements, carte2, by="CODE_DEPT", duplicateGeoms = FALSE)

spplot(carte2,13)

save(carte2,file ="carte2.rda")

