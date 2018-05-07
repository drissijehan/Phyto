load("pk.rda")
#Quantite par produit et par culture
pk_prod_cult<-aggregate(quantite_pk~ESPECE+PHYTOPROD, data = pk, sum)
names(pk_prod_cult)[3]<-"qte_prod_cult"
#Quantite par produit
pk_prod<-aggregate(quantite_pk~PHYTOPROD, data = pk, sum)
names(pk_prod)[2]<-"qte_prod"
#Induce de Shannon
Shannon<- merge(pk_prod_cult, pk_prod, by= "PHYTOPROD")
Shannon$Specificite<- Shannon$qte_prod_cult / Shannon$qte_prod

Shannon$var<-(Shannon$Specificite * log2(Shannon$Specificite)) * (-1)
Indice_Shannon<-aggregate(var~ESPECE, data = Shannon, sum)
names(Indice_Shannon)[2]<-"IS"

save(Indice_Shannon,file ="Indice_Shannon.rda")
