source("~/data/carto_init/Phyto/Coefpk_bnvd.R")
library(readr)
areas_bourville2014 <- read_csv("data/carto_init/fichiersOdr/areas_bourville2014.csv")
BNVD_CoefPK_reduit<- BNVD_CoefPK_CP %>%
  select(Code_commune_INSEE, Code.postal.acheteur,CODE_DEPT,CODE_REG,AMM,ESPECE,occ_sol,Quantite.produit,Coef) 
#les quantites dans areas_bourville sont calculées par commune => on va aggreger les surfaces des ilot par commune
ilot_commune<- ilot_cp %>%
  group_by(com_siege) %>%
  summarise(surf_com = sum(surf_tot_ilot))
BNVD_CoefPK_reduit<- ilot_commune %>%
  left_join(BNVD_CoefPK_reduit, by= c("com_siege"="Code_commune_INSEE")) %>%
  mutate(Quantite_com = Quantite.produit * Coef * surf_com)

com_bourville<- levels(as.factor(areas_bourville2014$code_insee_communes_resp))
BNVD_CoefPK_CP_bourville <- subset(BNVD_CoefPK_reduit, BNVD_CoefPK_reduit$com_siege %in% com_bourville)
BNVD_CoefPK_CP_bourville <- na.omit(BNVD_CoefPK_CP_bourville)

#Les quantités dans areas_bourville ne correspondent pas la dose*surface_totale (10000 qui manque (ha->m²))
areas_bourville2014<- areas_bourville2014 %>%
  mutate(Quantite = dose * surface_totale)

#il y a des differents dates dans areas_bourvilles pour chaque produit x espece x commune => aggregation ..
areas_bourville <- aggregate(Quantite~code_insee_communes_resp+no_amm+libelle_occup, areas_bourville2014, sum)
areas_bourville$code_insee_communes_resp<- as.factor(areas_bourville$code_insee_communes_resp)

library(readxl)
correspondanceCultureBourvillePk <- read_excel("data/carto_init/fichiersOdr/correspondanceCultureBourvillePk.xlsx")
areas_bourville<- merge(areas_bourville, correspondanceCultureBourvillePk, by= "libelle_occup")

x<- BNVD_CoefPK_CP_bourville %>%
    select(com_siege, AMM, ESPECE, occ_sol, Coef, Quantite_com) %>%
    left_join(areas_bourville, by= c("com_siege"="code_insee_communes_resp", "AMM"="no_amm", "ESPECE"))
y<-na.omit(x)
#Graphs
library(plotly)
QuantiteBourville<- z$Quantite
QuantitePk <- z$Quantite_com
plot_ly(z , x = ~QuantiteBourville , y = ~QuantitePk) %>%
  add_markers(text = ~paste( 'Commune:', com_siege), color= ~ESPECE , colors = "Set1")

##############################################################################
