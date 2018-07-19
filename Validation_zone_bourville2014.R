dataFolder<-"~/data"
load(file=file.path(dataFolder,"donnees_R/fichiersOdr/CoefPK_cp.rda"))
CoefPK_cp<-na.omit(CoefPK_cp)
load(file=file.path(dataFolder,"donnees_R/EPHY/EPHY.rda"))
load(file=file.path(dataFolder,"donnees_R/bnvdAcheteur/BNVD_2014.rda"))
iconv.data.frame<-function(df,...){ 
  df.names<-iconv(names(df),...) 
  df.rownames<-iconv(rownames(df),...) 
  names(df)<-df.names 
  rownames(df)<-df.rownames 
  df.list<-lapply(df,function(x){ 
    if(class(x)=="factor"){x<-factor(iconv(as.character(x),...))}else 
      if(class(x)=="character"){x<-iconv(x,...)}else{x} 
  }) 
  df.new<-do.call("data.frame",df.list) 
  return(df.new) 
} 
BNVD_2014 <- iconv.data.frame(BNVD_2014)
names(BNVD_2014)[5]<-"Quantite.produit"
library(readr)
areas_bourville2014 <- read_csv(file.path(dataFolder,"carto_init/fichiersOdr/areas_bourville2014.csv"))
laposte_hexasmal <- read_delim("~/data/carto_init/codePostal/laposte_hexasmal.csv", 
                               ";", escape_double = FALSE, trim_ws = TRUE)
############################################################################
###########################################################################
#Ajouter CP à Areas_bourville
areas_bourville<-merge(areas_bourville2014,laposte_hexasmal[,c("Code_commune_INSEE", "Code_postal")], by.x="code_insee_communes_resp", by.y = "Code_commune_INSEE")
areas_bourville<-aggregate(cbind(s_bv,dose,qte_produit)~no_amm+libelle_occup+Code_postal, data = areas_bourville, sum)
areas_bourville$QuantiteBv<-areas_bourville$dose*areas_bourville$s_bv*10^(-4)
###Code Postal commune
#areas_bourville<-merge(areas_bourville,unique(laposte_hexasmal[,c("Code_commune_INSEE","Code_postal")]), by.x = "code_insee_communes_resp", by.y = "Code_commune_INSEE")
#Ajouter correspondance Espece
library(readxl)
correspondanceCultureBourvillePk <- read_excel(file.path(dataFolder,"carto_init/fichiersOdr/correspondanceCultureBourvillePk.xlsx"))
areas_bourville<- merge(areas_bourville, correspondanceCultureBourvillePk, by= "libelle_occup")
#Ajouter Coef cp
areas_bourville<-merge(unique(areas_bourville),unique(CoefPK_cp[,c("Code_postal","CODE_REG","ESPECE","PHYTOPROD","CoefPk", "CoefDH")]), by.x = c("Code_postal","ESPECE","no_amm"),by.y = c("Code_postal","ESPECE","PHYTOPROD"))
#Ajouter quantite BNVD
areas_bourville<-merge(areas_bourville,unique(BNVD_2014[,c("Code.postal.acheteur","CODE_REG","AMM","Quantite.produit")]), by.x = c("Code_postal","no_amm","CODE_REG"), by.y = c("Code.postal.acheteur","AMM","CODE_REG"))
areas_bourville$QuantitePk<-areas_bourville$CoefPk*areas_bourville$s_bv*areas_bourville$Quantite.produit*10^(-4)
areas_bourville<-areas_bourville[areas_bourville$s_bv!=0,]
areas_bourville<-areas_bourville[areas_bourville$CoefPk!=0,]
#Ajouter les noms des produits et categories
areas_bourville<-merge(areas_bourville, unique(EPHY[,c("AMM","Nom.du.produit", "Fonction")]), by.x = "no_amm", by.y = "AMM", all.x = TRUE)

areas_bourville$rapport_bv_pk<-areas_bourville$qte_produit/areas_bourville$QuantitePk

#avec la quantiteBv= dose*surface
library(plotly)
areas_bourville %>%
  plot_ly(x = ~QuantiteBv) %>% 
  add_markers(y = ~QuantitePk, text = ~paste( 'Produit:', Nom.du.produit)) %>% 
  add_lines(x = ~QuantiteBv, y = ~QuantiteBv)
#avec la quantiteBv= qte_produit (dans areas bourville)
areas_bourville %>%
  plot_ly(x = ~qte_produit) %>% 
  add_markers(y = ~QuantitePk, color=~ESPECE, colors="Set1", text = ~paste( 'Produit:', Nom.du.produit, '<br>AMM:', no_amm, '<br>Code pstal::', Code_postal, '<br>Categorie:', Fonction)) %>% 
  add_lines(x = ~qte_produit, y = ~qte_produit)

plot_ly(areas_bourville[areas_bourville$rapport_bv_pk<15,], x=~rapport_bv_pk)

#La droite de regression en dehors des valeurs aberrantes
fit <- lm(QuantitePk ~ qte_produit, data = areas_bourville)
QuantiteBourville<-areas_bourville$qte_produit
p<-areas_bourville %>% 
  plot_ly(x = ~QuantiteBourville) %>% 
  add_markers(y = ~QuantitePk, color=~ESPECE, colors="Set1", text = ~paste( 'Produit:', Nom.du.produit, '<br>AMM:', no_amm, '<br>Code postal:', Code_postal, '<br>Categorie:', Fonction)) %>% 
  add_lines(x = ~QuantiteBourville, y = fitted(fit), name='Fitted') %>%
  add_lines(x = ~QuantiteBourville, y = ~QuantiteBourville, name='Bisectrice')


areas_bourville$QuantiteDH<- areas_bourville$CoefDH*areas_bourville$s_bv*areas_bourville$Quantite.produit*10^(-4)

fitdh <- lm(QuantiteDH ~ qte_produit, data = areas_bourville)
QuantiteBourville<-areas_bourville$qte_produit
p2<-areas_bourville %>% 
  plot_ly(x = ~QuantiteBourville) %>% 
  add_markers(y = ~QuantiteDH, color=~ESPECE, colors="Set1", text = ~paste( 'Produit:', Nom.du.produit, '<br>AMM:', no_amm, '<br>Commune:', code_insee_communes_resp, '<br>Categorie:', Fonction)) %>% 
  add_lines(x = ~QuantiteBourville, y = fitted(fitdh), name='Fitted') %>%
  add_lines(x = ~QuantiteBourville, y = ~QuantiteBourville, name='Bisectrice')


p3<-areas_bourville %>% 
  plot_ly(x = ~QuantiteDH) %>% 
  add_markers(y = ~QuantitePk, color=~ESPECE, colors="Set1", text = ~paste( 'Produit:', Nom.du.produit, '<br>AMM:', no_amm, '<br>Commune:', code_insee_communes_resp, '<br>Categorie:', Fonction)) %>% 
  
  add_lines(x = ~QuantiteDH, y = ~QuantiteDH, name='Bisectrice')

##########################################################################################################################

load(file=file.path(dataFolder,"donnees_R/PK/quanti.rda"))
#Les produits en commun pour rapport BNVD/PK et BV/PK
intersect(areas_bourville[areas_bourville$rapport_bv_pk>1.5,]$no_amm, quanti[quanti$rapport_bnvd_pk>1.5,]$PHYTOPROD)
# [1] 2000327 2000338 2000380 2010132 2070185 2090016 2090017 2090060 2100009 2110078 2110090 2110143 7700078 8100198 8100283 8200216 8400055
#[18] 8600243 8700462 8800488 9600103 9800100 9800210 9800336
intersect(areas_bourville[areas_bourville$rapport_bv_pk>1.5,]$Nom.du.produit, quanti[quanti$rapport_bnvd_pk>1.5,]$`Exemple de nom de produit`)
#[1] "SPOTLIGHT PLUS"   "ADERIO"           "FOLY R"           "PLENUM 50 WG"     "NIRVANA S"        "NANDO"            "PROTEUS"         
#[8] "HARMONY EXTRA SX" "DASKOR 440"       "AZOXYSTAR"        "SENCORAL SC"      "REGLONE 2"        "PENNCOZEB"        "TILT 250"        
#[15] "BAYTHROID"        "CHALLENGE 600"    "BANKO 500"        "ACROBAT M DG"     "TABLO 700"        "FASNET SC" 

