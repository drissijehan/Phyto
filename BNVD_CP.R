areas_bourville2014 <- read_csv("data/carto_init/fichiersOdr/areas_bourville2014.csv")
laposte_hexasmal <- read_delim("data/carto_init/codePostal/laposte_hexasmal.csv", ";", escape_double = FALSE, trim_ws = TRUE)
areas_cp<- merge(areas_bourville2014, laposte_hexasmal[,c("Code_commune_INSEE","Code_postal")], by.x="code_insee_communes_resp", by.y="Code_commune_INSEE")
areas_cp<-aggregate(qte_produit~Code_postal+no_amm, data = areas_cp, sum)


cp_bourville<- levels(as.factor(areas_cp$Code_postal))
bnvd <- subset(BNVD_2014, BNVD_2014$Code.postal.acheteur %in% cp_bourville)

areas_bnvd<- merge(areas_cp, bnvd[,c("Code.postal.acheteur","AMM","Quantite.produit")],by.x = c("Code_postal","no_amm"), by.y = c("Code.postal.acheteur", "AMM"))

areas_bnvd$QuantiteBourville <- areas_bnvd$qte_produit * 10000

plot_ly(areas_bnvd , x = ~QuantiteBourville , y = ~Quantite.produit) %>%
  add_markers(text = ~paste( 'CP:', Code_postal))
