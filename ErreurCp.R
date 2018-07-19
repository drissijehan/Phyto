library(sparklyr)
library(readr)

#Configuration Spark ######################################################
# sc<- spark_connect(master= "local")
conf <- spark_config()
# conf$`sparklyr.cores.local` <- 4
conf$`sparklyr.shell.driver-memory` <- "32G"
# conf$spark.memory.fraction <- 0.9
sc <- spark_connect(master = "local", 
                    version = "2.1.0",
                    config = conf)
###########################################################################

# test de sparklyr
library(dplyr)
#Test avec ptit db "iris"
iris_tbl <- sdf_copy_to(sc, iris)
src_tbls(sc)

#spark_write_table(iris_tbl,"iris_spark")
spark_read_parquet(sc,"iris_spark","/home/jehandrissi/spark-warehouse/iris_spark/")

dataFolder <- "/opt"
### INfo_Ilot
# info_ilot <- read_csv(file.path(dataFolder,"carto_init/fichiersOdr/info_ilot.csv"))
# save(info_ilot,file=file.path(dataFolder,"donnees_R/fichiersOdr/info_ilot.rda"))
load(file.path(dataFolder,"donnees_R/fichiersOdr/info_ilot.rda"))
gc()
info_ilot_tbl <- copy_to(sc, info_ilot)
spark_write_table(info_ilot_tbl,"info_ilot")
#spark_read_parquet(sc,"info_ilot_tbl","/home/jehandrissi/spark-warehouse/info_ilot/")
#print(info_ilot_tbl, width = Inf)
#glimpse(info_ilot_tbl)

###Code Postal
laposte_hexasmal <- read_delim("data/carto_init/codePostal/laposte_hexasmal.csv", 
                               ";", escape_double = FALSE, trim_ws = TRUE)
laposte_hexasmal_tbl <- sdf_copy_to(sc, laposte_hexasmal, overwrite = TRUE)
#glimpse(laposte_hexasmal_tbl)


### Aggreger les CP, communes, surfaces ################################
CP_Commune<- laposte_hexasmal_tbl %>%
  select(Code_commune_INSEE, Code_postal)

Commune_Surface<- info_ilot_tbl %>%
  select(ilot, surf_tot_ilot, com_siege)

Com_Cp_Surf_tbl<-CP_Commune %>% 
  inner_join(Commune_Surface, by=c("Code_commune_INSEE" = "com_siege"))

#-> surface par ilot avec info commune et cp

#glimpse(Com_Cp_Surf_tbl)
#sdf_dim(Com_Cp_Surf_tbl)

#### Reg -> Dep -> CP
DeptReg<- frenchLandscape::frenchDepartements@data[,c("CODE_REG","CODE_DEPT")]
DeptCP<- laposte_hexasmal[,"Code_postal"]
library(stringr)
remplacer<- function(vect){str_sub(vect,1,-4)}
CODE_DEPT<-unlist(lapply(DeptCP, remplacer),use.names=FALSE)
DeptCP <- cbind(DeptCP, CODE_DEPT)
DeptRegCP<- merge(DeptReg, DeptCP, by="CODE_DEPT")

#### Dose PK 
load(file="~/data/donnees_R/PK/pk2014.rda")
pk$DosePk <- pk$mean * pk$freq

#### Dose pk par CP

### INfo_Ilot
# info_ilot <- read_csv(file.path(dataFolder,"carto_init/fichiersOdr/info_ilot.csv"))
# save(info_ilot,file=file.path(dataFolder,"donnees_R/fichiersOdr/info_ilot.rda"))

DoseCP <- merge(pk[,c("PHYTOPROD","ESPECE","CODE_REG","DosePk")], unique(DeptRegCP), by= "CODE_REG")

library(readxl)
correspondanceCultureOccsol <- read_excel("data/carto_init/fichiersOdr/correspondanceCultureOccsol.xlsx")
DoseCP<- merge(DoseCP, correspondanceCultureOccsol, by="ESPECE", all.x = TRUE)
gc()
DoseCP_tbl <- copy_to(sc, DoseCP)
#spark_write_table(DoseCP_tbl,"dosecp")
#spark_read_parquet(sc,"DoseCp_tbl","/home/jehandrissi/spark-warehouse/dosecp/")
#-> DosePK(p,g,c) (Dose (d'après PK à l'échelle régionale) par produit, culture et code postal

### Ilot_gp_tbl 
#ilot_gp_culture <- read_csv("data/carto_init/fichiersOdr/ilot_gp_culture")
#save(ilot_gp_culture,file=file.path(dataFolder,"donnees_R/fichiersOdr/ilot_gp_culture.rda"))
load(file="~/data/donnees_R/fichiersOdr/ilot_gp_culture.rda")
#-> surface par ilot 

ilot_gp_culture_tbl <- copy_to(sc, ilot_gp_culture, overwrite = TRUE)
spark_write_table(ilot_gp_culture_tbl,"ilot_gp_culture")
#spark_read_parquet(sc,"ilot_gp_culture_tbl","/home/jehandrissi/spark-warehouse/ilot_gp_culture/")

#-> DosePK(p,g,c) (Dose (d'après PK à l'échelle régionale) par produit, culture et code postal
#-> surface par ilot 

ilot_occ_sol<- ilot_gp_culture_tbl %>%
  select(ilot, occ_sol)
surf_cp_occsol_tbl <- Com_Cp_Surf_tbl %>%
  inner_join(ilot_occ_sol, by="ilot")

#-> surface et occ_sol par ilot, avec information sur code postal (retire identifiant de l'ilot)

## on ajoute Espece a occ_sol
correspondanceCultureOccsol_tbl <- copy_to(sc,correspondanceCultureOccsol)
surf_cp_occsol_tbl <- surf_cp_occsol_tbl %>%
  left_join(correspondanceCultureOccsol_tbl, by="occ_sol")
#-> surface par ilot, avec code postal et culture PK

#Coef PK
#Erreurs CP
# BasePK <- DoseCP_tbl %>%K
#   group_by(PHYTOPROD, ESPECE, Code_postal,CODE_DEPT,CODE_REG)
# BasePKS_tbl<- BasePK %>% 
#   summarise(dose= sum(DosePk))
# #-> inutile

surf_espece_tbl <- surf_cp_occsol_tbl %>%
  group_by(Code_postal, ESPECE) %>%
  summarise(surface= sum(surf_tot_ilot))
#-> surface(g,c) (=surface par code postal et par culture)

BasePKS_tbl <- DoseCP_tbl %>%
  inner_join(surf_espece_tbl, by=c("ESPECE","Code_postal"))
#-> DosePK(p,g,c), avec les surfaces par code_postal et par culture
BasePKS_tbl<- BasePKS_tbl %>%
  mutate(DoseSurf= DosePk*surface)

SommeCulture_tbl <- BasePKS_tbl %>%
  group_by(PHYTOPROD, Code_postal,CODE_DEPT,CODE_REG) %>%
  summarise(doseSumSurf = sum(DoseSurf))
#-> sum_c (DosePK(p,g,c)*surface(g,c))

BasePKS_tbl <- BasePKS_tbl %>%
  left_join(SommeCulture_tbl, by=c("PHYTOPROD","Code_postal","CODE_DEPT","CODE_REG"))
#-> DosePK(p,g,c) avec sum_c(DosePK(p,g,c)*surface(g,c)) correspondante

BasePKS_tbl <- BasePKS_tbl %>%
  mutate(CoefPk = (DoseSurf /doseSumSurf))
#-> coefficients finaux !!!a thousand kisses deep

spark_write_table(BasePKS_tbl,"coefpk_amm_cp")
coefpk_amm_cp<-spark_read_parquet(sc,"coefpk_cp","/home/jehandrissi/spark-warehouse/coefpk_amm_cp/")
coefpk_amm_cp<-as.data.frame(coefpk_amm_cp)
save(coefpk_amm_cp,file=file.path(dataFolder,"donnees_R/fichiersOdr/coefpk_amm_cp.rda"))

##CoefDH (produit, culture)
load("~/data/donnees_R/EPHY/EPHY.rda")
CorrespondanceCultureEphyPk <- read_excel("data/carto_init/ephy/EPHY/CorrespondanceCultureEphyPk.xlsx")

library(spatDataManagement)
#culture <- sapply(strsplit(as.vector(EPHY$Intitule),"*",fixed = TRUE), function(x) x[1])
#EPHY <- cbind(EPHY,culture)
#EPHY<- merge(EPHY,CorrespondanceCultureEphyPk, by="culture")
EPHY<- merge(EPHY,CorrespondanceCultureEphyPk, by.x="intituleCulture",by.y="culture",all.x=TRUE)
DHCulture<- aggregate(Dose.d.application.retenue~AMM+ESPECE, data= EPHY, median)
DHCulture<- ChangeNameCol(DHCulture,"Dose.d.application.retenue","DH")

load(file.path("~/data/donnees_R","Agreste","AGRESTE_2014.rda"))
AGRESTE_2014<-AGRESTE_2014[AGRESTE_2014$CODE_REG!="00",]
DHCulture<-merge(unique(DHCulture),unique(AGRESTE_2014), by="ESPECE")
DHCulture$DHSurf<-DHCulture$DH*DHCulture$Area

SommeDHCulture<-aggregate(DHSurf~AMM+CODE_REG,data = DHCulture,sum, na.rm=TRUE)
SommeDHCulture<- ChangeNameCol(SommeDHCulture, "DHSurf","SumDHSurf")
SommeDHCulture<-SommeDHCulture[SommeDHCulture$AMM!="",]
BaseDH<-merge(DHCulture, SommeDHCulture, by=c("AMM","CODE_REG"))
BaseDH$CoefDH<-BaseDH$DHSurf/BaseDH$SumDHSurf
BaseDH$DHSurf<-NULL
BaseDH$SumDHSurf<-NULL
BaseDH_tbl <- copy_to(sc, BaseDH)

#coefpk/coefdh
BaseS_tbl <- BasePKS_tbl %>%
  left_join(BaseDH_tbl, by=c("PHYTOPROD" = "AMM", "ESPECE","CODE_REG")) %>%
  mutate(Coef= CoefPk/CoefDH)

#BaseS <- collect(BaseS_tbl)
spark_write_table(BaseS_tbl,"coefpk_cp")
#spark_write_csv(BaseS_tbl, "/home/jehandrissi/spark-warehouse/CoefPK_CP/")
#db_drop_table(sc, "___") --> remove data from spark
CoefPK_cp<-spark_read_parquet(sc,"coefpk_cp","/home/jehandrissi/spark-warehouse/coefpk_cp/")
print(CoefPK_cp, n=50, width = Inf)

CoefPK_cp %>% 
  dbplot_histogram(Coef)

CoefPK_cp<-as.data.frame(CoefPK_cp)


#=> les valeurs NaN dans les coef duent à: coef DH est NA ou surface est NA ( des codes postaux qui ne sont pas presents dans info_ilot)

save(CoefPK_cp,file=file.path(dataFolder,"donnees_R/fichiersOdr/CoefPK_cp.rda"))

MaxCoefCP<- aggregate(Coef~PHYTOPROD+Code_postal+CODE_DEPT+CODE_REG, data = CoefPK_cp, max)
save(MaxCoefCP,file=file.path(dataFolder,"donnees_R/fichiersOdr/MaxCoefCP.rda"))
##Hist
plot_ly(MaxCoefCP, x = ~ Coef, type = "histogram", text = ~paste("AMM:", AMM, "<br>Region" , CODE_REG,"<br>Departement" , CODE_DEPT,"<br>Code Postal" , Code.postal.acheteur), 
        name= "Erreurs Code Postal") %>%
  layout(yaxis = list(type = "log"))
