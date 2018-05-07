library(readr)
library(readxl)
library(stringr)
library(Hmisc)

ToutPPP_2008 <- read_delim("carto_init/ephy/EPHY/ToutPPP_2008.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE, 
                           skip = 3)

ToutPPP_2009 <- read_delim("carto_init/ephy/EPHY/ToutPPP_2009.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE, 
                           skip = 3)

ToutPPP_2010 <- read_delim("carto_init/ephy/EPHY/ToutPPP_2010.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE, 
                           skip = 3)

ToutPPP_2011 <- read_delim("carto_init/ephy/EPHY/ToutPPP_2011.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE, 
                           skip = 3)

ToutPPP_2012 <- read_delim("carto_init/ephy/EPHY/ToutPPP_2012.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE, 
                           skip = 3)

ToutPPP_2013 <- read_delim("carto_init/ephy/EPHY/ToutPPP_2013.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE, 
                           skip = 3)

ToutPPP_2014 <- read_delim("carto_init/ephy/EPHY/ToutPPP_2014.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE, 
                           skip = 3)

ToutPPP_2015 <- read_delim("carto_init/ephy/EPHY/ToutPPP_2015.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE, 
                           skip = 3)

ToutPPP_2016 <- read_delim("carto_init/ephy/EPHY/ToutPPP_2016.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE, 
                           skip = 3)

ToutPPP_2017 <- read_delim("carto_init/ephy/EPHY/ToutPPP_2017.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE, 
                           skip = 3)
EPHY<- rbind(ToutPPP_2008, ToutPPP_2009, ToutPPP_2010, ToutPPP_2011, ToutPPP_2012, ToutPPP_2013, ToutPPP_2014, ToutPPP_2015, ToutPPP_2016, ToutPPP_2017)
colnames(EPHY) <- c("Nom du produit","AMM","Fonction","Etat du produit","Description de l'intrant Date de decision"
                    ,"date de retrait","Filiere","Gamme d'usages","Etat de l usage","Usages.Date de decision","Intitule","Condition d emploi"
                    ,"Methode d application","Dose d application retenue","Unite de la dose retenue","Type commercial"
                    ,"nom SA1","nom SA2","nom SA3","nom SA4")

Annee_EPHY<- c(rep("2008",nrow(ToutPPP_2008)),rep("2009", nrow(ToutPPP_2009)), rep("2010", nrow(ToutPPP_2010)),rep("2011", nrow(ToutPPP_2011)), rep("2012", nrow(ToutPPP_2012)), 
               rep("2013", nrow(ToutPPP_2013)), rep("2014", nrow(ToutPPP_2014)), rep("2015", nrow(ToutPPP_2015)), rep("2016", nrow(ToutPPP_2016)), rep("2017", nrow(ToutPPP_2017)))
EPHY <- cbind(EPHY, Annee_EPHY)

EPHY<-subset(EPHY, EPHY$Fonction %nin% c("0,014","0,15","0,33","1","2","4","0,07","0,2","0,2","0,5","1,5","3","5","0,1","0,25","0,625","1,6","300"))

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
EPHY=iconv.data.frame(EPHY)

EPHY$Dose.d.application.retenue <-gsub(",", ".", EPHY$Dose.d.application.retenue)
EPHY$Dose.d.application.retenue<- as.numeric(as.character( EPHY$Dose.d.application.retenue))
save(EPHY,file ="EPHY.rda")
