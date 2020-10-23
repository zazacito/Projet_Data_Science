
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk1.8.0_144")

library("rJava")
library(RJDBC)

##classPath : add path to drivers jdbc

drv <- RJDBC::JDBC(driverClass = "oracle.jdbc.OracleDriver", classPath =  
                     Sys.glob("drivers/*"))


#Connexion OK
conn <- dbConnect(drv, "jdbc:oracle:thin:@(DESCRIPTION=(ADDRESS=(PROTOCOL=TCP)
                  (HOST=144.21.67.201)(PORT=1521))(CONNECT_DATA=
                  (SERVICE_NAME=pdbest21.631174089.oraclecloud.internal)))",
                  "AZALBERT2B20", "AZALBERT2B2001")

allTables <- dbGetQuery(conn, "SELECT owner, table_name FROM all_tables where 
                        owner = 'AZALBERT2B20'")

marketing <- dbGetQuery(conn, "select * from Marketing")
catalogue <- dbGetQuery(conn, "select * from Catalogue")
#immatriculationsOracle <- dbGetQuery(conn,"select * from Immatriculations")
client <- dbGetQuery(conn,"select * from Client")

#Impossible de sauvegarder la table immatriculations, car trop volumineuse
immatriculations <- read.csv("../DATA/data_initial/Immatriculations.csv", header = TRUE, 
                             sep = ",", dec = ".")

#Les données de la table client ont été nettoyées via sql
#Refactorisation des Données

client$SITUATIONFAMILIALE  <- as.factor(client$SITUATIONFAMILIALE )
client$DEUXIEMEVOITURE <- as.logical(client$DEUXIEMEVOITURE)
client$SEXE <- as.factor(client$SEXE)

marketing$SEXE <- as.factor(marketing$SEXE)
marketing$SITUATIONFAMILIALE <- as.factor(marketing$SITUATIONFAMILIALE)
marketing$DEUXIEMEVOITURE <- as.logical(marketing$DEUXIEMEVOITURE)

catalogue$MARQUE <- as.factor(catalogue$MARQUE)
catalogue$NOM <- as.factor(catalogue$NOM)
catalogue$LONGUEUR <- as.factor(catalogue$LONGUEUR)
catalogue$COULEUR <- as.factor(catalogue$COULEUR)
catalogue$OCCASION <- as.logical(catalogue$OCCASION)

immatriculations$marque <-  as.factor(immatriculations$marque)
immatriculations$nom  <- as.factor(immatriculations$nom)
immatriculations$longueur <-  as.factor(immatriculations$longueur)
immatriculations$couleur <-  as.factor(immatriculations$couleur)
immatriculations$occasion <-  as.logical(immatriculations$occasion)

summary(marketing)
summary(catalogue)
summary(immatriculations)
summary(client)
