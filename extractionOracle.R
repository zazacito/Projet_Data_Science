install.packages("rJava")
install.packages("RJDBC")
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk1.8.0_144")

library("rJava")
library(RJDBC)

##classPath : add path to drivers jdbc

drv <- RJDBC::JDBC(driverClass = "oracle.jdbc.OracleDriver", classPath =  
                     Sys.glob("C:/Users/j.chambord/Documents/GitHub/Projet_Data_Science/drivers/*"))


#Connexion OK
conn <- dbConnect(drv, "jdbc:oracle:thin:@(DESCRIPTION=(ADDRESS=(PROTOCOL=TCP)
                  (HOST=144.21.67.201)(PORT=1521))(CONNECT_DATA=
                  (SERVICE_NAME=pdbest21.631174089.oraclecloud.internal)))",
                  "CHAMBORD2B20", "CHAMBORD2B2001")


#Enregistrement de la table Marketing  dans la DB Oracle
marketing <- read.csv("../DATA/data_initial/Marketing.csv", header = TRUE, 
                      sep = ",", dec = ".")

names(marketing)[6] = ("DeuxiemeVoiture")

dbWriteTable(conn,"Marketing",marketing,   
             rownames=FALSE, overwrite = TRUE, append = FALSE)

#Enregistrement de la table Catalogue dans la DB Oracle
catalogue <- read.csv("../DATA/data_initial/Catalogue.csv", header = TRUE, 
                      sep = ",", dec = ".")


dbWriteTable(conn,"Catalogue",catalogue,   
             rownames=FALSE, overwrite = TRUE, append = FALSE)


#Enregistrement de la table Immatriculations dans la DB Oracle
Immatriculations <- read.csv("../DATA/data_initial/Immatriculations.csv", header = TRUE, 
                      sep = ",", dec = ".")
                      
dbWriteTable(conn, "Immatriculations",Immatriculations[0:1000000,])

dbAppendTable(conn,"Immatriculations",Immatriculations1)
dbWriteTable(conn,"Immatriculation",Immatriculations)


#Enregistrement de la table Client dans la DB Oracle
client <- read.csv("../DATA/data_initial/Clients_8.csv", header = TRUE, 
                      sep = ",", dec = ".")

names(client)[6] = ("DeuxiemeVoiture")


client$age <- as.integer(client$age)
client$taux <- as.integer(client$taux)
client$nbEnfantsAcharge <- as.integer(client$nbEnfantsAcharge)

dbWriteTable(conn,"Client",client,   
             rownames=FALSE, overwrite = TRUE, append = FALSE)



#Visualisation des tables
allTables <- dbGetQuery(conn, "SELECT owner, table_name FROM all_tables where 
                        owner = 'CHAMBORD2B20'")

marketingOracle <- dbGetQuery(conn, "select * from Marketing")
catalogueOracle <- dbGetQuery(conn, "select * from Catalogue")
immatriculationsOracle <- dbGetQuery(conn,"select * from Immatriculations")
clientOracle <- dbGetQuery(conn,"select * from Clients")

