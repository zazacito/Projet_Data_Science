install.packages("funModeling")
library(funModeling)


#-------------------------------------------------------------------------------
#CATALOGUE
#-------------------------------------------------------------------------------

catalogue <- read.csv("DATA/Catalogue.csv", header = TRUE, sep = ",", dec = ".")

catalogue$marque <- as.factor(catalogue$marque)
catalogue$nom <- as.factor(catalogue$nom)
catalogue$longueur <- as.factor(catalogue$longueur)
catalogue$couleur <- as.factor(catalogue$couleur)
catalogue$occasion <- as.logical(catalogue$occasion)


str(catalogue)
summary(catalogue)

#visualiser les valeurs nulles et NA
df_status(catalogue)

#-------------------------------------------------------------------------------
#IMMATRICULATION
#-------------------------------------------------------------------------------


immatriculations<- read.csv("DATA/Immatriculations.csv", header = TRUE, sep = ",", dec = ".")

immatriculations$marque <-  as.factor(immatriculations$marque)
immatriculations$nom  <- as.factor(immatriculations$nom)
immatriculations$longueur <-  as.factor(immatriculations$longueur)
immatriculations$couleur <-  as.factor(immatriculations$couleur)
immatriculations$nbPlaces <-  as.factor(immatriculations$nbPlaces)
immatriculations$nbPortes <-  as.factor(immatriculations$nbPortes)
immatriculations$occasion <-  as.logical(immatriculations$occasion)

#lecture du data_frame
str(immatriculations)
summary(immatriculations)

#visualiser les valeurs nulles et NA
df_status(immatriculations)


#-------------------------------------------------------------------------------
#MARKETING
#-------------------------------------------------------------------------------

marketing <- read.csv("DATA/Marketing.csv", header = TRUE, sep = ",", dec = ".")

marketing$sexe <- as.factor(marketing$sexe)
marketing$situationFamiliale <- as.factor(marketing$situationFamiliale)
marketing$X2eme.voiture <- as.logical(marketing$X2eme.voiture)

#lecture du data_frame
str(marketing)
summary(marketing)

#visualiser les valeurs nulles et NA
df_status(marketing)



#-------------------------------------------------------------------------------
#CLIENT
#-------------------------------------------------------------------------------


client <- read.csv("DATA/Clients_8.csv", header = TRUE, sep = ",", dec = ".")

client$age <- as.integer(client$age)
client$sexe <- as.factor(client$sexe)
client$taux <- as.integer(client$taux)
client$situationFamiliale <- as.factor(client$situationFamiliale)
client$nbEnfantsAcharge <- as.integer(client$nbEnfantsAcharge)
client$X2eme.voiture <- as.logical(client$X2eme.voiture)


#lecture du data_frame
str(client)
summary(client)


#visualiser les valeurs nulles et NA
df_status(client)

#Suppression des NA
client_Sans_NA <- na.omit(client)
summary(client_Sans_NA)

library(ggplot2)


#boite à moustache 
boxplot(client$age, data=client)

#valeurs aberrantes 
boxplot.stats(client_Sans_NA$age)
