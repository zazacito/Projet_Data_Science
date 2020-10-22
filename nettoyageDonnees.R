install.packages("funModeling")
library(funModeling)
library(ggplot2)


#-------------------------------------------------------------------------------
#                                    CATALOGUE
#-------------------------------------------------------------------------------

catalogue <- read.csv("../DATA/data_initial/Catalogue.csv", header = TRUE,
                      sep = ",", dec = ".")

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
#                                  IMMATRICULATION
#-------------------------------------------------------------------------------


immatriculations<- read.csv("../DATA/data_initial/Immatriculations.csv", header = TRUE,
                            sep = ",", dec = ".")

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
#                                 MARKETING
#-------------------------------------------------------------------------------

marketing <- read.csv("../DATA/data_initial/Marketing.csv", header = TRUE, sep = ",", dec = ".")

marketing$sexe <- as.factor(marketing$sexe)
marketing$situationFamiliale <- as.factor(marketing$situationFamiliale)
marketing$X2eme.voiture <- as.logical(marketing$X2eme.voiture)

#lecture du data_frame
str(marketing)
summary(marketing)

#visualiser les valeurs nulles et NA
df_status(marketing)



#-------------------------------------------------------------------------------
#                                     CLIENT
#-------------------------------------------------------------------------------


client <- read.csv("../DATA/data_initial/Clients_8.csv", header = TRUE, sep = ",", dec = ".")

client$age <- as.integer(client$age)
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


#Changement des noms sexe (généralisations)
client_Sans_NA$sexe<- ifelse(client_Sans_NA$sexe == "Féminin","F",client_Sans_NA$sexe)
client_Sans_NA$sexe<- ifelse(client_Sans_NA$sexe == "Femme","F",client_Sans_NA$sexe)
client_Sans_NA$sexe<- ifelse(client_Sans_NA$sexe == "Masculin","M",client_Sans_NA$sexe)
client_Sans_NA$sexe<- ifelse(client_Sans_NA$sexe == "Homme","M",client_Sans_NA$sexe)
client_Sans_NA$sexe<- ifelse(client_Sans_NA$sexe == " ","N/D",client_Sans_NA$sexe)
client_Sans_NA$sexe<- ifelse(client_Sans_NA$sexe == "?", "N/D",client_Sans_NA$sexe)

client_Sans_NA$sexe<- as.factor(client_Sans_NA$sexe)
summary(client_Sans_NA$sexe)
qplot(sexe, data=client_Sans_NA, fill=X2eme.voiture)

#boite à moustache 
boxplot(client$age, data=client, main="Boite à Moustache pour l'âge d'un client
        sans Nettoyage des Données")



#valeurs aberrantes 
boxplot.stats(client_Sans_NA$age)

#Suppression des valeur aberrantes, âge négatifs jusqu'à majorité
outliers <- c(-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)
outliers
outlier_idx <-which(client_Sans_NA$age %in% outliers)
outlier_idx

client_Sans_NA[outlier_idx,]

clients_nettoyes <- client_Sans_NA[-outlier_idx,]

#boite à moustache client nettoyés
boxplot(clients_nettoyes$age, data=client, main="Boite à Moustache pour 
  l'âge d'un client avec Nettoyage des Données")


# Ecriture des dataframe nettoyes dans des nouveaux fichiers csv

write.table(clients_nettoyes,"../DATA/data_nettoyes/ClientsNettoyes.csv",
            sep = ",", dec =".", row.names = F)

write.table(immatriculations,"../DATA/data_nettoyes/ImmatriculationsNettoyes.csv",
            sep = ",", dec =".", row.names = F)

write.table(marketing,"../DATA/data_nettoyes/MarketingNettoye.csv",
            sep = ",", dec =".", row.names = F)

write.table(catalogue,"../DATA/data_nettoyes/CatalogueNettoye.csv",
            sep = ",", dec =".", row.names = F)
