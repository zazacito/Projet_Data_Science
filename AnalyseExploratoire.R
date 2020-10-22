library(ggplot2)

#-------------------------------------------------------------------------------
#                                    CATALOGUE
#-------------------------------------------------------------------------------


catalogue <- read.csv("../DATA/data_nettoyes/CatalogueNettoye.csv", header = TRUE,
                      sep = ",", dec = ".")

catalogue$marque <- as.factor(catalogue$marque)
catalogue$nom <- as.factor(catalogue$nom)
catalogue$longueur <- as.factor(catalogue$longueur)
catalogue$couleur <- as.factor(catalogue$couleur)
catalogue$occasion <- as.logical(catalogue$occasion)

str(catalogue)

#-------------------------------------------------------------------------------
#                                  IMMATRICULATION
#-------------------------------------------------------------------------------


immatriculations<- read.csv("../DATA/data_nettoyes/ImmatriculationsNettoyes.csv", header = TRUE,
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

marketing <- read.csv("../DATA/data_nettoyes/MarketingNettoye.csv", header = TRUE, sep = ",", dec = ".")

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


client <- read.csv("../DATA/data_nettoyes/ClientsNettoyes.csv", header = TRUE, sep = ",", dec = ".")

client$age <- as.integer(client$age)
client$taux <- as.integer(client$taux)
client$situationFamiliale <- as.factor(client$situationFamiliale)
client$nbEnfantsAcharge <- as.integer(client$nbEnfantsAcharge)
client$X2eme.voiture <- as.logical(client$X2eme.voiture)
client$sexe <- as.factor(client$sexe)

str(client)
