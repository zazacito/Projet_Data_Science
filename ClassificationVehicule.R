immatriculations<- read.csv("../DATA/Immatriculations.csv", header = TRUE,
                            sep = ",", dec = ".")

immatriculations$marque <-  as.factor(immatriculations$marque)
immatriculations$nom  <- as.factor(immatriculations$nom)
immatriculations$longueur <-  as.factor(immatriculations$longueur)
immatriculations$couleur <-  as.factor(immatriculations$couleur)
immatriculations$nbPortes <-  as.factor(immatriculations$nbPortes)
immatriculations$occasion <-  as.logical(immatriculations$occasion)

# Création des différentes classes de véhicule


immatriculations$categories <- ifelse(immatriculations$longueur=="courte" & puissance >=55 & puissance <= 90,"citadine",
                               ifelse(immatriculations$longueur=="courte" & puissance > 90,"citadine sportive",
      ifelse(immatriculations$longueur=="moyenne","compacte",
      ifelse(immatriculations$longueur=="longue"& immatriculations$nbPlaces== 5& immatriculations$puissance<180,"routière",
      ifelse(immatriculations$longueur=="longue"&immatriculations$nbPlaces== 7&immatriculations$puissance<180,"familiale",
      ifelse(immatriculations$longueur=="longue" | immatriculations$longueur=="très longue" & immatriculations$puissance >180 ,"sportive","rien"))))))


str(immatriculations)