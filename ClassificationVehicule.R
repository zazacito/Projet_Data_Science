immatriculations<- read.csv("../DATA/data_initial/Immatriculations.csv", header = TRUE,
                            sep = ",", dec = ".")

immatriculations$marque <-  as.factor(immatriculations$marque)
immatriculations$nom  <- as.factor(immatriculations$nom)
immatriculations$longueur <-  as.factor(immatriculations$longueur)
immatriculations$couleur <-  as.factor(immatriculations$couleur)
immatriculations$nbPortes <-  as.factor(immatriculations$nbPortes)
immatriculations$occasion <-  as.logical(immatriculations$occasion)

# Création des différentes classes de véhicule


immatriculations$categories <- ifelse(immatriculations$longueur=="courte" & immatriculations$puissance >=55 & immatriculations$puissance <= 90 & immatriculations$nbPlaces== 5,"citadine"  ,
                               ifelse(immatriculations$longueur=="courte" & immatriculations$puissance > 90 & immatriculations$nbPlaces== 5,"citadine sportive" ,
                               ifelse(immatriculations$longueur=="moyenne" & immatriculations$puissance >=55 & immatriculations$puissance <= 100 & immatriculations$nbPlaces== 5,"routiere",
                               ifelse(immatriculations$longueur=="moyenne" & immatriculations$puissance >=100 & immatriculations$puissance <= 140 & immatriculations$nbPlaces== 5,"routiere sportive",
                               ifelse(immatriculations$longueur=="moyenne" & immatriculations$puissance > 140 & immatriculations$nbPlaces== 5,"routiere ultra sportive" ,
                               ifelse( immatriculations$longueur=="très longue" & immatriculations$puissance >=55 & immatriculations$puissance <= 150 & immatriculations$nbPlaces== 5,"berline" ,
                               ifelse( immatriculations$longueur=="très longue" & immatriculations$puissance >=150 & immatriculations$puissance <= 250 & immatriculations$nbPlaces== 5,"berline sportive" ,
                               ifelse( immatriculations$longueur=="très longue" & immatriculations$puissance > 250 & immatriculations$nbPlaces== 5,"berline ultra sportive" ,
                               ifelse(immatriculations$longueur=="longue" & immatriculations$puissance >=55 & immatriculations$puissance <= 120  &  immatriculations$nbPlaces== 5,"familliale",
                               ifelse(immatriculations$longueur=="longue"  & immatriculations$puissance > 120 & immatriculations$nbPlaces== 5 ,"familliale sportive","aucune" ))))))))))
                                      
'ifelse(immatriculations$longueur=="moyenne","compacte",
 ifelse(immatriculations$longueur=="longue"& immatriculations$nbPlaces== 5& immatriculations$puissance<180,"routière",
  ifelse(immatriculations$longueur=="longue"&immatriculations$nbPlaces== 7&immatriculations$puissance<180,"familiale",
    ifelse(immatriculations$longueur=="longue" | immatriculations$longueur=="très longue" & immatriculations$puissance >180 ,"sportive","rien"))))))
'

immatriculations$categories <-  as.factor(immatriculations$categories)
str(immatriculations$categories)
summary(immatriculations$categories)
