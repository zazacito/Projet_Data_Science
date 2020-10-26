
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk1.8.0_144")

library("rJava")
library(RJDBC)
library(ggplot2)
library(nnet)
library(kknn)
library(ROCR)
library(pROC)
library(randomForest)
library(C50)
library(e1071)
library(naivebayes)


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

#Analyse Exploratoire pour déterminer des catégories


qplot(LONGUEUR, PUISSANCE, data=catalogue, 
      main="Longueur de la voiture en fonction de la puissance",
      xlab="Longueur de la Voiture", ylab="Puissance (en Chevaux)",color=NBPLACES)+  geom_jitter(width = 0.4)

#Création de catégories de véhicules



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


immatriculations$categories <-  as.factor(immatriculations$categories)
str(immatriculations$categories)
summary(immatriculations$categories)


catalogue$categories <- ifelse(catalogue$LONGUEUR=="courte" & catalogue$PUISSANCE >=55 & catalogue$PUISSANCE <= 90 & catalogue$NBPLACES== 5,"citadine"  ,
                               ifelse(catalogue$LONGUEUR=="courte" & catalogue$PUISSANCE > 90 & catalogue$NBPLACES== 5,"citadine sportive" ,
                                      ifelse(catalogue$LONGUEUR=="moyenne" & catalogue$PUISSANCE >=55 & catalogue$PUISSANCE <= 100 & catalogue$NBPLACES== 5,"routiere",
                                             ifelse(catalogue$LONGUEUR=="moyenne" & catalogue$PUISSANCE >=100 & catalogue$PUISSANCE <= 140 & catalogue$NBPLACES== 5,"routiere sportive",
                                                    ifelse(catalogue$LONGUEUR=="moyenne" & catalogue$PUISSANCE > 140 & catalogue$NBPLACES== 5,"routiere ultra sportive" ,
                                                           ifelse( catalogue$LONGUEUR=="très longue" & catalogue$PUISSANCE >=55 & catalogue$PUISSANCE <= 150 & catalogue$NBPLACES== 5,"berline" ,
                                                                   ifelse( catalogue$LONGUEUR=="très longue" & catalogue$PUISSANCE >=150 & catalogue$PUISSANCE <= 250 & catalogue$NBPLACES== 5,"berline sportive" ,
                                                                           ifelse( catalogue$LONGUEUR=="très longue" & catalogue$PUISSANCE > 250 & catalogue$NBPLACES== 5,"berline ultra sportive" ,
                                                                                   ifelse(catalogue$LONGUEUR=="longue" & catalogue$PUISSANCE >=55 & catalogue$PUISSANCE <= 120   ,"familliale",
                                                                                          ifelse(catalogue$LONGUEUR=="longue"  & catalogue$PUISSANCE > 120 ,"familliale sportive","aucune"))))))))))

catalogue$categories <-  as.factor(catalogue$categories)
str(catalogue$categories)
summary(catalogue$categories)


#Suppression des doublons dans le fichier immatriculations

doublons <- which(duplicated(immatriculations$immatriculation))
immatriculations<-immatriculations[-doublons,]

#Suppression des doublons dans le fichier client

doublons1 <- which(duplicated(client$IMMATRICULATION))
client <-client[-doublons1,]


#fusion du fichiers client et Immatriculation

names(client)[7] = ("immatriculation")
clients_immatriculations <- merge(immatriculations, client , by ="immatriculation")

#Restructuration données

#Suppression de la colonne immatriculations
clients_immatriculations<-clients_immatriculations[,-1]

#ENSEMBLE D'APPRENTISSAGE
#clients_immatriculations_EA : sélection des 29014 premières lignes de clients_immatriculations.(70% de données)"
clients_immatriculations_EA <- clients_immatriculations[1:68773,]

#ENSEMBLE DE TEST
#☺clients_immatriculations_ET : sélection des  dernières lignes de clients_immatriculations.(30% de données)"
clients_immatriculations_ET <- clients_immatriculations[68773:98246,]

#ENSEMBLE D'APPRENTISSAGE
#Suppression de la colonne nbplaces

clients_immatriculations_EA <- subset(clients_immatriculations_EA, select = -nbPlaces)

clients_immatriculations_EA$marque <- as.factor(clients_immatriculations_EA$marque)
clients_immatriculations_EA$nom <- as.factor(clients_immatriculations_EA$nom)
clients_immatriculations_EA$puissance <- as.factor(clients_immatriculations_EA$puissance)
clients_immatriculations_EA$longueur <- as.factor(clients_immatriculations_EA$longueur)
clients_immatriculations_EA$nbPortes <- as.factor(clients_immatriculations_EA$nbPortes)
clients_immatriculations_EA$couleur <- as.factor(clients_immatriculations_EA$couleur)
clients_immatriculations_EA$occasion <- as.factor(clients_immatriculations_EA$occasion)
clients_immatriculations_EA$prix <- as.factor(clients_immatriculations_EA$prix)
clients_immatriculations_EA$categories <- as.factor(clients_immatriculations_EA$categories)
clients_immatriculations_EA$AGE <- as.factor(clients_immatriculations_EA$AGE)
clients_immatriculations_EA$SEXE <- as.factor(clients_immatriculations_EA$SEXE)
clients_immatriculations_EA$TAUX <- as.factor(clients_immatriculations_EA$TAUX)
clients_immatriculations_EA$SITUATIONFAMILIALE <- as.factor(clients_immatriculations_EA$SITUATIONFAMILIALE)
clients_immatriculations_EA$NBENFANTSACHARGE <- as.factor(clients_immatriculations_EA$NBENFANTSACHARGE)
clients_immatriculations_EA$DEUXIEMEVOITURE <- as.factor(clients_immatriculations_EA$DEUXIEMEVOITURE)

summary(clients_immatriculations_EA)

#ENSEMBLE DE TEST
#Suppression de la colonne nbplaces

clients_immatriculations_ET <- subset(clients_immatriculations_ET, select = -nbPlaces)

clients_immatriculations_ET$marque <- as.factor(clients_immatriculations_ET$marque)
clients_immatriculations_ET$nom <- as.factor(clients_immatriculations_ET$nom)
clients_immatriculations_ET$puissance <- as.factor(clients_immatriculations_ET$puissance)
clients_immatriculations_ET$longueur <- as.factor(clients_immatriculations_ET$longueur)
clients_immatriculations_ET$nbPortes <- as.factor(clients_immatriculations_ET$nbPortes)
clients_immatriculations_ET$couleur <- as.factor(clients_immatriculations_ET$couleur)
clients_immatriculations_ET$occasion <- as.factor(clients_immatriculations_ET$occasion)
clients_immatriculations_ET$prix <- as.factor(clients_immatriculations_ET$prix)
clients_immatriculations_ET$categories <- as.factor(clients_immatriculations_ET$categories)
clients_immatriculations_ET$AGE <- as.factor(clients_immatriculations_ET$AGE)
clients_immatriculations_ET$SEXE <- as.factor(clients_immatriculations_ET$SEXE)
clients_immatriculations_ET$TAUX <- as.factor(clients_immatriculations_ET$TAUX)
clients_immatriculations_ET$SITUATIONFAMILIALE <- as.factor(clients_immatriculations_ET$SITUATIONFAMILIALE)
clients_immatriculations_ET$NBENFANTSACHARGE <- as.factor(clients_immatriculations_ET$NBENFANTSACHARGE)
clients_immatriculations_ET$DEUXIEMEVOITURE <- as.factor(clients_immatriculations_ET$DEUXIEMEVOITURE)

summary(clients_immatriculations_ET)

#Suppression des colonnes ou la quantité de factor est trop éléve et des données inutiles
#Seules les catégories, et les données "humaines" sur les clients nous importent

clients_immatriculations_EA <- subset(clients_immatriculations_EA, select = -nbPortes)
clients_immatriculations_ET <- subset(clients_immatriculations_ET, select = -nbPortes)
clients_immatriculations_EA <- subset(clients_immatriculations_EA, select = -TAUX)
clients_immatriculations_ET <- subset(clients_immatriculations_ET, select = -TAUX)
clients_immatriculations_EA <- subset(clients_immatriculations_EA, select = -prix)
clients_immatriculations_ET <- subset(clients_immatriculations_ET, select = -prix)
clients_immatriculations_EA <- subset(clients_immatriculations_EA, select = -longueur)
clients_immatriculations_ET <- subset(clients_immatriculations_ET, select = -longueur)
clients_immatriculations_EA <- subset(clients_immatriculations_EA, select = -puissance)
clients_immatriculations_ET <- subset(clients_immatriculations_ET, select = -puissance)
clients_immatriculations_EA <- subset(clients_immatriculations_EA, select = -nom)
clients_immatriculations_ET <- subset(clients_immatriculations_ET, select = -nom)
clients_immatriculations_EA <- subset(clients_immatriculations_EA, select = -marque)
clients_immatriculations_ET <- subset(clients_immatriculations_ET, select = -marque)

#---------------------#
# K-NEAREST NEIGHBORS #
#---------------------#


kknn6<-kknn(categories~., clients_immatriculations_EA, clients_immatriculations_ET)

# Matrice de confusion
table(clients_immatriculations_ET$categories, kknn6$fitted.values)

# Conversion des probabilites en data frame
knn_prob <- as.data.frame(kknn6$prob)

knn_auc <-multiclass.roc(clients_immatriculations_ET$categories, knn_prob)
print(knn_auc)

#-----------------#
# NEURAL NETWORKS #
#-----------------#

nnet5<-nnet(categories ~., clients_immatriculations_EA, size=8)

# Test du classifieur : classe predite
nn_class <- predict(nnet5, clients_immatriculations_ET, type="class")
nn_class
table(nn_class)

# Matrice de confusion
table(clients_immatriculations_ET$categories, nn_class)

# Test du classifieur : probabilites pour chaque prediction
nn_prob <- predict(nnet5, clients_immatriculations_ET, type="raw")
nn_auc <-multiclass.roc(clients_immatriculations_ET$categories, nn_prob)
print(nn_auc)


#----------------#
# RANDOM FORESTS #
#----------------#

clients_immatriculations_EA_RF <- subset(clients_immatriculations_EA, select = -AGE)
# Apprentissage du classifeur de type random forest
rf <- randomForest(categories~., clients_immatriculations_EA_RF)

# Test du classifieur : classe predite
rf_class <- predict(rf,clients_immatriculations_ET, type="response")
table(rf_class)

# Matrice de confusion
table(clients_immatriculations_ET$categories, rf_class)

# Test du classifieur : probabilites pour chaque prediction
rf_prob <- predict(rf, clients_immatriculations_ET , type="prob")
rf_auc <-multiclass.roc(clients_immatriculations_ET$categories, rf_prob)
print(rf_auc)


#-------------#
# NAIVE BAYES #
#-------------#

# Apprentissage du classifeur de type naive bayes
nb <- naive_bayes(categories~., clients_immatriculations_EA)

# Test du classifieur : classe predite
nb_class <- predict(nb, clients_immatriculations_ET, type="class")
table(nb_class)

# Matrice de confusion
table( clients_immatriculations_ET$categories, nb_class)

# Test du classifieur : probabilites pour chaque prediction
nb_prob <- predict(nb,  clients_immatriculations_ET, type="prob")
nb_auc <- multiclass.roc(clients_immatriculations_ET$categories, nb_prob)
print(nb_auc)



#-------------------------#
# SUPPORT VECTOR MACHINES #
#-------------------------#

# Apprentissage du classifeur de type svm
svm <- svm(categories~., clients_immatriculations_EA, probability=TRUE)

# Test du classifieur : classe predite
svm_class <- predict(svm, clients_immatriculations_ET, type="response")
table(svm_class)

# Matrice de confusion
table(produit_QF_ET$Produit, svm_class)

# Test du classifieur : probabilites pour chaque prediction
svm_prob <- predict(svm, produit_QF_ET, probability=TRUE)

# L'objet genere est de type specifique aux svm
svm_prob

# Recuperation des probabilites associees aux predictions
svm_prob <- attr(svm_prob, "probabilities")

# Conversion en un data frame 
svm_prob <- as.data.frame(svm_prob)

# Courbe ROC sur le meme graphique
svm_pred <- prediction(svm_prob$Oui, produit_QF_ET$Produit)
svm_perf <- performance(svm_pred,"tpr","fpr")
plot(svm_perf, add = TRUE, col = "blue")

# Calcul de l'AUC
svm_auc <- performance(svm_pred, "auc")
attr(svm_auc, "y.values")




