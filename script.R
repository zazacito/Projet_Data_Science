
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
library(rpart)
library(C50)
library(tree)

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

#-----------------------Catégories Véhicules--------------------------#

#Analyse Exploratoire pour déterminer des catégories

catalogue$LONGUEUR <- factor(catalogue$LONGUEUR, c("courte", "moyenne", "longue","très longue"))

qplot(LONGUEUR, PUISSANCE, data=catalogue, 
      main="Longueur de la voiture en fonction de la puissance",
      xlab="Longueur de la Voiture", ylab="Puissance (en Chevaux)",color=NBPLACES)+  geom_jitter(width = 0.4) 



qplot(longueur, puissance, data=immatriculations[0:1000,], 
      main="Longueur de la voiture en fonction de la puissance",
      xlab="Longueur de la Voiture", ylab="Puissance (en Chevaux)",color=nbPlaces)+  geom_jitter(width = 0.4) 


  

#Création de catégories de véhicules


immatriculations$categories <- ifelse(immatriculations$longueur=="courte" & immatriculations$puissance >=55 & immatriculations$puissance <= 90 & immatriculations$nbPlaces== 5,"citadine"  ,
                                      ifelse(immatriculations$longueur=="courte" & immatriculations$puissance > 90 & immatriculations$nbPlaces== 5,"citadine sportive" ,
                                             ifelse(immatriculations$longueur=="moyenne" & immatriculations$puissance >=55 & immatriculations$puissance <= 100 & immatriculations$nbPlaces== 5,"routiere",
                                                    ifelse(immatriculations$longueur=="moyenne" & immatriculations$puissance >=100 & immatriculations$puissance <= 140 & immatriculations$nbPlaces== 5,"routiere sportive",
                                                           ifelse(immatriculations$longueur=="moyenne" & immatriculations$puissance > 140 & immatriculations$nbPlaces== 5,"routiere ultra sportive" ,
                                                                  ifelse( immatriculations$longueur=="très longue" & immatriculations$puissance >=55 & immatriculations$puissance <= 250 & immatriculations$nbPlaces== 5,"berline" ,
                                                                          ifelse( immatriculations$longueur=="très longue" & immatriculations$puissance >=250 & immatriculations$puissance <= 350 & immatriculations$nbPlaces== 5,"berline sportive" ,
                                                                                  ifelse( immatriculations$longueur=="très longue" & immatriculations$puissance > 350 & immatriculations$nbPlaces== 5,"berline ultra sportive" ,
                                                                                          ifelse(immatriculations$longueur=="longue" & immatriculations$puissance >=55 & immatriculations$puissance <= 140  &  immatriculations$nbPlaces== 5,"familliale",
                                                                                                 ifelse(immatriculations$longueur=="longue"  & immatriculations$puissance > 140 & immatriculations$nbPlaces== 5 ,"familliale sportive","aucune" ))))))))))


immatriculations$categories <-  as.factor(immatriculations$categories)
#str(immatriculations$categories)
summary(immatriculations$categories)


catalogue$categories <- ifelse(catalogue$LONGUEUR=="courte" & catalogue$PUISSANCE >=55 & catalogue$PUISSANCE <= 90 & catalogue$NBPLACES== 5,"citadine"  ,
                               ifelse(catalogue$LONGUEUR=="courte" & catalogue$PUISSANCE > 90 & catalogue$NBPLACES== 5,"citadine sportive" ,
                                      ifelse(catalogue$LONGUEUR=="moyenne" & catalogue$PUISSANCE >=55 & catalogue$PUISSANCE <= 100 & catalogue$NBPLACES== 5,"routiere",
                                             ifelse(catalogue$LONGUEUR=="moyenne" & catalogue$PUISSANCE >=100 & catalogue$PUISSANCE <= 140 & catalogue$NBPLACES== 5,"routiere sportive",
                                                    ifelse(catalogue$LONGUEUR=="moyenne" & catalogue$PUISSANCE > 140 & catalogue$NBPLACES== 5,"routiere ultra sportive" ,
                                                           ifelse( catalogue$LONGUEUR=="très longue" & catalogue$PUISSANCE >=55 & catalogue$PUISSANCE <= 250 & catalogue$NBPLACES== 5,"berline" ,
                                                                   ifelse( catalogue$LONGUEUR=="très longue" & catalogue$PUISSANCE >=250 & catalogue$PUISSANCE <= 350 & catalogue$NBPLACES== 5,"berline sportive" ,
                                                                           ifelse( catalogue$LONGUEUR=="très longue" & catalogue$PUISSANCE > 350 & catalogue$NBPLACES== 5,"berline ultra sportive" ,
                                                                                   ifelse(catalogue$LONGUEUR=="longue" & catalogue$PUISSANCE >=55 & catalogue$PUISSANCE <= 140   ,"familliale",
                                                                                          ifelse(catalogue$LONGUEUR=="longue"  & catalogue$PUISSANCE > 140 ,"familliale sportive","aucune"))))))))))

catalogue$categories <-  as.factor(catalogue$categories)
str(catalogue$categories)
summary(catalogue$categories)


#Histogrammes permettant de montrer les effectifs pour chacune des catégories
qplot(catalogue$categories, data=catalogue)
qplot(immatriculations$categories, data=immatriculations)

#-----------------------Nettoyage et fusion fichiers--------------------------#

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




#-----------------------Création ensemble Apprentissage et Test--------------------------#

#Suppression des colonnes
#Seules les catégories, et les données "humaines" sur les clients nous importent
clients_immatriculations<- subset(clients_immatriculations, select = -nbPlaces)
clients_immatriculations <- subset(clients_immatriculations, select = -nbPortes)
clients_immatriculations <- subset(clients_immatriculations, select = -prix)
clients_immatriculations <- subset(clients_immatriculations, select = -longueur)
clients_immatriculations <- subset(clients_immatriculations, select = -puissance)
clients_immatriculations <- subset(clients_immatriculations, select = -nom)
clients_immatriculations <- subset(clients_immatriculations, select = -marque)
clients_immatriculations <- subset(clients_immatriculations, select = -couleur)
clients_immatriculations <- subset(clients_immatriculations, select = -occasion)



#Factorisation des Colonnes
clients_immatriculations$categories <- as.factor(clients_immatriculations$categories)
clients_immatriculations$AGE <- as.factor(clients_immatriculations$AGE)
clients_immatriculations$SEXE <- as.factor(clients_immatriculations$SEXE)
clients_immatriculations$SITUATIONFAMILIALE <- as.factor(clients_immatriculations$SITUATIONFAMILIALE)
clients_immatriculations$NBENFANTSACHARGE <- as.factor(clients_immatriculations$NBENFANTSACHARGE)
clients_immatriculations$DEUXIEMEVOITURE <- as.factor(clients_immatriculations$DEUXIEMEVOITURE)

#Duplication du data set, pour ajout d'une colonne catégorie taux ultérieurement
clients_immatriculations_taux <- clients_immatriculations[0:98246,]
clients_immatriculations <- subset(clients_immatriculations, select = -TAUX)


#ENSEMBLE D'APPRENTISSAGE
#clients_immatriculations_EA : sélection des 29014 premières lignes de clients_immatriculations.(70% de données)"
clients_immatriculations_EA <- clients_immatriculations[1:68773,]

#ENSEMBLE DE TEST
#clients_immatriculations_ET : sélection des  dernières lignes de clients_immatriculations.(30% de données)"
clients_immatriculations_ET <- clients_immatriculations[68773:98246,]

summary(clients_immatriculations_EA)
summary(clients_immatriculations_ET)



#-----------------------Classifieurs--------------------------#

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

nnet5<-nnet(categories ~., clients_immatriculations_EA, size=6)

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

#-------------#
#      SVM    #
#-------------#

# Apprentissage du classifeur de type svm
svm <- svm(categories~., clients_immatriculations_EA, probability=TRUE)
svm

# Test du classifieur : classe predite
svm_class <- predict(svm, clients_immatriculations_ET, type="response")
svm_class
table(svm_class)

# Matrice de confusion
table(clients_immatriculations_ET$categories, svm_class)

# Test du classifieur : probabilites pour chaque prediction
svm_prob <- predict(svm, clients_immatriculations_ET, probability=TRUE)


# Recuperation des probabilites associees aux predictions
svm_prob <- attr(svm_prob, "probabilities")

# Conversion en un data frame 
svm_prob <- as.data.frame(svm_prob)

# Calcul de l'AUC
svm_auc <-multiclass.roc(clients_immatriculations_ET$categories, svm_prob)
print (svm_auc)





#-----------------------Catégories Taux--------------------------#

#boite a moustache du taux 

boxplot(clients_immatriculations_taux$TAUX, data=clients_immatriculations,
        main ="Taux"
        ,col=c("blue"))

summary(clients_immatriculations_taux$TAUX)

#Création de categorie de taux 


clients_immatriculations_taux$cateTaux<- ifelse(clients_immatriculations_taux$TAUX < 420 ,"Faible",
                                           ifelse(clients_immatriculations_taux$TAUX <607.1, "Moyen",
                                                  ifelse(clients_immatriculations_taux$TAUX <823, "Elevée", "Très élevée")))

clients_immatriculations_taux$cateTaux <- as.factor(clients_immatriculations_taux$cateTaux)

summary (clients_immatriculations_taux$cateTaux)

clients_immatriculations_taux <- subset(clients_immatriculations_taux, select = -TAUX)


#ENSEMBLE D'APPRENTISSAGE
#clients_immatriculations_EA : sélection des 29014 premières lignes de clients_immatriculations.(70% de données)"
clients_immatriculations_EA_taux <- clients_immatriculations_taux[1:68773,]

#ENSEMBLE DE TEST
#clients_immatriculations_ET : sélection des  dernières lignes de clients_immatriculations.(30% de données)"
clients_immatriculations_ET_taux <- clients_immatriculations_taux[68773:98246,]

summary(clients_immatriculations_EA_taux)
summary(clients_immatriculations_ET_taux)



#-----------------------Classifieurs avec le taux--------------------------#

#---------------------#
# K-NEAREST NEIGHBORS #
#---------------------#


kknn6_taux<-kknn(categories~., clients_immatriculations_EA_taux, clients_immatriculations_ET_taux)

# Matrice de confusion
table(clients_immatriculations_ET_taux$categories, kknn6_taux$fitted.values)

# Conversion des probabilites en data frame
knn_prob_taux <- as.data.frame(kknn6_taux$prob)

knn_auc_taux <-multiclass.roc(clients_immatriculations_ET_taux$categories, knn_prob_taux)
print(knn_auc_taux)

#-----------------#
# NEURAL NETWORKS #
#-----------------#

nnet5_taux<-nnet(categories ~., clients_immatriculations_EA_taux, size=7)

# Test du classifieur : classe predite
nn_class_taux<- predict(nnet5_taux, clients_immatriculations_ET_taux, type="class")
nn_class_taux
table(nn_class_taux)

# Matrice de confusion
table(clients_immatriculations_ET_taux$categories, nn_class_taux)

# Test du classifieur : probabilites pour chaque prediction
nn_prob_taux <- predict(nnet5_taux, clients_immatriculations_ET_taux, type="raw")
nn_auc_taux <-multiclass.roc(clients_immatriculations_ET_taux$categories, nn_prob_taux)
print(nn_auc_taux)


#----------------#
# RANDOM FORESTS #
#----------------#

clients_immatriculations_EA_taux_RF <- subset(clients_immatriculations_EA_taux, select = -AGE)
# Apprentissage du classifeur de type random forest
rf_taux <- randomForest(categories~., clients_immatriculations_EA_taux_RF)

# Test du classifieur : classe predite
rf_class_taux <- predict(rf_taux,clients_immatriculations_ET_taux, type="response")
table(rf_class_taux)

# Matrice de confusion
table(clients_immatriculations_ET_taux$categories, rf_class_taux)

# Test du classifieur : probabilites pour chaque prediction
rf_prob_taux <- predict(rf_taux, clients_immatriculations_ET_taux , type="prob")
rf_auc_taux <-multiclass.roc(clients_immatriculations_ET_taux$categories, rf_prob_taux)
print(rf_auc_taux)


#-------------#
# NAIVE BAYES #
#-------------#

# Apprentissage du classifeur de type naive bayes
nb_taux <- naive_bayes(categories~., clients_immatriculations_EA_taux)

# Test du classifieur : classe predite
nb_class_taux <- predict(nb_taux, clients_immatriculations_ET_taux, type="class")
table(nb_class_taux)

# Matrice de confusion
table( clients_immatriculations_ET_taux$categories, nb_class_taux)

# Test du classifieur : probabilites pour chaque prediction
nb_prob_taux <- predict(nb_taux,  clients_immatriculations_ET_taux, type="prob")
nb_auc_taux <- multiclass.roc(clients_immatriculations_ET_taux$categories, nb_prob_taux)
print(nb_auc_taux)


#-------------#
#      SVM    #
#-------------#

# Apprentissage du classifeur de type svm
svm <- svm(categories~., clients_immatriculations_EA_taux, probability=TRUE)
svm


# Test du classifieur : classe predite
svm_class <- predict(svm, clients_immatriculations_ET_taux, type="response")
svm_class
table(svm_class)

# Matrice de confusion
table(clients_immatriculations_ET_taux$categories, svm_class)

# Test du classifieur : probabilites pour chaque prediction
svm_prob <- predict(svm, clients_immatriculations_ET_taux, probability=TRUE)


# Recuperation des probabilites associees aux predictions
svm_prob <- attr(svm_prob, "probabilities")

# Conversion en un data frame 
svm_prob <- as.data.frame(svm_prob)


# Calcul de l'AUC
svm_auc <-multiclass.roc(clients_immatriculations_ET_taux$categories, svm_prob)
print (svm_auc)


#--------------------------------------------#
# APPLICATION DE LA METHODE NEURAL NETWORKS #
#------------------------------------------#

## Application des catégories de taux au fichier marketing

#Création de categorie de taux 


marketing$cateTaux<- ifelse(marketing$TAUX < 420 ,"Faible",
                                                ifelse(marketing$TAUX <607.1, "Moyen",
                                                       ifelse(marketing$TAUX <823, "Elevée", "Très élevée")))

marketing$cateTaux <- as.factor(marketing$cateTaux)
marketing$SEXE <- as.factor(marketing$SEXE)
marketing$SITUATIONFAMILIALE <- as.factor(marketing$SITUATIONFAMILIALE)
marketing$DEUXIEMEVOITURE <- as.factor(marketing$DEUXIEMEVOITURE)
marketing$AGE <- as.factor(marketing$AGE)
marketing$NBENFANTSACHARGE <- as.factor(marketing$NBENFANTSACHARGE)

summary(marketing)

marketingResultat <- marketing
marketing <- subset(marketing, select = -TAUX)


#Apprentissage
nnet5_marketing<-nnet(categories ~., clients_immatriculations_taux, size=7)

#Classification
catégorie_prédite<- predict(nnet5_marketing, marketing, type="class")
catégorie_prédite
table(catégorie_prédite)

# Recuperation des probabilites pour chaque prediction
nn_prob_marketing <- predict(nnet5_marketing, marketing, type="raw")

nn_prob_marketing



resultat <- data.frame(marketingResultat,catégorie_prédite,nn_prob_marketing)


#---------------------------------#
# ENREGISTREMENT DES PREDICTIONS  #
#---------------------------------#
# Enregistrement du fichier de resultats au format csv
write.table(resultat, file='predictions.csv', sep="\t", dec=".", row.names = F)


