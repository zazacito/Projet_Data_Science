marketing <- read.csv("DATA/Marketing.csv", header = TRUE, sep = ",", dec = ".")

marketing$sexe <- as.factor(marketing$sexe)
summary(marketing$sexe)

marketing$situationFamiliale <- as.factor(marketing$situationFamiliale)
summary(marketing$situationFamiliale)

marketing$X2eme.voiture  <- as.logical(marketing$X2eme.voiture)
summary(marketing$X2eme.voiture)

str(marketing)
