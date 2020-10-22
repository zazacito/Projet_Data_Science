Suppression de toutes les lignes ou l''âge était erroné

delete from client where age < '18';

Recherche des Valeurs erronnées pour le sexe

select * from client where sexe != 'M' And sexe != 'F' ;

On peut remplacer les masculins/hommes et féminins/femmes par des f et m
UPDATE client SET client.sexe ='M' WHERE sexe = 'Masculin' OR sexe='Homme' ;
UPDATE client SET client.sexe ='F' WHERE sexe = 'Féminin' OR sexe='Femme' ;

Suppression des valeurs vides ou 'N/D'
delete from client where sexe != 'M' And sexe != 'F' ;
