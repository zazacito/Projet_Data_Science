Suppression de toutes les lignes ou l''âge était erroné

select * from client where age <18;
delete from client where age < 18 ;


Recherche des Valeurs erronnées pour le sexe

select * from client where sexe != 'M' And sexe != 'F' ;

On peut remplacer les masculins/hommes et féminins/femmes par des f et m
UPDATE client SET client.sexe ='M' WHERE sexe = 'Masculin' OR sexe='Homme' ;
UPDATE client SET client.sexe ='F' WHERE sexe = 'Féminin' OR sexe='Femme' ;

Suppression des valeurs vides ou 'N/D'
delete from client where sexe != 'M' And sexe != 'F' ;


Recherche des Valeurs erronnées pour le nombre d''enfants à charge

select * from client where nbenfantsacharge <0;
select * from client where nbenfantsacharge >4;
delete from client where nbenfantsacharge <0;
delete from client where nbenfantsacharge >4;


Recherche des Valeurs erronnées pour la situationfamiliale

select * from client
where situationfamiliale != 'Célibataire'
AND situationfamiliale != 'Divorcé'
AND  situationfamiliale != 'Divorcée'
AND situationfamiliale != 'En Couple'
AND situationfamiliale != 'Marié(e)'
AND situationfamiliale != 'Seul'
AND situationfamiliale != 'Seule';

delete from client
where situationfamiliale != 'Célibataire'
AND situationfamiliale != 'Divorcé'
AND  situationfamiliale != 'Divorcée'
AND situationfamiliale != 'En Couple'
AND situationfamiliale != 'Marié(e)'
AND situationfamiliale != 'Seul'
AND situationfamiliale != 'Seule';



Recherche des Valeurs erronnées pour le taux
select * from client where taux < 0 ;
delete from client where taux < 0 ;

Recherche des Valeurs erronnées pour la deuxieme voiture
select * from client where deuxiemevoiture !='true' and deuxiemevoiture !='false';
delete from client where deuxiemevoiture !='true' and deuxiemevoiture !='false';
