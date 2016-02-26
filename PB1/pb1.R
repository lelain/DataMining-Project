#Pb 1 

#importation des donnees
data <- read.table("mammo.data",sep=",",
          col.names=c("assessment","age","shape","margin",
            "density","severity"),na.string="?")
head(data)

#pour avoir le nombre de donnees manquantes
apply(data,2,function(x) sum(is.na(x)))   #le 2 pour colonnes

summary(data)

#A VOIR : fonction na.omit pour supprimer les lignes avec donnees manquantes
#On peut eliminer toutes les lignes ayant des donnees manquantes 
aEliminer <- apply(is.na(data),MARGIN=1,FUN=any)    #la fonction any sur un vecteur renvoie TRUE si au moins un TRUE. On l'applique a chaque ligne avec MARGIN=1
cleanData <- data[!aEliminer,]
head(cleanData)

#Dans 1ere colonne, on remplace la valeur 55 par 5
#On elimine les lignes avec 0 ou 6
#Assessment devant aller de 1 a 5
cleanData[which(cleanData[1]==55,arr.ind=T)]=5
select = which(cleanData[1]==0,arr.ind=T)
cleanData = cleanData[-select[,1],]
select = which(cleanData[1]==6,arr.ind=T)
cleanData = cleanData[-select[,1],]

#on change en facteurs
cleanData$assessment=factor(cleanData$assessment,label=c("2","3","4","5"))

table(cleanData$assessment)   #pour avoir le nombre dans chaque classes
barplot(table(cleanData$assessment)) #pour l'histogramme 
#pas tres equilibre, beaucoup dans 4 et 5, peu dans 2 et 3

#on peut croiser avec la severity 
croise = table(cleanData$assessment,cleanData$severity)
addmargins(croise)
prop.table(croise) #pourcentage globaux
prop.table(croise,1) #pourcentage en lignes
#Comme on s'y attend, avec 2 et 3 on a le plus souvent un risque faible
#avec 5, risque tres eleve
#pour 4 c'est moins clair, 77% pour benin
  
  
#2eme colonne : l'age
summary(cleanData$age) 
hist(cleanData$age)
mean(cleanData$age)
#distribution normale, moyenne a 55.7

#on decoupe age en classes equivalentes, on cherche d'abord les quantiles
decoupe = quantile(cleanData$age,probs = seq(0,1,length=12))
Age_1 = cut(cleanData$age,breaks = decoupe,include.lowest = T)
table(Age_1)
croise = table(Age_1,cleanData$severity)
addmargins(croise)
prop.table(croise,1)
#On remarque que 
#1. modalite <35 tres faible risque
#2. un 2eme ensemble avec risque plutot faible : 35-51
#3. un 3eme ensemble avec risque moyen : 51-66
#4. 4eme ensemble avec risque tres eleve : >66
#On pourrait utiliser ce decoupage pour l'age
cleanData$age = cut(cleanData$age,breaks=c(0,35,51,66,100)) 
table(cleanData$age)
barplot(table(cleanData$age))
croise = table(cleanData$age,cleanData$severity)
addmargins(croise)
prop.table(croise,1)
#Pas mal comme ca

Age_3 = cut(cleanData$age,breaks=c(10,20,30,40,50,60,70,80,90,100)) 
table(Age_3)
croise = table(Age_3,cleanData$severity)
addmargins(croise)
prop.table(croise,1) #pourcentage en lignes
#le risque augmente avec l'age, saut du risque a partir de 40 ans 


#3eme colonne : shape, doit aller de 1 a 4
summary(cleanData$shape)
barplot(table(cleanData$shape))
#bien distribuee : modalites relativement equilibrees
croise = table(cleanData$shape,cleanData$severity)
addmargins(croise)
prop.table(croise,1) #pourcentage en lignes
#pour 1 et 2 : risque faible
#pour 3 : risque moyen
#pour 4 : risque eleve

#on change le code 1,2,3,4 avec les facteurs correspondants
cleanData$shape=factor(cleanData$shape,label=c("round","oval","lobular","irregular"))


#4eme colonne : margin, doit aller de 1 a 5
summary(cleanData$margin)
table(cleanData$margin)
barplot(table(cleanData$margin))
#relativement bien equilibree, la 2 faible, peut-etre a regrouper?
cleanData$margin=factor(cleanData$margin,label=c("circumscribed","microlobulated","obscured","ill-defined","spiculated"))

croise = table(cleanData$margin,cleanData$severity)
addmargins(croise)
prop.table(croise,1)
#le 1 : risque faible
#les autres : risque eleve
# -> on peut conclure que la 2 et 3 pourrait etre regroupees


#5eme colonne : density, doit aller de 1 a 4
#Attention a density car la modalite low est surrepresentee, les autres ont tres peu d'individus
#Ne faut-il mieux pas l'enlever de l'ACM?
summary(cleanData$density)
table(cleanData$density)
barplot(table(cleanData$density))
cleanData$density=factor(cleanData$density,label=c("high","iso","low","fat-containing"))

croise = table(cleanData$density,cleanData$severity)
addmargins(croise)
prop.table(croise,1)
#rien de net, chaque modalite a peu pres 50/50
#on peut conclure qu'on oublit cette variable pour l'etude
cleanData = cleanData[-5]

#6eme colonne : severity, doit etre 0 ou 1
summary(cleanData$severity)
table(cleanData$severity)
barplot(table(cleanData$severity))

cleanData$severity=factor(cleanData$severity,label=c("benign","malignant"))


#On essai de faire une Analyse des correspondances multiples sur les 3 variables age, shape, margin 
#pour ACM il faut FactoMineR
library(FactoMineR)

res.acm <- MCA(cleanData, quali.sup = c(1,5))

#Pour avoir le diagramme en barre pour choisir nombre d'axes a conserver
barplot(res.acm$eig[,2],names=paste("Dim",1:nrow(res.acm$eig)))
#Pour avoir les valeurs numeriques
round(res.acm$eig[1:5,],2)

plot(res.acm,invisible=c("var"),habillage="shape")
plot(res.acm,invisible=c("var","quanti.sup"),habillage="margin")
plot(res.acm,invisible=c("var","quanti.sup"),habillage="assessment")
plot(res.acm,invisible=c("var","quanti.sup"),habillage="severity")

#ce graphe est le plus interressant :
plot(res.acm,invisible="ind")
#on voit :
# - axe 1 celui du risque de cancer sever
# - age bien modeliser suivant l'axe 1, 
# - pareil pour shape, on retrouve les ensembles de la'anlyse univariee
# - margin pareil, on retrouve circumscribed d'un cote, les autres de l'autre
# - assessment aussi confirme, 5 a gauche : risque eleve, le reste de l'autre cote, dans le bon ordre
# - on a severite malignant proche de l'age >66, aussi 51-66, margin ill-defined, shape irregular, assessment 5
# - de l'autre cote, benign avec age <51, shape oval et round, margin circumscribed, assess 2,3,4

plot(res.acm,choix="var")
#variable margin le plus important pour constitution des 2 axes, shape suit pas loin, age plutot pour axe 1

res.acm$var
res.acm$quali.sup

plot(res.acm,invisible=c("ind"),axes = 3:4)
plot(res.acm,invisible=c("var","quanti.sup"),axes=3:4)
plot(res.acm,choix="var",axes=3:4)

dimdesc(res.acm)


#Analyse factorielle des correspondances entre shape et margin pour mieux
#etudier leur lien qui semble exister 
link1 = table(cleanData$shape,cleanData$margin)
res.ac1 = CA(link1)
res.ac1

#on voit 3 ensembles :
#1. round,oval et circumscribed a gauche -> benin
#2. au centre un peu droite : microlobulated et lobular -> deja a risque
#3. a droite : obscured, ill-defined, spiculated et irregular -> risque eleve

#diagramme en batons des valeurs propres
barplot(res.ac1$eig[,2],names=paste("Dim",1:nrow(res.ac$eig)))
#directement les pourcentages
round(res.ac1$eig,3) #les 2 premiers axes expriment plus de 99% de l'inertie totale

res.ac1$row
res.ac1$col

#entre shape et age
link2 = table(cleanData$shape,cleanData$age)
res.ac2 = CA(link2)
res.ac2
#on rejette largement l'independance
#d'un cote moins de 51 ans avec round et oval
#de l'autre plus de 51 avec irregular et lobular
#chaque modalite bien representee (dans l'ordre) sur l'axe 1


#entre age et margin
link3 = table(cleanData$age,cleanData$margin)
res.ac3 = CA(link3)
res.ac3
#on rejette largement l'independance
#d'un cote circumscribed avec <51
#de l'autre les 4 autres avec >51
#age bien distribue sur axe 1, les 4 modalites de margin ensemble a droite, circumscribed seule a droite


#on essaye de faire une analyse discriminante sur les coordonnes
#des individus sur les 5 premiers axes
library(MASS)
donnees = cbind.data.frame(cleanData$severity,res.acm$ind$coord)
modele <- lda(donnees$`cleanData$severity`~.,data=donnees)
prev <- lda(donnees$`cleanData$severity`~.,data=donnees,CV=T)$class
table(prev,donnees$`cleanData$severity`)
#estimation du taux de mauvais classement
sum(prev!=donnees$`cleanData$severity`)/nrow(donnees)

echAppr = sample(1:816,571,replace=F) #tire 70% des individus
donneesAppr = donnees[echAppr,]
donneesTest = donnees[-echAppr,]

modele = lda(donneesAppr$`cleanData$severity`~.,data=donneesAppr)
prediction = predict(modele,newdata=donneesTest)

Res = cbind.data.frame(donneesTest$`cleanData$severity`,prediction$class)
table(Res)
#A voir comment calculer l'aire sous la courbe ROC, et ce que s'est...

#On peut essayer de faire une ACM en ne prenant pas en compte density
res.acm <- MCA(cleanData,quanti.sup = 2 , quali.sup = c(5,6))

#Pour avoir le diagramme en barre pour choisir nombre d'axes a conserver
barplot(res.acm$eig[,2],names=paste("Dim",1:nrow(res.acm$eig)))
#Pour avoir les valeurs numeriques
round(res.acm$eig[1:5,],2)

plot(res.acm,invisible=c("var","quanti.sup"),habillage="shape")
plot(res.acm,invisible=c("var","quanti.sup"),habillage="density")
plot(res.acm,invisible=c("var","quanti.sup"),habillage="margin")
plot(res.acm,invisible=c("var","quanti.sup"),habillage="assessment")
plot(res.acm,invisible=c("var","quanti.sup"),habillage="severity")

plot(res.acm,invisible="ind")

plot(res.acm,choix="var")

res.acm$var
res.acm$quali.sup

plot(res.acm,invisible=c("ind"),axes = 3:4)
plot(res.acm,invisible=c("var","quanti.sup"),axes=3:4)
plot(res.acm,choix="var",axes=3:4)

dimdesc(res.acm)


#on peut essayer de mieux voir l'influence de l'age (on sait deja que plus age important -> plus de risque)
#pour cela, on decoupe en facteur
cleanData$age = cut(cleanData$age,breaks=c(10,20,30,40,50,60,70,80,90,100)) 
table(cleanData$age)
#on pourrait aussi essayer de decouper de telle maniere que les classes soient equilibrees
#pour cela voir p36 livre sur R

#on refait l'acm avec age, sans density
res.acm <- MCA(cleanData, quali.sup = c(5,6))

#Pour avoir le diagramme en barre pour choisir nombre d'axes a conserver
barplot(res.acm$eig[,2],names=paste("Dim",1:nrow(res.acm$eig)))
#Pour avoir les valeurs numeriques
round(res.acm$eig[1:5,],2)

plot(res.acm,invisible=c("var","quanti.sup"),habillage="shape")
plot(res.acm,invisible=c("var","quanti.sup"),habillage="density")
plot(res.acm,invisible=c("var","quanti.sup"),habillage="margin")
plot(res.acm,invisible=c("var","quanti.sup"),habillage="assessment")
plot(res.acm,invisible=c("var","quanti.sup"),habillage="severity")
plot(res.acm,invisible=c("var","quanti.sup"),habillage="age")

plot(res.acm,invisible="ind")

plot(res.acm,choix="var")

res.acm$var
res.acm$quali.sup

plot(res.acm,invisible=c("ind"),axes = 3:4)
plot(res.acm,invisible=c("var","quanti.sup"),axes=3:4)
plot(res.acm,choix="var",axes=3:4)

dimdesc(res.acm)
