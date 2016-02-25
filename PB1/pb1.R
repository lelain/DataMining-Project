#Pb 1 

#importation des donnees
data <- read.table("mammo.data",sep=",",col.names=c("assessment","age","shape","margin","density","severity"),na.string="?")
head(data)

#pour avoir le nombre de donnees manquantes
apply(data,2,function(x) sum(is.na(x)))   #le 2 pour colonnes

summary(data)

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

attach(cleanData)

table(assessment)   #pour avoir le nombre dans chaque classes

barplot(table(assessment)) #pour l'histogramme 

cleanData$assessment=factor(cleanData$assessment,label=c("2","3","4","5"))

#2eme colonne : l'age
summary(age) #a l'air ok
hist(age)

#3eme colonne : shape, doit aller de 1 a 4
summary(shape)
barplot(table(shape))

#on change le code 1,2,3,4 avec les facteurs correspondants
cleanData$shape=factor(cleanData$shape,label=c("round","oval","lobular","irregular"))


#4eme colonne : margin, doit aller de 1 a 5
summary(margin)
table(margin)
barplot(table(margin))
cleanData$margin=factor(cleanData$margin,label=c("circumscribed","microlobulated","obscured","ill-defined","spiculated"))

#5eme colonne : density, doit aller de 1 a 4
#Attention a density car la modalite low est surrepresentee, les autres ont tres peu d'individus
#Ne faut-il mieux pas l'enlever de l'ACM?
summary(density)
table(density)
barplot(table(density))
cleanData$density=factor(cleanData$density,label=c("high","iso","low","fat-containing"))


#6eme colonne : severity, doit etre 0 ou 1
summary(severity)
table(severity)
barplot(table(severity))

cleanData$severity=factor(cleanData$severity,label=c("benign","malignant"))


#On essai de faire une Analyse des correspondances multiples sur les 3 variables shape, margin et density
#pour ACM il faut FactoMineR
library(FactoMineR)

res.acm <- MCA(cleanData,quanti.sup = 2 , quali.sup = 6)

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

#entre shape et density
link2 = table(cleanData$shape,cleanData$density)
res.ac2 = CA(link2)
res.ac2
#presque independant, en tous cas pas de lien net


#entre density et margin
#density n'est pas tres bien distribue : beaucoup en low, peu ailleurs
#le test ne rejete pas l'independance
link3 = table(cleanData$density,cleanData$margin)
link3
res.ac3 = CA(link3)
res.ac3
res.ac3$row
res.ac3$col


#entre margin et assessment
#on rejete largement l'independance
#2 ensembles : 
#1. circumscribed avec 2,3,4 -> moins de risque
#2. microlobulated, obscured,ill-defined,spiculated,5 -> fort risque
link4 = table(cleanData$assessment,cleanData$margin)
link4
res.ac4 = CA(link4)
res.ac4
res.ac4$row
res.ac4$col

#entre shape et assessment
#on rejete largement l'independance
#3 ensembles : 
#1. irregular avec 5 -> fort risque
#2. lobular au centre tout seul -> risque medium
#3.2,3,4 avec oval et round -> moins de risque
link5 = table(cleanData$assessment,cleanData$shape)
link5
res.ac5 = CA(link5)
res.ac5

#entre density et assessment
#on rejete l'independance de peu, p-value 0.0015
#plot difficile a interpreter
link6 = table(cleanData$assessment,cleanData$density)
link6
res.ac6 = CA(link6)
res.ac6



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
#on pourrait aussi essayer de decouper de telle maniere que les classes soient equilibrer
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
