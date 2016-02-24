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
#a l'air ok
summary(density)
table(density)
barplot(table(density))
cleanData$density=factor(cleanData$density,label=c("high","iso","low","fat-containing"))


#6eme colonne : severity, doit etre 0 ou 1
#a l'air ok
summary(severity)
table(severity)
barplot(table(severity))

cleanData$severity=factor(cleanData$severity,label=c("benign","malignant"))


#On essai de faire une Analyse des correspondances multiples sur les 3 variables shape, margin et density
#pour ACM il faut FactoMineR
library(FactoMineR)

res.acm <- MCA(cleanData,quanti.sup = 2 , quali.sup = c(1,6))

res.acm
names(res.acm)
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
