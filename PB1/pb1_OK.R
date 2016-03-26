#Pb 1 

library(FactoMineR)  #pour l'ACM
library(lsr)  #pour le V de Cramer

#importation des donnees
data <- read.table("mammo.data",sep=",",
            col.names=c("assessment","age","shape","margin",
                   "density","severity"),na.string="?")

#on supprime les lignes ayant des valeurs manquantes
data = na.omit(data)

#Etude de la variable 1 : assessment
#valeurs aberantes
select = which(data[1]==0,arr.ind=T)
data = data[-select[,1],]
select = which(data[1]==6,arr.ind=T)
data = data[-select[,1],]
data[which(data[1]==55,arr.ind=T)]=5

table(data$assessment)   #repartition des effectifs
barplot(table(data$assessment))

#croisement avec severity, d'abord facteur pour severity 
data$severity=factor(data$severity,label=c("benign","malignant"))
croise = table(data$assessment,data$severity)
addmargins(croise)
prop.table(croise,1)


#2eme colonne : l'age
summary(data$age) 
hist(data$age)
mean(data$age)
#distribution normale, moyenne a 55.7

#on decoupe age en classes equivalentes, 
#on cherche d'abord les quantiles
decoupe = quantile(data$age,probs = seq(0,1,length=12))
Age_1 = cut(data$age,breaks = decoupe,include.lowest = T)
table(Age_1)
croise = table(Age_1,data$severity)
addmargins(croise)
prop.table(croise,1)
#On remarque que 
#1. modalite <=35 tres faible risque
#2. un 2eme ensemble avec risque plutot faible : 36-51
#3. un 3eme ensemble avec risque moyen : 52-65
#4. 4eme ensemble avec risque tres eleve : >=66
#On utilise ce decoupage pour l'age
data$age = cut(data$age,breaks=c(0,35,51,65,100)) 
table(data$age)
barplot(table(data$age))
croise = table(data$age,data$severity)
addmargins(croise)
prop.table(croise,1)
#changement du nom des facteurs
levels(data$age) = c("<=35","36-51","52-65",">=66")

#3eme colonne : shape, doit aller de 1 a 4
summary(data$shape)
barplot(table(data$shape))
#bien distribuee : modalites relativement equilibrees
croise = table(data$shape,data$severity)
croise
cramersV(croise)
addmargins(croise)
prop.table(croise,1) #pourcentage en lignes
#pour 1 et 2 : risque faible
#pour 3 : risque moyen
#pour 4 : risque eleve

#on change le code 1,2,3,4 avec les facteurs correspondants
data$shape=factor(data$shape,label=c("round","oval","lobular",
                                     "irregular"))


#4eme colonne : margin, doit aller de 1 a 5
summary(data$margin)
table(data$margin)
barplot(table(data$margin))
#relativement bien equilibree, la 2 faible, peut-etre a regrouper?
data$margin=factor(data$margin,label=c("circumscribed",
    "microlobulated","obscured","ill-defined","spiculated"))

croise = table(data$margin,data$severity)
cramersV(croise)
addmargins(croise)
prop.table(croise,1)
#le 1 : risque faible
#les autres : risque eleve
#on peut aussi conclure que la 2 et 3 pourraient etre regroupees


#5eme colonne : density, doit aller de 1 a 4
#Attention a density car la modalite low est surrepresentee, 
#les autres ont tres peu d'individus
#Ne faut-il mieux pas l'enlever de l'ACM?
summary(data$density)
table(data$density)
barplot(table(data$density))
data$density=factor(data$density,label=c("high","iso","low",
                                         "fat-containing"))

croise = table(data$density,data$severity)
cramersV(croise)
addmargins(croise)
prop.table(croise,1)
#rien de net, chaque modalite a peu pres 50/50
#on peut conclure qu'on oublit cette variable pour l'etude


#6eme colonne : severity, doit etre 0 ou 1
summary(data$severity)
table(data$severity)
barplot(table(data$severity))



#On fait une Analyse des correspondances multiples sur 
#les 3 variables age, shape, margin 
res.acm <- MCA(data, quanti.sup = 1, quali.sup = c(5,6))
res.acm

#Pour avoir le diagramme en barre pour choisir nombre 
#d'axes a conserver
barplot(res.acm$eig[,2],names=paste("Dim",1:nrow(res.acm$eig)))
#Pour avoir les valeurs numeriques
round(res.acm$eig[1:5,],2)

plot(res.acm,invisible=c("var"),habillage="shape")
plot(res.acm,invisible=c("var","quanti.sup"),habillage="margin")
plot(res.acm,invisible=c("var","quanti.sup"),habillage="severity")

#ce graphe est le plus interressant :
plot(res.acm,invisible="ind")
#on voit :
# - axe 1 celui du risque de cancer sever
# - age bien modeliser suivant l'axe 1, 
# - pareil pour shape, on retrouve les ensembles de 
#l'anlyse univariee
# - margin pareil, on retrouve circumscribed d'un cote, 
#les autres de l'autre
# - assessment aussi confirme, 5 a gauche : risque eleve, 
#le reste de l'autre cote, dans le bon ordre
# - on a severite malignant proche de l'age >66, 
#aussi 51-66, margin ill-defined, shape irregular, 
#assessment 5
# - de l'autre cote, benign avec age <51, shape oval et round, 
#margin circumscribed, assess 2,3,4

plot(res.acm,choix="var")
#variable margin le plus important pour constitution des 2 axes, 
#shape suit pas loin, age plutot pour axe 1

res.acm$var
res.acm$quali.sup

#Pour verifier les observations, on fait une AFC pour chaque 
#paire de variables 
link1 = table(data$shape,data$margin)
link1
cramersV(link1)
res.ac1 = CA(link1)
res.ac1

link2 = table(data$shape,data$age)
link2
cramersV(link2)
res.ac2 = CA(link2)
res.ac2

link3 = table(data$age,data$margin)
cramersV(link3)
res.ac3 = CA(link3)
res.ac3



#Rapide exploration du niveau 4 de assessment
#Surestimation de la severite, surtout pour jeunes individus
select = which(data[1]==4,arr.ind=T)
data2 = data[select[,1],]
summary(data2$age) 
barplot(table(data2$age))
croise = table(data2$age,data2$severity)
addmargins(croise)
prop.table(croise,1)

select = which(data2[2]=='<=35',arr.ind=T)
data3 = data2[select[,1],]
summary(data3)
