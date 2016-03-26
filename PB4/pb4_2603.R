<<<<<<< HEAD
#PB4

#importation des donnees
data <- read.table("viruses.dat",sep="",
      col.names=paste("Var",1:18),row.names = paste("Vir",1:61))
total = apply(data,MARGIN = 1, FUN = sum)
type = c(rep("Hordeviruses",3),rep("Tobraviruses",6),rep("Tobamoviruses",39),rep("Furoviruses",13))

data = cbind(type,data,total)
attach(data)
boxplot(total~type,ylab="nombre total d'acides amines")
hist(total,breaks=20,main="")
plot(total)
#on peut identifier 3 outliers : 11, 55 et 56
#on peut les supprimer pour voir si cela change les resultats de l'acp

data2 = data[-c(11,55,56),]

library(FactoMineR)
res.acp = PCA(data,quali.sup = 1,quanti.sup = 20)
plot(res.acp,choix = "ind",habillage = 1,cex=0.5)

res.acp2 = PCA(data2,quali.sup = 1,quanti.sup = 20)
plot(res.acp2,choix = "ind",habillage = 1,cex=0.7)
plot(res.acp2,choix = "ind",habillage = 1,cex=0.5,axes = c(1,3))
plot(res.acp2,choix = "ind",habillage = 1,cex=0.5,axes = c(1,4))
plot(res.acp2,choix = "ind",habillage = 1,cex=0.5,axes = c(2,3))
plot(res.acp2,choix = "ind",habillage = 1,cex=0.5,axes = c(2,4))
plot(res.acp2,choix = "ind",habillage = 1,cex=0.5,axes = c(3,4))
=======
#PB 4 

#lecture des donnees
X = read.table("viruses.dat")

# nombre de variables
p=dim(X)[2]
# nombre d'individus
n=dim(X)[1]

# nommage des variables
nom=rep(NA,p)
for(i in 1:p){
  nom[i]=paste("var",i,sep = "")
}
colnames(X)=nom

# nommage des individus
nom_lg=rep(NA,n)
for(i in 1:n){
  nom_lg[i]=paste("vir",i,sep="")
}
rownames(X)=nom_lg

attach(X)

summary(X)

library(FactoMineR)
res.acp = PCA(X,graph=T)
#trie des variables les plus caractérisantes
dimdesc(res.acp)
#1er axe surtout lié aux variables 15 (0.94),6 (0.84) et 17 (0.83) positivement
#2ème axe principalement décrit par rapport aux variables 16 (0.75),14 (0.63),3 (0.62) positivement et négativement par 11 (-0.61)

#donc deja pour la question : "y a-t-il des groupes qui se ressemblent?"
# on peut dire : grossièrement d'abord 3 groupes :
# - 1 pour lequel 1er axe élevé, cad élevé pour 15,6,17
# - 1 groupe pour faible sur 1er axe, élevé sur 2ème : cad faible 15,6,17 et élevé pour 16,14,3, faible pour 11
# - 1 pour faible sur 1er axe, faible sur 2ème


barplot(res.acp$eig[,2],names=paste("Dim",1:nrow(res.acp$eig)))
round(res.acp$eig[1:18,],2)
#On peut prendre en compte 5 axes : ils expliques près de 79% de la variance

plot(res.acp,choix="var",axes=c(1,2))
plot(res.acp,choix="var",axes=c(2,3))
plot(res.acp,choix="var",axes=c(1,3))
plot(res.acp,choix="var",axes=c(1,4))
plot(res.acp,choix="var",axes=c(2,4))
plot(res.acp,choix="var",axes=c(3,4))

#trie des variables les plus caractérisantes
dimdesc(res.acp)



plot(res.acp,choix="ind",cex=0.7)

#donnees brutes centrees reduites
round(scale(X),2)

#matrices des correlations
Cor = round(cor(X),2)
Cor
#1 avec 6,15 puis 12
#2 avec 7 négativement
#3 avec rien
#4 avec 15 (0.61)
#5 avec 16
#6 avec 1,15
#7 avec 2 négativement, 15 +
#8 avec rien
#9 avec 11,17
#10 avec rien
#11 avec 9
#12 avec 1,6
#13 avec rien
#14 avec rien
#15 avec 6,1,12,17,4,7
#16 avec 5
#17 avec 9, 15
#18 avec rien



#essai classification ascendante hierarchique 
library(cluster)
res = PCA(X,graph=F,ncp=11)
class = agnes(res$ind$coord, method="ward")
plot(class)

class2 = as.hclust(class)
plot(rev(class2$height),type="h",ylab="hauteurs")

classes = cutree(class,k=11)
classes

XPlus = cbind.data.frame(X,as.factor(classes))
colnames(XPlus)[19] = "Classe"

resACP = PCA(XPlus,quali.sup=19,graph=F)
plot(resACP,choix="ind",habillage = 19,axes=c(1,2))
plot(resACP,choix="ind",habillage = 19,axes=c(1,3))
plot(resACP,choix="ind",habillage = 19,axes=c(1,4))
plot(resACP,choix="ind",habillage = 19,axes=c(2,3))
plot(resACP,choix="ind",habillage = 19,axes=c(2,4))
plot(resACP,choix="ind",habillage = 19,axes=c(3,4))
>>>>>>> 669ea39a8529447515643e7bb1a7998596f244ca

