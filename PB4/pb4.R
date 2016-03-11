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
res.acp = PCA(X,scale.unit=T,graph=T)
#1er axe surtout lié aux variables 15 (0.94),6 (0.84) et 17 (0.83) positivement
#2ème axe principalement décrit par rapport aux variables 16 (0.75),14 (0.63),3 (0.62) positivement et négativement par 11 (-0.61)

barplot(res.acp$eig[,2],names=paste("Dim",1:nrow(res.acp$eig)))
round(res.acp$eig[1:18,],2)

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


summary(lm(var15~var6))

#essai classification 
library(cluster)
res = PCA(X,graph=F,ncp=11)
class = agnes(res$ind$coord, method="ward")
plot(class)

class2 = as.hclust(class)
plot(rev(class2$height),type="h",ylab="hauteurs")

classes = cutree(class,k=5)
classes

XPlus = cbind.data.frame(X,as.factor(classes))
colnames(XPlus)[19] = "Classe"

resACP = PCA(XPlus,quali.sup=19,graph=F)
plot(resACP,choix="ind",habillage = 19,axes=c(1,3))
