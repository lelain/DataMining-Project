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

