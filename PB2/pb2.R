#pb 2

#importation des donnees
data <- read.table("water-treatment.data",sep=",",
    col.names=c("date","Q-E","ZN-E","PH-E","DBO-E","DQO-E","SS-E","SSV-E",
                "SED-E","COND-E","PH-P","DBO-P","SS-P","SSV-P","SED-P","COND-P","PH-D","DBO-D","DQO-D","SS-D","SSV-D","SED-D",
                "COND-D","PH-S","DBO-S","DQO-S","SS-S","SSV-S","SED-S","COND-S","RD-DBO-P","RD-SS-P","RD-SED-P","RD-DBO-S","RD-DQO-S",
                "RD-DBO-G","RD-DQO-G","RD-SS-G","RD-SED-G"),na.string="?")

#on enleve ce qu'on veut pas pour l'instant
data2 = data[-c(1,31,32,33,34,35,36,37,38,39)]

library(FactoMineR)  #pour l'ACP
res.acp = PCA(data2)

#choix du nombre d'axes
barplot(res.acp$eig[,2],names=paste("Dim",1:nrow(res.acp$eig)))
round(res.acp$eig[1:6,],2)

dimdesc(res.acp)

#correlation
data3 = na.omit(data2)
round(cor(data3),2)
