setwd("~/Apprentissage")
library("FactoMineR")
library(PCAmixdata)
library("GGally")
library(MASS)



rm(list = ls(all = TRUE))
graphics.off()

load("./Data/Projets/spam_data_train.rda")
load("./Data/Projets/spam_data_test.rda")

#print(data_train,  digits=4)

###########
### ACP ###
###########

# Calcul de la moyenne et de l’écart type des variables
mean <- apply(data_train,2,mean)
std <- apply(data_train,2,sd)*sqrt(5/6) #standard deviation
stat <- rbind(mean,std)
# Affichage
print(stat,digits=4)

# Visualisation des données en description bivariée
#pairs(data_train[,25:40])
# Afficher la matrice de corrélation
#ggcorr(data_train[,25:40])
# Aller encore plus loin avec ggpairs
#ggpairs(data_train[,25:40])


# Analyse en composantes principales sur les données d’origine
# (scale.unit=FALSE)
res <- PCA(data_train[-58],graph=FALSE,scale.unit=FALSE)
# Figure individus
plot(res,choix="ind",cex=1.5,title="")
# Figure variables
plot(res,choix="var",cex=1.5,title="")

# Analyse en composantes principales sur les données centrées-réduites
# (par défaut: scale.unit=TRUE)
resnorm <- PCA(data_train[-58],graph=FALSE)
# Figure individus
plot(resnorm,choix="ind",cex=1.5,title="")
# Figure variables
plot(resnorm,choix="var",cex=1.5,title="")


# Inertie (variance) des composantes principales
res$eig
barplot(res$eig[,1])
resnorm$eig
barplot(resnorm$eig[,1])

res_no_caps <- PCA(data_train[,1:54],graph=FALSE,scale.unit=FALSE)
res_no_caps$eig
barplot(res_no_caps$eig[,1])

resnorm_no_caps <- PCA(data_train[,1:54],graph=FALSE)
resnorm_no_caps$eig
barplot(resnorm_no_caps$eig[,1])

#names(data_train)
