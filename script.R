setwd("~vbridonneau/projet-spam/")
library("FactoMineR")
library(PCAmixdata)
library("GGally")
library(MASS)



rm(list = ls(all = TRUE))
graphics.off()

load("./spam_data_train.rda")
load("./spam_data_test.rda")

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
res <- PCA(data_train,graph=FALSE,scale.unit=FALSE)
# Figure individus
plot(res,choix="ind",cex=1.5,title="")
# Figure variables
plot(res,choix="var",cex=1.5,title="")

# Analyse en composantes principales sur les données centrées-réduites
# (par défaut: scale.unit=TRUE)
resnorm <- PCA(data_train,graph=FALSE)
# Figure individus
plot(resnorm,choix="ind",cex=1.5,title="")
# Figure variables
plot(resnorm,choix="var",cex=1.5,title="")


# Inertie (variance) des composantes principales
resnorm$eig
barplot(resnorm$eig[,1])


##############
### KMEANS ###
##############

# Création des données centrées ...
data_train_norm <- sweep(data_train,2,mean,"-")
# ... et réduites
data_train_norm <- sweep(data_train_norm,2,std,"/")
# Affichage des données centrées - réduites

# Nombre de clusters souhaité
numcluster <- 2

# Algorithme des kmeans sur données centrées-réduites (avec affichage)
kmnorm <- kmeans(data_train_norm,numcluster,nstart=50)
print(kmnorm)

# Concatenation des données avec leur résultat de cluster
clusternorm <- as.factor(kmnorm$cluster)
data_train_normplusCluster <- data.frame(data_train_norm,cluster=clusternorm)
colclust <- length(data_train)+1
print(data_train_normplusCluster)

# ACP sur les données centrées-réduites
rPCAnorm <- PCA(data_train_normplusCluster,graph=FALSE,quali.sup=colclust)
# Nuage des individus et des variables dans le premier plan factoriel
par(mfrow=c(1,2))
plot.PCA(rPCAnorm,,axes=c(1,2),choix="ind",habillage=colclust,invisible="quali")
plot.PCA(rPCAnorm,axes=c(1,2),choix="var")
# Nuage des individus et des variables dans le deuxième plan factoriel
par(mfrow=c(1,2))
plot.PCA(rPCAnorm,axes=c(1,3),choix="ind",habillage=colclust,invisible="quali")
plot.PCA(rPCAnorm,axes=c(1,3),choix="var")

data_train$label <- as.factor(data_train$label)
lin_disc_an <- lda(label~., data = data_train_norm)
data_test_x <- data.frame(data_test, check.names = FALSE)
test_spam_predict <- predict(lin_disc_an, newdata=data_test_x, type="class")
print(table(test_spam_predict$class, data_train$label))

spam_error_rate <- mean(test_spam_predict$class != data_test$y)
cat("Error rate : ", spam_error_rate, "\n")