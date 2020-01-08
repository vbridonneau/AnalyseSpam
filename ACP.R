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
res$var
resnorm$var

res_no_caps <- PCA(data_train[,1:54],graph=FALSE,scale.unit=FALSE)
res_no_caps$eig
barplot(res_no_caps$eig[,1])
res_no_caps$var

resnorm_no_caps <- PCA(data_train[,1:54],graph=FALSE)
resnorm_no_caps$eig
barplot(resnorm_no_caps$eig[,1])

#names(data_train)

ggcorr(data_train[,55:58])
ggpairs(data_train[,55:58])

spam <- data_train[data_train$label == 1,]
non_spam <- data_train[data_train$label == 0,]

res_spam <- PCA(spam[-58],graph=FALSE)#,scale.unit=FALSE)
res_non_spam <- PCA(non_spam[-58],graph=FALSE)#,scale.unit=FALSE)

res_spam$eig
barplot(res_spam$eig[,1])
res_spam$var$contrib

res_non_spam$eig
barplot(res_non_spam$eig[,1])
res_non_spam$var$contrib

print(apply(spam[55],2,mean))
print(apply(spam[55],2,sd)*sqrt(5/6))
print(apply(non_spam[55],2,mean))
print(apply(non_spam[55],2,sd)*sqrt(5/6))
print(apply(spam[56],2,mean))
print(apply(spam[56],2,sd)*sqrt(5/6))
print(apply(non_spam[56],2,mean))
print(apply(non_spam[56],2,sd)*sqrt(5/6))
print(apply(spam[57],2,mean))
print(apply(spam[57],2,sd)*sqrt(5/6))
print(apply(non_spam[57],2,mean))
print(apply(non_spam[57],2,sd)*sqrt(5/6))

print(t.test(spam[55], conf.level = 0.99))
print(t.test(non_spam[55], conf.level = 0.99))
print(t.test(spam[56], conf.level = 0.99))
print(t.test(non_spam[56], conf.level = 0.99))
print(t.test(spam[57], conf.level = 0.99))
print(t.test(non_spam[57], conf.level = 0.99))


print(nrow(data_train))
print(nrow(spam))
print(nrow(non_spam))
d1 <- data_train[data_train$capital_run_length_total > 436.2698,]
print(nrow(d1))
d2 <- d1[d1$capital_run_length_longest > 87.57054,]
print(nrow(d2))
d3 <- d2[d2$capital_run_length_average > 6.944014,]
print(nrow(d3))

#print(table(d3[d3 %in% ]))

#d1 <- data_train[data_train$capital_run_length_total > 417,]
#d2 <- d1[d1$capital_run_length_longest > 80,]
#d3 <- d2[d2$capital_run_length_average > 6,]
#
#spam3 <- d3[d3$label == 1,]
#non_spam3 <- d3[d3$label == 0,]
#print(nrow(spam3))
#print(nrow(non_spam3))
#
##garde les spams
#vp <- nrow(spam3) # label = 1 & pred = 1
#vn <- nrow(non_spam) - nrow(non_spam3) # label = 0 & pred = 0
#
## le taux de vrais positifs
#print(vp / nrow(spam))
## le taux de vrais négatifs
#print(vn / nrow(non_spam))
#
#
##garde les non-spams
#vp <- nrow(non_spam3) # label = 0 & pred = 1
#vn <- nrow(spam) - nrow(spam3) # label = 1 & pred = 0
#
## le taux de vrais positifs
#print(vp / nrow(non_spam))
## le taux de vrais négatifs
#print(vn / nrow(spam))
#
