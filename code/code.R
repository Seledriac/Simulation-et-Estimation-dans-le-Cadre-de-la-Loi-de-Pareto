
rm(list=ls()) # nettoyage de l'espace de travail

# mise à jour de la graine de génération aléatoire
RNGkind()
set.seed(333)
sample(1:10, 5, replace=T)

# Taille de l'échantillon simulé
n = 400

# Paramètres de la loi de Pareto simulée
theta = 1
alpha = 2

# Simulation de l'échantillon X
U<-runif(n)
X<-(theta/((1-U)^(1/alpha)))

# Tracé de l'histogramme de fréquences des valeurs de l'échantillon
# (les fréquences de ces valeurs modélisent la densité de probabilité de la loi de X)
titre = paste(as.character(n)," réalisations de X ~ P(alpha = ", alpha, ", theta = ", theta, ")")
hist(X, proba=TRUE, col="cyan", cex.lab=1.5, 
     cex.main=1.7, main=titre, nclass=20, xlab="x", ylab="Densité")

# Tracé de la densité de probabilité exacte de la loi
t = seq(0, 80 , 0.1)
lines(t,(alpha/theta)*(theta/t)^(alpha+1), lwd=3)

legend("topright", c("fréquences des valeurs obtenues","f(x)"), fill=c("cyan",NA), col=c(NA,par("col")), lwd=c(NA,3), border=c("black",NA), x.intersp=c(0,1))

# Évolution de l'estimation de alpha par la méthode du maximum de vraisemblance
# en fonction de l'évolution de la taille de l'échantillon

# Graphique démontrant l'évolution transitoire de A_n
transi=40
A<-matrix(ncol=1,nrow=transi) # Estimateur A_n
s=0
# réalisation d'estimations sur des échantillons de taille grandissante
for(i in 1:transi){
  s=s+log(X[i])
  A[i]=i/s
}
titre = paste("Évolution transitoire de a_n : n=1 à n=",as.character(transi),"; alpha = ",alpha)
plot(A, main=titre, xlab="n", ylab="A_n")
abline(h = alpha, col="red", lwd=3, lty=3)

# Graphique démontrant la convergence de a_n
A<-matrix(ncol=1,nrow=n)
s=0
# réalisation d'estimations sur des échantillons de taille grandissante
for(i in 1:n){ 
  s=s+log(X[i])
  A[i]=i/s
}
titre = paste("Convergence de a_n : n=1 à n=", as.character(n), "; alpha = ", alpha)
plot(A, main=titre, xlab="n", ylab="a_n")
abline(h = alpha, col="red", lwd=3, lty=3)

# 400 échantillons de taille 1000 de X
Data<-matrix(ncol=400,nrow=1000)
U<-runif(400000)
Data[,]<-(theta/((1-U)^(1/alpha)))

# 400 estimations de alpha par le maximum de vraissemblance sur les 400 échantillons
# (on calcule l'estimation pour les tailles d'échantillon de 1 à 1000)
Data2<-matrix(ncol=400,nrow=1000)
for (j in 1:400){
  s=0
  for(i in 1:1000){
    s=s+log(Data[i,j])
    Data2[i,j]=i/s
  }
}

# Affichage de la boîte à moustaches des 400 estimations pour différentes tailles d'échantillon
val_n = c(25,50,100,200,500,1000)
titre = paste("Évolution des échantillons d'estimations de alpha = ",alpha ," en fonction de n")
boxplot(t(Data2)[,val_n],col = "cyan",names=c("25","50","100","200","500","1000"),
        cex.axis=1.3,cex.lab = 1.5, xlab="Taille des échantillons (n)",
        outline = TRUE, main=titre)
abline(h = alpha, col="red", lwd=3, lty=3)

# Affichage des moyennes des 400 estimations sur chaque boîte à moustaches
points(apply(t(Data2)[,val_n],2,mean),cex=1.5)

