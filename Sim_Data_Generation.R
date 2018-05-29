rm(list=ls())
#with intercept factor model.
#prec linked to latent instead of HR HER2 subgroup

library("MASS")
library("LaplacesDemon")

set.seed(100)

#p1,p2,p3,p4 refer to the GMM prob for four HR and HER2 subgroups
p1<-c(0.7,0.05,0.05,0.2)
p2<-c(0.5,0.4,0.05,0.05)
p3<-c(0.05,0.1,0.7,0.15)
p4<-c(0.1,0.05,0.05,0.8)


D=4 # num of factors
t=10 #how much folds to increase the sample size
N=300*t # num of obs
P=10 # num of variables
N1=70*t #partition of N into 4 subgroups
N2=100*t
N3=50*t
N4=80*t

# F mean and variance
mu_F <-rep(0,D) # the mean of factors
Phi<-diag(rep(1,D)) # the variance of factors

# Factors F, denoted as Fa
Fa<-mvrnorm(N, mu_F, Phi)

# Tau, the intercept of mu factor analysis
# Tau1<- rnorm(P,1,2)
# Tau2<- rnorm(P,5,2)
# Tau3<- rnorm(P,10,2)
# Tau4<- rnorm(P,15,2)
Tau1<-0
Tau2<-0
Tau3<-0
Tau4<-0

# mean of epsilon 
mu_epsilon<-rep(0,P)

# variance of epsilon, avoid for same Psi by setting sightly different mean
Psi1<-diag(abs(runif(P,0.00,2)))
Psi2<-diag(abs(runif(P,0.01,2)))
Psi3<-diag(abs(runif(P,0.02,2)))
Psi4<-diag(abs(runif(P,0.03,2)))

# Epsilon
Epsilon1<-mvrnorm(N,mu_epsilon,Psi1)
Epsilon2<-mvrnorm(N,mu_epsilon,Psi2)
Epsilon3<-mvrnorm(N,mu_epsilon,Psi3)
Epsilon4<-mvrnorm(N,mu_epsilon,Psi4)

# L, the factor loading of factor analysis, with dim P*D
Gen_L<-function(seed){
  set.seed(seed)
  L<-matrix(0,nrow=10,ncol=4)
  L[1:3,1]<-runif(3,0,10)
  L[4:5,2]<-runif(2,0,10)
  L[6:8,3]<-runif(3,0,10)
  L[9:10,4]<-runif(2,0,10)
  return(L)
}

L1<-Gen_L(1)
L2<-Gen_L(2)
L3<-Gen_L(3)
L4<-Gen_L(4)

#we dont really use all the components in Y1,Y2,Y3 and Y4, we will just pick those based on latent grouping info.


Y1<-Tau1+Fa%*%t(L1)+Epsilon1
apply(Y1,2,mean)
apply(Y1,2,var)
Y2<-Tau2+Fa%*%t(L2)+Epsilon2
apply(Y2,2,mean)
apply(Y2,2,var)
Y3<-Tau3+Fa%*%t(L3)+Epsilon3
apply(Y3,2,mean)
apply(Y3,2,var)
Y4<-Tau4+Fa%*%t(L4)+Epsilon4
apply(Y4,2,mean)
apply(Y4,2,var)

#in N1, N2, N3 and N4, the obs belongings in terms of latent grouping.
#each latent distribution follows N(mu,Psi)
LC1<-rcat(N1, p1)
LC2<-rcat(N2, p2)
LC3<-rcat(N3, p3)
LC4<-rcat(N4, p4)

# save.image("Data_Generation_Parameters.RData")

#Pick Y from Y1,Y2,Y3,Y4.

Y<-matrix(NA,nrow=N,ncol=P)

#Also control the variation, lower the variance for Tau and Psi
#####

for(i in 1:N1){
  Y[i,]<-get(paste("Y",LC1[i],sep=""))[i,]
}

for(i in 1:N2){
  Y[N1+i,]<-get(paste("Y",LC2[i],sep=""))[i,]
}

for(i in 1:N3){
  Y[N1+N2+i,]<-get(paste("Y",LC3[i],sep=""))[i,]
}

for(i in 1:N4){
  Y[N1+N2+N3+i,]<-get(paste("Y",LC4[i],sep=""))[i,]
}

cov(Y[1:N1,])
cov(Y[(N1+1):(N1+N2),])
cov(Y[(N1+N2+1):(N1+N2+N3),])
cov(Y[(N1+N2+N3+1):N,])

# write.table(Y,file="Sim_Data.txt")