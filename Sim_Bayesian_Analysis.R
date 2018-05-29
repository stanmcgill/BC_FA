rm(list=ls())
library(coda)
library(rjags) 
library(R2jags)
setwd("H:/BC_5")
Y<-read.table("Sim_Data_5_1.txt")

#with intercept factor model.
#prec linked to latent instead of HR HER2 subgroup
#model cov not mu.

cat("
    model{
    
    #################################
    #likelihood
    #################################
    #N1
    
    for(i in 1:N1 ) {					# Individual N1=(HR-, HER2-) 
    
    Z[i] ~ dcat(P1[])
    
	  # y[i,1:M]~dmnorm(mu[1:M],get(paste('prec',Z[i],sep='')))

    y[i,1:M]~dmnorm(mu[1:M],prec[Z[i],1:M,1:M]) 
    
    }
    
    #N2
    
    for(i in (N1+1):(N1+N2) ) {					# Individual N2=(HR-, HER2+) 
    
    Z[i] ~ dcat(P2[])
    
	  # y[i,1:M]~dmnorm(mu[1:M],get(paste('prec',Z[i],sep='')))
    y[i,1:M]~dmnorm(mu[1:M],prec[Z[i],1:M,1:M]) 
    }
    
    #N3
    
    for(i in (N1+N2+1):(N1+N2+N3) ) {					# Individual N3=(HR+, HER2-) 
    
    Z[i] ~ dcat(P3[])
    
    # y[i,1:M]~dmnorm(mu[1:M],get(paste('prec',Z[i],sep='')))
    y[i,1:M]~dmnorm(mu[1:M],prec[Z[i],1:M,1:M]) 
    }
    
    #N4
    
    for(i in (N1+N2+N3+1):N ) {					# Individual N4=(HR+, HER2+) 
    
    Z[i] ~ dcat(P4[])
    
    # y[i,1:M]~dmnorm(mu[1:M],get(paste('prec',Z[i],sep='')))
    y[i,1:M]~dmnorm(mu[1:M],prec[Z[i],1:M,1:M])    
    }
    
    #####################################
    #mu is zero factor
    #####################################
    
	  for(j in 1:M){
	  mu[j]=0
	  }
     
    #####################################
    #prec/var as factor analysis model
    #####################################

    prec[1,1:M,1:M]<-inverse(vari1[1:M,1:M])
    vari1[1:M,1:M]<-L1[1:M,1:D]%*%t(L1[1:M,1:D])+psi[1:M,1:M]    

    prec[2,1:M,1:M]<-inverse(vari2[1:M,1:M])
    vari2[1:M,1:M]<-L2[1:M,1:D]%*%t(L2[1:M,1:D])+psi[1:M,1:M]    

    prec[3,1:M,1:M]<-inverse(vari3[1:M,1:M])
    vari3[1:M,1:M]<-L3[1:M,1:D]%*%t(L3[1:M,1:D])+psi[1:M,1:M]    
   
    prec[4,1:M,1:M]<-inverse(vari4[1:M,1:M])
    vari4[1:M,1:M]<-L4[1:M,1:D]%*%t(L4[1:M,1:D])+psi[1:M,1:M]    

  	 
    ######################################
    #Hyperprior of prec/psi and tau
    ######################################
    
    for (j in 1:M){
    psi[j,j]<-1/inv.psi[j]
	  inv.psi[j]~dgamma(0.001,0.001)
    }
    
    ######################################
    #prior of L1,L2,L3,L4
    ######################################
    
    #L1
    for(j in 1:3){
    L1[j,2]<-0
    L1[j,3]<-0
    L1[j,4]<-0
    }
    for(j in 4:5){
    L1[j,1]<-0
    L1[j,3]<-0
    L1[j,4]<-0
    }
    for(j in 6:8){
    L1[j,1]<-0
    L1[j,2]<-0
    L1[j,4]<-0
    }
    for(j in 9:10){
    L1[j,1]<-0
    L1[j,2]<-0
    L1[j,3]<-0
    }
    
    for(j in 2:3){
    L1[j,1]~dnorm(0,0.01)
    }
    for(j in 5:5){
    L1[j,2]~dnorm(0,0.01)
    }
    for(j in 7:8){
    L1[j,3]~dnorm(0,0.01)
    }
    for(j in 10:10){
    L1[j,4]~dnorm(0,0.01)
    }
    
    L1[1,1]~dunif(0,10)
    L1[4,2]~dunif(0,10)
    L1[6,3]~dunif(0,10)
    L1[9,4]~dunif(0,10)
    
    #L2
    for(j in 1:3){
    L2[j,2]<-0
    L2[j,3]<-0
    L2[j,4]<-0
    }
    for(j in 4:5){
    L2[j,1]<-0
    L2[j,3]<-0
    L2[j,4]<-0
    }
    for(j in 6:8){
    L2[j,1]<-0
    L2[j,2]<-0
    L2[j,4]<-0
    }
    for(j in 9:10){
    L2[j,1]<-0
    L2[j,2]<-0
    L2[j,3]<-0
    }
    
    for(j in 2:3){
    L2[j,1]~dnorm(0,0.01)
    }
    for(j in 5:5){
    L2[j,2]~dnorm(0,0.01)
    }
    for(j in 7:8){
    L2[j,3]~dnorm(0,0.01)
    }
    for(j in 10:10){
    L2[j,4]~dnorm(0,0.01)
    }
    
    L2[1,1]~dunif(0,10)
    L2[4,2]~dunif(0,10)
    L2[6,3]~dunif(0,10)
    L2[9,4]~dunif(0,10)
    
    #L3
    for(j in 1:3){
    L3[j,2]<-0
    L3[j,3]<-0
    L3[j,4]<-0
    }
    for(j in 4:5){
    L3[j,1]<-0
    L3[j,3]<-0
    L3[j,4]<-0
    }
    for(j in 6:8){
    L3[j,1]<-0
    L3[j,2]<-0
    L3[j,4]<-0
    }
    for(j in 9:10){
    L3[j,1]<-0
    L3[j,2]<-0
    L3[j,3]<-0
    }
    
    for(j in 2:3){
    L3[j,1]~dnorm(0,0.01)
    }
    for(j in 5:5){
    L3[j,2]~dnorm(0,0.01)
    }
    for(j in 7:8){
    L3[j,3]~dnorm(0,0.01)
    }
    for(j in 10:10){
    L3[j,4]~dnorm(0,0.01)
    }
    
    L3[1,1]~dunif(0,10)
    L3[4,2]~dunif(0,10)
    L3[6,3]~dunif(0,10)
    L3[9,4]~dunif(0,10)
    
    
    #L4
    for(j in 1:3){
    L4[j,2]<-0
    L4[j,3]<-0
    L4[j,4]<-0
    }
    for(j in 4:5){
    L4[j,1]<-0
    L4[j,3]<-0
    L4[j,4]<-0
    }
    for(j in 6:8){
    L4[j,1]<-0
    L4[j,2]<-0
    L4[j,4]<-0
    }
    for(j in 9:10){
    L4[j,1]<-0
    L4[j,2]<-0
    L4[j,3]<-0
    }
    
    for(j in 2:3){
    L4[j,1]~dnorm(0,0.01)
    }
    for(j in 5:5){
    L4[j,2]~dnorm(0,0.01)
    }
    for(j in 7:8){
    L4[j,3]~dnorm(0,0.01)
    }
    for(j in 10:10){
    L4[j,4]~dnorm(0,0.01)
    }
    
    L4[1,1]~dunif(0,10)
    L4[4,2]~dunif(0,10)
    L4[6,3]~dunif(0,10)
    L4[9,4]~dunif(0,10)
    
    
    ######################################
    #P1,P2,P3,P4
    ######################################
    P1[1:LC] ~ ddirch(alpha1[])
    P2[1:LC] ~ ddirch(alpha2[])
    P3[1:LC] ~ ddirch(alpha3[])
    P4[1:LC] ~ ddirch(alpha4[])
    
    }
    
    ",file="FA_sim_5.txt")

jags.params<-c("P1","P2","P3","P4","L1","L2","L3","L4","psi","tau1",
               "tau2","tau3","tau4","prec")

N1=70
N2=100
N3=50
N4=80
N=300
LC=4
M=10
D=4

jagsdata<-list()
t=10
jagsdata$alpha1=c(1,1,1,1)
jagsdata$alpha2=c(1,1,1,1)
jagsdata$alpha3=c(1,1,1,1)
jagsdata$alpha4=c(1,1,1,1)
jagsdata$N1=N1*t
jagsdata$N2=N2*t
jagsdata$N3=N3*t
jagsdata$N=N*t
jagsdata$LC=LC
jagsdata$M=M
jagsdata$D=D
jagsdata$y=Y
jagsdata$psi=diag(M)
diag(jagsdata$psi)<-NA



jagsfit<-jags.model("FA_sim_5.txt", data = jagsdata, n.chains = 3)
jags.params<-c("P1","P2","P3","P4","L1","L2","L3","L4","psi")
update(jagsfit, n.iter = 3000)
jm.sample <- jags.samples(jagsfit, variable.names = jags.params, n.iter = 10000)

save(jm.sample,file="sim_jmsample_5_1.RData")
