library(rjags)
load(file="sim_jmsample_3000_10.RData")

load(file="Data_Generation_Parameters.RData")

plot(as.mcmc.list(jm.sample$P1)[,1])
plot(as.mcmc.list(jm.sample$P1)[,2])
plot(as.mcmc.list(jm.sample$P1)[,3])
plot(as.mcmc.list(jm.sample$P1)[,4])


P1_results=lapply(as.mcmc.list(jm.sample$P1),colMeans)
P2_results=lapply(as.mcmc.list(jm.sample$P2),colMeans)
P3_results=lapply(as.mcmc.list(jm.sample$P3),colMeans)
P4_results=lapply(as.mcmc.list(jm.sample$P4),colMeans)

p1;P1_results # chain 2,3
p2;P2_results # chain 2,3
p3;P3_results # chain 2,3
p4;P4_results # chain 2,3

L1_est<-lapply(as.mcmc.list(jm.sample$L1),colMeans)
L1_est<-lapply(L1_est, function(x){round(x,0)})
L1_Mtx<-lapply(L1_est,function(x){return(matrix(x,nrow = 10,ncol=4,byrow = F))})
L1_Mtx[[1]]
L1_Mtx[[2]]
L1_Mtx[[3]] #chain 3 close except [6,3],[7,3],[8,3]
L1<-round(L1,0);L1

L2_est<-lapply(as.mcmc.list(jm.sample$L2),colMeans)
L2_est<-lapply(L2_est, function(x){round(x,0)})
L2_Mtx<-lapply(L2_est,function(x){return(matrix(x,nrow = 10,ncol=4,byrow = F))})
L2_Mtx[[1]]
L2_Mtx[[2]] #chain 2 close
L2_Mtx[[3]]
L2<-round(L2,0);L2

L3_est<-lapply(as.mcmc.list(jm.sample$L3),colMeans)
L3_est<-lapply(L3_est, function(x){round(x,0)})
L3_Mtx<-lapply(L3_est,function(x){return(matrix(x,nrow = 10,ncol=4,byrow = F))})
L3_Mtx[[1]]
L3_Mtx[[2]]
L3_Mtx[[3]] #chian 3 close
L3<-round(L3,0);L3

L4_est<-lapply(as.mcmc.list(jm.sample$L4),colMeans)
L4_est<-lapply(L4_est, function(x){round(x,0)})
L4_Mtx<-lapply(L4_est,function(x){return(matrix(x,nrow = 10,ncol=4,byrow = F))})
L4_Mtx[[1]]
L4_Mtx[[2]] #chain 2 close
L4_Mtx[[3]]
L4<-round(L4,0);L4

L2_Mtx[[1]]; L3_Mtx[[2]]; L4_Mtx[[1]]

L1_Mtx[[2]]; L3_Mtx[[1]]; L4_Mtx[[3]]

L1_Mtx[[1]]; L2_Mtx[[3]] 

#rotation
#and non-identifiable issue.

########################################
plot(as.mcmc.list(jm.sample$L1))
x1<-as.mcmc.list(jm.sample$L1)

# x1[[1]][1,]

plot(x1[,26]) #L1[6,3]
plot(x1[,27]) #L1[7,3]
plot(x1[,28]) #L1[8,3]

# jm.sample$L1
# L1     
