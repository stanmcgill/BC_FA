setwd("H:/BC_5")
load(file="sim_jmsample_5_1.RData")

p1<-c(0.7,0.05,0.05,0.2)
p2<-c(0.5,0.4,0.05,0.05)
p3<-c(0.05,0.1,0.7,0.15)
p4<-c(0.1,0.05,0.05,0.8)

plot(as.mcmc.list(jm.sample$P1)[,1])
plot(as.mcmc.list(jm.sample$P1)[,2])
plot(as.mcmc.list(jm.sample$P1)[,3])
plot(as.mcmc.list(jm.sample$P1)[,4])


P1_results=lapply(as.mcmc.list(jm.sample$P1),colMeans)
P2_results=lapply(as.mcmc.list(jm.sample$P2),colMeans)
P3_results=lapply(as.mcmc.list(jm.sample$P3),colMeans)
P4_results=lapply(as.mcmc.list(jm.sample$P4),colMeans)

sink("P_Sim_results_5_1.txt")
print("P1")
P1_results
p1
print("P2")
P2_results
p2
print("P3")
P3_results
p3
print("P4")
P4_results
p4
sink()