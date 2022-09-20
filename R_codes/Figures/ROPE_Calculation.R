library(R.matlab)
Sys.setenv(JAGS_HOME="C:\\Program Files\\JAGS\\JAGS-4.3.0")
library(BEST)
library(ggplot2)

pop_names=c(sprintf('%d ',1:3), "4*", sprintf('%d ',5:7),
            "8*", "9","10*","11*","12*",
            "13","14*","15*")
#pop_names=subs
u_beta=readMat('ind_inf_beta.mat')
u_beta=u_beta$beta.smc

u_beta=data.frame(u_beta,rep("Independent estimation",5000))
colnames(u_beta)<-c(pop_names,"Method")

i_beta=readMat('hie_inf_beta.mat')
i_beta=i_beta$beta.smc

i_beta=data.frame(i_beta,rep("Hierarchical estimation",5000))
colnames(i_beta)=c(pop_names,"Method")

u_epsilon=readMat('ind_inf_epsilon.mat')
u_epsilon=u_epsilon$epsilon.smc


u_epsilon=data.frame(u_epsilon,rep("Independent estimation",5000))
colnames(u_epsilon)<-c(pop_names,"Method")

i_epsilon=readMat('hie_inf_epsilon.mat')
i_epsilon=i_epsilon$epsilon.smc

i_epsilon=data.frame(i_epsilon,rep("Hierarchical estimation",5000))
colnames(i_epsilon)=c(pop_names,"Method")

t_betas=readMat('true_betas.mat')
t_betas=t_betas$t.betas

t_epsilons=readMat('true_mus.mat')
t_epsilons=t_epsilons$t.mus

i=1 #i is the sub-population number. change this to study each sub-population

par(mfrow=c(3,2))

ind_beta=u_beta[,i]
hie_beta=i_beta[,i]
ind_eps=u_epsilon[,i]
hie_eps=i_epsilon[,i]

#png("2ROPES_sub-pop.png", width = 500, height = 1000)
plotPost(ind_beta, credMass = 0.95, compVal=t_betas[i], ROPE=c(t_betas[i]-0.5, t_betas[i]+.5),HDItextPlace = 1, showMode = T,showCurve=T, xlim=c(0.5,5.5),xlab=expression(beta),main="Independent estimation \n (Transmission rate)")
plotPost(hie_beta, credMass = 0.95, compVal=t_betas[i], ROPE=c(t_betas[i]-.5, t_betas[i]+.5),HDItextPlace = 1, showMode = T,showCurve=T, xlim=c(0.5,5.5),xlab=expression(beta),main="Hierarchical estimation \n (Transmission rate)" )
plotPost(ind_eps, credMass = 0.95, compVal=t_epsilons[i], ROPE=c(t_epsilons[i]-.025, t_epsilons[i]+.025),HDItextPlace = 1, showMode = T,showCurve=T, xlim=c(0,0.2),xlab=expression(mu),main="Independent estimation \n (Waning immunity rate)")
plotPost(hie_eps, credMass = 0.95, compVal=t_epsilons[i], ROPE=c(t_epsilons[i]-.025, t_epsilons[i]+.025),HDItextPlace = 1, showMode = T,showCurve=T, xlim=c(0,0.2),xlab=expression(mu),main="Hierarchical estimation \n (Waning immunity rate)")


#gammas


u_epsilon=readMat('ind_inf_gamma.mat')
u_epsilon=u_epsilon$gamma.smc


u_epsilon=data.frame(u_epsilon,rep("Independent estimation",5000))
colnames(u_epsilon)<-c(pop_names,"Method")

i_epsilon=readMat('hie_inf_gamma.mat')
i_epsilon=i_epsilon$gamma.smc

i_epsilon=data.frame(i_epsilon,rep("Hierarchical estimation",5000))
colnames(i_epsilon)=c(pop_names,"Method")



t_epsilons=readMat('true_gammas.mat')
t_epsilons=t_epsilons$t.gammas


i=15
ind_eps=u_epsilon[,i]
hie_eps=i_epsilon[,i]

par(mfrow=c(1,2))
plotPost(ind_eps, credMass = 0.95, compVal=t_epsilons[i], ROPE=c(t_epsilons[i]-.15, t_epsilons[i]+.15),HDItextPlace = 1, showMode = T,showCurve=T, xlim=c(0,2),xlab=expression(mu),main="Independent estimation \n (Waning immunity rate)")
plotPost(hie_eps, credMass = 0.95, compVal=t_epsilons[i], ROPE=c(t_epsilons[i]-.15, t_epsilons[i]+.15),HDItextPlace = 1, showMode = T,showCurve=T, xlim=c(0,2),xlab=expression(mu),main="Hierarchical estimation \n (Waning immunity rate)")

