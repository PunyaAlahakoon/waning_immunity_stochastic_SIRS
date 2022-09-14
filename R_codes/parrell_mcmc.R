
library(foreach)
library(doParallel)
library(tmvtnorm)
library(R.matlab)
library(trialr)

numCores <- detectCores()
registerDoParallel(numCores) 

#spcify starting values for beta gamma mu:
N=10000# %number of MCMC iterations

me=readMat('ind_uniform_beta.mat')
me=me$beta.smc

ga=readMat('ind_uniform_gamma.mat')
ga=ga$gamma.smc

ep=readMat('ind_uniform_epsilon.mat')
ep=ep$epsilon.smc



#for hyper means 
l=c(0.5,0.1,0)
u=c(6,2.5,0.2)

#sigmas=
ls=c(0,0,0)
us=c(2.5,1,0.15)



weit<-function(l,u,me,ga,ep,hs,sig){
  # weights=c()
  output<- foreach (j=1:15,.packages="tmvtnorm",.combine=c) %dopar%  {
    x=matrix(c(me[,j],ga[,j],ep[,j]),ncol=3)
    pdp=dtmvnorm(x,hs,sig,lower=l,upper=u)
    b_u=dunif(me[,j],l[1],u[1])
    g_u=dunif(ga[,j],l[2],u[2])
    e_u=dunif(ep[,j],l[3],u[3])
    wki=pdp/(b_u*g_u*e_u)
    #wki=pdp
    #weights[j]=sum(wki)
    sum(wki[wki>0])
  }
  weit=prod(output)
  
  
}


hb=matrix(ncol=3,nrow=N)

hb_sig=matrix(ncol=3,nrow=N)
#ros=vector(mode = "list", length = N)
ros=matrix(ncol=9,nrow=N)
poste=c()

hb[1,1]=1.5
hb[1,2]=0.5
hb[1,3]=0.01

hb_sig[1,1]=0.5
hb_sig[1,2]=0.05
hb_sig[1,3]=0.01


#A=ros[1,1]*hb_sig[1,2]*hb_sig[1,1]
#H=ros[1,2]*hb_sig[1,3]*hb_sig[1,1]
#G=ros[1,3]*hb_sig[1,3]*hb_sig[1,3]

#sigmat=matrix(c(hb_sig[1,1]^2,A,H,A,hb_sig[1,2]^2,G,H,G,hb_sig[1,3]^2),ncol=3)

ss= diag(c(hb_sig[1,1],hb_sig[1,2],hb_sig[1,3]),nrow = 3)
R=rlkjcorr(1, 3, eta =2) #correlation matrix 
#R=diag(3)
ros[1,]<-as.vector(R)
sigmat=ss*R*ss
sigmai=sigmat
Ri=R
#sigmat=ss*ss

w=weit(l,u,me,ga,ep,hb[1,],sigmat)

prior=dunif(hb[1,1],l[1],u[1])*dunif(hb[1,2],l[2],u[2])*dunif(hb[1,3],l[3],u[3])*dunif(hb_sig[1,1],ls[1],us[1])*dunif(hb_sig[1,2],ls[2],us[2])*dunif(hb_sig[1,3],ls[3],us[3])

#MCMC 
for(i in 2:N){
  #sample means=
  r_b=abs(rmvnorm(1,c(hb[i-1,1],hb_sig[i-1,1]),diag(2)*0.0025))
  r_hb=r_b[1]
  r_sb=r_b[2]
  
  r_g=abs(rmvnorm(1,c(hb[i-1,2],hb_sig[i-1,2]),diag(2)*0.0025))
  r_hg=r_g[1]
  r_sg=r_g[2]
  
  r_e=abs(rmvnorm(1,c(hb[i-1,3],hb_sig[i-1,3]),diag(2)*0.00025))
  r_he=r_e[1]
  r_se=r_e[2]
  #r_hb=abs(rnorm(1,hb[i-1,1],0.1))
  #r_hg=abs(rnorm(1,hb[i-1,2],0.1))
  #r_he=abs(rnorm(1,hb[i-1,3],0.1))
  
 # r_sb=abs(rnorm(1,hb_sig[i-1,1],0.1))
 # r_sg=abs(rnorm(1,hb_sig[i-1,2],0.1))
  #r_se=abs(rnorm(1,hb_sig[i-1,3],0.01))
  
  rh=c(r_hb,r_hg,r_he)
  ss=diag(c(r_sb,r_sg,r_se))
  R=rlkjcorr(1, 3, eta = 2) #correlation matric 
  
  sigmat=ss*R*ss
  #sigmat=ss*ss
  
  
  w=weit(l,u,me,ga,ep,rh,sigmat)
  prior=dunif(r_hb,l[1],u[1])*dunif(r_hg,l[2],u[2])*dunif(r_he,l[3],u[3])*dunif(r_sb,ls[1],us[1])*dunif(r_sg,ls[2],us[2])*dunif(r_se,ls[3],us[3])
  
  ps=w*prior
  
  prop=ps/poste[i-1]
  
  alpha=min(1,prop)
  uu=runif(1)
  
  if (uu<alpha) {
    hb[i,1]=r_hb
    hb[i,2]=r_hg
    hb[i,3]=r_he
    hb_sig[i,1]=r_sb
    hb_sig[i,2]=r_sg
    hb_sig[i,3]=r_se
    poste[i]=ps
    ros[i,]<-as.vector(R)
  } else{
    
    hb[i,1]= hb[i-1,1]
    hb[i,2]=hb[i-1,2]
    hb[i,3]=hb[i-1,3]
    hb_sig[i,1]=hb_sig[i-1,1]
    hb_sig[i,2]=hb_sig[i-1,2]
    hb_sig[i,3]=hb_sig[i-1,3]
    poste[i]=poste[i-1] 
    ros[i,]<-ros[i-1,]
  }
}



write.csv(hb,'hb5.csv')
write.csv(hb_sig,'hb_sig5.csv')
write.csv(ros,'ros5.csv')
