%Step (1)--b
%use this to estimate the hyper-parameters (ppsi_beta, sigma_beta) for an SIRS model 
%kept epsilon and gamma fixed 
%add a unif prior for hyper-parameter  
%functions need to run this:
        %weight3 --CALCULATES THE ESTIMATED LIKELIHOOD---MAKE SURE TO CHANGE
        %THE CONDITIONAL PRIOR IF REQUIRED 
        N=1000; %number of MCMC iterations

hb=zeros(3,N); %mean hyper parameters for beta ----psi_beta
hb_sig=zeros(3,N);%std hyper-parameteer  ---- sigma_beta
ros=zeros(3,N);

hb(:,1)= [2 1 0.01];
hb_sig(:,1)=[0.5 0.1 0.1];
ros(:,1)=[0.5 0.5 0.5];
%1:3 means 4:6 stds, 7:9=correlations 
%psi_beta:


A=ros(1,1)*hb_sig(2,1)*hb_sig(1,1);
F=ros(2,1)*hb_sig(3,1)*hb_sig(1,1);
G=ros(3,1)*hb_sig(3,1)*hb_sig(2,1);

sigmat=[hb_sig(1,1)^2 A F; A hb_sig(2,1)^2 G; F G hb_sig(3,1)^2];


%load parameter sets of all the populations as matrices
me=beta_smc; %from Step (1)--a  
ga=gamma_smc;
ep=epsilon_smc;



%N=length(gamma);
B=size(me,1); %length of particles in Step (1)--a
dim=size(me,2); %number of sub-pops 

 


%store hyper-parameters 
hhb=zeros(3,N); %mean hyper parameters for beta ----psi_beta
hhb_sig=zeros(3,N);%std hyper-parameteer  ---- sigma_beta
%store probabilities of accepting 
p=zeros(3,N); %not a necessary step

%MCMC initialisation 
hhb(:,1)= [b_hb(1) g_hb(1) e_hb(1)];
hhb_sig(:,1)=[b_sig(1) g_sig(1) e_sig(1)];

%calculate the weight: 
r=[1.5 0.7 0]; s=[3.5 1.5 .2]; %truncated interval for truncated normal 



w=weight7(me,ga,ep,hb(:,1),sigmat,dim,r,s);

p(1)=w*unifpdf(hhb(1,1),r(1),s(1))*unifpdf(hhb_sig(1,1),0,2.5)*...
    unifpdf(hhb(2,1),r(2),s(2))*unifpdf(hhb_sig(2,1),0,1)*...
    unifpdf(hhb(3,1),r(3),s(3))*unifpdf(hhb_sig(3,1),0,0.15);

%MCMC 
for i=2:N
   %propose values from beta and gamma matrices as vectors 

   hm_r=random(tpd1(hb(1,i-1)));
   sig_r=abs(normrnd(hb_sig(1,i-1),0.1));
    
   hg_r=random(tpd2(hb(2,i-1)));
   sigg_r=abs(normrnd(hb_sig(2,i-1),0.1));

   he_r=abs(normrnd(hb(3,i-1),0.01));
   sige_r=abs(normrnd(hb_sig(3,i-1),0.01));

   hh=[hm_r hg_r he_r]; ss=[sig_r sigg_r sige_r];
   w=weight6(me,ga,ep,hh,ss,dim,r,s);
   prior=unifpdf(hm_r,r(1),s(1))*unifpdf(sig_r,0,2.5)*...
       unifpdf(hg_r,r(2),s(2))*unifpdf(sigg_r,0,1)*...
       unifpdf(he_r,r(3),s(3))*unifpdf(sige_r,0,0.15);
   
   p_s=w*prior;
   prop=p_s/p(i-1);

   alpha=min(1,prop);
   u=rand(1);
   
   if u<alpha
       hhb(:,i)=hh;
       hhb_sig(:,i)=ss;
       p(i)=p_s;
      
   else
       hhb(:,i)=hhb(:,i-1);
        hhb_sig(:,i)=hhb_sig(:,i-1);
       p(i)=p(i-1);
   end
end


figure(1)
plot(1:N,hhb_sig);
 %   yline(10,'--','color','red','LineWidth',1);
    title('Trace plot for the hyper-parameter (mean) under the MCMC method') 
    
figure(2)
plot(1:N,hhb);
    xline(median(hhb),'--','color','blue','LineWidth',1);
    %xline(10,'--','color','red','LineWidth',1);
 title('Posterior of the hyper-parameter (mean) under the MCMC method')
 
%save('hb.mat','hb');
%save('hb_sig.mat','hb_sig');
 
