%Step (1)--b



%use this to estimate the hyper-parameters (ppsi_beta, sigma_beta) for an SIRS model 

%add a unif prior for hyper-parameter  
%functions need to run this:
        %weight6


%load parameter sets of all the populations as matrices
me=beta_smc; %from Step (1)--a  
ga=gamma_smc;
ep=epsilon_smc;

%N=length(gamma);
B=size(me,1); %length of particles in Step (1)--a
dim=size(me,2); %number of sub-pops 

N=10000; %number of MCMC iterations 


%store hyper-parameters 
hb=zeros(3,N); %mean hyper parameters for beta ----psi_beta
hb_sig=zeros(3,N);%std hyper-parameteer  ---- sigma_beta
%store probabilities of accepting 
p=zeros(3,N); %not a necessary step

%MCMC initialisation 
hb(:,1)= [2 0.7 0.03];
hb_sig(:,1)=[0.3 0.13 0.05];
%calculate the weight: 
r=[0.001 0.00001 0]; s=[10 3 .2]; %truncated interval for truncated normal 
pd=@(m) makedist('Normal','mu',m,'sigma',0.0003);
tpd=@(m) truncate(pd(m),r(3),s(3));

w=weight6(me,ga,ep,hb(:,1),hb_sig(:,1),dim,r,s);

p(1)=w*unifpdf(hb(1,1),r(1),s(1))*unifpdf(hb_sig(1,1),0,2.5)*...
    unifpdf(hb(2,1),r(2),s(2))*unifpdf(hb_sig(2,1),0,1)*...
    unifpdf(hb(3,1),r(3),s(3))*unifpdf(hb_sig(3,1),0,0.15);

%sigb=[1 0.0815;0.0815 1];


%MCMC 
for i=2:N
   %propose values from beta and gamma matrices as vectors 
xs_hb=zeros(1,3); x_sig=zeros(1,3);
   parfor j=1:3
       if j==1
        hB=abs(mvnrnd([hb(j,i-1) hb_sig(j,i-1)],eye(2)*0.003));
        hm_r= hB(1); sig_r=hB(2);
        hh=[hm_r hb(2:3,i-1)']; ss=[sig_r hb_sig(2:3,i-1)'];
       elseif j==2
        hB=abs(mvnrnd([hb(j,i-1) hb_sig(j,i-1)],eye(2)*0.003));
        hm_r= hB(1); sig_r=hB(2);
        hh=[hb(1,i-1) hm_r hb(3,i-1)]; ss=[hb_sig(1,i-1) sig_r hb_sig(3,i-1)];  
       else
       hB=abs(mvnrnd([hb(j,i-1) hb_sig(j,i-1)],eye(2)*0.0003));
        hm_r= hB(1); sig_r=hB(2);
        hh=[hb(1:2,i-1)' hm_r]; ss=[hb_sig(1:2,i-1)' sig_r];      
       end

   w=weight6(me,ga,ep,hh,ss,dim,r,s);

   prior=unifpdf(hh(1),r(1),s(1))*unifpdf(ss(1),0,2.5)*...
       unifpdf(hh(2),r(2),s(2))*unifpdf(ss(2),0,1)*...
       unifpdf(hh(3),r(3),s(3))*unifpdf(ss(3),0,0.15);
   
   p_s=w*prior;
   prop=p_s/p(i-1);

   alpha=min(1,prop);
   u=rand(1);
   
   if u<alpha
       xs_hb(j)=hm_r;
       x_sig(j)=sig_r;
     %p(i)=p_s;
      
   else
      xs_hb(j)=hb(j,i-1);
        x_sig(j)=hb_sig(j,i-1);
       %p(i)=p(i-1);
   end
   end

hb(:,i)=xs_hb;
hb_sig(:,i)=x_sig;

  w=weight6(me,ga,ep,hb(:,i),hb_sig(:,i),dim,r,s);

   prior=unifpdf(hb(1,i),r(1),s(1))*unifpdf(hb_sig(1,i),0,2.5)*...
       unifpdf(hb(2,i),r(2),s(2))*unifpdf(hb_sig(2,i),0,1)*...
       unifpdf(hb(3,i),r(3),s(3))*unifpdf(hb_sig(3,i),0,0.15);
   
   p_ss=w*prior;
   p(i)=p_ss;

end


figure(1)
plot(1:N,hb_sig);
 %   yline(10,'--','color','red','LineWidth',1);
    title('Trace plot for the hyper-parameter (mean) under the MCMC method') 
    
figure(2)
plot(1:N,hb);
    xline(median(hb),'--','color','blue','LineWidth',1);
    %xline(10,'--','color','red','LineWidth',1);
 title('Posterior of the hyper-parameter (mean) under the MCMC method')
 
save('hb_all.mat','hb');
save('hb_sig_all.mat','hb_sig');
 
