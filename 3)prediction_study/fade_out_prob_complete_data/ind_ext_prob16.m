%define the model 
%construct the SIRS model 
s0 =999;
i0 = 1;
r0=0;
ini_state=[s0 i0 r0]; %initial population sizes in each compartment
stoi= [-1 1 0;0 -1 1;1 0 -1]; %stoichimetry matrix 
st_time = 0; %start time to consider 
en_time=35; % make sure the end time is always >>> time perrios of the first trough

model_setup.ini_state=ini_state;
model_setup.time=st_time;
model_setup.end_time=en_time;
model_setup.stoi=stoi;



B=15000; %# of simulations per parameter set 

N=sum(ini_state);% population size 
%betas=linspace(1,4,5000);


betas=load('ind_inf_beta_comp.mat');
betas=betas.beta_smc;
betas=betas(:,1);

gamma=load('ind_inf_gamma_comp.mat');
gamma=gamma.gamma_smc;
gamma=gamma(:,1);

mu=load('ind_inf_epsilon_comp.mat');
mu=mu.epsilon_smc;
mu=mu(:,1);

n=length(betas);

ext_prob=zeros(1,n); % strore the Monte Carlo estimates of the prob. in the 1st trough 
parfor i=1:1
    %specificatins that dpend on parameters 
   % par=[2.6355 1.0169 0.0642];
    par=[betas(i) gamma(i) mu(i)];
    Ri = {@(n) par(1)*n(1)*n(2)/(sum(n(1)+n(2)+n(3))-1);...
            @(n) par(2)*n(2);@(n) (par(3))*n(3)}; %reactions for par1
     %model_setup.R=Ri;
     
    %extinction in the trough criteria 
    Sd=nearest(N*par(2)/par(1)); % endemic points 
    Id=nearest((N*par(3)*(par(1)-par(2)))/(par(1)*(par(2)+par(3)))); %endmic points 

    %give conditions for entering the first trough and leaving the first
    %trough 
     con1= @(n) ((n(1)<Sd) & (n(2)==Id));%entered the first trough 
     con2= @(n) ((n(2)==2*Id)); %left the first trough 
     con3=@(n) ((n(1)>=Sd) & (n(2)>=Id)); %left the first trough 
     
     trough_cond=struct('con1',con1,'con2',con2,'con3',con3);
     
     ext_cond=@(n) n(2)==0; %Extinction criteria; 
     
    ext_prob(i)=trough3(model_setup,Ri,trough_cond,ext_cond,B);
    %    ext_prob(i)=trough6(model_setup,Ri,trough_cond,ext_cond);


end

save('ext_prob_ind_pop16_comp.mat','ext_prob')

