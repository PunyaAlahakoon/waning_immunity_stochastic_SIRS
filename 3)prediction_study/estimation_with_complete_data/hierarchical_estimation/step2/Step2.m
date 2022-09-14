%Step 2
%use this to estimate sub-population specific parameters for an SIRS model
%given hyper-parameters 
%functions need to run this:
        %abc_hie2
        
% conststruct a parellel version for several populations 
tic 
%This is for an SIRS model  for two parameters  
%Number of particles/ parameters sets to run 
B=5000; 
data=load('sc2_data.mat', 'data'); % load the dataset you need
y_all=data.data; % you can insert data with different number of observed points
                    %in each sub-population
y_all=y_all(1:15,:);
dim=size(y_all,2); %number of pos
T=size(y_all,1);



    %load hyper-parameters 
    hb=load('hb_all.mat', 'hb'); %all the hyper_means ,beta, gamma, epsilon order 
    hb=hb.hb;
    hb=hb(:,5001:10000); 
  
    hb_sig=load('hb_sig_all.mat', 'hb_sig'); %all the hyper_sigams 
    hb_sig=hb_sig.hb_sig;
    hb_sig=hb_sig(:,5001:10000);
 

%m=1;
%the model and initial conditions: SIRS   
%the model and initial conditions: SIRS    
%initial conditions 
s0 =999; % #of  susceptibles 
i0 = 1; % # of infectious 
r0=0;% # of recovered 
ini_state=[s0 i0 r0]; %initial sizes in each compartment

stoi= [-1 1 0;0 -1 1;1 0 -1]; %stoichimetry matrix 
time = 0; %start time to consider 
stp1= @(n) n(2)==0; %stopping criteria  


eta=1; %number of samples to generate for each parameter set 

%store the final smc-proposed parameters and other needed values in a matrix 
beta_smc=zeros(B,dim);
gamma_smc=zeros(B,dim);
epsilon_smc=zeros(B,dim);
w_smc=zeros(B,dim); %weights 
AG_smc=zeros(1,dim);%number of steps generated to get B parameters 
s_x= zeros(B,dim);%save the distance criteria

for k=1:2
    %consider the kth population  
    y=y_all(:,k); %convert the array to a vector
    %stopping criteria, 
    T=length(y);
    stp2=T;
    t_seq=1:T;
    %Store the sets of tolerance values 
    E=load('E.mat','B');
    E=E.B;
    E=E(7,:);
    e=E(k);
    %E(1)=e;

      
    %store values for the current generation
    params0=zeros(B,3); %store the cluster based beta values from posterior 
    ag=0;%set the counter 
    ag0=zeros(1,B);%set the counter 
    rho_m=zeros(1,B);%store the distance values 

     parfor a=1:B %particle number     
     [params0(a,:),rho_m(a),ag0(a)]=abc_hie2(hb(:,a),hb_sig(:,a),y,e,ini_state,stoi,time,stp1,stp2,eta);
     end 
        s_xx= rho_m';%save the distance criteria
        params=params0; %store the cluster based beta values from posterior 
        AG=sum(ag0);
        
%store the final values of the population k
beta_smc(:,k)=params(:,1);
gamma_smc(:,k)=params(:,2);
epsilon_smc(:,k)=params(:,3);
AG_smc(k)=AG;%number of steps generated to get B parameters 
s_x(:,k)=s_xx;
k+1
end

save('s_x_smc.mat','s_x');
save('hie_inf_pred_beta.mat','beta_smc');
save('hie_inf_pred_gamma.mat','gamma_smc');
save('hie_inf_pred_epsilon.mat','epsilon_smc');
save('AG_smc_hie.mat','AG_smc');
save('w_smc_hie.mat','w_smc');

toc


