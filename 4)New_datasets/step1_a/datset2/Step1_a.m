%Step (1)--a of the two step algorithm 
%use this to estimate betas of an SIRS model  using ABC-SMC by Toni et al 
% conststruct a parellel version for several populations 
%functions need to run this:
    %Gillespie4-- The Gillespie algorithm
    %sample_gen --- reads data at discrete times
    %abc_ind --- ABC step--- 
    
tic 
%load data
data=load('sc2_data.mat', 'data'); % load the dataset you need
y_all=data.data; % you can insert data with different number of observed points
                    %in each sub-population
y_all=y_all(1:35,:);
%dim=size(y_all,2);%number of sub-populations%
dim=15;
%the model and initial conditions: SIRS    
%initial conditions 
s0 =999; % #of  susceptibles 
i0 = 1; % # of infectious 
r0=0;% # of recovered 
ini_state=[s0 i0 r0]; %initial sizes in each compartment

stoi= [-1 1 0;0 -1 1;1 0 -1]; %stoichimetry matrix 
time = 0; %start time to consider 
stp1= @(n) n(2)==0; % 1st stopping criteria for Gillespie (i.e., stop the algorithm when the #of infectious=0)


%Number of particles/ parameters sets to sample using ABC-SMC
B=5000; 

%ABC-SMC initilisation 
%number of generations to run 
G=7; 
eta=1; %number of samples to generate for each parameter set 

%store the final smc-sampled parameters and other needed values in a matrix 
beta_smc=zeros(B,dim);
gamma_smc=zeros(B,dim);
epsilon_smc=zeros(B,dim);
w_smc=zeros(B,dim); % store weights
 Es=load('E.mat','ans');
 Es=Es.ans;
 Es=Es';
E_smc=zeros(G+1,dim);%tolerance values-- not neccssary unless you decide to
                       %use dynamic methods to calculate the tolarance 
AG_smc=zeros(G,dim);%store the # of particles generated to get B parameters(if needed) 
s_x= zeros(B,dim);%save the distance criteria (if needed)


for k=1:dim
    %consider the kth population  
    y=y_all(:,k); %convert the array to a vector
    %stopping criteria, 
    T=length(y);
    stp2=T; % 2nd stopping criteria for Gillespie (i.e., the time-period 
            %the sample path is generated if the first criteria is not met
            %during this perid)
    t_seq=1:T;
    %Store the sets of tolerance values 
    %starting tolerance levels for each pop: starting values 
    E=Es(k,:);
    e=E(1);
    %set a counter for number of iterations to run for each gen 
    AG=zeros(1,G);

    %store parameter sets
    params=zeros(B,3);
   % betas=zeros(1,B); %store betas
    %gammas=zeros(1,B);
    %epsilons=zeros(1,B);
    w=zeros(1,B); %store weights 
    es=zeros(1,G+1);
    es(1)=e;
    g=1;
    
while (g<1+G) %number of generation 
    %store values for the current generation
    params0=zeros(B,3);
    %betas0=zeros(1,B); %store the sub-pop based beta values from posterior
    %gamas0=zeros(1,B);
    %epsilon0=zeros(1,B);
    w0=zeros(1,B); %store weights 
    ag=0;%set the counter 
    ag0=zeros(1,B);%set the counter 
    rho_m=zeros(1,B);%store the distance values 
     
   
     parfor a=1:B %particle number     
     [params0(a,:),w0(a),rho_m(a),ag0(a)]=abc_ind(g,B,params,w,y,e,ini_state,stoi,time,stp1,stp2,eta);
     end 
        s_xx= rho_m';%save the distance criteria
       % e=E(g+1);
        %e=median(rho_m);
         e=E(g+1);
        es(g+1)=e;
        %normalize the weights 
        w0=normalize(w0,'norm',1);
        %replecae the previous gen values from the new gen values
        params=params0;
        w=w0; %store weights 
        AG(g)=sum(ag0);
        g=g+1; %update the generation number 
end
%store the final values of the population k
beta_smc(:,k)=params(:,1);
gamma_smc(:,k)=params(:,2);
epsilon_smc(:,k)=params(:,3);
w_smc(:,k)=w; %weights 
E_smc(:,k)=es;%tolerance values 
AG_smc(:,k)=AG;%number of steps generated to get B parameters 
s_x(:,k)=s_xx;
k+1
end

save('s_x_smc.mat','s_x');
save('ind_uniform_beta.mat','beta_smc');
save('ind_uniform_gamma.mat','gamma_smc');
save('ind_uniform_epsilon.mat','epsilon_smc');
save('AG_smc.mat','AG_smc');
save('w_smc.mat','w_smc');
toc

