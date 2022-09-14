%generate data for dataset 1:



%use this to generate synthetic data from a hierachical structure
k=15; %number of clusters 
%load h_betas of hierachical models: h_beta=2 
betas=load('true_betas.mat', 't_betas');
betas=betas.t_betas;
%mu value for extinction prob=0.5
mu=load('true_mus.mat');
mu=mu.t_mus;

gamma=load('true_gammas.mat');
gamma=gamma.t_gammas;

%the model:

%%%%%%population parameters%%%%%
%setting parametrs for SIR with demography

s0 =1000;
i0 = 1;
r0=0;
ini_state=[s0 i0 r0]; %initial population sizes in each compartment

stoi= [-1 1 0;0 -1 1;1 0 -1]; %stoichimetry matrix 
time = 0; %start time to consider 
stp1= @(n) n(2)==0; %stopping criteria a 
stp2=60;
t_seq=1:stp2;

m=1; %number of gillespie paths created in Gillespie 4 (keep this at 1 for this algorithm)%
%data=zeros(stp2,k); %store data of observed value

%for j=1:1
j=2;
    %par=[betas(j) gammas(j) mu];
    Ri = {@(n) betas(j)*n(1)*n(2)/(sum(n(1)+n(2)+n(3))-1);...
            @(n) gamma(j)*n(2);@(n) (mu(j))*n(3)}; %reactions for par1
    %X=zeros(stp2,m);% store number of infecteds (observed)
    %[Times,paths]=Gillespe4(ini_state,time,stoi,Ri,stp1,stp2,m);

    data(:,j)=sample_gen(t_seq,ini_state,time,stoi,Ri,stp1,stp2,m);
    plot(t_seq,data(:,j));
    %plot(T{i},Gi{i})
    %hold on 
    
for i=1:k
    subplot(3,5,i);
    plot(t_seq,data(:,i));
    ylim([0 300]);
end

%save('data.mat','data');

