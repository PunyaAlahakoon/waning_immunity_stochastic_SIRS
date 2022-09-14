s0 =999; % #of  susceptibles 
i0 = 1; % # of infectious 
r0=0;% # of recovered 
ini_state=[s0 i0 r0]; %initial sizes in each compartment

stoi= [-1 1 0;0 -1 1;1 0 -1]; %stoichimetry matrix 
time = 0; %start time to consider 
stp1= @(n) n(2)==0; % 1st stopping criteria for Gillespie (i.e., stop the algorithm when the #of infectious=0)

B=1000; 
k=2;
stp2=35;
paths=zeros(stp2,B);
eta=1;
i=1;
while i<1+B
beta=beta_smc(i,k);
gamma=gamma_smc(i,k);
epsilon=epsilon_smc(i,k);
%Number of particles/ parameters sets to sample using ABC-SMC

   t_seq=1:stp2;
                Rj = {@(n) beta*n(1)*n(2)/(sum(n(1)+n(2)+n(3))-1);...
                @(n) gamma*n(2);@(n) (epsilon)*n(3)};
                %create eta sample sets using Rj
    X=sample_gen(t_seq,ini_state,time,stoi,Rj,stp1,stp2,eta);
    paths(:,i)=X;
    if X(2)>0
        i=i+1;
    end
end 