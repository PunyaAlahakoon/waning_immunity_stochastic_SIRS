
%construct the SIRS model 
s0 =999;
i0 = 1;
r0=0;
ini_state=[s0 i0 r0]; %initial population sizes in each compartment
stoi= [-1 1 0;0 -1 1;1 0 -1]; %stoichimetry matrix 
time = 0; %start time to consider 
stp1= @(n) n(2)==0; %stopping criteria a 
stp2=35;
m=20; %number of sample paths to generate 
N=1000;
times=1:0.01:stp2; %for the deterministic model 

pars=load('par_sets.mat');
pars=pars.par_sets;

k=3;
t=tiledlayout(1,3);
for i=1:k
    par=pars(i,:);
    [S,I]=ODE(par,times,ini_state,N);
     nexttile
    plot(times,I,'color','blue','LineWidth',1.5);
    Ie=(N*par(3)*(par(1)-par(2)))/(par(1)*(par(2)+par(3)));%Ie= 28.3019
    yline(Ie,'--','color','red','LineWidth',1)
  %  title([sprintf('\\beta = '),sprintf('%.4f',betas(i))]);
    %hold on 
   % ylim([0 300]);
    xlim([0 stp2]);
end
xlabel(t,'Time (days)', 'FontSize', 12);
ylabel(t,'Infectious individuals', 'FontSize', 12);
    %par=[4 1 mu];
    %[S,I]=ODE(par,times,ini_state,1000);
    %plot(times,I,'color','red','LineWidth',2.5);

%plot(S,I,'color','black','LineWidth',2);

