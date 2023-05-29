
pd = makedist('Normal','mu',1,'sigma',0.05);
tpd=truncate(pd,0,4);

m=mean(tpd); 
sigma=std(tpd);
k=15;
t_gammas=random(tpd,1,k);
histogram(t_gammas);
%save('true_gammas.mat','t_gammas');

%save('true_sc3_gammas.mat','t_gammas');