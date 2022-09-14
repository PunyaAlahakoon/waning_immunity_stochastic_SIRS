
pd = makedist('Normal','mu',0.06,'sigma',0.01);
tpd=truncate(pd,0.04,1);

m=mean(tpd); % 2.0276
sigma=std(tpd);%0.4708
k=15;
t_mus=random(tpd,1,k);
histogram(t_mus);
%save('true_mus.mat','t_mus');

