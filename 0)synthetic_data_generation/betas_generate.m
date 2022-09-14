

pd = makedist('Normal','mu',2.5,'sigma',0.25);
tpd=truncate(pd,1,10);

m=mean(tpd); % 2.0276
sigma=std(tpd);%0.4708
k=15;
t_betas=random(tpd,1,k);
histogram(t_betas);
%save('true_sc2_betas.mat','t_betas');
