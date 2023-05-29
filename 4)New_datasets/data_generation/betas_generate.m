
hyper_means=[1.5 4 8];
%pd = makedist('Normal','mu',hyper_means(2),'sigma',0.25);
pd = makedist('Normal','mu',hyper_means(1),'sigma',0.05);
tpd=truncate(pd,1.25,2.75);
%tpd=truncate(pd,2.75,6);
%tpd=truncate(pd,6,12);

m=mean(tpd); % 2.0276
sigma=std(tpd);%0.4708
k=15;
t_betas=random(tpd,1,k);
histogram(t_betas);

save('true_sc11_betas.mat','t_betas');
