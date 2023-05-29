hyper_means=[0.084 0.041 0.025];


pd = makedist('Normal','mu',hyper_means(3),'sigma',0.01);
%tpd=truncate(pd,0.0625, 1); %for par 1
%tpd=truncate(pd,0.0330,0.0625); %for par 2
%tpd=truncate(pd,0.01,0.0330); %for par 2

m=mean(tpd); % 2.0276
sigma=std(tpd);%0.4708
k=15;
t_mus=random(tpd,1,k);
histogram(t_mus);
save('true_mus.mat','t_mus');

%save('true_sc1_mus.mat','t_mus');

%save('true_sc2_mus.mat','t_mus');

%save('true_sc3_mus.mat','t_mus');