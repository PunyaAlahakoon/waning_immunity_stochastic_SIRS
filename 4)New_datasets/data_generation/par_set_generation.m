r0=[2 4 8];

beta=r0;
%mu=[0.02 0.04 0.06];

%[x, y,z]=meshgrid(r0,beta,mu);

[x, y]=meshgrid(r0,beta);

%combi=[x(:) y(:) z(:)];
combi=[x(:) y(:)];

%gamma=combi(:,2)./combi(:,1); %beta/r0
gamma=[1 1 1];
%par_sets=[combi(:,2) gamma combi(:,3)];

mu=[0.1 0.06 0.03];
par_sets=[beta' gamma' mu'];

save('par_sets.mat',"par_sets");
