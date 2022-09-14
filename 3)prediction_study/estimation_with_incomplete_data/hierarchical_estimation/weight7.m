%DO NOT CHANGE ANY NOTATION. THIS FN IS USED IN STEP1_B
%me = samled betas from step 1_a
%b= proposed psi_beta,
%hbs= proposed sigma_beta,
%dim= # of sub-pops,
%B = # of samled betas from step 1_a
%p,q=truncation interval 

function[w]=weight7(me,ga,ep,b,hbs,dim,p,q)
pd_b = @(f,h) makedist('Normal','mu',f,'sigma',h);
tpd=@(f,h) truncate(pd_b(f,h),p(1),q(1));
tpdb=tpd(b(1),hbs(1)); %beta 

tpd2=@(f,h) truncate(pd_b(f,h),p(2),q(2)); %gamma
tpdg=tpd2(b(2),hbs(2)); 

tpd3=@(f,h) truncate(pd_b(f,h),p(3),q(3)); %epsilon
tpde=tpd3(b(3),hbs(3));

pd=makedist('Normal','mu',0.03,'sigma',0.1);
tpd2=truncate(pd,0.01,0.2);

    %distributions of theta|psi:
   weight=zeros(1,dim);
    %poste=zeros(#OF PARTICLES,# OF SUB-POPS);
   % poste=zeros(B,dim);
for k=1:dim
    bet=me(:,k); gam=ga(:,k); eps=ep(:,k);
    pdb=pdf(tpdb,bet); pdg=pdf(tpdg,gam); pde=pdf(tpde,eps);
    pdu_b=unifpdf(me,p(1),q(1)); pdu_g=unifpdf(ga,p(2),q(2)); pdu_e=pdf(tpd2,eps); 
   wks=(pdb.*pdg.*pde)./(pdu_b.*pdu_g.*pdu_e);
  % wks=(pdb.*pdg.*pde)./(pdu_e);
   %wks=(pdb.*pdg.*pde);

    weight(k)=sum(wks(wks>0));
end
w=prod(weight);
end