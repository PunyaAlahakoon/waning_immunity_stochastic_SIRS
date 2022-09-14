%ABC step ====DO NOT CHANGE ANYTHING
%INPUT PARAMETERS:
%g = current generation number 
%B = # of particles to obtain from posterior 
%betas = samples particles in the previous generation 
%w= weights in the previous generation 
%y= data
%e = tolerance value 

function [params0,w0,rho0,ag0]= abc_ind(g,B,params,w,y,e,ini_state,stoi,time,stp1,stp2,eta)
%model definition 
aa=1; %to make sure that you get an accepted particle from the output 
ag=0; %set the counter
pd=makedist('Normal','mu',0.03,'sigma',0.1);
tpd=truncate(pd,0.01,0.2);
%initilise
params0=zeros(1,3); w0=0;
rho0=0;
while(aa<2)
    ag=ag+1;
 if g==1 
        be=unifrnd(0.001,10); % sample from prior 
        ga=unifrnd(0.00001,3);
        ep=random(tpd);
        para=[be ga ep];
    else    

        %find the index to the parameter set to use 
        ind=randsample(1:B,1,true,w);
        params0=params(ind,:);
        %perturbate:
        ss=std(params);
        para=abs(normrnd(params0,ss));
   
 end  
         p1=unifpdf(para(1),0.001,10)*unifpdf(para(2),0.00001,3)*pdf(tpd,para(3));
         
    if p1>0
                
                t_seq=1:stp2;
                Rj = {@(n) para(1)*n(1)*n(2)/(sum(n(1)+n(2)+n(3))-1);...
                @(n) para(2)*n(2);@(n) (para(3))*n(3)};
                %create eta sample sets using Rj:
                X=sample_gen(t_seq,ini_state,time,stoi,Rj,stp1,stp2,eta);
                %distance metric
                l=abs(X-y);
                cx=sqrt(sum((l.^2)));
                S=cx;
            if S<=e
                params0=para;
                %store s
                rho0=S;
                %store the weight w
                if g==1
                    w0=1;
                else
                %denominator:
                     pd=normpdf(para,params,ss); pd2=prod(pd,2);
                     den=w'.*pd2;
                    w0=p1/sum(den);   
                end
                %update a=a+1
                aa=aa+1;
                ag0=ag;
            end   
    end
end
end

