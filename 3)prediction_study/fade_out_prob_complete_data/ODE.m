%use this function to calculate the ODE paths 
% N=population size

function[S,I]=ODE(para,times,ini_state,N)
xc=para;
%for SIR  NOTE= xc=[beta gamma]
%fc=@(t,y)[(-(xc(1)*y(1)*y(2)/N));((xc(1)*y(1)*y(2)/N) - (xc(2))*y(2))]; 

%for SIR with demograpgy NOTE xc=[mu beta gamma]
%fc=@(t,y) [(-(xc(1)*y(1)*y(2)/N) + xc(3)*(N-y(1)));((xc(1)*y(1)*y(2)/N) - (xc(2)+xc(3))*y(2))];

%for SIRS, xc=[beta gamma mu]
fc=@(t,y) [(-(xc(1)*y(1)*y(2)/N)+ (xc(3)*y(3)));((xc(1)*y(1)*y(2)/N) - (xc(2))*y(2)); (xc(2)*y(2)-xc(3)*y(3))];
[~,y]=ode45(fc,times,ini_state);
I=y(:,2);
S=y(:,1);
end