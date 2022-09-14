%use this function to calculate a Monte Carlo estimate for the probability
%of extinction in the first trough 


%for the moment, I'm thinking of extending the Gillespie 
%model_setup = initial states, start time, end time, stochimetry, rates 
%trough_cond = con1= did the process enter the first trough? con2=did the
%process leave the first trough?
%ext_cond= what's the condition for extinction 

function[p]=trough3(model_setup,Ri,trough_cond,ext_cond,samples)

ini_state=model_setup.ini_state;
st_time=model_setup.time;
en_time=model_setup.end_time;
stoi=model_setup.stoi;
%R=model_setup.R;
R=Ri;
con1=trough_cond.con1;
con2=trough_cond.con2;
con3=trough_cond.con3;
%in case you hav to use more than 2 conditions above:
%n_con=numel(fieldnames(trough_cond));
%for i=1:n_con
   % [sprintf('cond_'),sprintf('%d',i)]=trough_cond.( [sprintf('con'),sprintf('%d',i)]);
%end

n = size(stoi,1); % Number of transitions
%m = size(stoi,2); % Number of states
B=samples;
%paths={1,B}; %store states of all the Gillespie paths
%Times={1,B};%store times of all the Gillespie paths  

%store the states whether or not they are in the first trough 
fds=zeros(1,B); %store if the process ended up with an ext in the 1st trough
fdn=zeros(1,B); %if the process left the 1st trough 
for j=1:B
%initialize the condition that the process has not entered the 1st trough: 
ft1=0;
ft2=0;
%when ft1=1 && ft2=0 at the end, ext in the 1st trough 
states= []; 
T=[];%store times
%initilization 
  states= [states;ini_state]; 
  T=[T st_time];
 
     i=1;
     while ((~ext_cond(states(i,:))) & (T<=en_time) )
         %& (~stp1(states(i,:))))
      
        %calculate rates 
        current_R = zeros(1,n);
        for k = 1:n
        func = R{k};
        current_R(k) = func(states(i,:));
        end
    
    % Total rate
    rtot = sum(current_R);
    
    %create random nums
    r=rand(1,2);
    
    %create time of the next event 
    tau=(1/rtot)*log(1/r(1));
    Tnext=T(i)+tau;
    T=[T Tnext];
    %choose transition 
    g=rtot*r(2);
    trans=find(cumsum(current_R)>=g, 1 );
    e=min(trans);
    
     state_next= states(i,:)+ stoi(e,:); 
     states=[states;state_next];
   
     if con1(state_next) 
         ft1=1;
     end
     
       if  (ft1==1 & con2(state_next)) | (ft1==1 & con3(state_next)) 
          ft2=1;
       end
 
     
       i=i+1;
     end
     
     %paths{1,j}=states;
     %Times{1,j}=T;
     
     %check if the process stayed in the 1st trough by the time while loop
     %ends:
     if (ft1==1) & (ft2==0) & (ext_cond(state_next))
        fds(j)=1; 
     end
     if  (ft2==1)
        fdn(j)=1; 
     end
end
       p=sum(fds)/(sum(fdn)+sum(fds));
       %p=sum(fds); %number of extinctions
end 