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
     while ((T<=en_time)& (~ext_cond(states(i,:))))
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
     
       if con2(state_next) & ft1==1
          ft2=1;
       end
 
     
       i=i+1;
     end
     
     %paths{1,j}=states;
     %Times{1,j}=T;
     
     %check if the process stayed in the 1st trough by the time while loop
     %ends:
     if (ft1==1 & ft2==0 & ext_cond(state_next))
        fds(j)=1; 
     end
end
     p=mean(fds);