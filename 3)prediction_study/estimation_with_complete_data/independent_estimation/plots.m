figure(3)
tiledlayout(3,5)

for i=1:10
    nexttile
  scatter(epsilon_smc(:,i),s_x(:,i))
%histogram(beta_smc(:,i))
end