
// mixed + for sparse and full

n=20;
m=30;
Av={ rand(n,m),2,zeros(n,m),ones(n,m)};

for i=1:4 
  for j=1:4 
    if  Av{i}+sparse(Av{j}) <>Av{i}+Av{j} then pause;end 
    if  sparse(Av{i})+Av{j} <>Av{i}+Av{j} then pause;end 
  end
end

n=0;
m=10;

for i=1:4 
  for j=1:4 
    if  Av{i}+sparse(Av{j}) <>Av{i}+Av{j} then pause;end 
    if  sparse(Av{i})+Av{j} <>Av{i}+Av{j} then pause;end 
  end
end

n=0;
m=0;

for i=1:4 
  for j=1:4 
    if  Av{i}+sparse(Av{j}) <>Av{i}+Av{j} then pause;end 
    if  sparse(Av{i})+Av{j} <>Av{i}+Av{j} then pause;end 
  end
end

