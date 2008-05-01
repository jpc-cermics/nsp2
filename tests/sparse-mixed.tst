// -*- Mode: scilab -*- 
// mixed + for sparse and full

n=20;
m=30;
Av={ rand(n,m),2,zeros_new(n,m),ones_new(n,m)};

for i=1:4 
  for j=1:4 
    if  or(Av{i}+sparse(Av{j}) <>Av{i}+Av{j}) then pause;end 
    if  or(sparse(Av{i})+Av{j} <>Av{i}+Av{j}) then pause;end 
  end
end

n=0;
m=10;
Av={ rand(n,m),2,zeros_new(n,m),ones_new(n,m)};

for i=1:4 
  for j=1:4 
    if  or(Av{i}+sparse(Av{j}) <>Av{i}+Av{j}) then pause;end 
    if  or(sparse(Av{i})+Av{j} <>Av{i}+Av{j}) then pause;end 
  end
end

n=0;
m=0;
Av={ rand(n,m),2,zeros_new(n,m),ones_new(n,m)};

for i=1:4 
  for j=1:4 
    if  or(Av{i}+sparse(Av{j}) <>Av{i}+Av{j}) then pause;end 
    if  or(sparse(Av{i})+Av{j} <>Av{i}+Av{j}) then pause;end 
  end
end


// mixed + for sparse and full A complex 
//-------------------------------------

n=20;
m=30;
Av={ rand(n,m)+%i*rand(n,m),2,zeros_new(n,m),ones_new(n,m)};

for i=1:4 
  for j=1:4 
    if  or(Av{i}+sparse(Av{j}) <>Av{i}+Av{j}) then pause;end 
    if  or(sparse(Av{i})+Av{j} <>Av{i}+Av{j}) then pause;end 
  end
end

n=0;
m=10;
Av={ rand(n,m)+%i*rand(n,m),2,zeros_new(n,m),ones_new(n,m)};

for i=1:4 
  for j=1:4 
    if  or(Av{i}+sparse(Av{j}) <>Av{i}+Av{j}) then pause;end 
    if  or(sparse(Av{i})+Av{j} <>Av{i}+Av{j}) then pause;end 
  end
end

n=0;
m=0;
Av={ rand(n,m)+%i*rand(n,m),2,zeros_new(n,m),ones_new(n,m)};

for i=1:4 
  for j=1:4 
    if  or(Av{i}+sparse(Av{j}) <>Av{i}+Av{j}) then pause;end 
    if  or(sparse(Av{i})+Av{j} <>Av{i}+Av{j}) then pause;end 
  end
end


// mixed - for sparse and full
//----------------------------- 

n=20;
m=30;
Av={ rand(n,m),2,zeros_new(n,m),ones_new(n,m)};

for i=1:4 
  for j=1:4 
    if  or(Av{i}-sparse(Av{j}) <>Av{i}-Av{j}) then pause;end 
    if  or(sparse(Av{i})-Av{j} <>Av{i}-Av{j}) then pause;end 
  end
end

n=0;
m=10;
Av={ rand(n,m),2,zeros_new(n,m),ones_new(n,m)};

for i=1:4 
  for j=1:4 
    if  or(Av{i}-sparse(Av{j}) <>Av{i}-Av{j}) then pause;end 
    if  or(sparse(Av{i})-Av{j} <>Av{i}-Av{j}) then pause;end 
  end
end

n=0;
m=0;
Av={ rand(n,m),2,zeros_new(n,m),ones_new(n,m)};

for i=1:4 
  for j=1:4 
    if  or(Av{i}-sparse(Av{j}) <>Av{i}-Av{j}) then pause;end 
    if  or(sparse(Av{i})-Av{j} <>Av{i}-Av{j}) then pause;end 
  end
end

// mixed - for sparse and full
// A complex 
//----------------------------- 

n=20;
m=30;
Av={ rand(n,m)+%i*rand(n,m),2,zeros_new(n,m),ones_new(n,m)};

for i=1:4 
  for j=1:4 
    if  or(Av{i}-sparse(Av{j}) <>Av{i}-Av{j}) then pause;end 
    if  or(sparse(Av{i})-Av{j} <>Av{i}-Av{j}) then pause;end 
  end
end

n=0;
m=10;
Av={ rand(n,m)+%i*rand(n,m),2,zeros_new(n,m),ones_new(n,m)};

for i=1:4 
  for j=1:4 
    if  or(Av{i}-sparse(Av{j}) <>Av{i}-Av{j}) then pause;end 
    if  or(sparse(Av{i})-Av{j} <>Av{i}-Av{j}) then pause;end 
  end
end

n=0;
m=0;
Av={ rand(n,m)+%i*rand(n,m),2,zeros_new(n,m),ones_new(n,m)};

for i=1:4 
  for j=1:4 
    if  or(Av{i}-sparse(Av{j}) <>Av{i}-Av{j}) then pause;end 
    if  or(sparse(Av{i})-Av{j} <>Av{i}-Av{j}) then pause;end 
  end
end

