
function Y=prop(x)
  Y= x(:,1) & x(:,2) |  x(:,3) 
endfunction 

function Y=prop1(x)
  Y= (x(:,1) & x(:,2) ) |  x(:,3) 
endfunction 

function Y=prop2(x)
  Y= x(:,1) & (x(:,2) |  x(:,3) )
endfunction 

function T=table(p,n) 
  // table de vérité automatique pour un operateur n-aire 
  T=[%t;%f]
  T1=ones_new(2^n,n) 
  for i=1:n 
    T1(:,i) =ones_new(2^(i-1),1).*.(bool2s(T).*.ones_new(2^(n-i),1)) ; 
  end 
  T=[T1==1 , p(T1)]
endfunction 

// on vérifie que l'association est a gauche 
n=3
T1=table(prop,3);
T2=table(prop1,3);
T3=table(prop2,3);

[T1(:,n+1),T2(:,n+1),T3(:,n+1)]
