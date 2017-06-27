function [S]=tf2des(G,flag)
//  Transfer function to descriptor form
//  E*dx=A*x+B*u
//  y=C*x+D*u
//  with flag="withD" a maximal rank D matrix is returned
//!
// Copyright INRIA
  
  if nargin==1 then flag="";end
  if nargin==2 && flag<>"withD"  then 
    error("Error: unknown flag");
    return;
  end
  if type(G,'short')<>'r' then 
    error("Error: expecting a rational matrix");
    return;
  end
  Num= G.num;Den=G.den;
  %s=poly(0,Den.get_var[]);
  [n,m]=size(Num);
  pol=m2p(zeros(n,m),dim=".",var=Den.get_var[]);
  pro= pol ./ (1+0*%s); // force to be a rational 
  // Pro = strictly proper part of G
  // Pol = polynomial part of G.
  for l=1:n,
    for k=1:m,
      denlk=Den(l,k);
      [r,q]=pdiv(Num(l,k)+0*%s,denlk+0*%s);
      pol(l,k)=q;
      pro(l,k)=r/denlk;
    end
  end
  
  sp=tf2ss(pro);
  D=zeros(size(Num));
  
  if flag.equal["withD"] then
    D=coeff(pol,0);pol=pol-D;
  end
  spol=tf2ss(horner(pol,1/%s,ttmode=%t)/%s);

  [n1,vn1]=size(sp(2));
  [n2,vn2]=size(spol(2));
  A=[sp(2),0*ones(n1,n2);
     0*ones(n2,n1),eye(n2,n2)];
  E=[eye(n1,n1),0*ones(n1,n2);
     0*ones(n2,n1),spol(2)];
  B=[sp(3);
     spol(3)];
  C=[sp(4),-spol(4)];
  S=hash(type='des',A=A,B=B,C=C,D=D,E=E);
endfunction
