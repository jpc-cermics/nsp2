// -*- Mode: scilab -*- 


function y=pcompare(p1,p2)
   q=p1-p2;
   y=norm( q.coeffs{1}) > 10*%eps;
endfunction 

function q=p_op(p,op)
  x=p.coeffs{1};
  execstr('x=x'+op);
  q=m2p(x);
endfunction

function p3=pprod(p1,p2)
  [m1,n1]=size(p1);
  [m2,n2]=size(p2);
  if size(p1,'*')==1 then 
    p3=p1*p2;return;
  end
  if size(p2,'*')==1 then 
    p3=p1*p2;return;
  end
  p3=pmat_create(m1,n2);
  for i=1:m1
    for j=1:n2
      for k=1:n1
	p3(i,j)=p3(i,j)+p1(i,k)*p2(k,j);
      end
    end
  end
endfunction

function p3=pdotstar(p1,p2)
  [m1,n1]=size(p1);
  [m2,n2]=size(p2);
  if size(p1,'*')==1 then 
    p3=p1*p2;return;
  end
  if size(p2,'*')==1 then 
    p3=p1*p2;return;
  end
  p3=pmat_create(m1,n1);
  for i=1:m1
    for j=1:n1
      p3(i,j)=p1(i,j)*p2(i,j);
    end
  end
endfunction

x=m2p([0,1]);

// creation 
// P = B + x*A 
A=int(rand(4,5)*10);
B=int(rand(4,5)*10);
P= pmat_create(4,5,A);
P.shift[1]
P = P + pmat_create(4,5,B);
// recover A and B 
C=P.coeffs;
if ~ce2m(C).equal[B] then pause;end 
if ~ce2m(C,indice=2,noti=0).equal[A] then pause;end 


// creation 
//---------
v=1:3;
p=m2p(v);
if ~ p.coeffs{1}.equal[v] then pause;end

vi = v + %i*[4:6];
p=m2p(vi);
if ~ p.coeffs{1}.equal[vi] then pause;end

// coeffs 
//---------
v=1:3;
p=m2p(v);
q=m2p([1,0,0,0,6]);
P=[p,q];
C=P.coeffs;
if ~C{1,1}.equal[1:3] then pause;end 
if ~C{1,2}.equal[[1,0,0,0,6]] then pause;end 
vc=[4:6]*%i;
pc=m2p(v+vc);
q=m2p([1,0,0,0,6]);
P=[pc,q];
C=P.coeffs;
if ~C{1,1}.equal[v+vc] then pause;end 
if ~C{1,2}.equal[[1,0,0,0,6]] then pause;end 

// horner 
//---------
// (r,r)
v=1:3;
p=m2p(v);
A=testmatrix('magic',4);
C=horner(p,A);
V=v(1)+A.*(v(2)+A*v(3));
if norm(C{1}-V) > 10*%eps then pause;end 

// (c,r)
vc=[1:3] + [4:6]*%i;
p=m2p(vc);
A=testmatrix('magic',4);
C=horner(p,A);
V=vc(1)+A.*(vc(2)+A*vc(3));
if norm(C{1}-V) > 10*%eps then pause;end 

// (r,c) 
v=[1:3];
p=m2p(v);
A=testmatrix('magic',4);
A= A+ %i*testmatrix('franck',4);
C=horner(p,A);
V=v(1)+A.*(v(2)+A*v(3));
if norm(C{1}-V) > 10*%eps then pause;end 

// (c,c)
vc=[1:3] + [4:6]*%i;
p=m2p(vc);
A=testmatrix('magic',4);
A= A+ %i*testmatrix('franck',4);
C=horner(p,A);
V=vc(1)+A.*(vc(2)+A*vc(3));
if norm(C{1}-V) > 10*%eps then pause;end 

// horner (vdim=%t)
//---------
// (r,r)
v=1:3;
p=m2p(v);
A=testmatrix('magic',4);
C=horner(p,A,vdim=%t);
M=ce2m(C);
V=v(1)+A.*(v(2)+A*v(3));
if norm(M-V) > 10*%eps then pause;end 

// (c,r)
vc=[1:3] + [4:6]*%i;
p=m2p(vc);
A=testmatrix('magic',4);
C=horner(p,A,vdim=%t);
M=ce2m(C);
V=vc(1)+A.*(vc(2)+A*vc(3));
if norm(M-V) > 10*%eps then pause;end 

// (r,c) 
v=[1:3];
p=m2p(v);
A=testmatrix('magic',4);
A= A+ %i*testmatrix('franck',4);
C=horner(p,A,vdim=%t);
M=ce2m(C);
V=v(1)+A.*(v(2)+A*v(3));
if norm(M-V) > 10*%eps then pause;end 

// (c,c)
vc=[1:3] + [4:6]*%i;
p=m2p(vc);
A=testmatrix('magic',4);
A= A+ %i*testmatrix('franck',4);
C=horner(p,A,vdim=%t);
M=ce2m(C);
V=vc(1)+A.*(vc(2)+A*vc(3));
if norm(M-V) > 10*%eps then pause;end 

// horner (vdim=%t)
//---------
// (r,r)
v=1:3;
p=m2p(v);
vc=[1:3] + [4:6]*%i;
q=m2p(vc);
P=[p,q];
A=testmatrix('magic',3);
C=horner(P,A,vdim=%t);
Vp=v(1)+A.*(v(2)+A*v(3));
Vq=vc(1)+A.*(vc(2)+A*vc(3));
Mp=ce2m(C,indice=1);
Mq=ce2m(C,indice=2);
if norm(Mp-Vp) > 10*%eps then pause;end 
if norm(Mq-Vq) > 10*%eps then pause;end 

// hornerm 
//---------
v=1:3;
vc=[1:3] + [4:6]*%i;
P={m2p(v), m2p(vc)};
A={ rand(4,4), rand(4,4)+%i*rand(4,4)};
for i=1:2
  for j=1:2 
    p=P{i}; a= A{j}; v =p.coeffs{1};
    C=hornerm(p,a);
    res = 0;
    for k=1:length(v)
      res = res + v(k)*a^(k-1);
    end;
    if norm(res - C{1} ) > 100*%eps then pause;end 
  end
end

//roots
//---------
r=[1:5]';
p=m2p([-r(1),1]);
for i=2:5, p = p*m2p([-r(i),1]);end 
r1=roots(p);
p2=m2p([-r(1),1]);
for i=2:5, p2 = p2*m2p([-r(i),1]);end 
q=p-p2;
if norm(q.coeffs{1}) > 10*%eps then pause;end 


r=[1:5]'+%i*[6:10]';
p=m2p([-r(1),1]);
for i=2:5, p = p*m2p([-r(i),1]);end 
r1=roots(p);
p2=m2p([-r(1),1]);
for i=2:5, p2 = p2*m2p([-r(i),1]);end 
q=p-p2;
if norm(q.coeffs{1}) > 10*%eps then pause;end 

//degree
//---------

p=m2p([1:5]);
q=m2p([0,0,7]);
r=m2p([0,0,0,4,0,0]);
Q=[p,q,r;r,q,p];
if ~ Q.degree[].equal[[4,2,3;3,2,4]];then pause;end

p=m2p([1:5]);
q=m2p([1+%i,0,7]);
r=m2p([0,0,0,4+%i,0,0]);
Q=[p,q,r;r,q,p];
if ~ Q.degree[].equal[[4,2,3;3,2,4]];then pause;end

// shift method 
//-------------
p=m2p([1:5]);
p.shift[1];
if ~p.coeffs{1}.equal[[0,1:5]] then pause;end
p.shift[2];
if ~p.coeffs{1}.equal[[0,0,0,1:5]] then pause;end

// + 
// - 

// .* 
//----


a1=int(10*rand(3,3));
b1=int(10*rand(3,3))+int(10*rand(3,3))*%i;
p1={ [1+2*x,2+5*x**2, 0;1, x, x+1; 0, x**4, 1+x**2],...
     [1+(2+3*%i)*x,2+(5+%i)*x**2, 0;1, x+6*%i, x+1; 0, x**4, 1+x**2 ], a1, b1, 8, 8+7*%i, 1+x**2};
for i=1:size(p1,'*')
  p3=pdotstar(p1{i},p1{1})
  if ~p3.equal[p1{i}.*p1{1}] then pause;end 
end

// *
//----
a1=int(10*rand(2,3));
b1=int(10*rand(2,3))+int(10*rand(2,3))*%i;
p1={ [1+2*x,2+5*x**2, 0;1 x x+1], [1+(2+3*%i)*x,2+(5+%i)*x**2, 0;1, x+6*%i, x+1], a1, b1, 8, 8+7*%i, 1+x**2};
a2=int(10*rand(3,2));
b2=int(10*rand(3,2))+%i*int(10*rand(3,2));
p2={ [1,x;-1,2*x;2,0] ,[1,x+3*%i;-1+7*%i,2*x+%i;2,%i], a2, b2, 8, 8+7*%i, 1+x**2 };
for i=1:size(p1,'*')
  p3=pprod(p1{i},p2{1})
  if ~p3.equal[p1{i}*p2{1}] then pause;end 
end
for j=1:size(p2,'*')
  p3=pprod(p1{1},p2{j})
  if ~p3.equal[p1{1}*p2{j}] then pause;end 
end

// transpose (')
p1={ [1+2*x,2+5*x**2, 0;1, x, x+1 ],[1+(2+3*%i)*x,2+(5+%i)*x**2, 0;1, x+6*%i, x+1; 0, x**4, 1+x**2 ]};
p2={ [1+2*x,1;2+5*x**2, x;0, x+1 ],[1+(2-3*%i)*x,1,0; 2+(5-%i)*x**2, x-6*%i, x**4;0, x+1,  1+x**2]};
for i=1:size(p1,'*')
  p3 = p1{i}';
  if ~p3.equal[p2{i}] then pause;end 
end

// transpose (.')
p1={ [1+2*x,2+5*x**2, 0;1, x, x+1 ],[1+(2+3*%i)*x,2+(5+%i)*x**2, 0;1, x+6*%i, x+1; 0, x**4, 1+x**2 ]};
p2={ [1+2*x,1;2+5*x**2, x;0, x+1 ],[1+(2+3*%i)*x,1,0; 2+(5+%i)*x**2, x+6*%i, x**4;0, x+1,  1+x**2]};
for i=1:size(p1,'*')
  p3 = p1{i}.';
  if ~p3.equal[p2{i}] then pause;end 
end

// ^

//sum 
//prod
//diag
//triu
//tril
//varn
//clean
//simp
//pdiv
//bezout
//sfact

