// -*- Mode: nsp -*- 

function p3=pprod(p1,p2)
// used for testing 
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
// used for testing
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

// creation
// m2p 

x=m2p([0,1]);

p=m2p([0,1,2]);
if ~p.equal[0+x+2*x^2] then pause;end 

p=m2p([0,1,2],dim="."); 
if ~p.equal[[m2p(0),m2p(1),m2p(2)]] then pause;end 

// ce2p

C={1:3,1:2;5,[0,2,1]};
P=ce2p(C);
if ~P(1).equal[m2p(1:3)] then pause;end 
C1=P.coeffs
if ~C.equal[C1]  then pause;end 
P=ce2p(C,var='u');
if ~P(1).equal[m2p(1:3,var='u')] then pause;end 

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

// horner (ttmode=%t)
//---------
// (r,r)
v=1:3;
p=m2p(v);
vc=[1:3] + [4:6]*%i;
q=m2p(vc);
P=[p,q];
A=[6,7];
C=horner(P,A,ttmode=%t);
C1=[horner(P(1),A(1)),horner(P(2),A(2))];
D=C-ce2m(C1);
if norm(D) > 10*%eps then pause;end 
//
C=horner(p,A,ttmode=%t);
C1=[horner(P(1),A(1)),horner(P(1),A(2))];
D=C-ce2m(C1);
if norm(D) > 10*%eps then pause;end 
//
C=horner([p,q],A(1),ttmode=%t);
C1=[horner(P(1),A(1)),horner(P(2),A(1))];
D=C-ce2m(C1);
if norm(D) > 10*%eps then pause;end 
// 

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

// derivative 

function d=deriv(p)
  [m,n]=size(p);
  d=p;
  for i=1:m*n 
    pc = p.coeffs{i};
    dc = pc(1,2:$).* (1:(size(pc,'*')-1))
    if isempty(dc) then dc = 0;end 
    d(i)= m2p(dc);
  end
endfunction

p1={ [1+2*x,2+5*x**2, 0;1, x, x+1 ],[1+(2+3*%i)*x,2+(5+%i)*x**2, 0;1, x+6*%i, x+1; 0, x**4, 1+x**2 ]};

for i=1:size(p1,'*')
  q= p1{i}.derivative[];
  q1 = deriv(p1{i});
  if ~q.equal[q1] then pause;end 
end

// taylor devpt 

function ok=check_taylor(p,a)
  [T]= taylor(p,a);
  q=m2p(T);
  pc = compose(q,m2p([-a,1]));
  ok=  p == pc 
endfunction;
  
p= m2p(rand(1,5));  a=4;
check_taylor(p,a,taylor(p,a)); 
a=4+2*%i;
check_taylor(p,a,taylor(p,a)); 
p= m2p(rand(1,5)+%i*rand(1,5));a=4;
check_taylor(p,a,taylor(p,a));

a=(1:3)';
T=taylor(p,a);
for i=1:size(a,'*')
  if ~check_taylor(p,a(i),T(i,:)) then pause;end 
end

// ^
//-------
C={1:3,1:2;5,[0,2,1]};
P=ce2p(C);
R=P^2;
if ~R.equal[P*P] then pause;end 

// .^
//-------
C={1:3,1:2;5,[0,2,1]};
P=ce2p(C);
R=P.^2;
if ~R.equal[P.*P] then pause;end 

//sum 
//-------
C={1:3,1:2;5,[0,2,1]};
P=ce2p(C);
S=sum(P,'c');
if ~S.equal[[P(1,1)+P(1,2);P(2,1)+P(2,2)]]  then pause;end
S=sum(P,'r');
if ~S.equal[[P(1,1)+P(2,1),P(1,2)+P(2,2)]]  then pause;end
S=sum(P,'*');
if ~S.equal[[P(1,1)+P(2,1)+P(1,2)+P(2,2)]]  then pause;end

//prod
//-------
C={1:3,1:2;5,[0,2,1]};
P=ce2p(C);
S=prod(P,'c');
if ~S.equal[[P(1,1)*P(1,2);P(2,1)*P(2,2)]]  then pause;end
S=prod(P,'r');
if ~S.equal[[P(1,1)*P(2,1),P(1,2)*P(2,2)]]  then pause;end
S=prod(P,'*');
if ~S.equal[[P(1,1)*P(2,1)*P(1,2)*P(2,2)]]  then pause;end

//diag
//triu
//tril
//varn
//-------
C={1:3,1:2;5,[0,2,1]};
P=ce2p(C);
if P.get_var[] <> 'x' then pause;end 
P.set_var['z'];
if P.get_var[] <> 'z' then pause;end 

//pdiv
//---------

p=m2p([]);
[r,q]=pdiv(p,p);if r <> p| q <> p then pause,end
if pdiv(p,p)<>p then pause,end

x=poly(0,'x');
p1=(1+x^2)*(1-x);p2=1-x;
[r,q]=pdiv(p1,p2);
if ~r.equal[m2p(0)] then pause;end 
if ~q.equal[1+x^2] then pause;end 

p3=(1+x^2)*(1-x)+5;p4=1-x;
[r,q]=pdiv(p3,p4);
if ~r.equal[m2p(5)] then pause;end 
if ~q.equal[1+x^2] then pause;end 

[r,q]=pdiv([p1,p3],[p2,p4]);
if ~r.equal[m2p([0,5],dim=".")] then pause;end 

//clean

eps=1.e-10;
a=[1,eps,eps,2];
p=m2p(a);
p=clean(p,10*eps);
q=m2p(clean(a,10*eps));
if ~p.equal[q] then pause;end 

//simp

s=poly(0,'s');
p=(s+1)*(s+2);q=(s+1)*(s-2);
[n,d]=simp(p,q);
if ~n.equal[(s+2)] then pause;end 
if ~d.equal[(s-2)] then pause;end 
[n,d]=simp([p,q],[q,p]);
if ~n.equal[[s+2,s-2]] then pause;end 
if ~d.equal[[s-2,s+2]] then pause;end 

//sfact
if %f then
p=(s-1/2)*(2-s);
w=sfact(p); 
r=w*horner(w, 1 ./ s){1}.num;
e = r-p;
if norm(e.coeffs{1}) > 20*%eps then pause;end 
p1 = m2p([1,2,3,2,1],var='s');
w=sfact(p1); 
r=w*horner(w, 1 ./ s){1}.num;
e = r-p1;
if norm(e.coeffs{1}) > 200*%eps then pause;end 
end

//bezout

s=poly(0,'s');

p1=(1+s)*(1+2*s)^2;p2=(1+s);
[p,U]=bezout(p1,p2);
if ~p.equal[1+s] then pause;end 

if norm(coeff([p1 p2]*U-[p 0]))> 100*%eps  then pause,end

//sfact
s=poly(0,'s');
p=(s-1/2)*(2-s);
w=sfact(p);
w1=w*horner(w,1/s,ttmode=%t);
if norm(w1.num -p) >10*%eps  then pause,end

F1=[s-1/2,s+1/2,s^2+2;1,s,-s;s^3+2*s,s,1/2-s];
P=F1*gtild(F1,"d");
F=sfact(P);if norm(coeff(P-F*gtild(F,"d")))>100*%eps  then pause,end
F=sfact(P+0);if norm(coeff(P-F*gtild(F,"d")))>100*%eps  then pause,end

// inv for polynomials

s=poly(0,'s');h=[s, 1+s; 4+ s^2,s];
r= inv(h,'L');
r1= r*h -m2r(eye(2,2),var='s',dim=".") ;
if ~r1.equal[m2r(zeros(2,2),var='s',dim=".")]  then pause;end 

r=inv(h,'C');
r1= r*h -m2r(eye(2,2),var='s',dim=".") ;
if ~r1.equal[m2r(zeros(2,2),var='s',dim=".")]  then pause;end 

// sE -A 

s=poly(0,'s');h=[s, 1+s; 4+ s,s];
r= inv(h,'L');
r1= r*h -m2r(eye(2,2),var='s',dim=".") ;
if ~r1.equal[m2r(zeros(2,2),var='s',dim=".")]  then pause;end 

r=inv(h,'C');
r1= r*h -m2r(eye(2,2),var='s',dim=".") ;
if ~r1.equal[m2r(zeros(2,2),var='s',dim=".")]  then pause;end 

// rationals 

s=poly(0,'s');h=[s,s ./ s^4;s^2,s^3];
r=inv(h,'L');
r1= r*h -m2r(eye(2,2),var='s',dim=".") ;
if ~r1.equal[m2r(zeros(2,2),var='s',dim=".")]  then pause;end 

r=inv(h,'C');
r1= r*h -m2r(eye(2,2),var='s',dim="."); 
if ~r1.equal[m2r(zeros(2,2),var='s',dim=".")]  then pause;end 

r=inv(h,'A');
r1= r*h -m2r(eye(2,2),var='s',dim=".") ;
if ~r1.equal[m2r(zeros(2,2),var='s',dim=".")]  then pause;end 

r=inv(h,'Cof');
r1= r*h -m2r(eye(2,2),var='s',dim=".") ;
if ~r1.equal[m2r(zeros(2,2),var='s',dim=".")]  then pause;end 

