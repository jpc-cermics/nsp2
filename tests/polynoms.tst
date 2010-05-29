// -*- Mode: scilab -*- 

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
// *
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
