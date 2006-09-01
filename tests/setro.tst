// -*- Mode: scilab -*- 
// test a(x,y)=z for sparse 
// Reste a ecrire le cas ou b est scalaire 

N=10;
M=8;
a=int(10*rand(N,M,'uniform'));
a(a>= 8) = 0.0;
asp=sparse(a);

b=int(10*rand(N,M,'uniform'));
b(b>= 8) = 0.0;
bsp=sparse(b);

a([1:N],[1:M])=b ;
asp([1:N],[1:M])=bsp ;
if or(a-full(asp)<>0)  then BUG,end

a(2*([1:N]),2*([1:M])) = b ;
asp(2*[1:N],2*[1:M])=bsp ;

if or(a-full(asp)<>0)  then BUG,end



