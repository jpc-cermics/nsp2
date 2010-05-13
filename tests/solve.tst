// -*- Mode: scilab -*- 
// basics tests for solve

//Small dimensions real
n=10;
b=randn(n,2);A=randn(n,n);

// lu 
x=solve(A,b,mode="std"); 
if norm(A*x-b) > 1.5e-12 then pause;end 

// lower triangular
x=solve(A,b,mode="lo"); 
if norm(tril(A)*x-b)> 1.5e-10  then pause;end 

// lower triangular but solve A'x=b
x=solve(A,b,mode="loT"); 
if norm(tril(A)'*x-b)> 1.5e-10  then pause;end 

// upper triangular 
x=solve(A,b,mode="up"); 
if norm(triu(A)*x-b)> 2e-10  then pause;end 

// upper triangular but solve A'x=b
x=solve(A,b,mode="upT"); 
if norm(triu(A)'*x-b)> 1.5e-10  then pause;end 

// symetric 
x=solve(A*A',b,mode="sym"); 
if norm(A*A'*x-b)> 1.5e-12 then pause;end 

// least square 
x=solve(A,b,mode="lsq"); 
if norm(A*x-b)> 1.5e-12 then pause;end 

x=solve([A;A],[b;b],mode="lsq"); 
if norm(A*x-b)> 1.5e-12 then pause;end 

// automatic 
x=solve(A,b,mode="\"); 
if norm(A*x-b) > 1.5e-12 then pause;end 

// symmetric positive definite 
x=solve(A*A',b,mode="sympos"); 
if norm(A*A'*x-b) > 1.5e-12 then pause;end 

//Small dimensions complex

b=randn(n,2);A=randn(n,n);
A= A + %i*randn(n,n);

// lu 
x=solve(A,b,mode="std"); 
if norm(A*x-b) > 1.5e-12 then pause;end 

// lower triangular
x=solve(A,b,mode="lo"); 
if norm(tril(A)*x-b)> 1.5e-10  then pause;end 

// lower triangular but solve A.'x=b
x=solve(A,b,mode="loT"); 
if norm(tril(A).'*x-b)> 1.5e-10  then pause;end 

// lower triangular but solve A'x=b
x=solve(A,b,mode="loH"); 
if norm(tril(A)'*x-b)> 1.5e-10  then pause;end 

// upper triangular 
x=solve(A,b,mode="up"); 
if norm(triu(A)*x-b)> 1.5e-10  then pause;end 

// upper triangular but solve A.'x=b
x=solve(A,b,mode="upT"); 
if norm(triu(A).'*x-b)> 1.5e-10  then pause;end 

// upper triangular but solve A'x=b
x=solve(A,b,mode="upH"); 
if norm(triu(A)'*x-b)> 1.5e-10  then pause;end 

// symetric 
x=solve(A*A.',b,mode="sym"); 
if norm(A*A.'*x-b)> 1.5e-12 then pause;end 

// least square 
x=solve(A,b,mode="lsq"); 
if norm(A*x-b)> 1.5e-12 then pause;end 

x=solve([A;A],[b;b],mode="lsq"); 
if norm(A*x-b)> 1.5e-12 then pause;end 

// automatic 
x=solve(A,b,mode="\"); 
if norm(A*x-b) > 1.5e-12 then pause;end 

// symmetric positive definite 
x=solve(A*A',b,mode="sympos"); 
if norm(A*A'*x-b) > 1.5e-12 then pause;end 

