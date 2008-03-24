// -*- Mode: scilab -*- 
// basics tests for solve

rand('normal')

//Small dimensions real
n=5;
b=rand(n,2);A=rand(n,n);

// lu 
x=solve(A,b,mode="std"); 
if norm(A*x-b) > 1.e-12 then pause;end 

// lower triangular
x=solve(A,b,mode="lo"); 
if norm(tril(A)*x-b)> 1.e-12  then pause;end 

// upper triangular 
x=solve(A,b,mode="up"); 
if norm(triu(A)*x-b)> 1.e-12  then pause;end 

// symetric poitive definite 
x=solve(A*A',b,mode="sym"); 
if norm(A*A'*x-b)> 1.e-12 then pause;end 

// least square 
x=solve(A,b,mode="lsq"); 
if norm(A*x-b)> 1.e-12 then pause;end 

x=solve([A;A],[b;b],mode="lsq"); 
if norm(A*x-b)> 1.e-12 then pause;end 

// automatic 
x=solve(A,b,mode="\"); 
if norm(A*x-b) > 1.e-12 then pause;end 
