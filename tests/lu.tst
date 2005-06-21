// -*- Mode: scilab -*- 
// small tests for lu (Bruno June 21 2005). Note that 
// this should be integrated one day in the algebra.tst file...

eps_lu= 1.e-12;
norm_type = 1;

// 1/ square matrices
A = rand(5,5);
[L,U,p] = lu(A);
err = norm(A(p,:)-L*U,norm_type)/norm(A,norm_type);
if err > eps_lu then pause;end

[L,U] = lu(A);
err = norm(A-L*U,norm_type)/norm(A,norm_type);
if err > eps_lu then pause;end

A = rand(5,5) + %i*rand(5,5);
[L,U,p] = lu(A);
err = norm(A(p,:)-L*U,norm_type)/norm(A,norm_type);
if err > eps_lu then pause;end

[L,U] = lu(A);
err = norm(A-L*U,norm_type)/norm(A,norm_type);
if err > eps_lu then pause;end

// 2/ rectangular matrices A is m x n with m > n
A = rand(9,5);
[L,U,p] = lu(A);
err = norm(A(p,:)-L*U,norm_type)/norm(A,norm_type);
if err > eps_lu then pause;end

[L,U] = lu(A);
err = norm(A-L*U,norm_type)/norm(A,norm_type);
if err > eps_lu then pause;end

A = rand(9,5) + %i*rand(9,5);
[L,U,p] = lu(A);
err = norm(A(p,:)-L*U,norm_type)/norm(A,norm_type);
if err > eps_lu then pause;end

[L,U] = lu(A);
err = norm(A-L*U,norm_type)/norm(A,norm_type);
if err > eps_lu then pause;end

// 3/ rectangular matrices A is m x n with m < n
A = rand(5,9);
[L,U,p] = lu(A);
err = norm(A(p,:)-L*U,norm_type)/norm(A,norm_type);
if err > eps_lu then pause;end

[L,U] = lu(A);
err = norm(A-L*U,norm_type)/norm(A,norm_type);
if err > eps_lu then pause;end

A = rand(5,9) + %i*rand(5,9);
[L,U,p] = lu(A);
err = norm(A(p,:)-L*U,norm_type)/norm(A,norm_type);
if err > eps_lu then pause;end

[L,U] = lu(A);
err = norm(A-L*U,norm_type)/norm(A,norm_type);
if err > eps_lu then pause;end
