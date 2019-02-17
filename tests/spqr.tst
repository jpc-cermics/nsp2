// -*- Mode: nsp -*- 
// tests for sparse qr (Jean-Philippe Chancelier 2016) 
// contains the tests for qr (Bruno July 06 2005).

if %spqr==%f then quit;end

eps_qr = 1.e-14;
norm_type = 1;

// FIRST PART : verify that Q * R = A (qr without columns permutation) or 
//              Q * R = A * P  (qr with columns permutation)
//              Remark : in nsp the permutation is returned as a
//                       permutation p and not as a associated matrix permutation P
//                       so A * P is done with A(:,p)

// square real matrix
A = rand(8,8);

[Q,R] = qr(sparse(A));
err = norm(A-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R,p] = qr(sparse(A));
err = norm(A(:,p)-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R] = qr(sparse(A),mode="e");
err = norm(A-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R,p] = qr(sparse(A),mode="e");
err = norm(A(:,p)-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

// square complex matrix
A = rand(8,8) + %i*rand(8,8);

[Q,R] = qr(sparse(A));
err = norm(A-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R,p] = qr(sparse(A));
err = norm(A(:,p)-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R] = qr(sparse(A),mode="e");
err = norm(A-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R,p] = qr(sparse(A),mode="e");
err = norm(A(:,p)-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

// rectangular real matrice m x n with m > n
A = rand(9,5);

[Q,R] = qr(sparse(A));
err = norm(A-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R,p] = qr(sparse(A));
err = norm(A(:,p)-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R] = qr(sparse(A),mode="e");
err = norm(A-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R,p] = qr(sparse(A),mode="e");
err = norm(A(:,p)-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

// rectangular complex matrice m x n with m > n
A = rand(9,5) + %i*rand(9,5);

[Q,R] = qr(sparse(A));
err = norm(A-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R,p] = qr(sparse(A));
err = norm(A(:,p)-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R] = qr(sparse(A),mode="e");
err = norm(A-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R,p] = qr(sparse(A),mode="e");
err = norm(A(:,p)-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end


// rectangular real matrice m x n with m < n
A = rand(5,9);

[Q,R] = qr(sparse(A));
err = norm(A-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R,p] = qr(sparse(A));
err = norm(A(:,p)-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R] = qr(sparse(A),mode="e");
err = norm(A-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R,p] = qr(sparse(A),mode="e");
err = norm(A(:,p)-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

// rectangular complex matrice m x n with m < n
A = rand(5,9) + %i*rand(5,9);

[Q,R] = qr(sparse(A));
err = norm(A-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R,p] = qr(sparse(A));
err = norm(A(:,p)-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R] = qr(sparse(A),mode="e");
err = norm(A-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R,p] = qr(sparse(A),mode="e");
err = norm(A(:,p)-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end


//
// SECOND PART : some tests with rank detection
// --------------------------------------------

// with real matrices

// here the matrix should be full rank
m = 20; n = 10;
A = rand(m,n);
[Q,R,p,rk] = qr(sparse(A),mode="e");
if rk ~= min(m,n) then pause;end
[Q,R,p,rk] = qr(sparse(A)); // the same without the economic mode
if rk ~= min(m,n) then pause;end

// now we change a column so that it is a linear
// combinaison of some others
A(:,3) = 2*A(:,1) +0.7*A(:,n-1) -0.4*A(:,n);
[Q,R,p,rk] = qr(sparse(A),mode="e");
if rk ~= min(m,n)-1 then pause;end
[Q,R,p,rk] = qr(sparse(A)); // the same without the economic mode
if rk ~= min(m,n)-1 then pause;end

// here the matrix should be full rank
m = 10; n = 20;
A = rand(m,n);
[Q,R,p,rk] = qr(sparse(A),mode="e");
if rk ~= min(m,n) then pause;end
[Q,R,p,rk] = qr(sparse(A)); // the same without the economic mode
if rk ~= min(m,n) then pause;end

// now we change a line so that it is a linear
// combinaison of some others
A(3,:) = 2*A(1,:) +0.7*A(m-1,:) -0.4*A(m,:);
[Q,R,p,rk] = qr(sparse(A),mode="e");
if rk ~= min(m,n)-1 then pause;end
[Q,R,p,rk] = qr(sparse(A)); // the same without the economic mode
if rk ~= min(m,n)-1 then pause;end


// with complex matrices

// here the matrix should be full rank
m = 20; n = 10;
A = rand(m,n)+%i*rand(m,n);
[Q,R,p,rk] = qr(sparse(A),mode="e");
if rk ~= min(m,n) then pause;end
[Q,R,p,rk] = qr(sparse(A)); // the same without the economic mode
if rk ~= min(m,n) then pause;end

// now we change a column so that it is a linear
// combinaison of some others
A(:,3) = (2+%i)*A(:,1) +0.7*A(:,n-1) -0.4*%i*A(:,n);
[Q,R,p,rk] = qr(sparse(A),mode="e");
if rk ~= min(m,n)-1 then pause;end
[Q,R,p,rk] = qr(sparse(A)); // the same without the economic mode
if rk ~= min(m,n)-1 then pause;end

// here the matrix should be full rank
m = 10; n = 20;
A = rand(m,n)+%i*rand(m,n);
[Q,R,p,rk] = qr(sparse(A),mode="e");
if rk ~= min(m,n) then pause;end
[Q,R,p,rk] = qr(sparse(A)); // the same without the economic mode
if rk ~= min(m,n) then pause;end

// now we change a line so that it is a linear
// combinaison of some others
A(3,:) = (2+%i)*A(1,:) +0.7*A(m-1,:) -0.4*%i*A(m,:);
[Q,R,p,rk] = qr(sparse(A),mode="e");
if rk ~= min(m,n)-1 then pause;end
[Q,R,p,rk] = qr(sparse(A)); // the same without the economic mode
if rk ~= min(m,n)-1 then pause;end

// qr_sp accepts standard matrices as argument 

// FIRST PART : verify that Q * R = A (qr without columns permutation) or 
//              Q * R = A * P  (qr with columns permutation)
//              Remark : in nsp the permutation is returned as a
//                       permutation p and not as a associated matrix permutation P
//                       so A * P is done with A(:,p)

// square real matrix
A = rand(8,8);

[Q,R] = qr_sp(A);
err = norm(A-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R,p] = qr_sp(A);
err = norm(A(:,p)-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R] = qr_sp(A,mode="e");
err = norm(A-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R,p] = qr_sp(A,mode="e");
err = norm(A(:,p)-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

// square complex matrix
A = rand(8,8) + %i*rand(8,8);

[Q,R] = qr_sp(A);
err = norm(A-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R,p] = qr_sp(A);
err = norm(A(:,p)-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R] = qr_sp(A,mode="e");
err = norm(A-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R,p] = qr_sp(A,mode="e");
err = norm(A(:,p)-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

// rectangular real matrice m x n with m > n
A = rand(9,5);

[Q,R] = qr_sp(A);
err = norm(A-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R,p] = qr_sp(A);
err = norm(A(:,p)-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R] = qr_sp(A,mode="e");
err = norm(A-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R,p] = qr_sp(A,mode="e");
err = norm(A(:,p)-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

// rectangular complex matrice m x n with m > n
A = rand(9,5) + %i*rand(9,5);

[Q,R] = qr_sp(A);
err = norm(A-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R,p] = qr_sp(A);
err = norm(A(:,p)-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R] = qr_sp(A,mode="e");
err = norm(A-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R,p] = qr_sp(A,mode="e");
err = norm(A(:,p)-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end


// rectangular real matrice m x n with m < n
A = rand(5,9);

[Q,R] = qr_sp(A);
err = norm(A-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R,p] = qr_sp(A);
err = norm(A(:,p)-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R] = qr_sp(A,mode="e");
err = norm(A-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R,p] = qr_sp(A,mode="e");
err = norm(A(:,p)-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

// rectangular complex matrice m x n with m < n
A = rand(5,9) + %i*rand(5,9);

[Q,R] = qr_sp(A);
err = norm(A-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R,p] = qr_sp(A);
err = norm(A(:,p)-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R] = qr_sp(A,mode="e");
err = norm(A-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end

[Q,R,p] = qr_sp(A,mode="e");
err = norm(A(:,p)-Q*R,norm_type)/norm(A,norm_type);
if err > eps_qr then pause;end


//
// SECOND PART : some tests with rank detection
// --------------------------------------------

// with real matrices

// here the matrix should be full rank
m = 20; n = 10;
A = rand(m,n);
[Q,R,p,rk] = qr_sp(A,mode="e");
if rk ~= min(m,n) then pause;end
[Q,R,p,rk] = qr_sp(A); // the same without the economic mode
if rk ~= min(m,n) then pause;end

// now we change a column so that it is a linear
// combinaison of some others
A(:,3) = 2*A(:,1) +0.7*A(:,n-1) -0.4*A(:,n);
[Q,R,p,rk] = qr_sp(A,mode="e");
if rk ~= min(m,n)-1 then pause;end
[Q,R,p,rk] = qr_sp(A); // the same without the economic mode
if rk ~= min(m,n)-1 then pause;end

// here the matrix should be full rank
m = 10; n = 20;
A = rand(m,n);
[Q,R,p,rk] = qr_sp(A,mode="e");
if rk ~= min(m,n) then pause;end
[Q,R,p,rk] = qr_sp(A); // the same without the economic mode
if rk ~= min(m,n) then pause;end

// now we change a line so that it is a linear
// combinaison of some others
A(3,:) = 2*A(1,:) +0.7*A(m-1,:) -0.4*A(m,:);
[Q,R,p,rk] = qr_sp(A,mode="e");
if rk ~= min(m,n)-1 then pause;end
[Q,R,p,rk] = qr_sp(A); // the same without the economic mode
if rk ~= min(m,n)-1 then pause;end


// with complex matrices

// here the matrix should be full rank
m = 20; n = 10;
A = rand(m,n)+%i*rand(m,n);
[Q,R,p,rk] = qr_sp(A,mode="e");
if rk ~= min(m,n) then pause;end
[Q,R,p,rk] = qr_sp(A); // the same without the economic mode
if rk ~= min(m,n) then pause;end

// now we change a column so that it is a linear
// combinaison of some others
A(:,3) = (2+%i)*A(:,1) +0.7*A(:,n-1) -0.4*%i*A(:,n);
[Q,R,p,rk] = qr_sp(A,mode="e");
if rk ~= min(m,n)-1 then pause;end
[Q,R,p,rk] = qr_sp(A); // the same without the economic mode
if rk ~= min(m,n)-1 then pause;end

// here the matrix should be full rank
m = 10; n = 20;
A = rand(m,n)+%i*rand(m,n);
[Q,R,p,rk] = qr_sp(A,mode="e");
if rk ~= min(m,n) then pause;end
[Q,R,p,rk] = qr_sp(A); // the same without the economic mode
if rk ~= min(m,n) then pause;end

// now we change a line so that it is a linear
// combinaison of some others
A(3,:) = (2+%i)*A(1,:) +0.7*A(m-1,:) -0.4*%i*A(m,:);
[Q,R,p,rk] = qr_sp(A,mode="e");
if rk ~= min(m,n)-1 then pause;end
[Q,R,p,rk] = qr_sp(A); // the same without the economic mode
if rk ~= min(m,n)-1 then pause;end



