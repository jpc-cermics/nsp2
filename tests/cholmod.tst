// -*- Mode: scilab -*- 
// test of cholmod objects 
//----------------------- 

if %cholmod==%f then quit;end

n=4;
A=sparse(rand(n,n));A=A*A';
ch=cholmod_create(A);
ld=ch.get_ld[];
L=tril(ld,-1)+sparse(diag(ones(1,n)));
D=diag(diag(ld));
if norm(full(L*D*L' - A)) > 100*%eps then pause;end 

// first mode just ise triu(A) 

ch=cholmod_create(triu(A));
ld=ch.get_ld[];
L=tril(ld,-1)+sparse(diag(ones(1,n)));
D=diag(diag(ld));
if norm(full(L*D*L' - A)) > 100*%eps then pause;end 

ch=cholmod_create(triu(A),type='sym');
ld=ch.get_ld[];
L=tril(ld,-1)+sparse(diag(ones(1,n)));
D=diag(diag(ld));
if norm(full(L*D*L' - A)) > 100*%eps then pause;end 

// add a beta*I 

beta=2
ch=cholmod_create(triu(A),type='sym',beta=beta);
ld=ch.get_ld[];
L=tril(ld,-1)+sparse(diag(ones(1,n)));
D=diag(diag(ld));
if norm(full(L*D*L' - (A+beta*sparse(eye(n,n))))) > 100*%eps then pause;end 

// factor A*A' 

A=sparse(rand(n,n));
ch=cholmod_create(A,type='row');
ld=ch.get_ld[];
L=tril(ld,-1)+sparse(diag(ones(1,n)));
D=diag(diag(ld));
if norm(full(L*D*L' - A*A')) > 100*%eps then pause;end 

beta=2
A=sparse(rand(n,n));
ch=cholmod_create(A,type='row',beta=beta);
ld=ch.get_ld[];
L=tril(ld,-1)+sparse(diag(ones(1,n)));
D=diag(diag(ld));
if norm(full(L*D*L' - (A*A'+beta*sparse(eye(n,n))))) > 100*%eps then pause;end 

// factor A'*A, 

A=sparse(rand(n,n));
ch=cholmod_create(A,type='col');
ld=ch.get_ld[];
L=tril(ld,-1)+sparse(diag(ones(1,n)));
D=diag(diag(ld));
if norm(full(L*D*L' - A'*A)) > 100*%eps then pause;end 

beta=2;
A=sparse(rand(n,n));
ch=cholmod_create(A,type='col',beta=beta);
ld=ch.get_ld[];
L=tril(ld,-1)+sparse(diag(ones(1,n)));
D=diag(diag(ld));
if norm(full(L*D*L' - (A'*A+beta*sparse(eye(n,n))))) > 100*%eps then pause;end 
  
// test methods 

// isreal 

if ch.isreal[] == %f then pause;end 

// solve 

B=rand(n,10);
X=ch.solve[B];
if norm(full(B - (A'*A+beta*sparse(eye(n,n)))*X)) > 100*%eps then pause;end 

// update 

C=sparse(rand(n,1));
ch.update[C]
ld1=ch.get_ld[];
L=tril(ld1,-1)+sparse(diag(ones(1,n)));
D=diag(diag(ld1));
if norm(full(L*D*L' - (A'*A+C*C'+beta*sparse(eye(n,n))))) > 100*%eps then pause;end 

// downdate 

ch.downdate[C];
ld2=ch.get_ld[];
if norm(full(ld-ld2)) > 100*%eps then pause;end 

// minor 

A=diag(sparse([1:3,0,0,0]));
// this one should return minor in p 
[ch,p]=cholmod_create(A);
if p<>4 then pause;end 
// then we have a partial factorisation 
ld1=ch.get_ld[];
// in matlab only the ld1(1:p-1,1:p-1) is returned 
L=tril(ld1,-1)+ sparse(diag(ones(1,6)));
D=diag(diag(ld1));
M=L*D*L';E=M-A;
if norm(E(1:p-1,1:p-1),'inf') > 100*%eps then pause;end 

// complex case 

n=4;
Ar=sparse(rand(n,n));
Ac=sparse(rand(n,n));
A=Ar+%i*Ac;
A=A*A';
ch=cholmod_create(A);
ld=ch.get_ld[];
L=tril(ld,-1)+sparse(diag(ones(1,n)));
D=diag(diag(ld));
if norm(full(L*D*L' - A)) > 100*%eps then pause;end 

B=rand(n,1);
X=ch.solve[B];
if norm(full(B - A*X)) > 100*%eps then pause;end 

// reste le pb de analyze et chol a tester 
// reste aussi le pb de la permutation a regler 
// reste le pb de LL' a tester 












