// -*- Mode: scilab -*- 
// umfpack objects tests 

if %umfpack==%f then quit;end

nn=10;
A=int(rand(nn,nn)*30);A(A>=15)=0;
Ai=int(rand(A)*30);Ai(Ai>=15)=0;
Ac=A+%i*Ai;
SpA=sparse(A);
U=umfpack_create(SpA);


// test of solve (Ar,Br)
B=int(rand(nn,nn)*30);
X=U.solve[B];
if norm(A*X-B) > 1.e-8 then pause;end 

// test of solve (Ar,Bc)
Bi=int(rand(nn,nn)*30);
X=U.solve[B+%i*Bi];
if norm(A*X-(B+%i*Bi)) > 1.e-8 then pause;end 

// test of solve (Ac,Br)
Uc=umfpack_create(SpA+sparse(%i*Ai));
X=Uc.solve[B];
if norm((A+%i*Ai)*X-B) > 1.e-8 then pause;end 

// test of solve (Ac,Bc)
Uc=umfpack_create(SpA+sparse(%i*Ai));
X=Uc.solve[B+%i*Bi];
if norm((A+%i*Ai)*X-(B+%i*Bi)) > 1.e-8 then pause;end 

// test of luget for A 
[L1,U1,p,q,r]=U.luget[];
A1 = diag( 1 ./ r) *A;
if norm(full(L1*U1) - A1(p,q)) > 1.e-8 then pause;end 

// test of luget for Ac
U=umfpack_create(sparse(Ac));
[L1,U1,p,q,r]=U.luget[];
A1 = diag( 1 ./ r) *Ac;
if norm(full(L1*U1) - A1(p,q)) > 1.e-8 then pause;end 


// test of short version 

nn=10;
A=int(rand(nn,nn)*30);A(A>=15)=0;
Ai=int(rand(nn,nn)*30);A(A>=15)=0;
SpA=sparse(A);
SpAi=sparse(Ai);
B=int(rand(nn,nn)*30);
Bi=int(rand(nn,nn)*30);
X=umfpack_solve(SpA, B);
if norm(A*X-B) > 1.e-8 then pause;end 

X=umfpack_solve(SpA+%i*SpAi, B);
if norm((A+%i*Ai)*X-B) > 1.e-8 then pause;end 
X=umfpack_solve(SpA, B+%i*Bi)
if norm(A*X-(B+%i*Bi)) > 1.e-8 then pause;end 
X=umfpack_solve(SpA+%i*SpAi, B+%i*Bi)
if norm((A+%i*SpAi)*X- (B+%i*Bi)) > 1.e-8 then pause;end 



