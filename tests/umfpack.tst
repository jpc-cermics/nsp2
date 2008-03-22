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

X=U.solve[B,irstep=10];
if norm(A*X-B) > 1.e-8 then pause;end 

X=U.solve[B,mode='At'];
if norm(A'*X-B) > 1.e-8 then pause;end 

X=U.solve[B,mode='Aat'];
if norm(A.'*X-B) > 1.e-8 then pause;end 

// test of solve (Ar,Bc)

Bi=int(rand(nn,nn)*30);
X=U.solve[B+%i*Bi];
if norm(A*X-(B+%i*Bi)) > 1.e-8 then pause;end 
X=U.solve[B+%i*Bi,mode="At"];
if norm(A'*X-(B+%i*Bi)) > 1.e-8 then pause;end 
X=U.solve[B+%i*Bi,mode="Aat"];
if norm(A.'*X-(B+%i*Bi)) > 1.e-8 then pause;end 

// test of solve (Ac,Br)
Uc=umfpack_create(SpA+sparse(%i*Ai));
X=Uc.solve[B];
if norm((A+%i*Ai)*X-B) > 1.e-8 then pause;end 
X=Uc.solve[B,mode='At'];
if norm((A+%i*Ai)'*X-B) > 1.e-8 then pause;end 
X=Uc.solve[B,mode='Aat'];
if norm((A+%i*Ai).'*X-B) > 1.e-8 then pause;end 

// test of solve (Ac,Bc)

Uc=umfpack_create(SpA+sparse(%i*Ai));
X=Uc.solve[B+%i*Bi];
if norm((A+%i*Ai)*X-(B+%i*Bi)) > 1.e-8 then pause;end 
X=Uc.solve[B+%i*Bi,mode='At'];
if norm((A+%i*Ai)'*X-(B+%i*Bi)) > 1.e-8 then pause;end 
X=Uc.solve[B+%i*Bi,mode='Aat'];
if norm((A+%i*Ai).'*X-(B+%i*Bi)) > 1.e-8 then pause;end 

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
X=umfpack_solve(SpA, B+%i*Bi);
if norm(A*X-(B+%i*Bi)) > 1.e-8 then pause;end 
X=umfpack_solve(SpA+%i*SpAi, B+%i*Bi);
if norm((A+%i*SpAi)*X- (B+%i*Bi)) > 1.e-8 then pause;end 


// test of solve variants 

nn=10;
A=int(rand(nn,nn)*30);A(A>=15)=0;
SpA=sparse(A);
U=umfpack_create(SpA);

// test of solve (Ar,Br)
B=int(rand(nn,1)*30);
X=U.solve[B];
if norm(A*X-B) > 1.e-8 then pause;end 
// luget for A 
[L1,U1,p,q,r]=U.luget[];


// test P'Lx=b 
X=U.solve[B,mode='Pt_L'];
if norm(L1*X -B(p) ) > 1.e-8 then pause;end 

// test Lx=b 
X=U.solve[B,mode='L'];
if norm(L1*X -B ) > 1.e-8 then pause;end 

// test L'Px = b 
X=U.solve[B,mode='Lt_P'];
if norm(L1'*X(p) -B ) > 1.e-8 then pause;end 

// test L.'Px=b 
X=U.solve[B,mode='Lat_P'];
if norm(L1.'*X(p) -B ) > 1.e-8 then pause;end 

// test L.'x=b 
X=U.solve[B,mode='Lt'];
if norm(L1.'*X -B ) > 1.e-8 then pause;end 

// test UQ'x=b 
X=U.solve[B,mode='U_Qt'];
if norm(U1*X(q) - B ) > 1.e-8 then pause;end 

// test Ux=b 
X=U.solve[B,mode='U'];
if norm(U1*X -B ) > 1.e-8 then pause;end 

// test QU'x=b 
X=U.solve[B,mode='Q_Ut'];
if norm(U1'*X -B(q) ) > 1.e-8 then pause;end 

// test QU.'x=b 
X=U.solve[B,mode='Q_Uat'];
if norm(U1.'*X -B(q) ) > 1.e-8 then pause;end 

// test U'x=b 
X=U.solve[B,mode='Ut'];
if norm(U1'*X -B ) > 1.e-8 then pause;end 

// test U.'x=b 
X=U.solve[B,mode='Uat'];
if norm(U1.'*X -B ) > 1.e-8 then pause;end 







