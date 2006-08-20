// -*- Mode: scilab -*- 
// test sp operations 
// sp -> spcol
// 
// faire tourner avec des matrices + ou - creuses 
// et carrees ou rect 
// + le cas complexe 

m=60;n=50;l=70;

A=int(rand(m,n)*30);A(A>=10)=0;
B=int(rand(n,l)*30);A(A>=10)=0;
SpA=sparse(A);
SpB=sparse(B);

R1=sp2m(SpA*SpB);
R2=A*B ;
if or(R1<>R2) then pause;end

R1=SpA*B;
R2=A*B ;
if or(R1<>R2) then pause;end

// XXX a metre 
R1=A*SpB;
R2=A*B ;
if or(R1<>R2) then pause;end

// idem for sprow 

SpA=sprow_sparse(A);
SpB=sprow_sparse(B);

R1=sprow2m(SpA*SpB);
R2=A*B ;
if or(R1<>R2) then pause;end

R1=SpA*B;
R2=A*B ;
if or(R1<>R2) then pause;end

// XXX a metre 
R1=A*SpB;
R2=A*B ;
if or(R1<>R2) then pause;end


