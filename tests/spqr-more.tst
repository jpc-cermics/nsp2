
// method to compute a min2-norm

A=rand(4,8);
B=rand(4,1);

C=spqr_create(A');
// solve Y = R'\(E'*B)
Y=C.rsolve[B,mode="R''\(E''*B)"];
// X = Q*Y
X=C.qmult[Y,mode="Q*X"];
norm(X,2)

C=spqr_create(A);
X=C.backslash[B];

// test with standard qr

[Q,R,e]=qr(A)
[m,n]=size(A);
E=eye(n,n);
E(e,:)=E;
norm(Q*R- A*E)

Y=Q'*B;
// R*Z = Q'B => Q*R*Z = B
Z=R\Y;
// E*X=Z 
X=Z;
X(e,:)=Z
A*X-B
norm(X,2)

// autre solution pour faire la même chose
// avec A'

[Q,R,e]=qr(A');
[m,n]=size(A');
E=eye(n,n);
E(e,:)=E;
norm(Q*R- A'*E)
// R'*Q' = E'*A
// R'*Q'*X = E'*B
Y=R'\(E'*B);
X=Q*Y;
norm(X,2)

// On trouve la même chose

