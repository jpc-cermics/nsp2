// -*- Mode: nsp -*- 
// test sp operations 
// sp -> spcol
// 

nn=12;
A=int(rand(nn,nn)*30);A(A>=15)=0;
Ai=int(rand(A)*30);Ai(Ai>=15)=0;
Ai=maxplus(A);A=maxplus(A);
Asizes=[144,1;1,144;6,24;24,6];

exec('spmp_common.tst');

A=int(rand(nn,nn)*30);A(A>=15)=0;
Ai=int(rand(A)*30);Ai(Ai>=15)=0;
Asizes=[144,1;1,144;6,24;24,6];
A=redim(A,24,6);
exec('spmp_common.tst');

A=int(rand(nn,nn)*30);A(A>=15)=0;
Ai=int(rand(A)*30);Ai(Ai>=15)=0;
Asizes=[144,1;1,144;6,24;24,6];
A=redim(A,6,24);
exec('spmp_common.tst');

A=int(rand(nn,nn)*30);A(A>=15)=0;
Ai=int(rand(A)*30);Ai(Ai>=15)=0;
Asizes=[144,1;1,144;6,24;24,6];
A=A+%i*Ai;
exec('spmp_common.tst');

A=int(rand(nn,nn)*30);A(A>=15)=0;
Ai=int(rand(A)*30);Ai(Ai>=15)=0;
Asizes=[144,1;1,144;6,24;24,6];
A=A+%i*Ai;
A=redim(A,24,6);
exec('spmp_common.tst');

A=int(rand(nn,nn)*30);A(A>=15)=0;
Ai=int(rand(A)*30);Ai(Ai>=15)=0;
Asizes=[144,1;1,144;6,24;24,6];
A=A+%i*Ai;
A=redim(A,6,24);
exec('spmp_common.tst');
