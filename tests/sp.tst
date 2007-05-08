// -*- Mode: scilab -*- 
// test sp operations 
// sp -> spcol
// 

nn=12
A=int(rand(nn,nn)*30);A(A>=15)=0;
Ai=int(rand(A)*30);Ai(Ai>=15)=0;
Asizes=[144,1;1,144;6,24;24,6];

exec('sp_common.tst');

A=int(rand(nn,nn)*30);A(A>=15)=0;
Ai=int(rand(A)*30);Ai(Ai>=15)=0;
Asizes=[144,1;1,144;6,24;24,6];
redim(A,24,6);
exec('sp_common.tst');

A=int(rand(nn,nn)*30);A(A>=15)=0;
Ai=int(rand(A)*30);Ai(Ai>=15)=0;
Asizes=[144,1;1,144;6,24;24,6];
redim(A,6,24);
exec('sp_common.tst');

A=int(rand(nn,nn)*30);A(A>=15)=0;
Ai=int(rand(A)*30);Ai(Ai>=15)=0;
Asizes=[144,1;1,144;6,24;24,6];
A=A+%i*Ai;
exec('sp_common.tst');

A=int(rand(nn,nn)*30);A(A>=15)=0;
Ai=int(rand(A)*30);Ai(Ai>=15)=0;
Asizes=[144,1;1,144;6,24;24,6];
A=A+%i*Ai;
redim(A,24,6);
exec('sp_common.tst');

A=int(rand(nn,nn)*30);A(A>=15)=0;
Ai=int(rand(A)*30);Ai(Ai>=15)=0;
Asizes=[144,1;1,144;6,24;24,6];
A=A+%i*Ai;
redim(A,6,24);
exec('sp_common.tst');
