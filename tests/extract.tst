// -*- Mode: scilab -*- 
// extraction tests for nsp
// from the delete tests 

A=matrix(1:20,4,5)';

ind1 = [1 3];
ind2 = [4 2 2 1];

A1r = [1:4;9:12]

A2r = [13:16;5:8;5:8;1:4];

A1c = [    1,    3;
	   5,    7;
	   9,   11;
	   13,   15;
	   17,   19 ];
A2c = [    4,    2,    2,    1;
	   8,    6,    6,    5;
	   12,   10,   10,    9;
	   16,   14,   14,   13;
	   20,   18,   18,   17 ];

ind3 = [1 6 7 9 16];
A3 =[1; 2; 6; 14; 4 ];

ind4 = [13 13 13 14 1];
A4 =[11; 11; 11; 15; 1];

// ----------------------------------------
//  matrix extraction 
//-----------------------------------------

if ~and(A(ind1,:) == A1r)  then pause,end;
if ~and(A(ind2,:) == A2r)  then pause,end;
if ~and(A(:,ind1) == A1c)  then pause,end;
if ~and(A(:,ind2) == A2c)  then pause,end;
if ~and(A(ind3) == A3)  then pause,end;
if ~and(A(ind4) == A4)  then pause,end;

// ----------------------------------------
// boolean matrix deletion
//-----------------------------------------
val = 10;
Ab = A > val;
Ab1r = A1r > val;
Ab2r = A2r > val;
Ab1c = A1c > val;
Ab2c = A2c > val;
Ab3 = A3 > val;
Ab4 = A4 > val;

if ~and(Ab(ind1,:) == Ab1r)  then pause,end;
if ~and(Ab(ind2,:) == Ab2r)  then pause,end;
if ~and(Ab(:,ind1) == Ab1c)  then pause,end;
if ~and(Ab(:,ind2) == Ab2c)  then pause,end;
if ~and(Ab(ind3) == Ab3)  then pause,end;
if ~and(Ab(ind4) == Ab4)  then pause,end;

// ----------------------------------------
// maxplus matrix deletion
//-----------------------------------------
Am = m2mp(A);
Am1r =m2mp( A1r);
Am2r = m2mp(A2r);
Am1c = m2mp(A1c);
Am2c = m2mp(A2c);
Am3 = m2mp(A3);
Am4 = m2mp(A4);

if ~and(Am(ind1,:) == Am1r)  then pause,end;
if ~and(Am(ind2,:) == Am2r)  then pause,end;
if ~and(Am(:,ind1) == Am1c)  then pause,end;
if ~and(Am(:,ind2) == Am2c)  then pause,end;
if ~and(Am(ind3) == Am3)  then pause,end;
if ~and(Am(ind4) == Am4)  then pause,end;

// ----------------------------------------
// string matrix deletion
//-----------------------------------------
//printf(" String Matrix deletion tests\n")

As = string(A); As1r = string(A1r); As2r = string(A2r);  
As1c = string(A1c); As2c = string(A2c);
As3 = string(A3); As4 = string(A4);

if ~and(As(ind1,:) == As1r)  then pause,end;
if ~and(As(ind2,:) == As2r)  then pause,end;
if ~and(As(:,ind1) == As1c)  then pause,end;
if ~and(As(:,ind2) == As2c)  then pause,end;
if ~and(As(ind3) == As3)  then pause,end;
if ~and(As(ind4) == As4)  then pause,end;


