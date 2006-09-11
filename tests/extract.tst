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

A12 = [ 4,  2,  2, 1;
       12, 10, 10, 9];
Av = [1,5,9,13,17,2,6,10,14,18,3,7,11,15,19,4,8,12,16,20]';

// ----------------------------------------
//  matrix extraction 
//-----------------------------------------
if ~and(A(ind1,:) == A1r)  then pause,end;
if ~and(A(ind2,:) == A2r)  then pause,end;
if ~and(A(:,ind1) == A1c)  then pause,end;
if ~and(A(:,ind2) == A2c)  then pause,end;
if ~and(A(ind1,ind2) == A12) then pause, end;
if ~and(A(ind3) == A3)  then pause,end;
if ~and(A(ind4) == A4)  then pause,end;
if ~and(A(:) == Av)  then pause,end;

// ----------------------------------------
// boolean matrix extraction
//-----------------------------------------
val = 10;
Ab = A > val;
Ab1r = A1r > val;
Ab2r = A2r > val;
Ab1c = A1c > val;
Ab2c = A2c > val;
Ab3 = A3 > val;
Ab4 = A4 > val;
Ab12 = A12 > val;
Abv = Av > val;

if ~and(Ab(ind1,:) == Ab1r)  then pause,end;
if ~and(Ab(ind2,:) == Ab2r)  then pause,end;
if ~and(Ab(:,ind1) == Ab1c)  then pause,end;
if ~and(Ab(:,ind2) == Ab2c)  then pause,end;
if ~and(Ab(ind1,ind2) == Ab12) then pause, end;
if ~and(Ab(ind3) == Ab3)  then pause,end;
if ~and(Ab(ind4) == Ab4)  then pause,end;
if ~and(Ab(:) == Abv)  then pause,end;

// ----------------------------------------
// maxplus matrix extraction
//-----------------------------------------
Am = m2mp(A);
Am1r =m2mp( A1r);
Am2r = m2mp(A2r);
Am1c = m2mp(A1c);
Am2c = m2mp(A2c);
Am3 = m2mp(A3);
Am4 = m2mp(A4);
Am12 = m2mp(A12);
Amv = m2mp(Av);

if ~and(Am(ind1,:) == Am1r)  then pause,end;
if ~and(Am(ind2,:) == Am2r)  then pause,end;
if ~and(Am(:,ind1) == Am1c)  then pause,end;
if ~and(Am(:,ind2) == Am2c)  then pause,end;
if ~and(Am(ind1,ind2) == Am12)  then pause,end;
if ~and(Am(ind3) == Am3)  then pause,end;
if ~and(Am(ind4) == Am4)  then pause,end;
if ~and(Am(:) == Amv)  then pause,end;

// ----------------------------------------
// string matrix extraction
//-----------------------------------------
As = string(A); As1r = string(A1r); As2r = string(A2r);  
As1c = string(A1c); As2c = string(A2c);
As3 = string(A3); As4 = string(A4);
As12 = string(A12); Asv = string(Av);

if ~and(As(ind1,:) == As1r)  then pause,end;
if ~and(As(ind2,:) == As2r)  then pause,end;
if ~and(As(:,ind1) == As1c)  then pause,end;
if ~and(As(:,ind2) == As2c)  then pause,end;
if ~and(As(ind1,ind2) == As12)  then pause,end;
if ~and(As(ind3) == As3)  then pause,end;
if ~and(As(ind4) == As4)  then pause,end;
if ~and(As(:) == Asv)  then pause,end;


// ----------------------------------------
// cells matrix extraction
//-----------------------------------------
function C = m2cells(A)
   [m,n] = size(A)
   C = cells_create(m,n)
   for i = 1:m*n, C{i} = A(i); end
endfunction

Ac = m2cells(A);
Ac1r =m2cells( A1r);
Ac2r = m2cells(A2r);
Ac1c = m2cells(A1c);
Ac2c = m2cells(A2c);
Ac3 = m2cells(A3);
Ac4 = m2cells(A4);
Ac12 = m2cells(A12);
Acv = m2cells(Av);

if ~and(Ac(ind1,:) == Ac1r)  then pause,end;
if ~and(Ac(ind2,:) == Ac2r)  then pause,end;
if ~and(Ac(:,ind1) == Ac1c)  then pause,end;
if ~and(Ac(:,ind2) == Ac2c)  then pause,end;
if ~and(Ac(ind1,ind2) == Ac12)  then pause,end;
if ~and(Ac(ind3) == Ac3)  then pause,end;
if ~and(Ac(ind4) == Ac4)  then pause,end;
if ~and(Ac(:) == Acv)  then pause,end;


// ----------------------------------------
// polynomial matrix extraction : currently
//  PMatCompOp is not implemented so this
// test is commented.
//-----------------------------------------
// $$$ function C = m2poly(A)
// $$$    [m,n] = size(A)
// $$$    C = pmat_create(m,n)
// $$$    for i = 1:m*n, C(i) = m2p(A(i)); end
// $$$ endfunction
// $$$ 
// $$$ Ap = m2poly(A);
// $$$ Ap1r =m2poly( A1r);
// $$$ Ap2r = m2poly(A2r);
// $$$ Ap1c = m2poly(A1c);
// $$$ Ap2c = m2poly(A2c);
// $$$ Ap3 = m2poly(A3);
// $$$ Ap4 = m2poly(A4);
// $$$ Ap12 = m2poly(A12);
// $$$ Apv = m2poly(Av);
// $$$ 
// $$$ if ~and(Ap(ind1,:) == Ap1r)  then pause,end;
// $$$ if ~and(Ap(ind2,:) == Ap2r)  then pause,end;
// $$$ if ~and(Ap(:,ind1) == Ap1c)  then pause,end;
// $$$ if ~and(Ap(:,ind2) == Ap2c)  then pause,end;
// $$$ if ~and(Ap(ind1,ind2) == Ap12)  then pause,end;
// $$$ if ~and(Ap(ind3) == Ap3)  then pause,end;
// $$$ if ~and(Ap(ind4) == Ap4)  then pause,end;
// $$$ if ~and(Ap(:) == Apv)  then pause,end;
// $$$ 

