// -*- Mode: nsp -*- 

test_file='TMPDIR/poo.mat'

X=rand(4,5);savematfile(test_file,'X')
X1=X;
loadmatfile(test_file);

if norm(X1-X) > %eps then pause;end 

X=sparse(X);savematfile(test_file,'X')
X1=X;
loadmatfile(test_file);

if norm(X1-X,'inf') > %eps then pause;end 

// cells 

C={ 1:10, %t, "foo"};
savematfile(test_file,'X')
C1=C;
loadmatfile(test_file);
if ~C1.equal[C] then pause; end 

// test use of int formats and struct 

clear X;
X.UINT8 =  [0  255 ];
X.INT16 =  [-128  127 ];
X.UINT16  =  [0  65535 ];
X._INT16 =   [-32768  32767 ];
X.DOUBLE =  [0  4294967295 ];
X.INT32 =  [-2147483648  2147483647 ];
X1=X;
savematfile(test_file,'X')
loadmatfile(test_file);

if ~X1.equal[X] then pause; end 

// force little or big endian 

savematfile(test_file,'X','-l_endian');
loadmatfile(test_file);

if ~X1.equal[X] then pause; end 

savematfile(test_file,'X','-b_endian');
loadmatfile(test_file);

if ~X1.equal[X] then pause; end 



