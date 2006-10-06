// -*- Mode: scilab -*- 
// insertion tests for nsp

A = [1,  2,  3,  4;
     5,  6,  7,  8;
     9, 10, 11, 12];

i = [1, 3];
j = [4, 2, 1];

// Aij must be the result for A(i,j) = B
B = [-1, -2, -3;
     -4, -5, -6];
Aij = [-3, -2,  3, -1;
        5,  6,  7,  8;
       -6, -5, 11, -4]; 
Res = A; Res(i,j) = B;
if ~and(Res == Aij)  then pause,end;

// Aij0 must be the result for A(i,j) = 0
Aij0 = [ 0,  0,  3,  0;
         5,  6,  7,  8;
         0,  0, 11, 0];
Res = A; Res(i,j) = 0;
if ~and(Res == Aij0)  then pause,end;

// Aijc must be the result for A(i,j) = C
C =  [ -%i, -2*%i, -3*%i;
     -4*%i, -5*%i, -6*%i];
Aijc = [-3*%i, -2*%i,  3, -%i;
         5,        6,  7,  8;
        -6*%i, -5*%i, 11, -4*%i];  
Res = A; Res(i,j) = C;
if ~and(Res == Aijc)  then pause,end;

// Aiji must be the result for A(i,j) = %i
Aiji =  [ %i,  %i,  3, %i;
           5,   6,  7, 8;
          %i,  %i, 11, %i]; 
Res = A; Res(i,j) = %i;
if ~and(Res == Aiji)  then pause,end;

// AA must be the result for A(4,5:6) = [13,14];
AA = [1,  2,  3,  4,  0,  0;
      5,  6,  7,  8,  0,  0;
      9, 10, 11, 12,  0,  0;
      0,  0,  0,  0, 13, 14];
Res = A; Res(4,5:6) = [13,14];
if ~and(Res == AA)  then pause,end;

// AAc must be the result for A(4,5:6) = %i;
AAc= [1,  2,  3,  4,  0,  0;
      5,  6,  7,  8,  0,  0;
      9, 10, 11, 12,  0,  0;
      0,  0,  0,  0, %i, %i];
Res = A; Res(4,5:6) = %i;
if ~and(Res == AAc)  then pause,end;


// Ak must be the result for A(k) = d
k = [1, 5, 9, 11];
d = [-1, -6, -11, -8];
Ak = [-1,  2,   3,   4;
       5, -6,   7,  -8;
       9, 10, -11,  12];
Res = A; Res(k) = d;
if ~and(Res == Ak)  then pause,end;

// Akc must be the result for A(k) = %i
Akc = [%i,  2,   3,   4;
        5, %i,   7,  %i;
        9, 10,  %i,  12];
Res = A; Res(k) = %i;
if ~and(Res == Akc)  then pause,end;

// test with multiple same index
// Ajj must be the result for A(:,jj) = Bj
jj = [2, 2];
Bj = [0, 1;
      2, 3;
      4, 5];
Ajj = [1,  1,  3,  4;
       5,  3,  7,  8;
       9,  5, 11, 12];
Res = A; Res(:,jj) = Bj;
if ~and(Res == Ajj)  then pause,end;

// tests enlarge vectors
x = [1,2];
xx = [1,2,0,4]
Res = x; Res(4) = 4;
if ~and(Res == xx)  then pause,end;

x = [1;2];
xx = [1;2;0;4]
Res = x; Res(4) = 4;
if ~and(Res == xx)  then pause,end;


x = 1;
xx = [1,0,3];
Res = x; Res(3) = 3;
if ~and(Res == xx)  then pause,end;


// CAUTION : now we suppose that insertion is not
// bugged for matrix...


// ----------------------------------------
// boolean matrix insertion tests
//-----------------------------------------
A = rand(4,5);
Abool = A < 0.5;
i = [1, 3, 4];
j = [4, 2, 1, 5];

B = rand(3,4)
Bbool = B < 0.5;

R = A; R(i,j) = B; Rbool = R < 0.5;
Res = Abool; Res(i,j) = Bbool; 
if ~and(Res == Rbool) then, pause, end

k = find(Abool);
Res = Abool; Res(k) = %f;
if ~and(Res == %f) then, pause, end

v = [%f, %f, %f, %f];
Cbool = [Abool, v'; v, %f, %t];
Res = Abool; Res(5,6) = %t;
if ~and(Res == Cbool) then, pause, end


// ----------------------------------------
// cells insertion tests
//-----------------------------------------
C = { "a", [1,2], list("aa","b");
      %f,   %i  , "foo"         };

D = { "a", "pipo", list("aa","b");
      %f,   %i   , {1,"b"}  };

Res = C; Res([3,6]) = { "pipo",  {1,"b"} };
if ~and(Res == D) then, pause, end

D =  { "a", [1,2], list("aa","b");
      %f,   {1}   , {%t} };
Res = C; Res(2,[2, 3]) = { {1},  {%t} };
if ~and(Res == D) then, pause, end

D = cells_create(2,4);
for i=1:6, D{i} = C{i};end
D(8) = {8};
Res = C; Res(2,4) = {8};
if ~and(Res == D) then, pause, end

E = C(:); F = E($:-1:1);
Res = E; Res(6:-1:1) = Res;
if ~and(Res == F) then, pause, end


// ----------------------------------------
// strings matrix insertion tests
//-----------------------------------------
A = [ "toto", "foo", "bar", "a";
      "e",    "i",   "o"  , "u"];

B = [ "titi", "foo", "bar", "a";
      "e",    "i",   "o"  , "u"]; 

Res = A; Res(1,1) = "titi";
if ~and(Res == B) then, pause, end

C =  [ "toto", "foo", "bar", "a", ".";
      "e",    "i",   "o"   , "u", "y"];

Res = A; Res(2,5) = "y";
if ~and(Res == C) then, pause, end

D =  [ "haha", "haha", "bar", "a";
      "e",    "haha",  "haha", "haha"];
Res = A;
Res(Res > "e") = "haha";
if ~and(Res == D) then, pause, end




