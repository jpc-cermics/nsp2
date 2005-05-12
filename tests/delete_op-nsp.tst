// deletion tests for nsp
// Bruno Pincon (mai 12 2005)
// to be completed

A = [ 1  2  3  4;
      5  6  7  8;
      9 10 11 12;
     13 14 15 16;
     17 18 19 20];

ind1 = [1 3];
ind2 = [4 2 2 1];

A1r = [ 5  6  7  8;
       13 14 15 16;
       17 18 19 20];

A2r = [ 9 10 11 12;
       17 18 19 20];

A1c = [ 2  4;
        6  8;
       10 12;
       14 16;
       18 20];

A2c = [ 3;
        7;
       11;
       15;
       19];

ind3 = [1 6 7 9 16];
A3 = [5,9,13,17,10,18,3,7,11,15,19,8,12,16,20]';

ind4 = [13 13 13 14 1];
A4 = [5,9,13,17,2,6,10,14,18,3,7,19,4,8,12,16,20]';


// ----------------------------------------
//  matrix deletion
//-----------------------------------------
printf("\n Matrix deletion tests")
// test 1
B = A;
B(ind1,:)=[];
if ~and(B == A1r) then
   printf("\n test 1 fails ")
else
   printf("\n test 1 OK ")
end

// test 2
B = A;
B(ind2,:)=[];
if ~and(B == A2r) then
   printf("\n test 2 fails ")
else
   printf("\n test 2 OK ")
end

// test 3
B = A;
B(:,ind1)=[];
if ~and(B == A1c) then
   printf("\n test 3 fails ")
else
   printf("\n test 3 OK ")
end

// test 4
B = A;
B(:,ind1)=[];
if ~and(B == A1c) then
   printf("\n test 4 fails ")
else
   printf("\n test 4 OK ")
end

// test 5
B = A;
B(ind3)=[];
if ~and(B == A3) then
   printf("\n test 5 fails ")
else
   printf("\n test 5 OK ")
end

// test 6
B = A;
B(ind4)=[];
if ~and(B == A4) then
   printf("\n test 6 fails ")
else
   printf("\n test 6 OK ")
end
printf("\n")

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
printf("\n Boolean Matrix deletion tests")
// test 1
B = Ab;
B(ind1,:)=[];
if ~and(B == Ab1r) then
   printf("\n test 1 fails ")
else
   printf("\n test 1 OK ")
end

// test 2
B = Ab;
B(ind2,:)=[];
if ~and(B == Ab2r) then
   printf("\n test 2 fails ")
else
   printf("\n test 2 OK ")
end

// test 3
B = Ab;
B(:,ind1)=[];
if ~and(B == Ab1c) then
   printf("\n test 3 fails")
else
   printf("\n test 3 OK")
end

// test 4
B = Ab;
B(:,ind1)=[];
if ~and(B == Ab1c) then
   printf("\n test 4 fails")
else
   printf("\n test 4 OK")
end

// test 5
B = Ab;
B(ind3)=[];
if ~and(B == Ab3) then
   printf("\n test 5 fails")
else
   printf("\n test 5 OK")
end

// test 6
B = Ab;
B(ind4)=[];
if ~and(B == Ab4) then
   printf("\n test 6 fails ")
else
   printf("\n test 6 OK ")
end
printf("\n")


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

printf("\n Maxplus Matrix deletion tests")
// test 1
B = Am;
B(ind1,:)=[];
if ~and(B == Am1r) then
   printf("\n test 1 fails ")
else
   printf("\n test 1 OK ")
end

// test 2
B = Am;
B(ind2,:)=[];
if ~and(B == Am2r) then
   printf("\n test 2 fails ")
else
   printf("\n test 2 OK ")
end

// test 3
B = Am;
B(:,ind1)=[];
if ~and(B == Am1c) then
   printf("\n test 3 fails")
else
   printf("\n test 3 OK")
end

// test 4
B = Am;
B(:,ind1)=[];
if ~and(B == Am1c) then
   printf("\n test 4 fails")
else
   printf("\n test 4 OK")
end

// test 5
B = Am;
B(ind3)=[];
if ~and(B == Am3) then
   printf("\n test 5 fails")
else
   printf("\n test 5 OK")
end

// test 6
B = Am;
B(ind4)=[];
if ~and(B == Am4) then
   printf("\n test 6 fails ")
else
   printf("\n test 6 OK ")
end
printf("\n")


// ----------------------------------------
// string matrix deletion
//-----------------------------------------
printf("\n String Matrix deletion tests")

As = string(A); As1r = string(A1r); As2r = string(A2r);  
As1c = string(A1c); As2c = string(A2c);
As3 = string(A3); As4 = string(A4);
// test 1
B = As;
B(ind1,:)=[];
if ~and(B == As1r) then
   printf("\n test 1 fails ")
else
   printf("\n test 1 OK ")
end

// test 2
B = As;
B(ind2,:)=[];
if ~and(B == As2r) then
   printf("\n test 2 fails ")
else
   printf("\n test 2 OK ")
end

// test 3
B = As;
B(:,ind1)=[];
if ~and(B == As1c) then
   printf("\n test 3 fails ")
else
   printf("\n test 3 OK ")
end

// test 4
B = As;
B(:,ind1)=[];
if ~and(B == As1c) then
   printf("\n test 4 fails ")
else
   printf("\n test 4 OK ")
end

// test 5
B = As;
B(ind3)=[];
if ~and(B == As3) then
   printf("\n test 5 fails ")
else
   printf("\n test 5 OK ")
end

// test 6
B = As;
B(ind4)=[];
if ~and(B == As4) then
   printf("\n test 6 fails ")
else
   printf("\n test 6 OK ")
end
printf("\n")



