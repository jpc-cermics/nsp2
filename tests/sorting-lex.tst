// -*- Mode: scilab -*- 

// lexical row 

N=7;P=4;
alr=[1,2,2,2;
     1,2,3,1;
     1,1,4,2;
     1,1,4,1;
     1,3,2,1;
     1,2,2,1;
     1,2,1,0];

// a random permutation 
[ax,perm]=sort(rand(1,N,'uniform'));
a=alr(perm,:);
ak=a;

a=ak;
[a1,ind]=gsort(a,'lr');                   
a=ak;
[a2,ind]=gsort(a,'ldr');

if ~and(a1==a2) then pause,end
if ~and(a==ak) then pause,end

N=4;P=3;
alr=[1,2,2;
     1,2,1;
     1,1,2;
     1,1,1];

// a random permutation 
[ax,perm]=sort(rand(1,N,'uniform'));
a=alr(perm,:);
ak=a;

a=ak;
[a1,ind]=gsort(a,'lr');                   
a=ak;
[a2,ind]=gsort(a,'ldr');

if ~and(a1==a2) then pause,end
if ~and(a==ak) then pause,end

// ------------------------

N=2;P=3;
alr=[2,1;
     2,2];
a=alr;

a=ak;
[a1,ind]=gsort(a,'lr');                   
a=ak;
[a2,ind]=gsort(a,'ldr');

if ~and(a1==a2) then pause,end
if ~and(a==ak) then pause,end
