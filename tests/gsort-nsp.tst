// -*- Mode: scilab -*- 
// Copyright INRIA/ENPC 
// test de gsort :
// a faire ensuite marcher pour vecteur complexes 

function [y]=string(x)
 y=m2s(x,"%.0f")
endfunction

N=4;P=5;
// a random permutation 
[ax,perm]=sort(rand(1,N*P,'uniform'));
a=matrix(perm,N,P);

//-----Global sort 

[a1,ind]=gsort(a,'g');

if or(a1<>matrix(N*P:-1:1,N,P)) then BUG,end;
if or(a1<>matrix(a(ind),N,P))  then BUG,end;

//increasing values 

[a1,ind]=gsort(a,'g','i');

if or(a1<> matrix(1:N*P,N,P))  then BUG,end;
if or(a1<> matrix(a(ind),N,P))  then BUG,end;


//----sort each column of a ('r' means that the row indice is used for sorting)

[a1,ind]=gsort(a,'r');
nc=size(a,'c');
tst=[];
for i=1:nc ; 
   tst= [tst, matrix(a(ind(:,i),i),N,1)];
end 
if or(a1<> tst)  then BUG,end;

//increasing values 

[a1,ind]=gsort(a,'r','i');
nc=size(a,'c');
tst=[];
for i=1:nc ; 
   tst= [tst, matrix(a(ind(:,i),i),N,1)];
end 
if or(a1<> tst)  then BUG,end;


//----sort each row of a ('c' means that the column indice is used for sorting)

[a1,ind]=gsort(a,'c')   ;
nr=size(a,'r');
tst=[];
for i=1:nr ; 
   tst= [tst; matrix(a(i,ind(i,:)),1,P)];
end 
if or(a1<> tst)  then BUG,end;

//increasing 

[a1,ind]=gsort(a,'c','i')   ;
nr=size(a,'r');
tst=[];
for i=1:nr ; 
   tst= [tst; matrix(a(i,ind(i,:)),1,P)];
end 
if or(a1<> tst )  then BUG,end;


//----sort the rows of a in lexicographic order 
//    i.e a(k,:) < a(l,:) if there's a number j 
//    such that a(k,j) < a(l,j) and a(k,p)=a(l,p) for p in [1,j-1];
//
N=4;P=3;
alr=[1,2,2;
     1,2,1;
     1,1,2;
     1,1,1];
// a random permutation 
[ax,perm]=sort(rand(1,N,'uniform'));
a=alr(perm,:);

[a1,ind]=gsort(a,'lr');                   

if or(a1<> alr)  then BUG,end;
if or(a1<> matrix(a(ind,:),N,P))  then BUG,end;

[a2,ind2]=gsort(a*[100;10;1],'g');
if or(ind2<>ind)  then BUG,end;

// increasing 

[a1,ind]=gsort(a,'lr','i');                   
if or(a1<> alr(N:-1:1,:))  then BUG,end;
if or(a1<> matrix(a(ind,:),N,P))  then BUG,end;
[a2,ind2]=gsort(a*[100;10;1],'g','i');
if or(ind2<>ind)  then BUG,end;

//----sort the columns of a in lexicographic order 
N=3;P=4;
alr=alr';
// a random permutation 
[ax,perm]=sort(rand(1,P,'uniform'));
a=alr(:,perm);

[a1,ind]=gsort(a,'lc');                   

if or(a1<> alr)  then BUG,end;
if or(a1<> matrix(a(:,ind),N,P))  then BUG,end;

[a2,ind2]=gsort([100,10,1]*a,'g');
if or(ind2<>ind)  then BUG,end;

//increasing 

[a1,ind]=gsort(a,'lc','i');                   

if or(a1<> alr(:,P:-1:1))  then BUG,end;
if or(a1<> matrix(a(:,ind),N,P))  then BUG,end;

[a2,ind2]=gsort([100,10,1]*a,'g','i');
if or(ind2<>ind)  then BUG,end;


//========================String sorts ================
N=10;P=20;
a=int(10*rand(N,P,'uniform'));

//-----Global sort 
a0=string(a);
[a1,ind]=gsort(a0,'g');
[a2,ind2]=gsort(a,'g');
if or(a1<> string(matrix(a(ind),N,P)))  then BUG,end;
// a and string(a) are in the same order in the previous example 
if or(a1<>string(a2) )  then BUG,end;

//-- rows 
[a1,ind1]=gsort(string(a),'r');   
[a2,ind2]=gsort(a,'r');
if or(ind1<>ind2)  then BUG,end;
// a and string(a) are in the same order in the previous example 
if or(a1<> string(a2) )  then BUG,end;

//--columns 
[a1,ind1]=gsort(string(a),'c')   ;                      
[a2,ind2]=gsort(a,'c');
if or(ind1<>ind2)  then BUG,end;
// a and string(a) are in the same order in the previous example 
if or(a1<>string(a2) )  then BUG,end;

//----sort the rows of a in lexicographic order 

N=4;P=3;
alr=[1,2,2;
     1,2,1;
     1,1,2;
     1,1,1];
// a random permutation 
[ax,perm]=sort(rand(1,N,'uniform'));
a=alr(perm,:);
[a1,ind]=gsort(string(a),'lr') ;        
[a2,ind]=gsort(a,'lr') ;
//
if or(a1<>string( matrix(a(ind,:),N,P)))  then BUG,end;
// a and string(a) are in the same order in the previous example 
if or(a1<>string(a2) )  then BUG,end;

//----sort the columns of a in lexicographic order 
N=3;P=4;
alr=alr';
// a random permutation 
[ax,perm]=sort(rand(1,P,'uniform'));
a=alr(:,perm);

[a1,ind]=gsort(string(a),'lc');                   
[a2,ind]=gsort(a,'lc') ;
//
if or(a1<>string( matrix(a(:,ind),N,P)))  then BUG,end;
// a and string(a) are in the same order in the previous example 
if or(a1<>string(a2) )  then BUG,end;


