// -*- Mode: scilab -*- 

function [y]=string(x)
  y=m2s(x,"%.0f")
endfunction;

N=4;P=5;
// a random permutation 
[ax,perm]=sort(rand(1,N*P,'uniform'));
a=matrix(perm,N,P);

//  global sort decreasing

[a1,ind]=gsort(a,'g');
if or(a1<>matrix(N*P:-1:1,N,P)) then pause,end;
if or(a1<>matrix(a(ind),N,P))  then pause,end;

[a1,ind]=gsort(a,'gb');
if or(a1<>matrix(N*P:-1:1,N,P)) then pause,end;
if or(a1<>matrix(a(ind),N,P))  then pause,end;

[a1,ind]=gsort(a,'gm');
if or(a1<>matrix(N*P:-1:1,N,P)) then pause,end;
if or(a1<>matrix(a(ind),N,P))  then pause,end;

[a1,ind]=gsort(a,'gs');
if or(a1<>matrix(N*P:-1:1,N,P)) then pause,end;
if or(a1<>matrix(a(ind),N,P))  then pause,end;

//  global sort  increasing

[a1,ind]=gsort(a,'g','i');
if or(a1<> matrix(1:N*P,N,P))  then pause,end;
if or(a1<> matrix(a(ind),N,P))  then pause,end;

[a1,ind]=gsort(a,'gb','i');
if or(a1<> matrix(1:N*P,N,P))  then pause,end;
if or(a1<> matrix(a(ind),N,P))  then pause,end;

[a1,ind]=gsort(a,'gm','i');
if or(a1<> matrix(1:N*P,N,P))  then pause,end;
if or(a1<> matrix(a(ind),N,P))  then pause,end;

[a1,ind]=gsort(a,'gs','i');
if or(a1<> matrix(1:N*P,N,P))  then pause,end;
if or(a1<> matrix(a(ind),N,P))  then pause,end;

// decreasing sort of each column independently 

[a1,ind]=gsort(a,'r');
nc=size(a,'c');
tst=[];
for i=1:nc ; 
  tst= [tst, matrix(a(ind(:,i),i),N,1)];
end 
if or(a1<> tst)  then pause,end;

// increasing sort of each column independently 

[a1,ind]=gsort(a,'r','i');
nc=size(a,'c');
tst=[];
for i=1:nc ; 
   tst= [tst, matrix(a(ind(:,i),i),N,1)];
end 
if or(a1<> tst)  then pause,end;

// decreasing sort of each row independently 

[a1,ind]=gsort(a,'c')   ;
nr=size(a,'r');
tst=[];
for i=1:nr ; 
   tst= [tst; matrix(a(i,ind(i,:)),1,P)];
end 
if or(a1<> tst)  then pause,end;

// increasing sort of each row independently

[a1,ind]=gsort(a,'c','i')   ;
nr=size(a,'r');
tst=[];
for i=1:nr ; 
   tst= [tst; matrix(a(i,ind(i,:)),1,P)];
end 
if or(a1<> tst )  then pause,end;

// decreasing lexicographic sort of the rows 

ask=[   1,   3,   2,   1;
	1,   2,   3,   1;
	1,   2,   2,   2;
	1,   2,   2,   1;
	1,   2,   1,   0;
	1,   1,   4,   2;
	1,   1,   4,   1 ];
N=size(ask,'r');

// mix data 
perm=grand(1,"prm",(1:N)');
a =ask(perm,:);
ak =a; 

[a1,ind]=gsort(a,'lr');                   
if ~and(a==ak) then pause,end;
if ~and(a1==ask) then pause,end;
ii=ones(N,1);ii(perm)=(1:N)';
if ~and(ii==ind) then pause,end;
if ~and(a(ind,:)==a1) then pause,end;

[a2,ind]=gsort(a,'ldr');
if ~and(a==ak) then pause,end;
if ~and(a2==ask) then pause,end;
ii=ones(N,1);ii(perm)=(1:N)';
if ~and(ii==ind) then pause,end;
if ~and(a(ind,:)==a2) then pause,end;

// decreasing lexicographic sort of the columns 

a =ask(perm,:)';
ak =a; 

[a1,ind]=gsort(a,'lc');                   
if ~and(a==ak) then pause,end;
if ~and(a1==ask') then pause,end;
ii=ones(1,N);ii(perm)=(1:N);
if ~and(ii==ind) then pause,end;
if ~and(a(:,ind)==a1) then pause,end;

[a2,ind]=gsort(a,'ldc');
if ~and(a==ak) then pause,end;
if ~and(a2==ask') then pause,end;
ii=ones(1,N);ii(perm)=(1:N);
if ~and(ii==ind) then pause,end;
if ~and(a(:,ind)==a2) then pause,end;

// increasing lexicographic sort of the rows 

ask1=ask($:-1:1,:);
a = ask(perm,:);
ak=a;

[a1,ind]=gsort(a,'lr','i');                   
if ~and(a==ak) then pause,end;
if ~and(a1==ask1) then pause,end;
ii=ones(N,1);ii(perm)=(1:N)';
ii=ii(N:-1:1);
if ~and(ii==ind) then pause,end;

[a2,ind]=gsort(a,'ldr','i');
if ~and(a==ak) then pause,end;
if ~and(a2==ask1) then pause,end;
ii=ones(N,1);ii(perm)=(1:N)';
ii=ii(N:-1:1);
if ~and(ii==ind) then pause,end;

// increasing lexicographic sort of the columns 

// test of the stable sort 
// ------------------------

N=20;
a=int(rand(N,1)*4);
[a1,ind1]=gsort(a,'gs');
[a2,ind2]=gsort([a,(N:-1:1)'],'lr');
if ~ind1.equal[ind2] then pause;end 

[a1,ind1]=gsort(a,'gs','i');
[a2,ind2]=gsort([a,(1:N)'],'lr','i');
if ~ind1.equal[ind2] then pause;end 

// string sort 
//-------------------------------------

N=10;P=20;
a=int(10*rand(N,P,'uniform'));

//  global sort decreasing 

a0=string(a);
[a1,ind]=gsort(a0,'g');
[a2,ind2]=gsort(a,'g');
if or(a1<> string(matrix(a(ind),N,P)))  then pause,end;
if or(a1<>string(a2) )  then pause,end;

// rows 
[a1,ind1]=gsort(string(a),'r');   
[a2,ind2]=gsort(a,'r');
if or(ind1<>ind2)  then pause,end;
if or(a1<> string(a2) )  then pause,end;

// columns 
[a1,ind1]=gsort(string(a),'c')   ;                      
[a2,ind2]=gsort(a,'c');
if or(ind1<>ind2)  then pause,end;
if or(a1<>string(a2) )  then pause,end;

// lexicographic sort of the rows. 

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

if or(a1<>string( matrix(a(ind,:),N,P)))  then pause,end;
if or(a1<>string(a2) )  then pause,end;

// lexicographic sort of the columns 

N=3;P=4;
alr=alr';
// a random permutation 
[ax,perm]=sort(rand(1,P,'uniform'));
a=alr(:,perm);

[a1,ind]=gsort(string(a),'lc');                   
[a2,ind]=gsort(a,'lc') ;
//
if or(a1<>string( matrix(a(:,ind),N,P)))  then pause,end;
if or(a1<>string(a2) )  then pause,end;






