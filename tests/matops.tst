// -*- Mode: scilab -*- 
// Copyright (C) 2005-2009 J.P Chancelier Cermics/Enpc
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
// 
// Test for basic matrix operations
// 

function [y]=test(x1,x2)
  y=or(x1<>x2)
endfunction

function [y]=testN(x1,x2,z)
  y=norm(x1-x2)>z
endfunction 

nr=4
nc=7;
M=[nr,nc,0,1,1 ,nr];
N=[nc,nr,0,1,nc,1 ];

//----- test of size

for i=1:6, m=M(i);n=N(i);
  a=rand(m,n,'uniform');
  if test(size(a),[m,n]) then pause,end
  if test(size(a,'*'),m*n) then pause,end
  if test(size(a,'r'),m) then pause,end
  if test(size(a,'c'),n) then pause,end
  [m1,n1]=size(a);
  if test([m1,n1],[m,n]) then pause,end
end 

//----- test of eye

for i=1:6, m=M(i);n=N(i);
  a=rand(m,n,'uniform');
  b=0*a;for j=1:min(m,n), b(j,j)=1;end
  if test(eye_new(m,n),b) then pause,end
  if test(eye(size(a)),b) then pause,end
end 

//----- test of ones 

function [a]=Mones(m,n)
  a=rand(m,n,'uniform'),
  a(a>=0)=1
endfunction

for i=1:6, m=M(i);n=N(i);a=rand(m,n,'uniform');
  if test(ones_new(m,n),Mones(m,n)) then pause,end
  if test(ones(size(a)),Mones(m,n)) then pause,end
end 

//------- cross test eye-ones-diag 

function [a]=Meye(m,n)
  if m<n then a=[diag(ones_new(1,m)),0*ones_new(m,n-m)];
  else a=[diag(ones_new(1,n));0*ones_new(m-n,n)];end
endfunction

for i=1:6, m=M(i);n=N(i);
  a=rand(m,n,'uniform');
  if test(eye_new(m,n),Meye(m,n)) then pause,end
  if test(eye(size(a)),Meye(m,n)) then pause,end
end 

//------- test of diag (creation) 

function [a]=Mdiag(d)
  m=size(d,'*');a=0*ones_new(m,m);for j=1:m, a(j,j)=d(j);end
endfunction

for i=1:6, m=M(i);
  d=1:m
  b=0*ones_new(m,m);for j=1:m, b(j,j)=d(j);end
  if test(diag(d),b) then pause,end
end 

function [a]=MFdiag(d,j)
  if isempty(d) then 
    a=zeros(max(abs(j)-1,0),max(abs(j)-1,0));
  else 
    a=Mdiag(d);m=size(a,'r');
    if ~isempty(a) then 
      if j>=0; a=[0*ones_new(m+j,j),[a;0*ones_new(j,m)]];
      else a=[0*ones_new(-j,m-j);[a,0*ones_new(m,-j)]];end
    end
  end
endfunction

for i=1:6, m=M(i);
  for j=-2:2
    d=1:m;
    if test(diag(d,j),MFdiag(d,j)) then pause,end
    if test(diag(d',j),MFdiag(d,j)) then pause,end
  end 
end 


//-------- test of diag (extraction) 
// diag(a)
Mde=[nr,nc,0];
Nde=[nc,nr,0];
for i=1:3, m=Mde(i);n=Nde(i);
  d=rand(m,n,'uniform');
  mm=min(m,n);
  b=0*ones_new(mm,min(1,mm));for j=1:min(m,n), b(j)=d(j,j);end
  if test(diag(d),b) then pause,end
end 
// diag(a,j) 

function [d]=DiagE(a,j)
  if j>0 ; d=diag(a(1:$-j,j+1:$));
  elseif j==0; d=diag(a);
  else d=diag(a(-j+1:$,1:$+j));end
endfunction

for i=1:3, m=Mde(i);n=Nde(i);
  for j=-(m+1):m+1;
    d=rand(m,n,'uniform');
    mm=min(m,n);
    b=0*ones_new(mm,min(1,mm));for k=1:min(m,n), b(k)=d(k,k);end
    if test(diag(d),b) then pause,end
  end 
end 

//----------test of  triu

function [T]=Triu(a,k)
  [m,n]=size(a);T=a;
  if k>=0;for i=1:m;T(i,1:min(i+k-1,n))=0;end;
  else if -k+1<=m; for i=(-k+1):m; T(i,1:min(i+k-1,n))=0;end;end;end
endfunction

for i=1:6, m=M(i);n=N(i);
  for j=-(m+1):m+1;
    A=int(10*rand(m,n,'uniform'));
    if test(triu(A,j),Triu(A,j)) then pause,end
  end 
end 

//----------test of  tril 

function [T]=Tril(a,k)
  [m,n]=size(a);T=a;
  if k>=0;for i=1:m;for j=i+k+1:n,T(i,j)=0;end;end;
  else for i=1:m;for j=max(i+k+1,1):n,T(i,j)=0;end;end;end
endfunction

for i=1:6, m=M(i);n=N(i);
  for j=-(m+1):m+1;
    A=int(10*rand(m,n,'uniform'));
    if test(tril(A,j),Tril(A,j)) then pause,end
  end 
end 

//--------- test of abs  

function [b]=Abs(a)
  if isempty(a) then b=a;else b=max(a,0)+max(-a,0);end;
endfunction

for i=1:6, m=M(i);n=N(i);
  A=rand(m,n,'normal');
  if test(abs(A),Abs(A)) then pause,end
end 

//--------- test of real 

for i=1:6, m=M(i);n=N(i);
  A=rand(m,n,'normal');
  if test(real(A),A) then pause,end
end 

//--------- test of imag

for i=1:6, m=M(i);n=N(i);
  A=rand(m,n,'normal');
  if test(imag(A),0*A) then pause,end
end 

//--------- test of conj 

for i=1:6, m=M(i);n=N(i);
  A=rand(m,n,'normal');
  if test(conj(A),A) then pause,end
end 



//--------- test of int 

for i=1:6, m=M(i);n=N(i);
  A=int(10*rand(m,n,'uniform'));
  U=rand(m,n,'uniform');
  B=A+U;
  if test(int(B),A) then pause,end
  B=-A-U;
  if test(int(B),-A) then pause,end
end 

//----------test of round

//limit 
//m=10;
//AVOIR A FINIR 
//round en C anint ne donne pas la meme chose 
//if test(round((0:m)+0.5),0:m) then pause,end
//if test(round(-(1:m)+0.5),-(1:m)+1) then pause,end

//generic 
for i=1:6, m=M(i);n=N(i);
  A=int(10*rand(m,n,'uniform'));
  U=rand(m,n,'uniform')*0.5;
  B=A+U;
  if test(round(B),A) then pause,end
  if A<>[] then 
    B=A+U+0.5;
    if test(round(B),A+1) then pause,end
  end
end 

//----------test of ceil 

function [y]=Mceil(x)
  if isempty(x) then y=x;else y=int(x)+1;y(x<0)=y(x<0)-1;end
endfunction

for i=1:6, m=M(i);n=N(i);
  A=10*rand(m,n,'normal');
  if test(ceil(A),Mceil(A)) then pause,end
end 

//----------test of floor

function [y]=Mfloor(x)
  if isempty(x) then y=x;else y=int(x);y(x<0)=y(x<0)-1;end
endfunction

for i=1:6, m=M(i);n=N(i);
  A=10*rand(m,n,'normal');
  if test(floor(A),Mfloor(A)) then pause,end
end 

//----------test of sign

function [y]=Msign(x)
  z=x(x<>0);z=z./abs(z);y=x;y(x<>0)=z;
endfunction

if sign(0)<>0 then pause,end

for i=1:6, m=M(i);n=N(i);
  A=rand(m,n,'normal');
  if test(sign(A),Msign(A)) then pause,end
end 

//----------test of sum 

function [y]=Msum(x,j)
  [m,n]=size(x);
  if j==2, y= x*ones_new(n,1);
  elseif j==1,y=ones_new(1,m)*x;
  else y= ones_new(1,m)*x*ones_new(n,1);end;
endfunction;

function [y]=MsumG(x,j)
  [m,n]=size(x);
  if m==0 && n==0 && j==0 then y=0;else y=Msum(x,j);end
endfunction;

for i=1:6, m=M(i);n=N(i);
  A=int(10*rand(m,n,'normal'));
  if testN(sum(A),MsumG(A,0),0.1) then pause,end
  if testN(sum(A,'c'),MsumG(A,2),0.1) then pause,end
  //AFAIRE if testN(sum(A,2),MsumG(A,2),0.1) then pause,end
  if testN(sum(A,'r'),MsumG(A,1),0.1) then pause,end
  //AFAIRE if testN(sum(A,1),MsumG(A,1),0.1) then pause,end
end 

//----------test of prod

function [p]=Mprod(x,j)
  [m,n]=size(x);
  if j==2 then ;p=0*ones(m,1);for i=1:m,p(i)=Vprod(x(i,:));end;
  elseif j==1 then p=0*ones(1,n);for i=1:n,p(i)=Vprod(x(:,i));end;
  else p= Mprod(Mprod(x,1),2);end;
endfunction

function [vp]=Vprod(x)
  m=size(x,'*');
  vp=x(1);for i=2:m,vp=vp*x(i);end
endfunction

function [y]=MprodG(x,j)
  if m==0 && n==0 && j==0  then y=1;else y=Mprod(x,j);end
endfunction

nr=3;
nc=4;
Mp=[nr,nc,0,1,1 ,nr];
Np=[nc,nr,0,1,nc,1 ];

for i=1:6, m=Mp(i);n=Np(i);
  A=int(10*rand(m,n,'uniform'));
  if testN(prod(A),MprodG(A,0),0.1) then pause,end
  if testN(prod(A,'c'),MprodG(A,2),0.1) then pause,end
  if testN(prod(A,2),MprodG(A,2),0.1) then pause,end
  if testN(prod(A,'r'),MprodG(A,1),0.1) then pause,end
  if testN(prod(A,1),MprodG(A,1),0.1) then pause,end
end 

//----------test of cumsum

function [y]=Mcumsum(x,j)
  [m,n]=size(x);
  if j==2, y=0*x; for i=1:n; y(:,i)= x(:,1:i)*ones_new(i,min(1,i));end;
  elseif j==1,y=Mcumsum(x',2)';
  else y=x(:);y=Mcumsum(y,1);y=matrix(y,m,n);end
endfunction

function [y]=McumsumG(x,j)
  if isempty(x) then y=x; else y=Mcumsum(x,j);end
endfunction

for i=1:6, m=M(i);n=N(i);
  A=10*rand(m,n,'normal');
  if testN(cumsum(A),McumsumG(A,0),1.e-8) then pause,end
  if testN(cumsum(A,'c'),McumsumG(A,2),1.e-8) then pause,end
  if testN(cumsum(A,2),McumsumG(A,2),1.e-8) then pause,end
  if testN(cumsum(A,'r'),McumsumG(A,1),1.e-8) then pause,end
  if testN(cumsum(A,1),McumsumG(A,1),1.e-8) then pause,end
end 

//----------test of cumprof 

//cumprod
a=[1 2;-3 4;5,-6];
if or(cumprod(a)<>[1, -30;-3, -120;-15 720]) then pause,end
if or(cumprod(a+0)<> [1, -30;-3, -120;-15 720]) then pause,end
if or(cumprod(a,'r')<>[1 2;-3, 8;-15 ,-48]) then pause,end
if or(cumprod(a+0,'r')<>[1 2;-3, 8;-15, -48]) then pause,end
n='r';
if or(cumprod(a,n)<>[1 2;-3 8;-15, -48]) then pause,end
if or(cumprod(a+0,n)<>[1 2;-3 8;-15 ,-48]) then pause,end
if or(cumprod(a,'r')<>[1 2;-3 8;-15 ,-48]) then pause,end
if or(cumprod(a+0,'r')<>[1 2;-3 8;-15, -48]) then pause,end
n='r';
if or(cumprod(a,n)<>[1 2;-3 8;-15, -48]) then pause,end
if or(cumprod(a+0,n)<>[1 2;-3 8;-15, -48]) then pause,end

if or(cumprod(a,'c')<>[1 2;-3 ,-12;5, -30]) then pause,end
if or(cumprod(a+0,'c')<>[1 2;-3, -12;5, -30]) then pause,end
n='c';
if or(cumprod(a,n)<>[1 2;-3, -12;5, -30]) then pause,end
if or(cumprod(a+0,n)<>[1 2;-3 ,-12;5, -30]) then pause,end
if or(cumprod(a,'c')<>[1 2;-3, -12;5, -30]) then pause,end
if or(cumprod(a+0,'c')<>[1 2;-3, -12;5, -30]) then pause,end
n='c';
if or(cumprod(a,n)<>[1 2;-3, -12;5 ,-30]) then pause,end
if or(cumprod(a+0,n)<>[1 2;-3, -12;5 ,-30]) then pause,end

a=[1 2;-3 4;5,-6]+0*%i;
if cumprod(a)<>[1, -30;-3, -120;-15 720]+0*%i then pause,end
if cumprod(a+0)<>[1, -30;-3, -120;-15 720]+0*%i then pause,end
if or(cumprod(a,'r')<>[1 2;-3 8;-15, -48]+0*%i) then pause,end
if or(cumprod(a+0,'r')<>[1 2;-3 8;-15, -48]+0*%i) then pause,end
n='r';
if or(cumprod(a,n)<>[1 2;-3 8;-15, -48]+0*%i) then pause,end
if or(cumprod(a+0,n)<>[1 2;-3 8;-15, -48]+0*%i) then pause,end
if or(cumprod(a,'r')<>[1 2;-3 8;-15, -48]+0*%i) then pause,end
if or(cumprod(a+0,'r')<>[1 2;-3 8;-15, -48]+0*%i) then pause,end
n='r';
if or(cumprod(a,n)<>[1 2;-3 8;-15, -48]+0*%i) then pause,end
if or(cumprod(a+0,n)<>[1 2;-3 8;-15 ,-48]+0*%i) then pause,end

if or(cumprod(a,'c')<>[1 2;-3, -12;5, -30]+0*%i) then pause,end
if or(cumprod(a+0,'c')<>[1 2;-3, -12;5, -30]+0*%i) then pause,end
n='c';
if or(cumprod(a,n)<>[1 2;-3, -12;5, -30]+0*%i) then pause,end
if or(cumprod(a+0,n)<>[1 2;-3, -12;5, -30]+0*%i) then pause,end
if or(cumprod(a,'c')<>[1 2;-3, -12;5 ,-30]+0*%i) then pause,end
if or(cumprod(a+0,'c')<>[1 2;-3, -12;5, -30]+0*%i) then pause,end
n='c';
if or(cumprod(a,n)<>[1 2;-3, -12;5, -30]+0*%i) then pause,end
if or(cumprod(a+0,n)<>[1 2;-3, -12;5 ,-30]+0*%i) then pause,end

a=[];
if cumprod(a)<>[] then pause,end
if cumprod([])<>[] then pause,end
if cumprod(a,'r')<>[] then pause,end
if cumprod([],'r')<>[] then pause,end
n='r';
if cumprod(a,n)<>[] then pause,end
if cumprod([],n)<>[] then pause,end
if cumprod(a,'r')<>[] then pause,end
if cumprod([],'r')<>[] then pause,end
n='r';
if cumprod(a,n)<>[] then pause,end
if cumprod([],n)<>[] then pause,end

if cumprod(a,'c')<>[] then pause,end
if cumprod([],'c')<>[] then pause,end
n='c';
if cumprod(a,n)<>[] then pause,end
if cumprod([],n)<>[] then pause,end
if cumprod(a,'c')<>[] then pause,end
if cumprod([],'c')<>[] then pause,end
n='c';
if cumprod(a,n)<>[] then pause,end
if cumprod([],n)<>[] then pause,end

a=sparse([1 2;-3 4;5,-6]);
//if cumprod(a)<> [1 -30;-3 -120;-15 720] then pause,end
//if cumprod(a+0*a)<> [1 -30;-3 -120;-15 720] then pause,end
a=sparse([1 2;-3 4;5,-6]+0*%i);
//if cumprod(a)<>[1 -30;-3 -120;-15 720]+0*%i then pause,end
//if cumprod(a+0*a)<>[1 -30;-3 -120;-15 720]+0*%i then pause,end

//----------test of max 

function [y,k]=Mmax(x)
  if isempty(x) then y=x;k=x;return;end  
  k=1,y=x(1);
  for i=1:size(x,'*'),
    if x(i)>y,y=x(i),k=i;end;
  end
endfunction

function [y,k]=MmaxG(x,j)
  [m,n]=size(x);
  if isempty(x) then y=x;k=x;return;end 
  if j=='c', y=0*ones_new(m,1),k=y;
    for i=1:m,[yi,ki]=Mmax(x(i,:));
      y(i)=yi;k(i)=ki;
    end;
  else  
    y=0*ones_new(1,n),k=y;
    for i=1:n,[yi,ki]=Mmax(x(:,i));
      y(i)=yi;k(i)=ki;
    end;
  end;
endfunction

for i=1:6, m=M(i);n=N(i);
  A=10*rand(m,n,'normal');
  if test(max(A),Mmax(A)) then pause,end
  [Am,Km]=max(A);[Am1,Km1]=Mmax(A);
  if test(Am,Am1) then pause,end
  // A FINIR      if test(Km,Km1) then pause,end
  // 
  if test(max(A,'r'),MmaxG(A,'r')) then pause,end
  [Am,Km]=max(A,'r');[Am1,Km1]=MmaxG(A,'r');
  if test(Am,Am1) then pause,end
  if test(Km,Km1) then pause,end
  //
  if test(max(A,'c'),MmaxG(A,'c')) then pause,end
  [Am,Km]=max(A,'c');[Am1,Km1]=MmaxG(A,'c');
  if test(Am,Am1) then pause,end
  if test(Km,Km1) then pause,end
end 

//min
a=[1 2;-3 4;5,-6];
if or(min(a)<>-6) then pause,end
if or(min(a+0)<>-6) then pause,end
if or(min(a,'r')<>[-3,-6]) then pause,end
if or(min(a+0,'r')<>[-3,-6]) then pause,end
n='r';
if or(min(a,n)<>[-3,-6]) then pause,end
if or(min(a+0,n)<>[-3,-6]) then pause,end

if or(min(a,'c')<>[1;-3;-6]) then pause,end
if or(min(a+0,'c')<>[1;-3;-6]) then pause,end
n='c';
if or(min(a,n)<>[1;-3;-6]) then pause,end
if or(min(a+0,n)<>[1;-3;-6]) then pause,end

//a=[1 2;-3 4;5,-6]+0*%i;
//if min(a)<>[1 -30;-3 -120;-15 720]+0*%i then pause,end
//if min(a+0)<>[1 -30;-3 -120;-15 720]+0*%i then pause,end
//if or(min(a,'r')<>[-3,-6]+0*%i) then pause,end
//if or(min(a+0,'r')<>[-3,-6]+0*%i) then pause,end
//n='r';
//if or(min(a,n)<>[-3,-6]+0*%i) then pause,end
//if or(min(a+0,n)<>[-3,-6]+0*%i) then pause,end

//if or(min(a,'c')<>[1;-3;-6]+0*%i) then pause,end
//if or(min(a+0,'c')<>[1;-3;-6]+0*%i) then pause,end
//n='c';
//if or(min(a,n)<>[1;-3;-6]+0*%i) then pause,end
//if or(min(a+0,n)<>[1;-3;-6]+0*%i) then pause,end

a=[];
if min(a)<>[] then pause,end
if min([])<>[] then pause,end

if min(a,'r')<>[] then pause,end
if min([],'r')<>[] then pause,end
n='r';
if min(a,n)<>[] then pause,end
if min([],n)<>[] then pause,end

if min(a,'c')<>[] then pause,end
if min([],'c')<>[] then pause,end
n='c';
if min(a,n)<>[] then pause,end
if min([],n)<>[] then pause,end


a=sparse([1 2;-3 4;5,-6]);
//if min(a)<>-6 then pause,end
//if min(a+0*a)<>-6 then pause,end
a=sparse([1 2;-3 4;5,-6]+0*%i);
//if min(a)<>[1 -30;-3 -120;-15 720]+0*%i then pause,end
//if min(a+0*a)<>[1 -30;-3 -120;-15 720]+0*%i then pause,end

//sort
a=[5 1 3 2 4]
if or(sort(a)<>[5 4 3 2 1]) then pause,end
if or(sort(a+0)<>[5 4 3 2 1]) then pause,end
[s,k]=sort(a);
if or(k<>[1 5 3 4 2]) then pause,end
if or(s<>[5 4 3 2 1]) then pause,end
[s,k]=sort(a+0);
if or(k<>[1 5 3 4 2]) then pause,end
if or(s<>[5 4 3 2 1]) then pause,end

a=string([5 1 3 2 4])
//if or(sort(a)<>string(1:5)) then pause,end
//if or(sort(string([5 1 3 2 4]))<>string(1:5)) then pause,end
[s,k]=sort(a);
//if or(k<>[2 4 3 5 1]) then pause,end
//if or(s<>string(1:5)) then pause,end
[s,k]=sort(string([5 1 3 2 4]));
//if or(k<>[2 4 3 5 1]) then pause,end
//if or(s<>string(1:5)) then pause,end

a=[]
if sort(a)<>[] then pause,end
[s,k]=sort(a);if s<>[]|k<>[] then pause,end

if sort([])<>[] then pause,end
[s,k]=sort([]);if s<>[]|k<>[] then pause,end

//kron
function y=kron(a,b) y=a.*.b;endfunction

a=[1 2];b=[3;4];
if or(kron(a,b)<>[3 6;4 8]) then pause,end
if or(kron(a+0,b)<>[3 6;4 8]) then pause,end
if or(kron(a,b+0)<>[3 6;4 8]) then pause,end
if or(kron(a+0,b+0)<>[3 6;4 8]) then pause,end
if kron([],b)<>[] then pause,end
if kron([],b+0)<>[] then pause,end
a=[];
if kron(a,b)<>[] then pause,end
if kron(a,b+0)<>[] then pause,end
a=[1 2];b=[]
if kron(a,b)<>[] then pause,end
if kron(a+0,b)<>[] then pause,end
if kron(a,[])<>[] then pause,end
if kron(a+0,[])<>[] then pause,end
a=[];b=[];
if kron(a,b)<>[] then pause,end
if kron(a,[])<>[] then pause,end
if kron([],b)<>[] then pause,end
if kron([],[])<>[] then pause,end

//matrix
a=[1 2 3 4 5 6];
n=1;m=6;
if or(matrix(a,1,6)<>a) then pause,end
if or(matrix(a,n,6)<>a) then pause,end
if or(matrix(a,1,m)<>a) then pause,end
if or(matrix(a,n,m)<>a) then pause,end
if or(matrix(a+0,1,6)<>a) then pause,end
if or(matrix(a+0,n,6)<>a) then pause,end
if or(matrix(a+0,1,m)<>a) then pause,end
if or(matrix(a+0,n,m)<>a) then pause,end

n=3;m=2; b=[1 4;2 5;3 6];
if or(matrix(a,3,2)<>b) then pause,end
if or(matrix(a,n,2)<>b) then pause,end
if or(matrix(a,3,m)<>b) then pause,end
if or(matrix(a,n,m)<>b) then pause,end
if or(matrix(a+0,3,2)<>b) then pause,end
if or(matrix(a+0,n,2)<>b) then pause,end
if or(matrix(a+0,3,m)<>b) then pause,end
if or(matrix(a+0,n,m)<>b) then pause,end

a=[1+%i 2 3 4 5 6];
n=1;m=6;
if or(matrix(a,1,6)<>a) then pause,end
if or(matrix(a,n,6)<>a) then pause,end
if or(matrix(a,1,m)<>a) then pause,end
if or(matrix(a,n,m)<>a) then pause,end
if or(matrix(a+0,1,6)<>a) then pause,end
if or(matrix(a+0,n,6)<>a) then pause,end
if or(matrix(a+0,1,m)<>a) then pause,end
if or(matrix(a+0,n,m)<>a) then pause,end

n=3;m=2; b=[1+%i 4;2 5;3 6];
if or(matrix(a,3,2)<>b) then pause,end
if or(matrix(a,n,2)<>b) then pause,end
if or(matrix(a,3,m)<>b) then pause,end
if or(matrix(a,n,m)<>b) then pause,end
if or(matrix(a+0,3,2)<>b) then pause,end
if or(matrix(a+0,n,2)<>b) then pause,end
if or(matrix(a+0,3,m)<>b) then pause,end
if or(matrix(a+0,n,m)<>b) then pause,end

a=string([1 2 3 4 5 6]);n=1;m=6;
 if or(matrix(a,1,6)<>a) then pause,end
 if or(matrix(a,n,6)<>a) then pause,end
 if or(matrix(a,1,m)<>a) then pause,end
 if or(matrix(a,n,m)<>a) then pause,end
 if or(matrix(a+a,1,6)<>a+a) then pause,end
 if or(matrix(a+a,n,6)<>a+a) then pause,end
 if or(matrix(a+a,1,m)<>a+a) then pause,end
 if or(matrix(a+a,n,m)<>a+a) then pause,end

n=3;m=2; b=string([1 4;2 5;3 6]);
if or(matrix(a,3,2)<>b) then pause,end
if or(matrix(a,n,2)<>b) then pause,end
if or(matrix(a,3,m)<>b) then pause,end
if or(matrix(a,n,m)<>b) then pause,end
if or(matrix(a+a,3,2)<>b+b) then pause,end
if or(matrix(a+a,n,2)<>b+b) then pause,end
if or(matrix(a+a,3,m)<>b+b) then pause,end
if or(matrix(a+a,n,m)<>b+b) then pause,end

//AFAIRE a=[1 2 3 4 5 6]+%s;
//AFAIRE n=1;m=6;
//AFAIRE if or(matrix(a,1,6)<>a) then pause,end
//AFAIRE if or(matrix(a,n,6)<>a) then pause,end
//AFAIRE if or(matrix(a,1,m)<>a) then pause,end
//AFAIRE if or(matrix(a,n,m)<>a) then pause,end
//AFAIRE if or(matrix(a+0,1,6)<>a) then pause,end
//AFAIRE if or(matrix(a+0,n,6)<>a) then pause,end
//AFAIRE if or(matrix(a+0,1,m)<>a) then pause,end
//AFAIRE if or(matrix(a+0,n,m)<>a) then pause,end

//AFAIRE n=3;m=2; b=[1 4;2 5;3 6]+%s;
//AFAIRE if or(matrix(a,3,2)<>b) then pause,end
//AFAIRE if or(matrix(a,n,2)<>b) then pause,end
//AFAIRE if or(matrix(a,3,m)<>b) then pause,end
//AFAIRE if or(matrix(a,n,m)<>b) then pause,end
//AFAIRE if or(matrix(a+0,3,2)<>b) then pause,end
//AFAIRE if or(matrix(a+0,n,2)<>b) then pause,end
//AFAIRE if or(matrix(a+0,3,m)<>b) then pause,end
//AFAIRE if or(matrix(a+0,n,m)<>b) then pause,end
//AFAIRE 
//AFAIRE a=[1+%i 2 3 4 5 6]+%s;
//AFAIRE n=1;m=6;
//AFAIRE if or(matrix(a,1,6)<>a) then pause,end
//AFAIRE if or(matrix(a,n,6)<>a) then pause,end
//AFAIRE if or(matrix(a,1,m)<>a) then pause,end
//AFAIRE if or(matrix(a,n,m)<>a) then pause,end
//AFAIRE if or(matrix(a+0,1,6)<>a) then pause,end
//AFAIRE if or(matrix(a+0,n,6)<>a) then pause,end
//AFAIRE if or(matrix(a+0,1,m)<>a) then pause,end
//AFAIRE if or(matrix(a+0,n,m)<>a) then pause,end
//AFAIRE 
//AFAIRE n=3;m=2; b=[1+%i 4;2 5;3 6]+%s;
//AFAIRE if or(matrix(a,3,2)<>b) then pause,end
//AFAIRE if or(matrix(a,n,2)<>b) then pause,end
//AFAIRE if or(matrix(a,3,m)<>b) then pause,end
//AFAIRE if or(matrix(a,n,m)<>b) then pause,end
//AFAIRE if or(matrix(a+0,3,2)<>b) then pause,end
//AFAIRE if or(matrix(a+0,n,2)<>b) then pause,end
//AFAIRE if or(matrix(a+0,3,m)<>b) then pause,end
//AFAIRE if or(matrix(a+0,n,m)<>b) then pause,end


//clean
a=[1 1.d-12 1.d-5 2d8];
b=[1 0 0 2d8];
if or(clean(a)<>b) then pause,end
if or(clean(a+0)<>b) then pause,end
epsa=1.d-10;
if or(clean(a,epsa)<>b) then pause,end
if or(clean(a+0,epsa)<>b) then pause,end
if or(clean(a,epsa+0)<>b) then pause,end
if or(clean(a+0,epsa+0)<>b) then pause,end
epsr=1.d-5;b=[0 0 0 2d8];
if or(clean(a,epsa,epsr)<>b) then pause,end
if or(clean(a+0,epsa,epsr)<>b) then pause,end
if or(clean(a,epsa+0,epsr)<>b) then pause,end
if or(clean(a+0,epsa,epsr+0)<>b) then pause,end
if or(clean(a,epsa+0,epsr)<>b) then pause,end
if or(clean(a+0,epsa,epsr+0)<>b) then pause,end
if or(clean(a,epsa+0,epsr)<>b) then pause,end
if or(clean(a+0,epsa+0,epsr+0)<>b) then pause,end

a=[1+%i 1.d-12 1.d-5 2d8];
b=[1+%i 0 0 2d8];
if or(clean(a)<>b) then pause,end
if or(clean(a+0)<>b) then pause,end
epsa=1.d-10;
if or(clean(a,epsa)<>b) then pause,end
if or(clean(a+0,epsa)<>b) then pause,end
if or(clean(a,epsa+0)<>b) then pause,end
if or(clean(a+0,epsa+0)<>b) then pause,end
epsr=1.d-5;b=[0+0*%i 0 0 2d8];
if or(clean(a,epsa,epsr)<>b) then pause,end
if or(clean(a+0,epsa,epsr)<>b) then pause,end
if or(clean(a,epsa+0,epsr)<>b) then pause,end
if or(clean(a+0,epsa,epsr+0)<>b) then pause,end
if or(clean(a,epsa+0,epsr)<>b) then pause,end
if or(clean(a+0,epsa,epsr+0)<>b) then pause,end
if or(clean(a,epsa+0,epsr)<>b) then pause,end
if or(clean(a+0,epsa+0,epsr+0)<>b) then pause,end

//log and exp
a=[1.2 ,2.51;-3.4, 4.52;5.8,-6.2];
if norm(exp(log(a))-a)>20*%eps then pause,end
if norm(exp(log(a+0))-a)>20*%eps then pause,end
b=log(a);if norm(exp(b)-a)>20*%eps then pause,end
a=[1+%i 2.51;-3.4 4.52;5.8,-6.2];
if norm(exp(log(a))-a)>20*%eps then pause,end
if norm(exp(log(a+0))-a)>20*%eps then pause,end
b=log(a);if norm(exp(b)-a)>20*%eps then pause,end

if ~isempty(exp([])) then pause,end
a=[];if ~isempty(exp(a)) then pause,end
if ~isempty(log([])) then pause,end
a=[];if ~isempty(log(a)) then pause,end

//sin and cos
a=[1.2 2.51;-3.4 4.52;5.8,-6.2];
if norm(sin(a).^2+cos(a).^2-1)>10*%eps then pause,end
if norm(sin(a+0).^2+cos(a+0).^2-1)>10*%eps then pause,end
a=[1+%i 2.51;-3.4 4.52;5.8,-6.2];
if norm(sin(a).^2+cos(a).^2-1)>10*%eps then pause,end
if norm(sin(a+0).^2+cos(a+0).^2-1)>10*%eps then pause,end

if cos([])<>[] then pause,end
a=[];if cos(a)<>[] then pause,end
if sin([])<>[] then pause,end
a=[];if sin(a)<>[] then pause,end


//tan et atan
a=[1.2 2.51;-3.4 4.52;5.8,-6.2];
if norm(tan(atan(a))-a)>100*%eps then pause,end
if norm(tan(atan(a+0))-a)>100*%eps then pause,end
b=log(a);if norm(exp(b)-a)>100*%eps then pause,end
a=[1+%i 2.51;-3.4 4.52;5.8,-6.2];
if norm(tan(atan(a))-a)>100*%eps then pause,end
if norm(tan(atan(a+0))-a)>100*%eps then pause,end

if atan([])<>[] then pause,end
a=[];if atan(a)<>[] then pause,end

if ~isempty(atan([],[])) then pause,end
a=[];if ~isempty(atan(a,[])) then pause,end
a=[];if ~isempty(atan(a,a)) then pause,end
a=[];if ~isempty(atan([],a)) then pause,end

//expm
a=[0 2;0 0];
if norm(expm(a)-[1 2;0 1])>10*%eps then pause,end
a=[0 2*%i;0 0];
if norm(expm(a)-[1 2*%i;0 1])>10*%eps then pause,end
if expm([])<>[] then pause,end
a=[];if expm(a)<>[] then pause,end

//sqrt
a=[1.2 2.51;-3.4 4.52;5.8,-6.2];
if norm(sqrt(a).^2-a)>100*%eps then pause,end
if norm(sqrt(a+0).^2-a)>100*%eps then pause,end
a=[1+%i 2.51;-3.4 4.52;5.8,-6.2];
if norm(sqrt(a).^2-a)>100*%eps then pause,end
if norm(sqrt(a+0).^2-a)>100*%eps then pause,end

if sqrt([])<>[] then pause,end
a=[];if sqrt(a)<>[] then pause,end

// issymmetric 

a=triu(rand(4,4));
if issymmetric(a) then pause;end 
if ~issymmetric(a+a') then pause;end 
b=a + %i*triu(rand(4,4));
if issymmetric(b) then pause;end 
if ~issymmetric(b+b') then pause;end 

