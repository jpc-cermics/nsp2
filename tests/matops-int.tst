// -*- Mode: nsp -*- 
// Copyright (C) 2005-2015 J.P Chancelier Cermics/Enpc
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
// Test for basic int matrix operations
// 

nr=4
nc=7;
M=[nr,nc,0,1,1 ,nr];
N=[nc,nr,0,1,nc,1 ];
l_itypes=['int32', 'int64'];
sign_itypes=['int','short', 'long','int8', 'int16','int32', 'int64']
itypes=[sign_itypes, 'u'+sign_itypes];

//----- test of size

for i=1:6, m=M(i);n=N(i);
  a=m2i(rand(m,n));
  if or(size(a)<>[m,n]) then pause,end
  if or(size(a,'*')<>m*n) then pause,end
  if or(size(a,'r')<>m) then pause,end
  if or(size(a,'c')<>n) then pause,end
  [m1,n1]=size(a);
  if or([m1,n1]<>[m,n]) then pause,end
end 

//----- test of ieye

for i=1:6, m=M(i);n=N(i);
  b=eye(m,n);
  for itype=itypes
    if or(ieye(m,n,itype)<>m2i(b,itype)) then pause,end
    if or(ieye(size(b),itype)<>m2i(b,itype)) then pause,end
  end 
end


//----- test of iones 

for i=1:6, m=M(i);n=N(i);
  a=m2i(rand(m,n),itype);
  for itype=itypes
    if or(iones(m,n,itype)<>m2i(ones(m,n),itype)) then pause,end
    if or(iones(size(a),itype)<>m2i(ones(m,n),itype)) then pause,end
  end
end 

// test of izeros 

for i=1:6, m=M(i);n=N(i);
  a=m2i(rand(m,n),itype);
  for itype=itypes
    if or(izeros(m,n,itype)<>m2i(zeros(m,n),itype)) then pause,end
    if or(izeros(size(a),itype)<>m2i(zeros(m,n),itype)) then pause,end
  end
end 

//------- test of diag (creation) 

for i=1:6, m=M(i);
  for itype=itypes
    d=1:m;id=m2i(d,itype);
    for k=-2:2 
      if or(diag(id,k)<> m2i(diag(d,k),itype)) then pause,end
    end 
  end
end

//-------- test of diag (extraction) 

Mde=[nr,nc,0];
Nde=[nc,nr,0];
for i=1:3, m=Mde(i);n=Nde(i);
  for k=-(m+1):m+1;
    for itype=itypes
      d=rand(m,n);id=m2i(d,itype);
      mm=min(m,n);
      if or(diag(id,k)<>m2i(diag(d,k),itype)) then pause,end
    end 
  end
end

//----------test of  triu

for i=1:6, m=M(i);n=N(i);
  for j=-(m+1):m+1;
    for itype=itypes
      a=10*rand(m,n);ia=m2i(a,itype);
      if or(triu(ia,j)<>m2i(triu(a,j),itype)) then pause,end
    end 
  end 
end

//----------test of  tril 

for i=1:6, m=M(i);n=N(i);
  for j=-(m+1):m+1;
    for itype=itypes
      a=10*rand(m,n);ia=m2i(a,itype);
      if or(tril(ia,j)<>m2i(tril(a,j),itype)) then pause,end
    end 
  end 
end

//--------- test of abs  

for i=1:6, m=M(i);n=N(i);
  for itype=sign_itypes
    A=randn(m,n);ia=m2i(A,itype)
    if or(abs(ia)<>m2i(abs(A),itype)) then pause,end
  end 
end


//--------- test of real 
//--------- test of imag
//--------- test of conj 
//--------- test of int 
//----------test of round
//----------test of ceil 
//----------test of floor
//----------test of sign 

for i=1:6, m=M(i);n=N(i);
  for itype=sign_itypes
    A=int(10*randn(m,n));ia=m2i(A,itype)
    if or(sign(ia)<>m2i(sign(A),itype)) then pause,end
  end 
end

//----------test of sum 

for i=1:6, m=M(i);n=N(i);
  A=int(10*randn(m,n));
  for itype=sign_itypes
    ia=m2i(A,itype)
    if or(sum(ia)<>m2i(sum(A),itype)) then pause,end
    if or(sum(ia,'c')<>m2i(sum(A,'c'),itype)) then pause,end
    if or(sum(ia,'r')<>m2i(sum(A,'r'),itype)) then pause,end
    if or(sum(ia,'*')<>m2i(sum(A,'*'),itype)) then pause,end
  end 
end

//----------test of prod

nr=2;
nc=3;
Mp=[nr,nc,0,1,1 ,nr];
Np=[nc,nr,0,1,nc,1 ];

for i=1:6, m=Mp(i);n=Np(i);
  A=int(10*rand(m,n));
  for itype=l_itypes
    ia=m2i(A,itype)
    if or(prod(ia)<>m2i(prod(A),itype)) then pause,end
    if or(prod(ia,'c')<>m2i(prod(A,'c'),itype)) then pause,end
    if or(prod(ia,2)<>m2i(prod(A,2),itype)) then pause,end
    if or(prod(ia,'r')<>m2i(prod(A,'r'),itype)) then pause,end
    if or(prod(ia,1)<>m2i(prod(A,1),itype)) then pause,end
  end 
end

//----------test of cumsum

for i=1:6, m=M(i);n=N(i);
  A=int(10*randn(m,n));
  for itype=sign_itypes
    ia=m2i(A,itype)
    if or(cumsum(ia)<> m2i(cumsum(A,0),itype)) then pause,end
    if or(cumsum(ia,'c')<>m2i(cumsum(A,2),itype)) then pause,end
    if or(cumsum(ia,2)<>m2i(cumsum(A,2),itype)) then pause,end
    if or(cumsum(ia,'r')<>m2i(cumsum(A,1),itype)) then pause,end
    if or(cumsum(ia,1)<>m2i(cumsum(A,1),itype)) then pause,end
  end 
end

//----------test of cumprod

for i=1:6, m=Mp(i);n=Np(i);
  A=int(10*randn(m,n));
  for itype=l_itypes
    ia=m2i(A,itype)
    if or(cumprod(ia)<> m2i(cumprod(A,0),itype)) then pause,end
    if or(cumprod(ia,'c')<>m2i(cumprod(A,2),itype)) then pause,end
    if or(cumprod(ia,2)<>m2i(cumprod(A,2),itype)) then pause,end
    if or(cumprod(ia,'r')<>m2i(cumprod(A,1),itype)) then pause,end
    if or(cumprod(ia,1)<>m2i(cumprod(A,1),itype)) then pause,end
  end 
end

//----------test of max 

for i=1:6, m=M(i);n=N(i);
  A=int(10*randn(m,n));
  for itype=sign_itypes
    ia=m2i(A,itype)
    if or(max(ia)<> m2i(max(A),itype)) then pause,end
    if or(max(ia,'c')<>m2i(max(A,'c'),itype)) then pause,end
    if or(max(ia,'r')<>m2i(max(A,'r'),itype)) then pause,end
    if or(max(ia,'*')<>m2i(max(A,'*'),itype)) then pause,end
  end 
end

//----------test of min 

for i=1:6, m=M(i);n=N(i);
  A=int(10*randn(m,n));
  for itype=sign_itypes
    ia=m2i(A,itype)
    if or(max(ia)<> m2i(max(A),itype)) then pause,end
    if or(max(ia,'c')<>m2i(max(A,'c'),itype)) then pause,end
    if or(max(ia,'r')<>m2i(max(A,'r'),itype)) then pause,end
    if or(max(ia,'*')<>m2i(max(A,'*'),itype)) then pause,end
  end 
end

//------------test of kron

for i=1:6, m=M(i);n=N(i);
  A=int(10*randn(m,n));  B=int(10*randn(m,n));
  for itype=sign_itypes
    ia=m2i(A,itype); ib=m2i(B,itype); 
    if or( ia .*. ib <> m2i(A .*. B ,itype)) then pause,end
    if or(ia .*. ib  <> m2i(A .*. B,itype)) then pause,end
    if or(ia .*. ib  <> m2i(A .*. B,itype)) then pause,end
    if or(ia .*. ib  <> m2i(A .*. B,itype)) then pause,end
  end 
end

//------------test of matrix 

a=[1 2 3 4 5 6];
s=[1,6;6,1;2,3;3,2;-1,1;1,-1];

for k=1:size(s,'r')
  m=s(k,1);n=s(k,2);
  for itype=sign_itypes
    ia=m2i(a,itype);
    if or(matrix(ia,m,n)<>m2i(matrix(a,m,n),itype)) then pause,end
  end 
end

//clean
//log and exp
//sin and cos
//tan et atan
//expm
//sqrt
// issymmetric 

