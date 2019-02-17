// -*- Mode: nsp -*- 
// Copyright (C) 2011-2015 J.P Chancelier Cermics/Enpc
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

// basic test for gcd,lcm,euclide 

// Build data for tests 

xv=[3,5,7];
n=100 ;
for i=1:n
  cpx=grand(1,size(xv,'*'),'uin',0,4);
  x(i)=prod(xv.^cpx);
  cpy=grand(1,size(xv,'*'),'uin',0,4);
  y(i)=prod(xv.^cpy);
  gcds(i)=prod(xv.^min(cpx,cpy));
  lcms(i)=prod(xv.^(cpx+cpy- min(cpx,cpy)));
end

// test the extended euclide algorithm
// x and y can be two matrices of the same size

// case of double matrices 

g= euclide(x,y);
// g is the gcd. we check that gcd is ok 
if max(abs(gcds-g)) <> 0 then pause;end

[g,U,idet]= euclide(x,y);
// g is the gcd. we check that gcd is ok 
if max(abs(gcds-g)) <> 0 then pause;end
// checks that U matrix is ok 
for i=1:n 
  if max(abs([x(i),y(i)]*U{1,i} - [g(i),0])) <> 0  then pause;end
end
// checks that we recover the factors from U
for i=1:n 
  u = U{1,i};
  f = [ u(2,2), - u(1,2)]*idet(i);
  if max(abs(x(i) - f(1)*g(i))) <> 0  then pause;end
  if max(abs(y(i) - f(2)*g(i))) <> 0  then pause;end
end

// case of int matrices 

itypes=["int32", "uint32","int64", "uint64", "int", "long", "ulong"];
for itype=itypes
  ix=m2i(x,itype);iy=m2i(y,itype);zero=m2i(0,itype);
  g= euclide(ix,iy);
  if max(abs(m2i(gcds,itype)- g)) <> zero then pause;end
  [g,U,idet]= euclide(ix,iy);
  if max(abs(m2i(gcds,itype)- g)) <> zero then pause;end
  // checks that U matrix is ok 
  for i=1:n 
    if max(abs([ix(i),iy(i)]*U{1,i} - [g(i),zero])) <> zero  then pause;end
  end
  // checks that we recover the factors from U
  for i=1:n 
    u = U{1,i};
    f = [ u(2,2), - u(1,2)]*idet(i);
    if max(abs(ix(i) - f(1)*g(i))) <> zero  then pause;end
    if max(abs(iy(i) - f(2)*g(i))) <> zero  then pause;end
  end
end

// test of lcm with two arguments (similar to euclide but 
// returns lcm instead of gcd
// case of double matrices 
[l]= lcm(x,y);
// g is the gcd. we check that gcd is ok 
if max(abs(lcms-l)) <> 0 then pause;end
// case of int matrices 
itypes=["int32", "uint32","int64", "uint64", "int", "long", "ulong"];
for itype=itypes
  ix=m2i(x,itype);iy=m2i(y,itype);zero=m2i(0,itype);
  [l]= lcm(ix,iy);
  if max(abs(m2i(lcms,itype)- l)) <> zero then pause;end
end

// test lcm and gcd for sequences 
xv=[3,5,7];
n=6;
x=[];
for k=1:10 
  for i=1:n
    cpx=grand(1,size(xv,'*'),'uin',0,4);
    x(i)=prod(xv.^cpx);
  end
  [g,U]=gcd(x)
  g1=x(1); 
  for i=2:n do g1=euclide(g1,x(i)); end;
  if abs(g1-g)<>0 then pause;end 
  if abs(x*U-[zeros(1,n-1),g])<>0 then pause;end;
  
  itypes=["int32", "uint32","int64", "uint64", "int", "long", "ulong"];
  for itype=itypes
    ix=m2i(x,itype);
    [g,U]=gcd(ix)
    if abs(m2i(g1,itype)-g)<> m2i(0,itype) then pause;end 
    if abs(ix*U - [m2i(zeros(1,n-1),itype),g])<>m2i(0,itype) then pause;end;
  end
end
  
// lcm 

xv=[3,5,7];
n=6;
x=[];
for k=1:10 
  for i=1:n
    cpx=grand(1,size(xv,'*'),'uin',0,4);
    x(i)=prod(xv.^cpx);
  end
  [l,F]=lcm(x)
  l1=x(1); 
  for i=2:n do l1=lcm(l1,x(i)); end;
  if abs(l1-l)<>0 then pause;end 
  if abs(x.*F -l )<>0 then pause;end;
  itypes=["int32", "uint32","int64", "uint64", "int", "long", "ulong"];
  for itype=itypes
    ix=m2i(x,itype);
    [l,F]=lcm(ix);
    if abs(m2i(l1,itype)-l)<>m2i(0,itype) then pause;end 
    if abs(ix.*F -l )<>m2i(0,itype) then pause;end;
  end
end











