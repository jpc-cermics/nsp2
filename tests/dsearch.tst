// -*- Mode: scilab -*- 
// Copyright (C) 2005 Bruno Pincon 
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
// bsearch 

// discrete version 
// ----------------

N=50;
A=int(10*rand(1:N));
B=3:7;
[a,b,c]=bsearch(A,B,match='v');
if and(size(a)<>size(A)) then pause,end
if and(size(b)<>size(B)) then pause,end
aa=0*a;bb=0*b;
for k=1:size(B,'*')
  I=find(A==B(k));
  aa(I)=k;
  bb(k)=length(I);
end
if max(aa-a)<>0 then pause,end
if max(bb-b)<>0 then pause,end

