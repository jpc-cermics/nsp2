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


// interval version
// ----------------
N = 1e4;
x = randn(1,N);
val = -4:4;
[ind, occ, out] = bsearch(x, val, match="i", interval = "[--)");
indv = zeros(size(ind));
occv = zeros(size(occ));
nint = 1;
for k = -4:2
   I = find( k <= x & x < k+1 );
   indv(I) = nint;
   occv(nint) = numel(I);
   nint = nint + 1;
end
I = find( 3 <= x & x <= 4 );
indv(I) = nint;
occv(nint) = numel(I);
outv = N - sum(occv);
if ~ (ind.equal[indv] && occ.equal[occv] && out.equal[outv]) then, pause, end

// test for ind returns as int
[indi, occ, out] = bsearch(x, val, match="i", interval = "[--)",ind_type="int");
if  ~indi.equal[m2i(ind)] then, pause, end

// tests for x being an empty matrix (ind should be an empty mat with
// same dim than x)
x = zeros(0,5);
[ind, occ, out] = bsearch(x, val);
if  ~ (ind.equal[x] && occ.equal[zeros(1,numel(val)-1)] && out.equal[0]) then, pause, end

x = zeros(0,5);
[ind, occ, out] = bsearch(x, val, match="v");
if  ~ (ind.equal[x] && occ.equal[zeros(1,numel(val))] && out.equal[0]) then, pause, end

x = zeros(7,0);
ind = bsearch(x, val);
if  ~ind.equal[x] then, pause, end

x = zeros(0,0);
ind = bsearch(x, val);
if  ~ind.equal[x] then, pause, end


// tests with interval option
x = [1,2,3,4];
val = [1,3,4];
[ind, occ] = bsearch(x, val, interval="[--)");
if ~(ind.equal[[1,1,2,2]] && occ.equal[[2,2]]) then, pause, end
[ind, occ] = bsearch(x, val, interval="(--]");
if ~(ind.equal[[1,1,1,2]] && occ.equal[[3,1]]) then, pause, end
