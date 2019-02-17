// -*- Mode: nsp -*- 
// Copyright (C) 2008-2015 B.Pincon Iecn/Esial
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

// tests for sparse(ij,val,[m,n])

// 1-real case
A = sprand(12,12,0.05);
[ij,val] = spget(A);
B = sparse(ij,val,[12,12]);
if ~A.equal[B] then, pause; end

A = sprand(1200,1207,0.008);
[ij,val] = spget(A);
B = sparse(ij,val,[1200,1207]);
if ~A.equal[B] then, pause; end

// same example but permute indices
m = size(ij,1);
p = grand(1,"perm",m);
ij = ij(p,:); val = val(p);
B = sparse(ij,val,[1200,1207]);
if ~A.equal[B] then, pause; end

// same example but add doublon and zeros
ijplus = grand(56,2,"uin",1,1200);
vplus = zeros(56,1);
B = sparse([ij;ijplus;ij],[0.5*val;vplus;0.5*val],[1200,1207]);
if ~A.equal[B] then, pause; end

// 2-sames tests in complex case
A = sprand(12,12,0.05) + %i*sprand(12,12,0.05);
[ij,val] = spget(A);
B = sparse(ij,val,[12,12]);
if ~A.equal[B] then, pause; end

A = sprand(1200,1207,0.008)+%i*sprand(1200,1207,0.008);
[ij,val] = spget(A);
B = sparse(ij,val,[1200,1207]);
if ~A.equal[B] then, pause; end

// same example but permute indices
m = size(ij,1);
p = grand(1,"perm",m);
ij = ij(p,:); val = val(p);
B = sparse(ij,val,[1200,1207]);
if ~A.equal[B] then, pause; end

// same example but add doublon and zeros
ijplus = grand(56,2,"uin",1,1200);
vplus = zeros(56,1);
B = sparse([ij;ijplus;ij],[0.5*val;vplus;0.5*val],[1200,1207]);
if ~A.equal[B] then, pause; end

// 3- doublon and cancellation of coefs
ij = [1, 4, 4, 4, 4, 9, 9, 1, 2, 2, 2, 9, 9, 9;...
      2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1]';
val= [1, 1,-2, 2,-1, 1,-5, 1, 3,-6,-4, 5,-4,-1];
Res = zeros(9,2);
Res(1,1) = 1; Res(1,2)= 1; Res(2,1)=-7; Res(9,2) = -4; 
A = sparse(ij,val);
if ~A.equal[sparse(Res)] then, pause; end

// 4- same in imaginary
val = %i*val;
Res = %i*Res;
A = sparse(ij,val);
if ~A.equal[sparse(Res)] then, pause; end

// 5- null coefs
ij = [1, 4, 4, 4, 4, 9, 9, 1, 2, 2, 2, 9, 9, 9;...
      2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1]';
val = zeros(14,1);
Res = zeros(9,2);
A = sparse(ij,val);
if ~A.equal[sparse(Res)] then, pause; end

A = sparse(ij,val,[45,78]);
if ~A.equal[sparse([],[],[45,78])] then, pause; end

