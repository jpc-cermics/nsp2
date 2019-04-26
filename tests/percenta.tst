// -*- Mode: nsp -*-
// Copyright (C) 2012-2015 J.-Ph. Chancelier Cermics/Enpc
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
 
// basic test for %a to print floating-point numbers in hexadecimal form.
// See https://www.exploringbinary.com/hexadecimal-floating-point-constants/
// 2, 2^8 et 2^-6

M=[2; 256; 0.015625; 2^8 ; 2^-6 ];
S=sprintf("%a",M);
if or(S<>["0x1p+1";"0x1p+8";"0x1p-6";"0x1p+8";"0x1p-6"]) then pause;end 

x="0x1.b7p-1";
xn=sprintf("%a",sscanf(x,"%la"));
if xn<>x then pause;end 

// take care to use %la when reading 
M=[%pi;%e;0.1];
Mn=sscanf(size(M,'*'),sprintf("%a",M),"%la");
if ~Mn.equal[M] then pause;end 

// works also for %inf ans %nan 
// since scanf with %a can scan inf -nan nan etc ...

M=[%nan;%inf];
Mn=sscanf(size(M,'*'),sprintf("%a",M),"%la");
if ~isnan(Mn(1)) || ~isinf(Mn(2)) then pause;end 

// 100 random numbers 

M=randn(100,1);
Mn=sscanf(size(M,'*'),sprintf("%a",M),"%la");
if ~Mn.equal[M] then pause;end 


