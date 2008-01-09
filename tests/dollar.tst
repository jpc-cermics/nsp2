// -*- Mode: scilab -*-
// Copyright (C) 2005 J.P Chancelier Cermics/Enpc
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

// test the dollar 

global dollar;

function y=dollar_check(x,k)
  global dollar;
  dollar(k)=x
  y=x;
endfunction

A=rand(4,5);

B=A(dollar_check($,1),dollar_check($,2));
if dollar(1)<>4 then pause;end 
if dollar(2)<>5 then pause;end 
B1=A($,$);
if norm(B1-B) > %eps then pause;end 

B=A(dollar_check($,1),2);
if dollar(1)<>4 then pause;end 
B1=A($,2);
if norm(B1-B) > %eps then pause;end 

B=A(2,dollar_check($,1));
if dollar(1)<>5 then pause;end 
B1=A(2,$);
if norm(B1-B) > %eps then pause;end 

C=rand(20,30);
B=A(2,0*C(1,1+0*dollar_check($,1))+dollar_check($,2));
if dollar(1)<>30 then pause;end 
if dollar(2)<>5 then pause;end 
B1=A(2,$);
if norm(B1-B) > %eps then pause;end 

