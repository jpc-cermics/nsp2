// -*- Mode: nsp -*-
// Copyright (C) 2013-2015 J.P Chancelier Cermics/Enpc
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

function y=f()
  persistent(x=0);
  x=x+1;
  y=x;
endfunction;

y=[];
for i=1:100,  y.concatr[f()]; end;

if ~y.equal[1:100] then pause;end 

// persistent in function inside a function 

function y=g() 
  function y=f()
    persistent(x=0);
    x=x+1;
    y=x;
  endfunction;
  y=[];
  for i=1:100,  y.concatr[f()]; end;
endfunction ;

if ~g().equal[1:100] then pause;end 

// persistent in recursive function 

function y=f(rec=0)
  persistent(x=0);
  // printf("Inside f recursion %d x=%d\n",rec,x);
  if rec == 100 then y=%f;return;end 
  if x == 10 then y=%t;return;end
  x=x+1;
  y=f(rec=rec+1);
endfunction;

if ~f() then pause;end


