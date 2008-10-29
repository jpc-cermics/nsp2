// -*- Mode: scilab -*-
// Copyright (C) 2008 J.P Chancelier Cermics/Enpc
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

H=hash_create(A=1,B=2,C=3);
function y=f(x,varargopt);y=varargopt;endfunction;
function y=g(x,varargopt);y=f(x,varargopt(:));endfunction;
y=g(5,H(:));
if y<>H then pause;end

function [y,z]=f(x,varargin,varargopt);y=varargopt;z=varargin;endfunction;
[y,z]=f(6,1,2,3,H(:));
if y<>H then pause;end 
if z<>list(1,2,3) then pause;end 

function [z,t]=f(x,y,varargin);  z=x; t = varargin; endfunction;
// y ignored 
[z,t]=f(4);
if z<>4 | length(t)<>0 then pause;end 
// y unused 
[z,t]=f(4,5);
if z<>4 | length(t)<>0 then pause;end 
[z,t]=f(4,5,7,8);
if z<>4 | length(t)<>2 | ~t.equal[list(7,8)] then pause;end 

function [z,t,w]=f(x,y,varargin,varargopt);  z=x; t = varargin;w=varargopt;endfunction;
[z,t,w]=f(4);
if z<>4 | length(t)<>0 | length(w)<> 0 then pause;end 
// y unused 
[z,t,w]=f(4,5);
if z<>4 | length(t)<>0 | length(w)<> 0 then pause;end 
[z,t,w]=f(4,5,7,8);
if z<>4 | length(t)<>2 | ~t.equal[list(7,8)] |  length(w)<> 0 then pause;end 

[z,t,w]=f(4,5,7,8,H(:));
if z<>4 | length(t)<>2 | ~t.equal[list(7,8)] | ~w.equal[H] then pause;end 

function [z,w]=f(x,y,a=1,b=2,c=3);z=x; w=hash_create(3,A=a,B=b,C=c);endfunction;

[z,w]=f(4);
if z<>4 | ~w.equal[hash_create(A=1,B=2,C=3)] then pause;end 
[z,w]=f(4,5);
if z<>4 | ~w.equal[hash_create(A=1,B=2,C=3)] then pause;end 
[z,w]=f(4,5,a=100);
if z<>4 | ~w.equal[hash_create(A=100,B=2,C=3)] then pause;end 
[z,w]=f(4,5,c=100);
if z<>4 | ~w.equal[hash_create(A=1,B=2,C=100)] then pause;end 
[z,w]=f(4,5,foo=100);
if z<>4 | ~w.equal[hash_create(A=1,B=2,C=3)] then pause;end 


  

  



