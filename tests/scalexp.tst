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
// test for scalexp used in scicos EXPRESSION blocks.


S=scalexp_create('(u1 <= a)*sin(u1)+(u1 > a && u1 <= b)*cos(u1);');
S.get_method_names[];

if or(S.get_vars[]<>['a';'b';'u1']) then pause;end 
a=1;b=3;
S.apply_context[hash(a=a,b=b)];
if or(S.get_vars[]<>['u1']) then pause;end 
u1=linspace(0,5,10000);
yref=(u1 <= a).*sin(u1)+(u1 > a && u1 <= b).*cos(u1);

y=S.eval[u1];
if norm(yref-y) > 10*%eps then pause;end 

S.bcomp[];
y=S.byte_eval[u1];
if norm(yref-y) > 10*%eps then pause;end 

S.set_extra_names['z']
if or(S.get_vars[]<>['u1';'z']) then pause;end 


S=scalexp_create('u1 + cos(%pi*u2)');
S.apply_context[hash(%pi=%pi)];
if or(S.get_vars[]<>['u1';'u2']) then pause;end 
S.bcomp[];
u2=5*u1;
y=S.byte_eval[[u1;u2]];
yref = u1 + cos(%pi*u2);
if norm(yref-y) > 10*%eps then pause;end 


