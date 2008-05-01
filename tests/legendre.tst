// -*- Mode: scilab -*- 
// 
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
 
// basic test for legendre 

// (n+1)P_{n+1}(x)=(2n+1)xP_{n}(x) - nP_{n-1}(x)
// function y=P0(x) y=1 ; endfunction;
// function y=P1(x) y=x; endfunction;
// function y=P2(x) y=(1/2)*(3*x.*x-1); endfunction;
// function y=P3(x) y=(1/2)*(5*x.^3-3*x); endfunction;
// function y=P4(x) y=(1/8)*(35*x.^4-30*x.^2+3); endfunction;

x=linspace(-1,1,100);

function y=P5_0(x) y=(1/8)*(63*x.^5-70*x.^3+15*x); endfunction;
function y=P5_1(x) y=(1/8)*(63*5*x.^4-70*3*x.^2+15); endfunction;
function y=P5_2(x) y=(1/8)*(63*5*4*x.^3-70*3*2*x); endfunction;
function y=P5_3(x) y=(1/8)*(63*5*4*3*x.^2-70*3*2); endfunction;
function y=P5_4(x) y=(1/8)*(63*5*4*3*2*x); endfunction;
function y=P5_5(x) y=(1/8)*(63*5*4*3*2); endfunction;
function y=P5_6(x) y=0; endfunction;

//                               m
//                m        m/2  d  
//  Pn_m(x) = (-1)  (1-x^2)    --- Pn(x)
//                               m  
//                             dx 

for m=0:6 
    execstr('y2=P5_'+string(m)+'(x);');
    y2 = (-1)^m*(1-x.^2).^(m/2).*y2;
    y1 = legendre(5,m,x);
    if norm(y1-y2) > 1.e-10 then pause;end 
end 

Y2 = ones_new(7,length(x));
for m=0:6 
    execstr('y2=P5_'+string(m)+'(x);');
    Y2(m+1,:) = (-1)^m*(1-x.^2).^(m/2).*y2;
end 

Y1 = legendre(5,0:6,x);
 
if norm(Y1-Y2) > 1.e-10 then pause;end 
