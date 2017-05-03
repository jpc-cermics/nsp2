// -*- Mode:scilab -*- 
// Copyright (C) 2005-2017 J.P Chancelier Cermics/Enpc
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

// Object used to store functions defined on grids or defined by cuts 

// TOBEDONE: we could use int64 indices and we should check that the 
// range of indices covers the range of discrete points. 
// when we have too many points indices could be turned to 
// array of int64 

// grid function without values i.e only used to convert points to 
// indices 

V=gridfn([11],[0],[100],values=%f);
I=1:V.get_nx[];
// convert indices to points 
Pt=V.i2p[I];

// grid functions dimension 1
//---------------------------

// the grid contains 11 points in the range [0,100] (regular grid)
V=gridfn([11],[0],[100]);
// fill the function looping on possible indices 1:10
I=1:V.get_nx[];
// convert indices to points 
Pt=V.i2p[I];
// set the values 
V.pt_set_value[Pt,sin(Pt)];
// check 
if max(abs(V.pt_get_value[Pt] - sin(Pt))) > 10*%eps then pause;end 
if max(abs(V.i_get_value[I] - sin(Pt))) > 10*%eps then pause;end 

// set the values through indices 
V.i_set_value[I,sin(Pt)];
// check 
if max(abs(V.pt_get_value[Pt] - sin(Pt))) > 10*%eps then pause;end 
if max(abs(V.i_get_value[I] - sin(Pt))) > 10*%eps then pause;end 

// grid functions dimension 2 
//---------------------------

// the grid contains 10x20 points in the range [1,10]x[1,20] (regular grid)
n1=10;n2=20;
V=gridfn([n1,n2],[1,1],[10,20]);
// fill the function looping on possible indices 1:10
I=1:prod(V.get_nx[]);
// convert indices to points 
Pt=V.i2p[I];

if ~size(Pt).equal[[2,n1*n2]] then pause;end 

// set the values 
values = sin(Pt(1,:)).*cos(Pt(2,:));
V.pt_set_value[Pt,values];
// check 

if max(abs(V.pt_get_value[Pt] - values)) > 10*%eps then pause;end 
if max(abs(V.i_get_value[I] - values)) > 10*%eps then pause;end 

// set the values through indices 
V.i_set_value[I,values];
// check 
if max(abs(V.pt_get_value[Pt] - values)) > 10*%eps then pause;end 
if max(abs(V.i_get_value[I] - values)) > 10*%eps then pause;end 

// set values within a loop 
function z=f(x,y) z=sin(x)*cos(y);endfunction;
for i=I 
  pt =V.i2p[i];
  V.i_set_value[i,f(pt(1),pt(2))];
end
// check 
if max(abs(V.pt_get_value[Pt] - values)) > 10*%eps then pause;end 
if max(abs(V.i_get_value[I] - values)) > 10*%eps then pause;end 

// cuts functions dimension 1
//---------------------------
// we give the heights and the slopes to define the function by its cuts 
// approximate exp(x) 

x=linspace(-1,1,11);

// y -> exp(x) + <exp(x),y-x> 

heights = exp(x) - x.* exp(x);
slopes = exp(x);

V=cutsfn(heights,slopes);
values=V.get_value[x];

if max(abs(values - exp(x))) > 10*%eps then pause;end
if max(abs(heights - V.get_heights[])) > 10*%eps then pause;end
if max(abs(slopes - V.get_slopes[])) > 10*%eps then pause;end

V=cutsfn(heights(1),slopes(1));
// then add slopes 
V.add_slopes[heights,slopes];
values=V.get_value[x];

// check 
if max(abs(values - exp(x))) > 10*%eps then pause;end

// cuts functions dimension 2
//---------------------------
// we give the heights and the slopes to define the function by its cuts 
// approximate exp(x) 
n=30;
x=linspace(0,1,n);
y=linspace(0,1,n);
heights=[];
slopes=[];
// (x',y') -> exp(2*x)*exp(3*y) + <exp(3*y)*2*exp(2*x),x'-x> + <exp(2*x)*3*exp(3*y),y'-y> 
vref = [];
for i=1:n 
  for j=1:n
    val=exp(2*x(i))*exp(3*y(j));
    vref(i,j)=val;
    heights.concatr[ val -2*val*x(i) - 3*val*y(j)];
    slopes.concatr[[2*val;3*val]];
  end
end

V=cutsfn(heights,slopes);

// evaluate V on the grid 

values=[];
for i=1:n 
  for j=1:n
    values(i,j)=V.get_value[[x(i);y(j)]];
  end
end

if max(abs(values-vref))> 1.e+6*%eps then pause;end 

// dynamic programming using bellman functions stored as grid functions
//---------------------------------------------------------------------

timer();
T=24
B=cell(1,T);

// fill the bellman function at T 
N=11;xmin=0;xmax=10;
VT=gridfn([N],[xmin],[xmax]);

I=1:VT.get_nx[];
// convert indices to points 
Pt=VT.i2p[I];
// set the values 
val = 5;
VT.pt_set_value[Pt,max(0,Pt-val)];

function y=c(x) y=x;endfunction;

// loop 
B{T}=VT;
for t=T-1:-1:1 
  Vt= gridfn([N],[xmin],[xmax]);
  I=1:Vt.get_nx[];
  Pts=Vt.i2p[I]; 
  for i=I 
    controls=linspace(-2,3,5);
    xtp1= min(max(xmin,Pts(i) + controls),xmax);
    Vtp1 = B{t+1};
    Vt.i_set_value[i, min(c(Pts(i)) + Vtp1.pt_get_value[xtp1])];
  end
  B{t}=Vt;
end
Time=timer();
// plots 

if %f then 
  xset('window',1);
  for t=1:T 
    plot2d(Pt,B{t}.pt_get_value[Pt]);
  end
end

// doing the same with linear programming 
//---------------------------------------------------------------------

T=24
B1=cell(1,T);

// fill the bellman function at T 
N=11;xmin=0;xmax=10;

// max(0,Pt-val) + < 1, y- Pt>I_{Pt>=val} 
heights = [0,-5];
slopes  = [0,1];

VT=cutsfn(heights,slopes);

pts = linspace(xmin,xmax,N);

// loop 
B1{T}=VT;
for t=T-1:-1:1
  nslopes=[];
  nheights=[];
  Vtp1 = B1{t+1};
  slopes = Vtp1.get_slopes[];
  heights = Vtp1.get_heights[];
  // [slopes',slopes', -1]*(x,u,alpha) <= - heights' 
  ns = size(slopes,2);
  A0 = [slopes', slopes', -ones(ns,1)];
  b0 = - heights';
  A1 = [1,1,0]; b1 =[xmax];
  A2 = [-1,-1,0]; b2 =[-xmin];
  A=[A0;A1;A2];b=[b0;b1;b2];
  c = [1,0,1];
  Ae = [1,0,0]; // x = pt
  fpts =[];
  for pt=pts 
    // min pt + alpha 
    //  u
    // alpha >= heights + slopes*(pt+u)
    // and we want an estimate of the new slope 
    // on rajoute pt dans l'etat 
    be = [pt]
    // 
    [xopt,fopt,flag,extras]=linprog(c,A,b,Ae,be,sense="min",lb=[xmin,-2,-%inf],ub=[xmax,3,%inf]);
    // on a une coupe 
    // fopt + extra.lambda(1)*(y-pt);
    nheights.concatr[ fopt - (extras.lambda($))*pt ];
    nslopes.concatr[ +(extras.lambda($))];
    fpts.concatr[fopt];
  end
  B1{t}=cutsfn(nheights,nslopes);
end

if %f then 
  xset('window',2);
  for t=1:T
    plot2d(Pt,B1{t}.get_value[Pt]);
  end
end

