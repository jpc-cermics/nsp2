// -*- Mode: scilab -*- 
// A small example of MESH Creation 
// and visualisation of a function linear on triangles
// Copyright Enpc.

if ~new_graphics() then switch_graphics();end 
N=20;
n=1:N;
x=cos(n*2*%pi/N);
y=sin(n*2*%pi/N);
noeul=[(1:(N))',x',y',0*ones(N,1)];
noeul=[noeul;(N+1),0,0,0];
trianl=[(1:N)',[(2:N)';1],(N+1)*ones(N,1)];
triangles=[(1:N)',trianl,zeros(N,1)];
rect=[-1.2,-1.2,1.2,1.2];
z=(1:N+1)';
if new_graphics() then z(1:4)=%nan;end
xset('colormap',jetcolormap(32));
fec(noeul(:,2),noeul(:,3),triangles,z,strf='030',axesflag=2,rect=rect,mesh=%t);








