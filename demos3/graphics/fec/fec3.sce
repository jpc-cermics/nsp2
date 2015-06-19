// -*- Mode: scilab -*-
// A small example of fec
// and visualisation of a function linear on triangles
// Copyright Enpc.

if ~new_graphics() then switch_graphics();end
if file('exists',getenv('NSP')+'/macros/mottelet/tridem.nsp')
  load('NSP/macros/mottelet/tridem.nsp')
else
  quit;
end

ntri=size(nodes,2);
T=[nodes'];
for i=1:size(P,2)
  xset('colormap',jetcolormap(64));
  fec(xy(1,:)',xy(2,:)',T,P(:,i),strf='030',axesflag=2,mesh=%f);
  xclick();
  xclear();
end

xclear();
xset('colormap',jetcolormap(64));
Pf=fec(xy(1,:)',xy(2,:)',T,P(:,1),strf='030',axesflag=2,mesh=%f);
F=get_current_figure();
n = size(Pf.func,'*');

for i=1:100
  Pf.func =  sin(xy(1,:)*i/100).*cos(xy(2,:)*exp(i/100));
  Pf.invalidate[];
  F.draw_now[]; // will activate a process_updates
end
