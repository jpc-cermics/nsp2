// colored marks + strings 

n=60;
x=rand(n,1);
y=rand(n,1);
noms= 'E'+string(1:n);

Ip=find(y>=0.6);
Im=find(y>=0.4 & y <0.6 );
Ir=find(y< 0.4);

I={Ip,Im,Ir};

rect=[0.9*min(x),0.9*min(y),1.1*max(x),1.1*max(y)];
xsetech(frect=rect,iso=%t,clip=%t);

for i=1:size(I,'*')
  xc=x(I{i});yc=y(I{i});Nc=noms(I{i});
  plot2d(xc,yc,line_color=-2,mark_color=i+1,mark=i+4,mark_size=3,rect=rect);
  for j=1:size(Nc,'*');xstring(xc(j)+0.01,yc(j),Nc(j),posx="left",posy="center",color=i+1);end;
end



