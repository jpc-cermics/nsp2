n=20;
x=rand(n,1,'u');
y=rand(n,1,'u');
noms= 'Piece '+string(1:n);

Ip=find(y>=0.6);
Im=find(y>=0.4 & y <0.6 );
Ir=find(y< 0.4);
xp=x(Ip);yp=y(Ip);Np=noms(Ip);
xm=x(Im);ym=y(Im);Nm=noms(Im);
xr=x(Ir);yr=y(Ir);Nr=noms(Ir);

xset('font size',2)
xset('mark size',3);

rect=[0.9*min(x),0.9*min(y),1.1*max(x),1.1*max(y)];
xset('color',3);
r=xstringl(0,0,'xx');
plot2d(xp,yp,style=-2,rect=rect,strf='070');
for i=1:size(Np,'*');xstring(xp(i)+r(2),yp(i),Np(i));end;
xset('color',4);
plot2d(xm,ym,style=-3,rect=rect,strf='000');
for i=1:size(Nm,'*');xstring(xm(i)+r(2),ym(i),Nm(i));end;
xset('color',5);
plot2d(xr,yr,style=-4,rect=rect,strf='000');
for i=1:size(Nr,'*');xstring(xr(i)+r(2),yr(i),Nr(i));end;
xset('color',0);
xset('thickness',0);
plot2d([],[],strf='001');

////xs2ps(0,'icm.ps');
//unix(SCI+'/bin/Blatexpr -p 1 1 icm.ps')

