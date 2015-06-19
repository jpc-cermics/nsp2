xclear();
t=%pi*(-1:0.2:1);
m=sin(t)'*cos(t);
plot3d1(t,t,m)
// socle
tm=[-%pi,%pi];
mm=-1*ones_new(2,2);
plot3d(tm,tm,mm,alpha=80,theta=58);
// un bord 

z=m(:,1)';
x=t;
zm=-1*ones(size(t));
xp=[t(1:$-1);t(2:$);t(2:$);t(1:$-1)];
zp=[zm(1:$-1);zm(2:$);z(2:$);z(1:$-1)];
yp=%pi*ones_new(4,1)*[ones(size(t(1:$-1)))];
plot3d(xp,yp,zp,alpha=80,theta=58);

z=m(:,$)';
x=t;
zm=-1*ones(size(t));
xp=[t(1:$-1);t(2:$);t(2:$);t(1:$-1)];
zp=[zm(1:$-1);zm(2:$);z(2:$);z(1:$-1)];
yp=-%pi*ones_new(4,1)*[ones(size(t(1:$-1)))];
plot3d(xp,yp,zp,alpha=80,theta=58);

z=m(1,:);
x=t;
zm=-1*ones(size(t));
xp=[t(1:$-1);t(2:$);t(2:$);t(1:$-1)];
zp=[zm(1:$-1);zm(2:$);z(2:$);z(1:$-1)];
yp=%pi*ones_new(4,1)*[ones(size(t(1:$-1)))];
plot3d(yp,xp,zp,alpha=80,theta=58);

z=m($,:);
x=t;
zm=-1*ones(size(t));
xp=[t(1:$-1);t(2:$);t(2:$);t(1:$-1)];
zp=[zm(1:$-1);zm(2:$);z(2:$);z(1:$-1)];
yp=-%pi*ones_new(4,1)*[ones(size(t(1:$-1)))];
plot3d(yp,xp,zp,alpha=80,theta=58);





