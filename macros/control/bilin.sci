function Sl2=bilin(Sl1,v)
  // Copyright INRIA
  [A,B,C,D]=abcd(Sl1);
  dom=Sl1.dom;
  [ra,ca]=size(A);
  a=v(1);d=v(2);c=v(3);b=v(4);
  i=inv(a*eye(ra,ra)-c*A);
  AB=(b*A-d*eye(ra,ra))*i;
  BB=(a*b-c*d)*i*B;
  CB=C*i;
  DB=D+c*C*i*B;
  Sl2=linear_system(AB,BB,CB,DB,Sl1.X0,dom = Sl1.dom,sample = Sl1.dt);
endfunction
