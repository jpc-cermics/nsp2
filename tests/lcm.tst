// -*- Mode: scilab -*- 


xv=[-3,5,7];
n=1000 ;
for i=1:n
  cpx=grand(1,size(x,'*'),'uin',0,4);
  x=prod(xv.^cpx);
  cpy=grand(1,size(x,'*'),'uin',0,4);
  y=prod(xv.^cpy);
  gcds=prod(xv.^min(cpx,cpy));
  lcms=prod(xv.^(cpx+cpy- min(cpx,cpy)))
  if gcds <> gcd(x,y) then pause;end 
  if lcms <> lcm(x,y) then pause;end 
end


