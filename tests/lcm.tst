// -*- Mode: scilab -*- 

// testing euclide remains 
// 

xv=[3,5];
n=1000 ;
for i=1:n
  cpx=grand(1,size(xv,'*'),'uin',0,4);
  x=prod(xv.^cpx);
  cpy=grand(1,size(xv,'*'),'uin',0,4);
  y=prod(xv.^cpy);
  gcds=prod(xv.^min(cpx,cpy));
  lcms=prod(xv.^(cpx+cpy- min(cpx,cpy)))
  if gcds <> gcd(x,y) then pause;end 
  if lcms <> lcm(x,y) then pause;end 
end

// test the integer cases 
// removing the types with size <= 8 

xv=[3,5];
n=1000 ;
for i=1:n
  cpx=grand(1,size(xv,'*'),'uin',0,4);
  x=prod(xv.^cpx);
  cpy=grand(1,size(xv,'*'),'uin',0,4);
  y=prod(xv.^cpy);
  gcds=prod(xv.^min(cpx,cpy));
  lcms=prod(xv.^(cpx+cpy- min(cpx,cpy)))
  itypes=[ "int32" "uint32","int64" , "uint64",...
	   'int', 'uint',  'long', 'ulong'];
  for itype=itypes
    if m2i(gcds,itype) <> gcd(m2i(x,itype),m2i(y,itype)) then pause;end 
    if m2i(lcms,itype) <> lcm(m2i(x,itype),m2i(y,itype)) then pause;end 
  end
end



