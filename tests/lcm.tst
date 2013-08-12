// -*- Mode: scilab -*- 

// Build data for tests 

xv=[3,5,7];
n=100 ;
for i=1:n
  cpx=grand(1,size(xv,'*'),'uin',0,4);
  x(i)=prod(xv.^cpx);
  cpy=grand(1,size(xv,'*'),'uin',0,4);
  y(i)=prod(xv.^cpy);
  gcds(i)=prod(xv.^min(cpx,cpy));
  lcms(i)=prod(xv.^(cpx+cpy- min(cpx,cpy)));
end

// test the extended euclide algorithm for int matrices. 
// x and y can be two matrices of the same size

// case of double matrices 

[g,U,idet]= ext_euclide(x,y);
// g is the gcd. we check that gcd is ok 
if max(abs(gcds-g)) <> 0 then pause;end
// checks that U matrix is ok 
for i=1:n 
  if max(abs([x(i),y(i)]*U{1,i} - [g(i),0])) <> 0  then pause;end
end
// checks that we recover the factors from U
for i=1:n 
  u = U{1,i};
  f = [ u(2,2), - u(1,2)]*idet(i);
  if max(abs(x(i) - f(1)*g(i))) <> 0  then pause;end
  if max(abs(y(i) - f(2)*g(i))) <> 0  then pause;end
end

// case of int matrices 

itypes=["int32", "uint32","int64", "uint64", "int", "long", "ulong"];
for itype=itypes
  ix=m2i(x,itype);iy=m2i(y,itype);zero=m2i(0,itype);
  [g,U,idet]= ext_euclide(ix,iy);
  if max(abs(m2i(gcds,itype)- g)) <> zero then pause;end
  // checks that U matrix is ok 
  for i=1:n 
    if max(abs([ix(i),iy(i)]*U{1,i} - [g(i),zero])) <> zero  then pause;end
  end
  // checks that we recover the factors from U
  for i=1:n 
    u = U{1,i};
    f = [ u(2,2), - u(1,2)]*idet(i);
    if max(abs(ix(i) - f(1)*g(i))) <> zero  then pause;end
    if max(abs(iy(i) - f(2)*g(i))) <> zero  then pause;end
  end
end


// test euclide, gcd and lcm 

if gcds <> gcd(x,y) then pause;end 
if lcms <> lcm(x,y) then pause;end 

// test the integer cases 
// removing the types with size <= 8 

itypes=[ "int32" "uint32","int64" , "uint64",...
	 'int', 'uint',  'long', 'ulong'];
for itype=itypes
  if m2i(gcds,itype) <> gcd(m2i(x,itype),m2i(y,itype)) then pause;end 
  if m2i(lcms,itype) <> lcm(m2i(x,itype),m2i(y,itype)) then pause;end 
end





