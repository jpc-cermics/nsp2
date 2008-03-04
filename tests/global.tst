// -*- Mode: scilab -*-
// test global frame 

clearglobal z;

function y=f();global z;z=rand(rand(1,1)*5+1,4);y=z;endfunction;
y=f();
if exists('z','local') then pause;end 
if ~exists('z','global') then pause;end 

global z;
if ~exists('z','local') then pause;end 
if ~z.equal[y] then pause;end 

clearglobal z;
if exists('z','global') then pause;end 


// global objects and extraction 
// insertion 

global z;
z = 1:10;

function f();
  global z;
  A=rand(10,10);
  A(1,z)= 7;
  A(z,1)=8;
  A(z)=78;
  A(z,z)=67;
  A(z,1)=z';
  A(1,z)=z;
  A(1,z)=[];
  A=rand(10,10);
  A(1,z)
  A(z,1)
  A(z)
  A(z,z)
endfunction ;

f();

function f();
  global z;
  global A;
  A(1,z)= 7;
  A(z,1)=8;
  A(z)=78;
  A(z,z)=67;
  A(z,1)=z';
  A(1,z)=z;
  A(1,z)=[];
  A=rand(10,10);
  A(1,z)
  A(z,1)
  A(z)
  A(z,z)
endfunction ;

global A;
A=rand(10,10);
f()

function f(z);
  A(1,z)= 7;
  A(z,1)=8;
  A(z)=78;
  A(z,z)=67;
  A(z,1)=z';
  A(1,z)=z;
  A(1,z)=[];
  A=rand(10,10);
  A(1,z)
  A(z,1)
  A(z)
  A(z,z)
endfunction ;

z1=1:10;
f(z1)

function f(A,z);
  A(1,z)= 7;
  A(z,1)=8;
  A(z)=78;
  A(z,z)=67;
  A(z,1)=z';
  A(1,z)=z;
  A(1,z)=[];
  A=rand(10,10);
  A(1,z)
  A(z,1)
  A(z)
  A(z,z)
endfunction ;

A1=rand(10,10);
z1=1:10;
f(A1,z1)
