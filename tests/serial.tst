// -*- Mode: nsp -*- 
// test serialization of objects 

A=testmatrix('magic',6);
A1=unserialize(serialize(A));
if ~A1.equal[A] then pause;end 

A=testmatrix('magic',6);
As=serialize(A);
A1=As.unserialize[]; // use a method 
if ~A1.equal[A] then pause;end 

A=rand(5,6) >=0.5 ;
A1=unserialize(serialize(A));
if ~A1.equal[A] then pause;end 

A=m2s(testmatrix('magic',10));
A1=unserialize(serialize(A));
if ~A1.equal[A] then pause;end 

A=sparse(testmatrix('magic',6));
A1=unserialize(serialize(A));
// equal method for sparse is to be done
A1=full(A1);
if ~A1.equal[full(A)] then pause;end 

A=hash_create(A1=testmatrix('magic',6),A2="poo",A3=%t);
A1=unserialize(serialize(A));
if ~A1.equal[A] then pause;end 

A={testmatrix('magic',6),"poo",%t};
A1=unserialize(serialize(A));
if ~A1.equal[A] then pause;end 

A=list(testmatrix('magic',6),"poo",%t);
A1=unserialize(serialize(A));
if ~A1.equal[A] then pause;end 

function y=f(x);y=sin(x)+cos(x);endfunction;
f1=unserialize(serialize(f));
//to be done if ~ f1.equal[f] then pause;end 

// serialize and unserialize in a matrix 
// This is dangerous but used in scicos 
// to store data in block states.
//--------------------------------------

A=testmatrix('magic',6);
A1=unserialize(serialize(A,'m'));
if ~A1.equal[A] then pause;end 

A=rand(5,6) >=0.5 ;
A1=unserialize(serialize(A,'m'));
if ~A1.equal[A] then pause;end 

A=m2s(testmatrix('magic',10));
A1=unserialize(serialize(A,'m'));
if ~A1.equal[A] then pause;end 

A=sparse(testmatrix('magic',6));
A1=unserialize(serialize(A,'m'));
// equal method for sparse is to be done
A1=full(A1);
if ~A1.equal[full(A)] then pause;end 

A=hash_create(A1=testmatrix('magic',6),A2="poo",A3=%t);
A1=unserialize(serialize(A,'m'));
if ~A1.equal[A] then pause;end 

A={testmatrix('magic',6),"poo",%t};
A1=unserialize(serialize(A,'m'));
if ~A1.equal[A] then pause;end 

A=list(testmatrix('magic',6),"poo",%t);
A1=unserialize(serialize(A,'m'));
if ~A1.equal[A] then pause;end 

function y=f(x);y=sin(x)+cos(x);endfunction;
f1=unserialize(serialize(f,'m'));

// serialize then convert serialized object 
// to a base64 string 
//----------------------------------------

x=rand(1,1);
A=serialize(x);
str=A.tobase64[];
// back to serialized object.
A1=base64toserial(str);
if ~A1.equal[A] then pause;end 
y=A1.unserialize[];
if ~x.equal[y] then pause;end 

// serialize then convert serialized object 
// to a base64 string splitted.
//----------------------------------------

x=rand(1,10);
A=serialize(x);
nc=length(A);
str=A.tobase64[20];
// back to serialized object.
A1=base64toserial(str);
if ~A1.equal[A] then pause;end 
y=A1.unserialize[];
if ~x.equal[y] then pause;end 

// serialized objects can be printed 
// as_read using a base64 string conversion.
//----------------------------------------

S=sprint(A,as_read=%t);
A1=A;
clear A;
ok=execstr(S,errcatch=%t);
if ~ok then pause;end 
if ~A1.equal[A] then pause;end 
y=A1.unserialize[];
if ~x.equal[y] then pause;end 

// serialize and compress 

A=testmatrix('magic',6);
As=serialize(A);
As1=As.compress[];
A1=As1.unserialize[];
if ~A1.equal[A] then pause;end 






