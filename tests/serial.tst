// test serialization of objects 

A=testmatrix('magic',6);
A1=unserialize(serialize(A));
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

A=hcreate(A1=testmatrix('magic',6),A2="poo",A3=%t);
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

A=hcreate(A1=testmatrix('magic',6),A2="poo",A3=%t);
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




