name = file('join',[getenv('NSP_TMPDIR'),'test.bin']);

// matrix 
//----------

// test the save/load of mat bmat smat 

for i=1:10
  A=rand(3,7,'n');
  sci_save('test.bin',A1=A,A2=string(A),A3=A>=0,A4=A+%i*A);
  sci_load('test.bin');
  if norm(A-A1)<>0 then pause;end 
  if norm(A+%i*A-A4)<>0 then pause;end 
  if norm(evstr(string(A))-evstr(A2))<>0 then pause;end 
  if ~A3.equal[A>=0] then pause;end 
end 

// the same inside a list 

for i=1:10
  A=rand(3,7,'n');
  L=list(A,string(A),A>=0,A+%i*A);
  sci_save('test.bin',L=L);
  sci_load('test.bin');
  if norm(A-L(1))<>0 then pause;end 
  if norm(A+%i*A-L(4))<>0 then pause;end 
  if norm(evstr(string(A))-evstr(L(2)))<>0 then pause;end 
  if ~L(3).equal[A>=0] then pause;end 
end 

// the same inside a hash 

for i=1:10
  A=rand(3,7,'n');
  L=hash_create(A=A,B=string(A),C=A>=0,D=A+%i*A);
  sci_save('test.bin',L=L);
  sci_load('test.bin');
  if norm(A-L.A)<>0 then pause;end 
  if norm(A+%i*A-L.D)<>0 then pause;end 
  if norm(evstr(string(A))-evstr(L.B))<>0 then pause;end 
  if ~L.C.equal[A>=0] then pause;end 
end 


//------------------------------
// sparse matrices 

// test the conversion back and forward to matlab triplet.


for i=1:10
  m=50;n=70;
  A=sprand(50,70,0.2);
  [Jc,Ir,P]=spget_mtlb(A);
  A1=spfrommtlb(Jc,Ir,P,[m,n]);
  if nnz(A-A1)<>0 then pause;end 
end

for i=1:10
  m=50;n=70;
  A=sprand(50,70,0.2);
  A=A+%i*sprand(50,70,0.2);
  [Jc,Ir,P]=spget_mtlb(A);
  A1=spfrommtlb(Jc,Ir,P,[m,n]);
  if nnz(A-A1)<>0 then pause;end 
end


// test the save/load of sparses 

for i=1:10
  A=sprand(100,200,0.1);sci_save('test.bin',A1=A,A2=full(A));
  sci_load('test.bin');
  if nnz(A-A1)<>0 then pause;end 
  if nnz(A-sparse(A2))<>0 then pause;end 
end 

for i=1:10
  A=sprand(100,200,0.1);
  A= A+%i*sprand(100,200,0.1);
  sci_save('test.bin',A1=A,A2=full(A));
  sci_load('test.bin');
  if nnz(A-A1)<>0 then pause;end 
  if nnz(A-sparse(A2))<>0 then pause;end 
end 

// test the save/load of sparses inside lists 

for i=1:10
  A=sprand(100,200,0.1);
  L1=list(A,full(A));
  sci_save('test.bin',A1=L1);
  sci_load('test.bin');
  if nnz(A-A1(1))<>0 then pause;end 
  if nnz(A-sparse(A1(2)))<>0 then pause;end 
end 

for i=1:10
  A=sprand(100,200,0.1);
  A=A +%i*sprand(100,200,0.1);
  L1=list(A,full(A));
  sci_save('test.bin',A1=L1);
  sci_load('test.bin');
  if nnz(A-A1(1))<>0 then pause;end 
  if nnz(A-sparse(A1(2)))<>0 then pause;end 
end 

// inside a hash 


for i=1:10
  A=sprand(100,200,0.1);
  L=hash_create(A=A,B=full(A));
  sci_save('test.bin',L=L);
  sci_load('test.bin');
  if nnz(A-L.A)<>0 then pause;end 
  if nnz(A-sparse(L.B))<>0 then pause;end 
end 

for i=1:10
  A=sprand(100,200,0.1);
  A=A +%i*sprand(100,200,0.1);
  L=hash_create(A=A,B=full(A));
  sci_save('test.bin',L=L);
  sci_load('test.bin');
  if nnz(A-L.A)<>0 then pause;end 
  if nnz(A-sparse(L.B))<>0 then pause;end 
end 

