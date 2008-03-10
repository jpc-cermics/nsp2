// -*- Mode: scilab -*- 
// tests for sparse(ij,val,[m,n])

// 1-real case
A = sprand(12,12,0.05);
[ij,val] = spget(A);
B = sparse(ij,val,[12,12]);
if ~A.equal[B] then, pause; end

A = sprand(1200,1207,0.008);
[ij,val] = spget(A);
B = sparse(ij,val,[1200,1207]);
if ~A.equal[B] then, pause; end

// same example but permute indices
m = size(ij,1);
p = grand(1,"perm",m);
ij = ij(p,:); val = val(p);
B = sparse(ij,val,[1200,1207]);
if ~A.equal[B] then, pause; end

// same example but add doublon and zeros
ijplus = ij([1000;600;78],:);
valplus = -val([1000;600;78]);
val([1000;600;78]) = 2*val([1000;600;78]);
B = sparse([ij;ijplus],[val;valplus],[1200,1207]);
if ~A.equal[B] then, pause; end


// 2-same test in complex case
A = sprand(12,12,0.05) + %i*sprand(12,12,0.05);
[ij,val] = spget(A);
B = sparse(ij,val,[12,12]);
if ~A.equal[B] then, pause; end

A = sprand(1200,1207,0.008)+%i*sprand(1200,1207,0.008);
[ij,val] = spget(A);
B = sparse(ij,val,[1200,1207]);
if ~A.equal[B] then, pause; end

// same example but permute indices
m = size(ij,1);
p = grand(1,"perm",m);
ij = ij(p,:); val = val(p);
B = sparse(ij,val,[1200,1207]);
if ~A.equal[B] then, pause; end

// same example but add doublon and zeros
ijplus = ij([1000;600;78],:);
valplus = -val([1000;600;78]);
val([1000;600;78]) = 2*val([1000;600;78]);
B = sparse([ij;ijplus],[val;valplus],[1200,1207]);
if ~A.equal[B] then, pause; end


// 3- doublon and cancellation of coefs
ij = [1, 4, 4, 4, 4, 9, 9, 1, 2, 2, 2, 9, 9, 9;...
      2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1]';
val= [1, 1,-2, 2,-1, 1,-5, 1, 3,-6,-4, 5,-4,-1];
Res = zeros(9,2);
Res(1,1) = 1; Res(1,2)= 1; Res(2,1)=-7; Res(9,2) = -4; 
A = sparse(ij,val);
if ~A.equal[sparse(Res)] then, pause; end

// 4- same in imaginary
val = %i*val;
Res = %i*Res;
A = sparse(ij,val);
if ~A.equal[sparse(Res)] then, pause; end

// 5- null coefs
ij = [1, 4, 4, 4, 4, 9, 9, 1, 2, 2, 2, 9, 9, 9;...
      2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1]';
val = zeros(14,1);
Res = zeros(9,2);
A = sparse(ij,val);
if ~A.equal[sparse(Res)] then, pause; end

A = sparse(ij,val,[45,78]);
if ~A.equal[sparse([],[],[45,78])] then, pause; end
