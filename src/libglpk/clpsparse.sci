// clp matrix description 
// similar to spget_mtlb 
// but cmatbeg and cmatind are int32 matrices here

function [cmatbeg,cmatind,cmatval]=clpsparse_soft(A)
  function y=rem(a,b); y = modulo(a,b);endfunction;
// A=[1,0,0;3,4,9;0,1,2];
  cmatcnt = sum(A ~= 0,1);
  cmatbeg = full(cumsum([0 cmatcnt]));
  cmatbeg = cmatbeg(:)';
  nzA = find(A);
  cmatind = full(rem(nzA-1,size(A,1))');
  cmatind = cmatind(:)';
  cmatval = full(A(nzA));
  cmatval = cmatval(:)';
  //
  cmatind= m2i(cmatind);
  cmatbeg= m2i(cmatbeg);
endfunction

A=[1,0,0;3,4,9;0,1,2];
A1=[0,0,0;5,0,0;5,1,2];

// test 1
[beg,ind,val]=clpsparse_soft(A);
[beg1,ind1,val1]=clp_sparse(A);

if beg<>beg1 then pause;end 
if ind<>ind1 then pause;end 
if val<>val1 then pause;end 

[beg1,ind1,val1]=clp_sparse(sparse(A));

if beg<>beg1 then pause;end 
if ind<>ind1 then pause;end 
if val<>val1 then pause;end 

// test2
[beg,ind,val]=clpsparse_soft([A1;A]);
[beg1,ind1,val1]=clp_sparse2(A,A1);

if beg<>beg1 then pause;end 
if ind<>ind1 then pause;end 
if val<>val1 then pause;end 

[beg1,ind1,val1]=clp_sparse2(sparse(A),sparse(A1));

if beg<>beg1 then pause;end 
if ind<>ind1 then pause;end 
if val<>val1 then pause;end 

A=[1,0,0,5;3,0,4,9;0,1,5,2];

// test3 
[beg,ind,val]=clpsparse_soft(A);
[beg1,ind1,val1]=clp_sparse(A);

if beg<>beg1 then pause;end 
if ind<>ind1 then pause;end 
if val<>val1 then pause;end 


// test 4 

[beg1,ind1,val1]=clp_sparse(sparse(A));

if beg<>beg1 then pause;end 
if ind<>ind1 then pause;end 
if val<>val1 then pause;end 

// test 5
[beg,ind,val]=clpsparse_soft([A;A]);
[beg1,ind1,val1]=clp_sparse2(A,A);

if beg<>beg1 then pause;end 
if ind<>ind1 then pause;end 
if val<>val1 then pause;end 

[beg1,ind1,val1]=clp_sparse2(sparse(A),sparse(A));

if beg<>beg1 then pause;end 
if ind<>ind1 then pause;end 
if val<>val1 then pause;end 

// from beg,ind,val to sparse 

function A=sparse_from_triplet(m,n,beg,ind,val)
  ind=ind(:);val=val(:);
  IJ=[];
  V=[];
  for j=1:n
    cstart=beg(j);
    cend=beg(j+1);
    ij=ind(cstart+1:cend)+1;
    ij=[ij,j*ones(size(ij,'*'),1)];
    IJ=[IJ;ij];
    V=[V;val(cstart+1:cend)];
  end
  A=sparse(IJ,V,[m,n]);
endfunction

A=[1,0,0;3,4,9;0,1,2];

// test 1
[beg,ind,val]=clp_sparse(A);
[m,n]=size(A);
A1= sparse_from_triplet(m,n,i2m(beg),i2m(ind),val);

if norm(A1-A)<>0 then pause;end


