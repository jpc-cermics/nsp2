// -*- Mode: scilab -*- 
// test sp operations 
// sp -> spcol
// 
// faire tourner avec des matrices + ou - creuses 
// et carrees ou rect 
// + le cas complexe 

// convert m2sp and sp2m 

Sp=m2sp(A);
A1=sp2m(Sp);
if or(A1<>A) then pause;end

// create 
// same as zeros 

Sp1=sp_create(7,8);
A1=sp2m(Sp1);
if or(A1<>zeros(7,8)) then pause;end

// nsp_spcolmatrix_sparse
// nsp_spcolmatrix_get

[RC,V,mn]=spget(Sp);
Sp1=sparse(RC,V,mn);
A1=sp2m(Sp);
if or(A1<>A) then pause;end

// nsp_spcolmatrix_copy

Sp1=Sp;
A1=sp2m(Sp);
if or(A1<>A) then pause;end

//  nsp_spcolmatrix_nnz 
//  XXX should be a method 

n=nnz(Sp);
n1=size(find(A<>0),'*');
if n1<>n then pause;end 

// nsp_spcolmatrix_redim
// this should be a method 
//Asizes=[144,1;1,144;6,24;24,6];

for i=1:size(Asizes,'r');
  Sp1=redim(Sp,Asizes(i,1),Asizes(i,2));
  A1=sp2m(Sp1);
  A2=redim(A,Asizes(i,1),Asizes(i,2));
  if or(A1<>A2) then pause;end
end

// nsp_spcolmatrix_enlarge_cols
// nsp_spcolmatrix_enlarge
// XXX pas interfacée : devrait etre des méthodes 

// nsp_spcolmatrix_concatr

Sp1=[Sp,Sp];
A1=sp2m(Sp1);
if or(A1<>[A,A]) then pause;end

// nsp_spcolmatrix_concatd 

Sp1=[Sp;Sp];
A1=sp2m(Sp1);
if or(A1<>[A;A]) then pause;end

// nsp_spcolmatrix_concatdiag 

Sp1=[Sp # Sp];
A1=sp2m(Sp1);
if or(A1<>[A # A]) then pause;end

// nsp_spcolmatrix_insert_elt(A,i,j,B,rb,cb)
// nsp_spcolmatrix_delete_elt(A,row,col,amin,amax)
// nsp_spcolmatrix_get_elt(B,i,j)
// nsp_spcolmatrix_store(A,r,c,col,B,r1,c1)

// insert a sparse 

Sp1=Sp;
Sp1(1:2,3)=m2sp([7;8]);
A1=sp2m(Sp1);
A2=A;
A2(1:2,3)=[7;8];
if or(A1<>A2) then pause;end

// insert a full 

Sp1=Sp;
Sp1(1:2,3)=[7;8];
A1=sp2m(Sp1);
A2=A;
A2(1:2,3)=[7;8];
if or(A1<>A2) then pause;end

//

Sp1=Sp;
Sp1(3,5:7)=m2sp([7,8,9]);
A1=sp2m(Sp1);
A2=A;
A2(3,5:7)=[7,8,9];
if or(A1<>A2) then pause;end

Sp1=Sp;
Sp1(3,5:7)=[7,8,9];
A1=sp2m(Sp1);
A2=A;
A2(3,5:7)=[7,8,9];
if or(A1<>A2) then pause;end

// 

Sp1=Sp;
Sp1(3,5)=m2sp([7]);
A1=sp2m(Sp1);
A2=A;
A2(3,5)=[7];
if or(A1<>A2) then pause;end

Sp1=Sp;
Sp1(3,5)=[7];
A1=sp2m(Sp1);
A2=A;
A2(3,5)=[7];
if or(A1<>A2) then pause;end

//

Sp1=Sp;
Sp1([1,10],[5,11])=m2sp([1,2;3,4]);
A1=sp2m(Sp1);
A2=A;
A2([1,10],[5,11])=[1,2;3,4];
if or(A1<>A2) then pause;end

Sp1=Sp;
Sp1([1,10],[5,11])=[1,2;3,4];
A1=sp2m(Sp1);
A2=A;
A2([1,10],[5,11])=[1,2;3,4];
if or(A1<>A2) then pause;end

// nsp_spcolmatrix_set_row 

Sp1=Sp;
Sp1([1,4,7])=m2sp(0);
A1=sp2m(Sp1);
A2=A;
A2([1,4,7])=0;
if or(A1<>A2) then pause;end

Sp1=Sp;
Sp1([1,4,7])=0;
A1=sp2m(Sp1);
A2=A;
A2([1,4,7])=0;
if or(A1<>A2) then pause;end

Sp1=Sp;
Sp1([1,4,7])=m2sp([8,9,10]);
A1=sp2m(Sp1);
A2=A;
A2([1,4,7])=[8,9,10];
if or(A1<>A2) then pause;end

Sp1=Sp;
Sp1([1,4,7])=[8,9,10];
A1=sp2m(Sp1);
A2=A;
A2([1,4,7])=[8,9,10];
if or(A1<>A2) then pause;end

// nsp_spcolmatrix_delete_cols

Sp1=Sp;
Sp1(:,[1,4,6])=[];
A1=sp2m(Sp1);
A2=A;
A2(:,[1,4,6])=[];
if or(A1<>A2) then pause;end

Sp1=Sp;
Sp1(:,[6,4,1])=[];
A1=sp2m(Sp1);
A2=A;
A2(:,[1,4,6])=[];
if or(A1<>A2) then pause;end

Sp1=Sp;
Sp1(:,[6,4,1,4,6])=[];
A1=sp2m(Sp1);
A2=A;
A2(:,[1,4,6])=[];
if or(A1<>A2) then pause;end

// nsp_spcolmatrix_delete_rows

Sp1=Sp;
Sp1([1,4,6],:)=[];
A1=sp2m(Sp1);
A2=A;
A2([1,4,6],:)=[];
if or(A1<>A2) then pause;end

Sp1=Sp;
Sp1([6,4,1],:)=[]
A1=sp2m(Sp1);
A2=A;
A2([1,4,6],:)=[];
if or(A1<>A2) then pause;end

Sp1=Sp;
Sp1([6,4,1,4,6],:)=[]
A1=sp2m(Sp1);
A2=A;
A2([1,4,6],:)=[];
if or(A1<>A2) then pause;end

//
// A(elts) = []
// A is changed. 
// elts must be increasing 
// XXXXXXXXXXX pas ecrite


// Res=nsp_matrix_extract(A,Rows,Cols)
// nsp_spcolmatrix_extract

Sp1=Sp([1,4],[2,4,5]);
A1=sp2m(Sp1);
A2=A([1,4],[2,4,5]);
if or(A1<>A2) then pause;end

Sp1=Sp([4,1],[2,4,5]);
A1=sp2m(Sp1);
A2=A([4,1],[2,4,5]);
if or(A1<>A2) then pause;end

Sp1=Sp(1,[1,4,5]);
A1=sp2m(Sp1);
A2=A([1],[1,4,5]);
if or(A1<>A2) then pause;end

// Res=nsp_matrix_extract_elements
// nsp_spcolmatrix_extract_elts 

Sp1=Sp([1,4,2,4,5]);
A1=sp2m(Sp1);
A2=A([1,4,2,4,5]);
if or(A1<>A2) then pause;end

Sp1=Sp([1,4,2,4,5]+6);
A1=sp2m(Sp1);
A2=A([1,4,2,4,5]+6);
if or(A1<>A2) then pause;end

// nsp_spcolmatrix_extract_cols

Sp1=Sp(:,[2,4,5]);
A1=sp2m(Sp1);
A2=A(:,[2,4,5]);
if or(A1<>A2) then pause;end


Sp1=Sp(:,[2,4,5,5,3]);
A1=sp2m(Sp1);
A2=A(:,[2,4,5,5,3]);
if or(A1<>A2) then pause;end

// Res=nsp_matrix_extract_rows(A,Rows,err)

Sp1=Sp([2,4,5],:);
A1=sp2m(Sp1);
A2=A([2,4,5],:);
if or(A1<>A2) then pause;end

Sp1=Sp([2,4,5,5,3],:);
A1=sp2m(Sp1);
A2=A([2,4,5,5,3],:);
if or(A1<>A2) then pause;end

// nsp_spcolmatrix_diag_extract 

for k =[-7:7]
  Sp1=diag(Sp,k);
  A1=sp2m(Sp1);
  A2=diag(A,k);
  if or(A1<>A2) then pause;end
end 

// nsp_spcolmatrix_diag_set 
// XXXXXXXX should be replaced by a method 

Sp1=Sp;
nd=size(diag(Sp1,0),'*');
diagset(Sp1,m2sp(1:nd),0);
A1=sp2m(Sp1);
[ma,na]=size(A);
B=[diag(diag(A))-diag(1:nd)];
[mb,nb]=size(B);
B=[B;zeros(ma-mb,na)];
if nb < na then B=[B,zeros(ma,na-nb)];end
A2=A-B;
if or(A1<>A2) then pause;end

dd=[1,7,0,0,9,5,0,0,0,1,2,3];
for i=-3:3 
  Sp1=Sp;
  nd=size(diag(Sp1,i),'*');
  dd1=dd(1:nd);
  diagset(Sp1,m2sp(dd1),i);
  A1=sp2m(Sp1);
  [ma,na]=size(A);
  B=[diag(diag(A,i),i)-diag(dd1,i)];
  [mb,nb]=size(B);
  if nb > na then B(:,(na+1):nb)=[];end;
  if mb > ma then B(ma+1:mb,:)=[];
  else 
    B=[B;zeros(ma-mb,na)];
  end
  if nb < na then B=[B,zeros(ma,na-nb)];end
  A2=A-B;
  if or(A1<>A2) then pause;end
end

// nsp_spcolmatrix_diag_create 
// XXXXX revoir diag pour savoir si on fait une matrice carrée 
// ou pas ? 

for i=-3:3
  Sp1=diagcre(m2sp([1,3,0,0,4,5]),i);
  A1=sp2m(Sp1);
  A2=diag([1,3,0,0,4,5],i);
  if or(A1<>A2) then pause;end
  Sp1=diagcre(m2sp([1,3,0,0,4,5]'),i);
  A1=sp2m(Sp1);
  A2=diag([1,3,0,0,4,5],i);
  if or(A1<>A2) then pause;end
end

// nsp_spcolmatrix_mult 

Sp1= Sp*Sp';
A1=sp2m(Sp1);
A2=A*A';
if or(A1<>A2) then pause;end

// nsp_spcolmatrix_mult_matrix
// XX Attention car Sp*scal fait tomber la dessus.
// et dim pas compatibles.

Sp1= Sp*(A');
A1=Sp1;
A2=A*A';
if or(A1<>A2) then pause;end

// nsp_spcolmatrix_mult_m_sp 

Sp1= A*Sp';
A1=Sp1;
A2=A*A';
if or(A1<>A2) then pause;end

// nsp_spcolmatrix_mult_scal 

Sp1= Sp*m2sp(7);
A1=sp2m(Sp1);
A2=A*7;
if or(A1<>A2) then pause;end

Sp1= m2sp(7)*Sp;
A1=sp2m(Sp1);
A2=7*A;
if or(A1<>A2) then pause;end

//ATESTER 

// nsp_spcolmatrix_complexify 

// ATESTER 

// nsp_spcolmatrix_setr 
// XXXX pas interfacé : ATESTER 
// nsp_spcolmatrix_seti
// XXXX pas interfacé : ATESTER 


// nsp_spcolmatrix_transpose 

Sp1=Sp';
A1=sp2m(Sp1);
if or(A1<>A') then pause;end

Sp1=Sp.';
A1=sp2m(Sp1);
if or(A1<>A.') then pause;end

// Sp + Sp 

A2=rand(A);
Sp2= sparse(A2)
Sp1=Sp + Sp2 
A1=sp2m(Sp1);
if or(A1<>A +  A2 ) then pause;end

// Sp + full -> full

A2=rand(A);
A1=Sp + A2 
if or(A1<>A +  A2 ) then pause;end

// full + Sp -> full

A2=rand(A);
A1= A2 + Sp;
if or(A1<>A +  A2 ) then pause;end

// NspSpColMatrix *nsp_spcolmatrix_sub(NspSpColMatrix *A, NspSpColMatrix *B)

// SP .* SP 

A2=rand(A);
Sp2= sparse(A2)
Sp1=Sp .* Sp2 
A1=sp2m(Sp1);
if or(A1<>A .* A2 ) then pause;end

// SP .* full

A2=rand(A);
Sp2= sparse(A2)
Sp1=Sp .* A2
A1=sp2m(Sp1);
if or(A1<>A .* A2 ) then pause;end

// full .* SP

A2=rand(A);
Sp2= sparse(A2)
Sp1= A2 .* Sp 
A1=sp2m(Sp1);
if or(A1<>A .* A2 ) then pause;end

// SP .* scalar 

Sp1=Sp .* 4 
A1=sp2m(Sp1);
if or(A1<>A * 4 ) then pause;end

Sp1=Sp .* sparse(4) 
A1=sp2m(Sp1);
if or(A1<>A * 4 ) then pause;end

Sp1= 4 .* Sp 
A1=sp2m(Sp1);
if or(A1<>A * 4 ) then pause;end

Sp1= sparse(4) .* Sp 
A1=sp2m(Sp1);
if or(A1<>A * 4 ) then pause;end

// Sp * scalar 

Sp1=Sp*m2sp(4);
A1=sp2m(Sp1);
if or(A1<>A*4) then pause;end

Sp1=Sp*m2sp(zeros(12,0));
A1=sp2m(Sp1);
if or(A1<>zeros(12,0)) then pause;end


// op can be '+'(A+B) ,'-' (A-B), '#' (-A+B)
// NspMatrix *nsp_spcolmatrix_op_scal

Sp1=Sp + m2sp(4);
if or(Sp1<>A+4) then pause;end

Sp1=Sp - m2sp(4);
if or(Sp1<>A-4) then pause;end

Sp1= - Sp + m2sp(4);
if or(Sp1<>-A+4) then pause;end


// nsp_spcolmatrix_clean(NspSpColMatrix *A, int rhs, double epsa, double epsr)

Sp1 = sparse(Sp +m2sp(0.01));
Sp2 = clean(Sp1,0.1);
A1=sp2m(Sp2);
if or(A1<>clean(A+0.01,0.1)) then pause;end

// nsp_spcolmatrix_realpart: 

Sp1 = real(Sp);
A1=real(A);
if or(A1<>full(Sp1)) then pause;end

// nsp_spcolmatrix_imagpart: 

Sp1 = imag(Sp);
A1=imag(A);
if or(A1<>full(Sp1)) then pause;end


// /*
//  *nsp_mat_inv_el: a(i,j)=1/a(i,j) A est changee
//  */
 
// /*
//  *nsp_mat_kron: produit de Kroeneker
//  * A et B sont inchanges 
//  */

// /*
//  *nsp_mat_sort: Index=Sort(A)
//  * A is changed, Index created with the indexes 
//  * return NULLMAT on error 
//  * WARNING : A must be real but the test is not done here 
//  * ======
//  */

// nsp_spcolmatrix_sum

for c=['f','c','r'] 
  Sp1=sum(Sp);
  A1=sp2m(Sp1);
  if or(A1<>sum(A)) then pause;end
end
 

// /*
//  * Prod =nsp_mat_prod(A ,B])
//  *     A is unchanged 
//  * if B= 'c' the prod for the column indices is computed 
//  *       and a column vector is returned. 
//  * if B= 'r' the prod for the row indices is computed 
//  *       and a Row vector is returned.
//  * if B= 'f' the full prod is computed 
//  */

// /*
//  *nsp_mat_cum_prod: Cumulative Product of all elements of A
//  * A is unchanged 
//  */

// /*
//  *nsp_mat_cum_sum: Cumulative Sum of all elements of A
//  * A is unchanged 
//  */


// nsp_spcolmatrix_maxitt 
// XXXX n-ary version to be implemented 

Spr=real(Sp);Ar=real(A);
A1=int(rand(A)*30);A1(A1>=5)=0;
Sp1=m2sp(A1);
Sp2=max(Spr,Sp1);
A2=max(Ar,A1);
if or(A2<>sp2m(Sp2)) then pause;end

[Sp2,SpImax2]=max(Spr,Sp1);
[A2,AImax2]=max(Ar,A1);
if or(A2<>sp2m(Sp2)) then pause;end
I=find(Ar==0 && A1==0);
AImax2(I)=0;
if or(sp2m(SpImax2)<>AImax2) then pause;end

// nsp_spcolmatrix_minitt 
// XXXX to be done 

// Max =nsp_mat_maxi(A,B,Imax,lhs)
// nsp_mat_mini: Mini(A)

[Sp1]=max(Spr);
A1=sp2m(Sp1);
A2=max(Ar);
if or(A1<>A2) then pause;end

[Sp1,Imax]=max(Spr);
A1=sp2m(Sp1);
[A2,Imax2]=max(Ar);
if or(A1<>A2) then pause;end
if or(Imax<>Imax2) then pause;end

[Sp1]=max(Spr,'c');
A1=sp2m(Sp1);
A2=max(Ar,'c');
if or(A1<>A2) then pause;end

[Sp1,Imax]=max(Spr,'c');
A1=sp2m(Sp1);
[A2,Imax1]=max(Ar,'c');
if or(A1<>A2) then pause;end
if or(Imax<>Imax1) then pause;end

[Sp1]=max(Spr,'r');
A1=sp2m(Sp1);
A2=max(Ar,'r');
if or(A1<>A2) then pause;end

[Sp1,Imax]=max(Spr,'r');
A1=sp2m(Sp1);
[A2,Imax1]=max(Ar,'r');
if or(A1<>A2) then pause;end
if or(Imax<>Imax1) then pause;end



// /*
//  * Creates a Matrix and initialize it with the 
//  * function func 
//  * R=func(i,j) or R=func(i,j,&Imag) 
//  */


// nsp_mat_triu 

for k=-3:3
  Sp1=triu(Sp,k);
  A1=sp2m(Sp1);
  A2=triu(A,k);
  if or(A1<>A2) then pause;end
end

// nsp_mat_tril 

for k=-3:3
  Sp1=tril(Sp,k);
  A1=sp2m(Sp1);
  A2=tril(A,k);
  if or(A1<>A2) then pause;end
end

// NspSpColMatrix *nsp_spcolmatrix_eye(int m, int n)

Sp1=sp_eye(4,7);
A1=sp2m(Sp1);
A2=eye(4,7);
if or(A1<>A2) then pause;end

// 
// spones is not similar to ones ! 
// spones(X) is a 1 0 matrix with same shape as X 

Sp1=sp_ones(Sp);
A1=sp2m(Sp1);
A2= b2m(m2b(full(Sp)));
if or(A1<>A2) then pause;end

// NspSpColMatrix *nsp_spcolmatrix_zeros(int m, int n)

Sp1=sp_zeros(4,7);
A1=sp2m(Sp1);
A2=zeros(4,7);
if or(A1<>A2) then pause;end

// nsp_mat_rand: A=rand(m,n,percent)
// Faire une fonction + efficace  tester 

function A=sprand(m,n,percent)
  nv=m*percent;
  RC=[];V=[];
  for i=1:n 
    RC=[RC;[grand(nv,1,'uin',1,m),ones(nv,1)*i]];
    V =[V;rand(nv,1)];
  end
  A=sparsecol(RC,V,[m,n]);
endfunction



//   A Set of term to term function on Matrices (complex or real)

// /*
//  *nsp_mat_pow_el(A,B) a(i,i)**b(i,i) 
//  * A is changed  since 0.^0 --> 1 the returned matrix is full
//  */

// /*
//  *nsp_mat_pow_scalar(A,B) a(i,i)**b
//  * A is changed 
//  */

// /*
//  * MatPowScalarMat(A,B) a(i,j)=b**a(i,j)
//  * A is changed 
//  */

// /*
//  *nsp_mat_div_el(A,B) a(i,i)/b(i,i) 
//  * A is changed 
//  */

// /*
//  *nsp_mat_div_scalar(A,B) a(i,i)/b
//  * A is changed 
//  */

// /*
//  *nsp_mat_bdiv_el(A,B) a(i,j) = a(i,j) \ b(i,j) 
//  * A is changed 
//  */

// /*
//  *nsp_mat_bdiv_scalar(A,B) a(i,j)= a(i,j) \ b
//  * A is changed 
//  */

// /*
//  * A=nsp_mat_mult_el(A,B) a(i,i).*b(i,i) 
//  * A is changed 
//  */

// A=Acos(A), 

A1=A/max(real(A));
Sp1=m2sp(A1);
A2=acos(Sp1);
if norm(acos(A1)-A2) > 1.e-8  then pause;end 

// A=Acosh(A), 

A1=abs(A)+1;
Sp1=m2sp(A1);
A2=acosh(Sp1);
if norm(acosh(A1)-A2) > 1.e-8  then pause;end 

// SpColUnary
// A=Asin(A), 
 
A1=A/max(real(A));
Sp1=m2sp(A1);
Sp1=asin(Sp1);
if norm(asin(A1)-sp2m(Sp1)) > 1.e-8  then pause;end 

// asinh 

A1=A;
Sp1=m2sp(A1);
Sp1=asinh(Sp1);
if norm(asinh(A1)-sp2m(Sp1)) > 1.e-8  then pause;end 

// atan 

A1=A/max(abs(A));
Sp1=m2sp(A1);
Sp1=atan(Sp1);
if norm(atan(A1)-sp2m(Sp1)) > 1.e-8  then pause;end 

// atanh 

A1=A/(2*max(real(A)));
Sp1=m2sp(A1);
Sp1=atanh(Sp1);
if norm(atanh(A1)-sp2m(Sp1)) > 1.e-8  then pause;end 

// ceil

A1=A*1.23;
Sp1=m2sp(A1);
Sp1=ceil(Sp1);
if norm(ceil(A1)-sp2m(Sp1)) > 1.e-8  then pause;end 

// int 

A1=A*1.23;
Sp1=m2sp(A1);
Sp1=int(Sp1);
if norm(int(A1)-sp2m(Sp1)) > 1.e-8  then pause;end 

// floor 

A1=A*1.23;
Sp1=m2sp(A1);
Sp1=floor(Sp1);
if norm(floor(A1)-sp2m(Sp1)) > 1.e-8  then pause;end 

// round 

A1=A*1.23;
Sp1=m2sp(A1);
Sp1=round(Sp1);
if norm(round(A1)-sp2m(Sp1)) > 1.e-8  then pause;end 

// sign 

A1=A*1.23;
Sp1=m2sp(A1);
Sp1=sign(Sp1);
if norm(sign(A1)-sp2m(Sp1)) > 1.e-8  then pause;end 

// tan 

A1=A;
Sp1=m2sp(A1);
Sp1=tan(Sp1);
if norm(tan(A1)-sp2m(Sp1)) > 1.e-8  then pause;end 

// tanh 

A1=A;
Sp1=m2sp(A1);
Sp1=tanh(Sp1);
if norm(tanh(A1)-sp2m(Sp1)) > 1.e-8  then pause;end 

// abs 

A1=A - 20;
Sp1=m2sp(A1);
Sp1=abs(Sp1);
if norm(abs(A1)-sp2m(Sp1)) > 1.e-8  then pause;end 

// erf 
// XXXXX interface mising 

// A1=A - 20;
// Sp1=m2sp(A1);
// Sp1=erf(Sp1);
// if norm(erf(A1)-sp2m(Sp1)) > 1.e-8  then pause;end 

// XXXXX A=Erfc(A), Erf function 

// arg 

A1=A - 20;
Sp1=m2sp(A1);
Sp1=arg(Sp1);
if norm(arg(A1)-sp2m(Sp1)) > 1.e-8  then pause;end 

// A=Polar(A,B),
// XXXX A=A(cos(B)+%i*sin(B);

// conj 
 
A1=A - 20;
Sp1=m2sp(A1);
Sp1=conj(Sp1);
if norm(conj(A1)-sp2m(Sp1)) > 1.e-8  then pause;end 
 

//  *nsp_mat_cos: A=Cos(A)
//  *nsp_mat_cosh: A=Cosh(A)

A1=A;
Sp1=m2sp(A1);
A2=cos(Sp1);
if norm(cos(A1)-A2) > 1.e-8  then pause;end 

A1=abs(A)+1;
Sp1=m2sp(A1);
A2=cosh(Sp1);
if norm(cosh(A1)-A2) > 1.e-8  then pause;end 

// MatExpl : Exponentiation term to term 


// NspMatrix *nsp_spcolmatrix_expel(NspSpColMatrix *A)
// {
//   return SpColUnary2Full(A,exp,nsp_exp_c);
// }

// /*
//  *nsp_mat_logel: A=LogEl(A)  log term to term 
//  * A is changed  
//  * The real case is special since the result can be complex
//  */

// int nsp_spcolmatrix_logel(NspSpColMatrix *A)
// {
//   int i,k;
//   if ( A->rc_type == 'r')
//     {
//       /* Check if really real or imaginary case */
//       int itr = 0;
//       for ( i=0 ; i < A->m ; i++) 
// 	for ( k=0 ; k < A->D[i]->size ; k++) 
// 	  if ( A->D[i]->R[k] < 0.00 ) 
// 	    {
// 	      itr = 1; break;
// 	    }
//       if ( itr == 0) 
// 	{
// 	  /* real case sqrt(A) is real  */
// 	  SpColUnary(A,log,nsp_log_c);
// 	  return OK;
// 	}
//       else 
// 	{
// 	  /* result is complex  */
// 	  if (nsp_spcolmatrix_seti(A,0.00) == FAIL ) return FAIL;
// 	  SpColUnary(A,log,nsp_log_c);
// 	  return OK;
// 	}
//     }
//   /* A is complex and sqrt(A) too **/
//   SpColUnary(A,log,nsp_log_c);
//   return OK;
// }

// /*
//  *nsp_mat_sin: A=Sin(A)
//  * A is changed  
//  * return 0 if error 
//  */

// void nsp_spcolmatrix_sin(NspSpColMatrix *A)
// {
//   SpColUnary(A,sin,nsp_sin_c);
// }


// /*
//  *nsp_mat_sinh: A=Sinh(A)
//  * A is changed  
//  * return 0 if error 
//  */

// void nsp_spcolmatrix_sinh(NspSpColMatrix *A)
// {
//   SpColUnary(A,sinh,nsp_sinh_c);
// }

// /*
//  *nsp_mat_sqrtel: A=SqrtEl(A)  term to term square root
//  * A is changed  
//  * return 0 if error 
//  * The real case is special since the result can be complex
//  */

// int nsp_spcolmatrix_sqrtel(NspSpColMatrix *A)
// {
//   int i,k;
//   if ( A->rc_type == 'r')
//     {
//       /* Check if really real or imaginary case */
//       int itr = 0;
//       for ( i=0 ; i < A->m ; i++) 
// 	for ( k=0 ; k < A->D[i]->size ; k++) 
// 	  if ( A->D[i]->R[k] < 0.00 ) 
// 	    {
// 	      itr = 1; break;
// 	    }
//       if ( itr == 0) 
// 	{
// 	  /* real case sqrt(A) is real  */
// 	  SpColUnary(A,sqrt,nsp_sqrt_c);
// 	  return OK;
// 	}
//       else 
// 	{
// 	  /* result is complex  */
// 	  if (nsp_spcolmatrix_seti(A,0.00) == FAIL ) return FAIL;
// 	  SpColUnary(A,sqrt,nsp_sqrt_c);
// 	  return OK;
// 	}
//     }
//   /* A is complex and sqrt(A) too **/
//   SpColUnary(A,sqrt,nsp_sqrt_c);
//   return OK;
// }

// /*
//  *nsp_mat_minus(A),  A= -A 
//  * A is changed 
//  */

// int nsp_spcolmatrix_minus(NspSpColMatrix *A)
// {
//   int i,k ;
//   if ( A->rc_type  == 'r') 
//     {
//       for ( i = 0 ; i < A->m ; i++)
// 	for ( k = 0 ; k < A->D[i]->size ; k++)
// 	  {
// 	    A->D[i]->R[k] = - A->D[i]->R[k];
// 	  }
//     }
//   else
//     {
//       for ( i = 0 ; i < A->m ; i++)
// 	for ( k = 0 ; k < A->D[i]->size ; k++)
// 	  {
// 	    A->D[i]->C[k].r = - A->D[i]->C[k].r;
// 	    A->D[i]->C[k].i = - A->D[i]->C[k].i;
// 	  }
//     }
//   return(OK);
// }


// /*
//  * Kronecker product of two Matrices 
//  * PK is the result it must be created 
//  * before calling this function size (AmxBm,AnxBn)
//  * The rule to compute PK is the following 
//  * PK[ i + j*B->m + k*(B->m*A->m) + p*(B->m*A->m*B->n)] = a(j,p)*b(i,k)
//  * The i-loop leads to dcopy calls 
//  */

// /*
//  *nsp_mat_magic: A=Magic(n)
//  */

// /*
//  *nsp_mat_franck: A=Franck(n)
//  */

// /*
//  *nsp_mat_hilbert: A=Hilbert(n)
//  */

// /*
//  * Comparison operators
//  */

// /*
//  * Operation on Matrices leading to Boolean Matrices results 
//  * Res = A(i,j) op B(i;j) 
//  * with the special case : 
//  *      A(i;j)op B(0,0) or A(0,0) op B(i,j) if A or B are of size 1x1
//  *      
//  * A and B are unchanged : Res is created 
//  */

// find 

A=testmatrix('magic',6);A(A<=15)=0;
Sp=m2sp(A);
I=find(A);
Is=find(Sp);
if or(I<>Is) then pause;end

[I,J]=find(A);
II=[I;J];
II=sort(II,'lc');
[Is,Js]=find(Sp);
IIs=[Is;Js];
IIs=sort(IIs,'lc');
if or(II<>IIs) then pause;end


