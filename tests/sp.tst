// test sp operations 

A=testmatrix('magic',6);A(A<=15)=0;
Sp=m2sp(A);
A1=sp2m(Sp);
if or(A1<>A) then pause;end

Sp1=create_sp(7,8);
A1=sp2m(Sp1);
if or(A1<>zeros(7,8)) then pause;end

// nsp_spmatrix_sparse
// nsp_spmatrix_get

[RC,V,mn]=spget(Sp);
Sp1=sparsecol(RC,V,mn);
A1=sp2m(Sp);
if or(A1<>A) then pause;end

// nsp_spmatrix_copy

Sp1=Sp;
A1=sp2m(Sp);
if or(A1<>A) then pause;end

//  nsp_spmatrix_nnz 

n=nnz_sp(Sp);
n1=size(find(A<>0),'*');
if n1<>n then pause;end 

// nsp_spmatrix_redim

Sp1=spredim(Sp,1,36);
A1=sp2m(Sp);
A2=spredim(Sp,1,36);
if or(A1<>A) then pause;end
Sp2=spredim(Sp,36,1);
A1=sp2m(Sp);
A2=spredim(Sp,36,1);
if or(A1<>A) then pause;end
Sp2=spredim(Sp,2,18);
A1=sp2m(Sp);
A2=spredim(Sp,2,18);
if or(A1<>A) then pause;end
Sp2=spredim(Sp,9,4);
A1=sp2m(Sp);
A2=spredim(Sp,9,4);
if or(A1<>A) then pause;end

// nsp_spmatrix_enlarge_cols
// nsp_spmatrix_enlarge
// nsp_spmatrix_concatr

Sp1=[Sp,Sp];
A1=sp2m(Sp1);
if or(A1<>[A,A]) then pause;end

// nsp_spmatrix_concatd 

Sp1=[Sp;Sp];
A1=sp2m(Sp1);
if or(A1<>[A;A]) then pause;end

// nsp_spmatrix_concatdiag 

Sp1=[Sp # Sp];
A1=sp2m(Sp1);
if or(A1<>[A # A]) then pause;end

// nsp_spmatrix_insert_elt(A,i,j,B,rb,cb)
// nsp_spmatrix_delete_elt(A,row,col,amin,amax)
// nsp_spmatrix_get_elt(B,i,j)
// nsp_spmatrix_store(A,r,c,col,B,r1,c1)

Sp1=Sp;
Sp1(1:2,3)=m2sp([7;8]);
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

// 

Sp1=Sp;
Sp1(3,5)=m2sp([7]);
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

// nsp_spmatrix_set_row 

Sp1=Sp;
Sp1([1,4,7])=m2sp(0);
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

// nsp_spmatrix_from_mat : m2sp 
// nsp_spmatrix_to_mat: sp2m
// already tested 

// nsp_spmatrix_transpose 

Sp1=Sp';
A1=sp2m(Sp1);
if or(A1<>A') then pause;end

Sp1=Sp.';
A1=sp2m(Sp1);
if or(A1<>A.') then pause;end

// NspSpColMatrix *nsp_spmatrix_add(NspSpColMatrix *A, NspSpColMatrix *B)
// NspSpColMatrix *nsp_spmatrix_sub(NspSpColMatrix *A, NspSpColMatrix *B)
// NspSpColMatrix *nsp_spmatrix_multtt(NspSpColMatrix *A, NspSpColMatrix *B)
// nsp_spmatrix_mult_scal(NspSpColMatrix *A, NspSpColMatrix *B)

Sp1=Sp*m2sp(4);
A1=sp2m(Sp1);
if or(A1<>A*4) then pause;end

Sp1=Sp*m2sp([]);
A1=sp2m(Sp1);
if or(A1<>A.*[]) then pause;end

// op can be '+'(A+B) ,'-' (A-B), '#' (-A+B)
// NspMatrix *nsp_spmatrix_op_scal

Sp1=Sp + m2sp(4);
if or(Sp1<>A+4) then pause;end

Sp1=Sp - m2sp(4);
if or(Sp1<>A-4) then pause;end

Sp1= - Sp + m2sp(4);
if or(Sp1<>-A+4) then pause;end

// nsp_spmatrix_clean(NspSpColMatrix *A, int rhs, double epsa, double epsr)
// XXXXX : interface to be added 

// nsp_spmatrix_maxitt 
// 
// nsp_spmatrix_sum

 for c=['f','c','r'] 
   Sp1=sum(Sp);
   A1=sp2m(Sp1);
   if or(A1<>sum(A)) then pause;end
 end
 

// SpColUnary
// A=Asin(A), 
 
A1=A/max(A);
Sp1=m2sp(A1);
Sp1=asin(Sp1);
if norm(asin(A1)-sp2m(Sp1)) > 1.e-8  then pause;end 

// asinh 

A1=A;
Sp1=m2sp(A1);
Sp1=asinh(Sp1);
if norm(asinh(A1)-sp2m(Sp1)) > 1.e-8  then pause;end 

// atan 

A1=A/max(A);
Sp1=m2sp(A1);
Sp1=atan(Sp1);
if norm(atan(A1)-sp2m(Sp1)) > 1.e-8  then pause;end 

// atanh 

A1=A/(2*max(A));
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

A1=A - 20;
Sp1=m2sp(A1);
//XXX Sp1=erf(Sp1);
//if norm(erf(A1)-sp2m(Sp1)) > 1.e-8  then pause;end 

 // arg 
 
 A1=A - 20;
Sp1=m2sp(A1);
Sp1=arg(Sp1);
if norm(arg(A1)-sp2m(Sp1)) > 1.e-8  then pause;end 

// conj 
 
A1=A - 20;
Sp1=m2sp(A1);
Sp1=conj(Sp1);
if norm(conj(A1)-sp2m(Sp1)) > 1.e-8  then pause;end 
 
// find 

A=testmatrix('magic',6);A(A<=15)=0;
Sp=m2sp(A);
I=find(A);
Is=find(Sp);
if or(I<>gsort(Is,'g','i')) then pause;end
[I,J]=find(A);
[J,k]=sort(J);
I=I(k);
//XXX[Is,Js]=find(Sp);
//if or(I<>Is) then pause;end
//if or(J<>Js) then pause;end

