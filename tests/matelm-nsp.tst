// -*- Mode: scilab -*- 

// fonction temporaires 
// -------------------------------------------------------
function [z]=plus_sp_m(a,b) z=full(a)+b endfunction 
function [z]=plus_m_sp(a,b) z=a+full(b) endfunction 
function [z]=minus_sp_m(a,b) z=full(a)-b endfunction 
function [z]=minus_m_sp(a,b) z=a-full(b) endfunction 
function [z]=concatr_sp_m(a,b) z=[a,sparse(b)] endfunction 
function [z]=concatd_sp_m(a,b) z=[a;sparse(b)] endfunction 
function [z]=mult_m_sp(a,b)  z=sparse(a)* b ; endfunction
function [z]=mult_sp_m(a,b)  z=a * sparse(b); endfunction
function [z]=dst_m_sp(a,b)   z=sparse(a)* b endfunction
function [z]=dst_sp_m(a,b)   z=a * sparse(b) endfunction
function [z]=string(a) z=m2s(a) endfunction 
function [z]=norm(a) z=max(abs(a)) endfunction 
function [z]=mult_m_sp(a,b)  z=sparse(a)* b endfunction
function y=kron(a,b) ; y = a.*.b ; endfunction 
//-------------------------------------------------------
%eps=1.e-8

//test of elementary functions

a=[1 2;3 4];
if or(size(a)<>[2 2]) then pause,end
if or(size(a+0)<>[2 2]) then pause,end
if size(a,'*')<>4 then pause,end
if size(a+0,'*')<>4 then pause,end
[m,n]=size(a);if or([m,n]<>[2 2]) then pause,end

a=[1;2];
if size(a,1)<>2 then pause,end
if size(a+0,1)<>2 then pause,end
if size(a,'r')<>2 then pause,end
if size(a+0,'r')<>2 then pause,end
if size(a,2)<>1 then pause,end
if size(a+0,2)<>1 then pause,end
if size(a,'c')<>1 then pause,end
if size(a+0,'c')<>1 then pause,end
[m,n]=size(a);if or([m,n]<>[2 1]) then pause,end

a=string([1 2;3 4]);
if or(size(a)<>[2 2]) then pause,end
if or(size(a+a)<>[2 2]) then pause,end
if size(a,'*')<>4 then pause,end
if size(a+a,'*')<>4 then pause,end
[m,n]=size(a);if or([m,n]<>[2 2]) then pause,end

a=string([1;2]);
if size(a,1)<>2 then pause,end
if size(a+a,1)<>2 then pause,end
if size(a,'r')<>2 then pause,end
if size(a+a,'r')<>2 then pause,end
if size(a,2)<>1 then pause,end
if size(a+a,2)<>1 then pause,end
if size(a,'c')<>1 then pause,end
if size(a+a,'c')<>1 then pause,end
[m,n]=size(a);if or([m,n]<>[2 1]) then pause,end

//TOBEDONE a=[1 2;3 4]+%s;
//TOBEDONE if or(size(a)<>[2 2]) then pause,end
//TOBEDONE if or(size(a+0)<>[2 2]) then pause,end
//TOBEDONE if size(a,'*')<>4 then pause,end
//TOBEDONE if size(a+0,'*')<>4 then pause,end
//TOBEDONE [m,n]=size(a);if or([m,n]<>[2 2]) then pause,end
//TOBEDONE 
//TOBEDONE a=[1;2]+%s;
//TOBEDONE if size(a,1)<>2 then pause,end
//TOBEDONE if size(a+0,1)<>2 then pause,end
//TOBEDONE if size(a,'r')<>2 then pause,end
//TOBEDONE if size(a+0,'r')<>2 then pause,end
//TOBEDONE if size(a,2)<>1 then pause,end
//TOBEDONE if size(a+0,2)<>1 then pause,end
//TOBEDONE if size(a,'c')<>1 then pause,end
//TOBEDONE if size(a+0,'c')<>1 then pause,end
//TOBEDONE [m,n]=size(a);if or([m,n]<>[2 1]) then pause,end
//TOBEDONE 

a=[1 2;3 4]==1;
if or(size(a)<>[2 2]) then pause,end
if or(size(a&a)<>[2 2]) then pause,end
if size(a,'*')<>4 then pause,end
if size(a|a,'*')<>4 then pause,end
[m,n]=size(a);if or([m,n]<>[2 2]) then pause,end

a=[1;2]==1;
if size(a,1)<>2 then pause,end
if size(a|a,1)<>2 then pause,end
if size(a,'r')<>2 then pause,end
if size(a|a,'r')<>2 then pause,end
if size(a,2)<>1 then pause,end
if size(a|a,2)<>1 then pause,end
if size(a,'c')<>1 then pause,end
if size(a|a,'c')<>1 then pause,end
[m,n]=size(a);if or([m,n]<>[2 1]) then pause,end

//eye
if or(eye(2,2)<>[1 0;0 1]) then pause,end
if or(eye(2,1)<>[1;0]) then pause,end
n=2;
if or(eye(n,2)<>[1 0;0 1]) then pause,end
if or(eye(2,n)<>[1 0;0 1]) then pause,end
if or(eye(n,n)<>[1 0;0 1]) then pause,end
//TOBEDONE if or(size(eye())<>[-1 -1]) then pause,end
a=[1 2;3 4];
if or(eye(a)<>[1 0;0 1]) then pause,end
if or(eye(a+0)<>[1 0;0 1]) then pause,end
//TOBEDONE a=[1 2;3 4]+%s;
//TOBEDONE if or(eye(a)<>[1 0;0 1]) then pause,end
//TOBEDONE if or(eye(a+0)<>[1 0;0 1]) then pause,end
a=string([1 2;3 4]);
if or(eye(a)<>[1 0;0 1]) then pause,end
if or(eye(a+a)<>[1 0;0 1]) then pause,end
a=[1 2;3 4]>1;
if or(eye(a)<>[1 0;0 1]) then pause,end
if or(eye(a&a)<>[1 0;0 1]) then pause,end

if eye([])<>[] then pause,end
//ones
if or(ones(2,2)<>[1 1;1 1]) then pause,end
if or(ones(2,1)<>[1;1]) then pause,end
n=2;
if or(ones(n,2)<>[1 1;1 1]) then pause,end
if or(ones(2,n)<>[1 1;1 1]) then pause,end
if or(ones(n,n)<>[1 1;1 1]) then pause,end
if ones(1)<>1 then pause,end
if ones([])<>[] then pause,end
a=[1 2;3 4];
if or(ones(a)<>[1 1;1 1]) then pause,end
if or(ones(a+0)<>[1 1;1 1]) then pause,end
//TOBEDONE a=[1 2;3 4]+%s;
//TOBEDONE if or(ones(a)<>[1 1;1 1]) then pause,end
//TOBEDONE if or(ones(a+0)<>[1 1;1 1]) then pause,end
a=string([1 2;3 4]);
if or(ones(a)<>[1 1;1 1]) then pause,end
if or(ones(a+a)<>[1 1;1 1]) then pause,end
a=[1 2;3 4]>1;
if or(ones(a)<>[1 1;1 1]) then pause,end
if or(ones(a&a)<>[1 1;1 1]) then pause,end
//rand
if or(size(rand(2,2))<>[2 2]) then pause,end
if or(size(rand(2,1))<>[2 1]) then pause,end
n=2;
if or(size(rand(n,2))<>[n 2]) then pause,end
if or(size(rand(2,n))<>[2 n]) then pause,end
if or(size(rand(n,n))<>[n n]) then pause,end
//if or(size(rand())<>[1 1]) then pause,end
if rand([])<>[] then pause,end
a=[1 2;3 4];
if or(size(rand(a))<>size(a)) then pause,end
if or(size(rand(a+0))<>size(a)) then pause,end
//TOBEDONE a=[1 2;3 4]+%s;
//TOBEDONE if or(size(rand(a))<>size(a)) then pause,end
//TOBEDONE if or(size(rand(a+0))<>size(a)) then pause,end
a=string([1 2;3 4]);
//TOBEDONE if or(size(rand(a))<>size(a)) then pause,end
//TOBEDONE if or(size(rand(a+a))<>size(a)) then pause,end
a=[1 2;3 4]>1;
//TOBEDONE if or(size(rand(a))<>size(a)) then pause,end
//TOBEDONE if or(size(rand(a+a))<>size(a)) then pause,end
rand('seed',1);if rand('seed')<>1 then pause,end
a=2;rand('seed',a);if rand('seed')<>a then pause,end

//diagcre
//TOBEDONE : faire une fonction diag 
//Attention dans NSP on a en plus diagset 
if or(diagcre([1 2 3])<>[1 0 0;0 2 0;0 0 3]) then pause,end
a=[1 2 3];
if or(diagcre(a)<>[1 0 0;0 2 0;0 0 3]) then pause,end
if or(diagcre([1 %i 2])<>[1 0 0;0 %i 0;0 0 2]) then pause,end
a=[1 %i 2 ];
if or(diagcre(a)<>[1 0 0;0 %i 0;0 0 2]) then pause,end
a=[1 2 3 4;5 6 7 8];
if or(diage(a)<>[1;6]) then pause,end
if or(diage(a+0)<>[1;6]) then pause,end
if or(diage(a,1)<>[2;7]) then pause,end
if or(diage(a+0,1)<>[2;7]) then pause,end
if or(diage(a,-1)<>[5]) then pause,end
if or(diage(a+0,-1)<>[5]) then pause,end
if diage(a,4)<>[] then pause,end
a(1,1)=%i
if or(diage(a)<>[%i;6]) then pause,end
if or(diage(a+0)<>[%i;6]) then pause,end
if or(diage(a,1)<>[2;7]) then pause,end
if or(diage(a+0,1)<>[2;7]) then pause,end
if or(diage(a,-1)<>[5]) then pause,end
if or(diage(a+0,-1)<>[5]) then pause,end
if diage(a,4)<>[] then pause,end

if diage([])<>[] then pause,end
if diage([],1)<>[] then pause,end
if diage([],-1)<>[] then pause,end
a=[];
if diage(a)<>[] then pause,end
if diage(a,1)<>[] then pause,end
if diage(a,-1)<>[] then pause,end

// triu
a=[1 2 3 4;5 6 7 8];
if or(triu(a)<>[1 2 3 4;0 6 7 8]) then pause,end
if or(triu(a+0)<>[1 2 3 4;0 6 7 8]) then pause,end
if or(triu(a,1)<>[0 2 3 4;0 0 7 8]) then pause,end
if or(triu(a+0,1)<>[0 2 3 4;0 0 7 8]) then pause,end
if or(triu(a,-1)<>a) then pause,end
if or(triu(a+0,-1)<>a) then pause,end
if or(triu(a,4)<>0*a) then pause,end
a(1,1)=%i;
if or(triu(a)<>[%i 2 3 4;0 6 7 8]) then pause,end
if or(triu(a+0)<>[%i 2 3 4;0 6 7 8]) then pause,end
if or(triu(a,1)<>[0 2 3 4;0 0 7 8]) then pause,end
if or(triu(a+0,1)<>[0 2 3 4;0 0 7 8]) then pause,end
if or(triu(a,-1)<>a) then pause,end
if or(triu(a+0,-1)<>a) then pause,end
if or(triu(a,4)<>0*a) then pause,end

if triu([])<>[] then pause,end
if triu([],1)<>[] then pause,end
if triu([],-1)<>[] then pause,end
a=[];
if triu(a)<>[] then pause,end
if triu(a,1)<>[] then pause,end
if triu(a,-1)<>[] then pause,end

//tril
a=[1 2 3 4;5 6 7 8];
if or(tril(a)<>[1 0 0 0;5 6 0 0]) then pause,end
if or(tril(a+0)<>[1 0 0 0;5 6 0 0]) then pause,end
if or(tril(a,1)<>[1 2 0 0;5 6  7 0]) then pause,end
if or(tril(a+0,1)<>[1 2 0 0;5 6  7 0]) then pause,end
if or(tril(a,4)<>a) then pause,end
if or(tril(a+0,4)<>a) then pause,end
if or(tril(a,-3)<>0*a) then pause,end
a(1,1)=%i;
if or(tril(a)<>[%i 0 0 0;5 6 0 0]) then pause,end
if or(tril(a+0)<>[%i 0 0 0;5 6 0 0]) then pause,end
if or(tril(a,1)<>[%i 2 0 0;5 6  7 0]) then pause,end
if or(tril(a+0,1)<>[%i 2 0 0;5 6  7 0]) then pause,end
if or(tril(a,4)<>a) then pause,end
if or(tril(a+0,4)<>a) then pause,end
if or(tril(a,-3)<>0*a) then pause,end

if tril([])<>[] then pause,end
if tril([],1)<>[] then pause,end
if tril([],-1)<>[] then pause,end
a=[];
if tril(a)<>[] then pause,end
if tril(a,1)<>[] then pause,end
if tril(a,-1)<>[] then pause,end

//abs
a=[1 2;-3 4;5,-6];
if or(abs(a)<>[1 2;3 4;5 6]) then pause,end
if or(abs(a+0)<>[1 2;3 4;5 6]) then pause,end
if abs([])<>[] then pause,end
a=[];if abs(a)<>[] then pause,end
a=[1+%i 2;-3 4;5,-6];
if or(abs(a)<>[sqrt(2) 2;3 4;5 6]) then pause,end
if or(abs(a+0)<>[sqrt(2) 2;3 4;5 6]) then pause,end

//TOBEDONE a=[1 2;-3 4;5, -6]+%s;
//TOBEDONE if or(abs(a)<>[1 2;3 4;5 6]+%s) then pause,end
//TOBEDONE if or(abs(a+0)<>[1 2;3 4;5 6]+%s) then pause,end
//TOBEDONE a=[1+%i 2;-3 4;5 -6]+%s;
//TOBEDONE if or(abs(a)<>[sqrt(2) 2;3 4;5 6]+%s) then pause,end
//TOBEDONE if or(abs(a+0)<>[sqrt(2) 2;3 4;5 6]+%s) then pause,end

a=sparse([1 2;-3 4;5 ,-6]);
if or(full(abs(a))<>[1 2;3 4;5 6]) then pause,end
if or(full(abs(a+a))<>2*[1 2;3 4;5 6]) then pause,end
a=sparse([1+%i 2;-3 4;5, -6]);
if or(full(abs(a))<>[sqrt(2) 2;3 4;5 6]) then pause,end
if or(full(abs(a+a))<>2*[sqrt(2) 2;3 4;5 6]) then pause,end

//real
a=[1 2;-3 4;5, -6];
if or(real(a)<>a) then pause,end
if or(real(a+0)<>a) then pause,end
if real([])<>[] then pause,end
a=[];if real(a)<>[] then pause,end
a=[1+%i 2;-3 4;5, -6];
if or(real(a)<>[1 2;-3 4;5 ,-6]) then pause,end
if or(real(a+0)<>[1 2;-3 4;5, -6]) then pause,end

//TOBEDONE a=[1 2;-3 4;5, -6]+%s;
//TOBEDONE if or(real(a)<>a) then pause,end
//TOBEDONE if or(real(a+0)<>a) then pause,end
//TOBEDONE a=[1+%i 2;-3 4;5 ,-6]+%s;
//TOBEDONE if or(real(a)<>[1 2;-3 4;5 ,-6]+%s) then pause,end
//TOBEDONE if or(real(a+0)<>[1 2;-3 4;5 ,-6]+%s) then pause,end

//TOBEDONEa=sparse([1 2;-3 4;5, -6]);
//TOBEDONEif or(real(a)<>a) then pause,end
//TOBEDONEif or(real(a+a)<>2*a) then pause,end
//TOBEDONEa=sparse([1+%i 2;-3 4;5 ,-6]);
//TOBEDONEif or(full(real(a))<>[1 2;-3 4;5, -6]) then pause,end
//TOBEDONEif or(full(real(a+a))<>2*[1 2;-3 4;5, -6]) then pause,end

//imag
a=[1 2;-3 4;5 ,-6];
if or(imag(a)<>0*a) then pause,end
if or(imag(a+0)<>0*a) then pause,end
if imag([])<>[] then pause,end
a=[];if imag(a)<>[] then pause,end
a=[1+%i 2;-3 4;5 ,-6];
if or(imag(a)<>[1 0;0 0;0 0]) then pause,end
if or(imag(a+0)<>[1 0;0 0;0 0]) then pause,end

//TOBEDONE a=[1 2;-3 4;5 ,-6]+%s;
//TOBEDONE if or(imag(a)<>0*a) then pause,end
//TOBEDONE if or(imag(a+0)<>0*a) then pause,end
//TOBEDONE a=[1+%i 2;-3 4;5 ,-6]+%s;
//TOBEDONE if or(imag(a)<>[poly(1,'s','c') 0;0 0;0 0]) then pause,end
//TOBEDONE if or(imag(a+0)<>[poly(1,'s','c') 0;0 0;0 0]) then pause,end

//TOBEDONEa=sparse([1 2;-3 4;5, -6]);
//TOBEDONEif or(imag(a)<>0*a) then pause,end
//TOBEDONEif or(imag(a+a)<>0*a) then pause,end
//TOBEDONEa=sparse([1+%i 2;-3 4;5 ,-6]);
//TOBEDONEif or(full(imag(a))<>[1 0;0 0;0 0]) then pause,end
//TOBEDONEif or(full(imag(a+a))<>2*[1 0;0 0;0 0]) then pause,end


//conj
a=[1 2;-3 4;5,-6];
if or(conj(a)<>a) then pause,end
if or(conj(a+0)<>a) then pause,end
if conj([])<>[] then pause,end
a=[];if conj(a)<>[] then pause,end
a=[1+%i 2;-3 4;5,-6];
if or(conj(a)<>[1-%i 2;-3 4;5,-6]) then pause,end
if or(conj(a+0)<>[1-%i 2;-3 4;5,-6]) then pause,end

//TOBEDONE a=[1 2;-3 4;5,-6]+%s;
//TOBEDONE if or(conj(a)<>a) then pause,end
//TOBEDONE if or(conj(a+0)<>a) then pause,end
//TOBEDONE a=[1+%i 2;-3 4;5,-6]+%s;
//TOBEDONE if or(conj(a)<>[1-%i 2;-3 4;5,-6]+%s) then pause,end
//TOBEDONE if or(conj(a+0)<>[1-%i 2;-3 4;5,-6]+%s) then pause,end

//a=sparse([1 2;-3 4;5,-6]);
//TOBEDONEif or(conj(a)<>a) then pause,end
//TOBEDONEif or(conj(a+a)<>2*a) then pause,end
//TOBEDONEa=sparse([1+%i 2;-3 4;5,-6]);
//TOBEDONEif or(full(conj(a))<>[1-%i 2;-3 4;5,-6]) then pause,end
//TOBEDONEif or(full(conj(a+a))<>2*[1-%i 2;-3 4;5,-6]) then pause,end




//int
a=[1.2 2.5;-3.4 4.5;5.8,-6.2];
if or(int(a)<>[1 2;-3 4;5,-6]) then pause,end
if or(int(a+0)<>[1 2;-3 4;5,-6]) then pause,end
a=[1.2+2.5*%i 2.5;-3.4 4.5;5.8,-6.2];
if or(int(a)<>[1+2*%i 2;-3 4;5,-6]) then pause,end
if or(int(a+0)<>[1+2*%i 2;-3 4;5,-6]) then pause,end

//TOBEDONE a=[1.2 2.5;-3.4 4.5;5.8,-6.2]+1.1*%s;
//TOBEDONE if or(int(a)<>[1 2;-3 4;5,-6]+%s) then pause,end
//TOBEDONE if or(int(a+0)<>[1 2;-3 4;5,-6]+%s) then pause,end
//TOBEDONE a=[1.2+2.5*%i 2.5;-3.4 4.5;5.8,-6.2]+1.1*%s;
//TOBEDONE if or(int(a)<>[1+2*%i 2;-3 4;5,-6]+%s) then pause,end
//TOBEDONE if or(int(a+0)<>[1+2*%i 2;-3 4;5,-6]+%s) then pause,end

if int([])<>[] then pause,end
a=[];if int(a)<>[] then pause,end


//round

a=[1.2 2.51;-3.4 4.52;5.8,-6.2];
if or(round(a)<>[1 3;-3 5;6,-6]) then pause,end
if or(round(a+0)<>[1 3;-3 5;6,-6]) then pause,end
a=[1.2+2.51*%i 2.52;-3.4 4.52;5.8,-6.2];
if or(round(a)<>[1+3*%i 3;-3 5;6,-6]) then pause,end
if or(round(a+0)<>[1+3*%i 3;-3 5;6,-6]) then pause,end

//TOBEDONE a=[1.2 2.51;-3.4 4.52;5.8,-6.2]+1.1*%s;
//TOBEDONE if or(round(a)<>[1 3;-3 5;6,-6]+%s) then pause,end
//TOBEDONE if or(round(a+0)<>[1 3;-3 5;6,-6]+%s) then pause,end
//TOBEDONE a=[1.2+2.51*%i 2.52;-3.4 4.52;5.8,-6.2]+1.1*%s;
//TOBEDONE if or(round(a)<>[1+3*%i 3;-3 5;6,-6]+%s) then pause,end
//TOBEDONE if or(round(a+0)<>[1+3*%i 3;-3 5;6,-6]+%s) then pause,end

if round([])<>[] then pause,end
a=[];if round(a)<>[] then pause,end

//ceil
a=[1.2 2.51;-3.4 4.52;5.8,-6.2];
if or(ceil(a)<>[2 3;-3 5;6,-6]) then pause,end
if or(ceil(a+0)<>[2 3;-3 5;6,-6]) then pause,end
a=[1.2+2.51*%i 2.52;-3.4 4.52;5.8,-6.2];
if or(ceil(a)<>[2+3*%i 3;-3 5;6,-6]) then pause,end
if or(ceil(a+0)<>[2+3*%i 3;-3 5;6,-6]) then pause,end

//TOBEDONE a=[1.2 2.51;-3.4 4.52;5.8,-6.2]+1.1*%s;
//TOBEDONE if or(ceil(a)<>[2 3;-3 5;6,-6]+2*%s) then pause,end
//TOBEDONE if or(ceil(a+0)<>[2 3;-3 5;6,-6]+2*%s) then pause,end
//TOBEDONE a=[1.2+2.51*%i 2.52;-3.4 4.52;5.8,-6.2]+1.1*%s;
//TOBEDONE if or(ceil(a)<>[2+3*%i 3;-3 5;6,-6]+2*%s) then pause,end
//TOBEDONE if or(ceil(a+0)<>[2+3*%i 3;-3 5;6,-6]+2*%s) then pause,end

if ceil([])<>[] then pause,end
a=[];if ceil(a)<>[] then pause,end


//floor
a=[1.2 2.51;-3.4 4.52;5.8,-6.2];
if or(floor(a)<>[1 2;-4 4;5 ,-7]) then pause,end
if or(floor(a+0)<>[1 2;-4 4;5, -7]) then pause,end
a=[1.2+2.51*%i 2.52;-3.4 4.52;5.8,-6.2];
if or(floor(a)<>[1+2*%i 2;-4 4;5, -7]) then pause,end
if or(floor(a+0)<>[1+2*%i 2;-4 4;5, -7]) then pause,end

//TOBEDONE a=[1.2 2.51;-3.4 4.52;5.8,-6.2]+1.1*%s;
//TOBEDONE if or(floor(a)<>[1 2;-4 4;5, -7]+%s) then pause,end
//TOBEDONE if or(floor(a+0)<>[1 2;-4 4;5 ,-7]+%s) then pause,end
//TOBEDONE a=[1.2+2.51*%i 2.52;-3.4 4.52;5.8,-6.2]+1.1*%s;
//TOBEDONE if or(floor(a)<>[1+2*%i 2;-4 4;5, -7]+%s) then pause,end
//TOBEDONE if or(floor(a+0)<>[1+2*%i 2;-4 4;5, -7]+%s) then pause,end

if floor([])<>[] then pause,end
a=[];if floor(a)<>[] then pause,end

//sign
a=[1.2 2.51;-3.4 4.52;5.8,-6.2];
if or(sign(a)<>[1 1;-1 1;1, -1]) then pause,end
//TOBEDONEa=[1+%i 2.51;-3.4 4.52;5.8,-6.2];
//TOBEDONE if norm(sign(a)-[sqrt(2)/2*(1+%i) 1;-1 1;1 -1])>10*%eps then pause,end
if sign([])<>[] then pause,end
a=[];if sign(a)<>[] then pause,end

//clean
//XXXXX

//log and exp
a=[1.2 2.51;-3.4 4.52;5.8,-6.2];
if norm(exp(log(a))-a)>10*%eps then pause,end
if norm(exp(log(a+0))-a)>10*%eps then pause,end
b=log(a);if norm(exp(b)-a)>10*%eps then pause,end
a=[1+%i 2.51;-3.4 4.52;5.8,-6.2];
if norm(exp(log(a))-a)>10*%eps then pause,end
if norm(exp(log(a+0))-a)>10*%eps then pause,end
b=log(a);if norm(exp(b)-a)>10*%eps then pause,end

if exp([])<>[] then pause,end
a=[];if exp(a)<>[] then pause,end
if log([])<>[] then pause,end
a=[];if log(a)<>[] then pause,end

//sin and cos
a=[1.2 2.51;-3.4 4.52;5.8,-6.2];
if norm(sin(a).^2+cos(a).^2-1)>10*%eps then pause,end
if norm(sin(a+0).^2+cos(a+0).^2-1)>10*%eps then pause,end
a=[1+%i 2.51;-3.4 4.52;5.8,-6.2];
if norm(sin(a).^2+cos(a).^2-1)>10*%eps then pause,end
if norm(sin(a+0).^2+cos(a+0).^2-1)>10*%eps then pause,end

if cos([])<>[] then pause,end
a=[];if cos(a)<>[] then pause,end
if sin([])<>[] then pause,end
a=[];if sin(a)<>[] then pause,end

//tan et atan
a=[1.2 2.51;-3.4 4.52;5.8,-6.2];
if norm(tan(atan(a))-a)>100*%eps then pause,end
if norm(tan(atan(a+0))-a)>100*%eps then pause,end
b=log(a);if norm(exp(b)-a)>100*%eps then pause,end
a=[1+%i 2.51;-3.4 4.52;5.8,-6.2];
if norm(tan(atan(a))-a)>100*%eps then pause,end
if norm(tan(atan(a+0))-a)>100*%eps then pause,end

if atan([])<>[] then pause,end
a=[];if atan(a)<>[] then pause,end

//TOBEDONE if atan([],[])<>[] then pause,end
//TOBEDONE a=[];if atan(a,[])<>[] then pause,end
//TOBEDONE a=[];if atan(a,a)<>[] then pause,end
//TOBEDONE a=[];if atan([],a)<>[] then pause,end

//expm
//TOBEDONE a=[0 2;0 0];
//TOBEDONE if norm(expm(a)-[1 2;0 1])>10*%eps then pause,end
//TOBEDONE a=[0 2*%i;0 0];
//TOBEDONE if norm(expm(a)-[1 2*%i;0 1])>10*%eps then pause,end
//TOBEDONE if expm([])<>[] then pause,end
//TOBEDONE a=[];if expm(a)<>[] then pause,end

//sqrt
a=[1.2 2.51;-3.4 4.52;5.8,-6.2];
if norm(sqrt(a).^2-a)>100*%eps then pause,end
if norm(sqrt(a+0).^2-a)>100*%eps then pause,end
a=[1+%i 2.51;-3.4 4.52;5.8,-6.2];
if norm(sqrt(a).^2-a)>100*%eps then pause,end
if norm(sqrt(a+0).^2-a)>100*%eps then pause,end

if sqrt([])<>[] then pause,end
a=[];if sqrt(a)<>[] then pause,end

//sum
a=[1 2;-3 4;5,-6];
if sum(a)<>3 then pause,end
if sum(a+0)<>3 then pause,end
if or(sum(a,'r')<>[3 0]) then pause,end
if or(sum(a+0,'r')<>[3 0]) then pause,end
n='r';
if or(sum(a,n)<>[3 0]) then pause,end
if or(sum(a+0,n)<>[3 0]) then pause,end
if or(sum(a,'r')<>[3 0]) then pause,end
if or(sum(a+0,'r')<>[3 0]) then pause,end
n='r';
if or(sum(a,n)<>[3 0]) then pause,end
if or(sum(a+0,n)<>[3 0]) then pause,end

if or(sum(a,'c')<>[3;1;-1]) then pause,end
if or(sum(a+0,'c')<>[3;1;-1]) then pause,end
n='c';
if or(sum(a,n)<>[3;1;-1]) then pause,end
if or(sum(a+0,n)<>[3;1;-1]) then pause,end
if or(sum(a,'c')<>[3;1;-1]) then pause,end
if or(sum(a+0,'c')<>[3;1;-1]) then pause,end
n='c';
if or(sum(a,n)<>[3;1;-1]) then pause,end
if or(sum(a+0,n)<>[3;1;-1]) then pause,end

a=[1 2;-3 4;5,-6]+0*%i;
if or(sum(a)<>3+0*%i) then pause,end
if or(sum(a+0)<>3+0*%i) then pause,end
if or(sum(a,'r')<>[3 0]+0*%i) then pause,end
if or(sum(a+0,'r')<>[3 0]+0*%i) then pause,end
n='r';
if or(sum(a,n)<>[3 0]+0*%i) then pause,end
if or(sum(a+0,n)<>[3 0]+0*%i) then pause,end
if or(sum(a,'r')<>[3 0]+0*%i) then pause,end
if or(sum(a+0,'r')<>[3 0]+0*%i) then pause,end
n='r';
if or(sum(a,n)<>[3 0]+0*%i) then pause,end
if or(sum(a+0,n)<>[3 0]+0*%i) then pause,end

if or(sum(a,'c')<>[3;1;-1]+0*%i) then pause,end
if or(sum(a+0,'c')<>[3;1;-1]+0*%i) then pause,end
n='c';
if or(sum(a,n)<>[3;1;-1]+0*%i) then pause,end
if or(sum(a+0,n)<>[3;1;-1]+0*%i) then pause,end
if or(sum(a,'c')<>[3;1;-1]+0*%i) then pause,end
if or(sum(a+0,'c')<>[3;1;-1]+0*%i) then pause,end
n='c';
if or(sum(a,n)<>[3;1;-1]+0*%i) then pause,end
if or(sum(a+0,n)<>[3;1;-1]+0*%i) then pause,end

// TEST DE FIN A FINIR 

a=[]
if sum(a)<>0 then pause,end
if sum([])<>0 then pause,end
if sum(a,'r')<>[] then pause,end
if sum([],'r')<>[] then pause,end
n='r';
if sum(a,n)<>[] then pause,end
if sum([],n)<>[] then pause,end
if sum(a,'r')<>[] then pause,end
if sum([],'r')<>[] then pause,end
n='r';
if sum(a,n)<>[] then pause,end
if sum([],n)<>[] then pause,end

if sum(a,'c')<>[] then pause,end
if sum([],'c')<>[] then pause,end
n='c';
if sum(a,n)<>[] then pause,end
if sum([],n)<>[] then pause,end
if sum(a,'c')<>[] then pause,end
if sum([],'c')<>[] then pause,end
n='c';
if sum(a,n)<>[] then pause,end
if sum([],n)<>[] then pause,end

// sum -sparse 
a=sparse([1 2;-3 4;5,-6]);
// les comparaisons sur les sparses n'existent pas encore 
// A FINIR 
if full(sum(a))<>3 then pause,end
if full(sum(a+0))<>3 then pause,end

if or(full(sum(a,'r'))<>[3 0]) then pause,end
if or(full(sum(a+0,'r'))<>[3 0]) then pause,end
n='r';
if or(full(sum(a,n))<>[3 0]) then pause,end
if or(full(sum(a+0,n))<>[3 0]) then pause,end
if or(full(sum(a,'r'))<>[3 0]) then pause,end
if or(full(sum(a+0,'r'))<>[3 0]) then pause,end
n='r';
if or(full(sum(a,n))<>[3 0]) then pause,end
if or(full(sum(a+0,n))<>[3 0]) then pause,end

if or(full(sum(a,'c'))<>[3;1;-1]) then pause,end
if or(full(sum(a+0,'c'))<>[3;1;-1]) then pause,end
n='c';
if or(full(sum(a,n))<>[3;1;-1]) then pause,end
if or(full(sum(a+0,n))<>[3;1;-1]) then pause,end
if or(full(sum(a,'c'))<>[3;1;-1]) then pause,end
if or(full(sum(a+0,'c'))<>[3;1;-1]) then pause,end
n='c';
if or(full(sum(a,n))<>[3;1;-1]) then pause,end
if or(full(sum(a+0,n))<>[3;1;-1]) then pause,end

a=sparse([1 2;-3 4;5,-6]+0*%i);
if or(full(sum(a))<>3+0*%i) then pause,end
if or(full(sum(a+0))<>3+0*%i) then pause,end
if or(full(sum(a,'r'))<>[3 0]+0*%i) then pause,end
if or(full(sum(a+0,'r'))<>[3 0]+0*%i) then pause,end
n='r';
if or(full(sum(a,n))<>[3 0]+0*%i) then pause,end
if or(full(sum(a+0,n))<>[3 0]+0*%i) then pause,end
if or(full(sum(a,'r'))<>[3 0]+0*%i) then pause,end
if or(full(sum(a+0,'r'))<>[3 0]+0*%i) then pause,end
n='r';
if or(full(sum(a,n))<>[3 0]+0*%i) then pause,end
if or(full(sum(a+0,n))<>[3 0]+0*%i) then pause,end

if or(full(sum(a,'c'))<>[3;1;-1]+0*%i) then pause,end
if or(full(sum(a+0,'c'))<>[3;1;-1]+0*%i) then pause,end
n='c';
if or(full(sum(a,n))<>[3;1;-1]+0*%i) then pause,end
if or(full(sum(a+0,n))<>[3;1;-1]+0*%i) then pause,end
if or(full(sum(a,'c'))<>[3;1;-1]+0*%i) then pause,end
if or(full(sum(a+0,'c'))<>[3;1;-1]+0*%i) then pause,end
n='c';
if or(full(sum(a,n))<>[3;1;-1]+0*%i) then pause,end
if or(full(sum(a+0,n))<>[3;1;-1]+0*%i) then pause,end

// TEST DE FIN A FINIR 

a=sparse([])
if full(sum(a))<>0 then pause,end
if full(sum([]))<>0 then pause,end
if full(sum(a,'r'))<>[] then pause,end
if full(sum([],'r'))<>[] then pause,end
n='r';
if full(sum(a,n))<>[] then pause,end
if full(sum([],n))<>[] then pause,end
if full(sum(a,'r'))<>[] then pause,end
if full(sum([],'r'))<>[] then pause,end
n='r';
if full(sum(a,n))<>[] then pause,end
if full(sum([],n))<>[] then pause,end

if full(sum(a,'c'))<>[] then pause,end
if full(sum([],'c'))<>[] then pause,end
n='c';
if full(sum(a,n))<>[] then pause,end
if full(sum([],n))<>[] then pause,end
if full(sum(a,'c'))<>[] then pause,end
if full(sum([],'c'))<>[] then pause,end
n='c';
if full(sum(a,n))<>[] then pause,end
if full(sum([],n))<>[] then pause,end

//prod
a=[1 2;-3 4;5,-6];
if prod(a)<>720 then pause,end
if prod(a+0)<>720 then pause,end
if prod(a,'r')<>[-15,-48] then pause,end
if or(prod(a+0,'r')<>[-15,-48]) then pause,end
n='r';
if or(prod(a,n)<>[-15 ,-48]) then pause,end
if or(prod(a+0,n)<>[-15, -48]) then pause,end
if or(prod(a,'r')<>[-15, -48]) then pause,end
if or(prod(a+0,'r')<>[-15 ,-48]) then pause,end
n='r';
if or(prod(a,n)<>[-15, -48]) then pause,end
if or(prod(a+0,n)<>[-15, -48]) then pause,end

if or(prod(a,'c')<>[2;-12;-30]) then pause,end
if or(prod(a+0,'c')<>[2;-12;-30]) then pause,end
n='c';
if or(prod(a,n)<>[2;-12;-30]) then pause,end
if or(prod(a+0,n)<>[2;-12;-30]) then pause,end
if or(prod(a,'c')<>[2;-12;-30]) then pause,end
if or(prod(a+0,'c')<>[2;-12;-30]) then pause,end
n='c';
if or(prod(a,n)<>[2;-12;-30]) then pause,end
if or(prod(a+0,n)<>[2;-12;-30]) then pause,end

a=[1 2;-3 4;5,-6]+0*%i;
if prod(a)<>720+0*%i then pause,end
if prod(a+0)<>720+0*%i then pause,end
if or(prod(a,'r')<>[-15, -48]+0*%i) then pause,end
if or(prod(a+0,'r')<>[-15, -48]+0*%i) then pause,end
n='r';
if or(prod(a,n)<>[-15, -48]+0*%i) then pause,end
if or(prod(a+0,n)<>[-15, -48]+0*%i) then pause,end
if or(prod(a,'r')<>[-15, -48]+0*%i) then pause,end
if or(prod(a+0,'r')<>[-15, -48]+0*%i) then pause,end
n='r';
if or(prod(a,n)<>[-15, -48]+0*%i) then pause,end
if or(prod(a+0,n)<>[-15, -48]+0*%i) then pause,end

if or(prod(a,'c')<>[2;-12;-30]+0*%i) then pause,end
if or(prod(a+0,'c')<>[2;-12;-30]+0*%i) then pause,end
n='c';
if or(prod(a,n)<>[2;-12;-30]+0*%i) then pause,end
if or(prod(a+0,n)<>[2;-12;-30]+0*%i) then pause,end
if or(prod(a,'c')<>[2;-12;-30]+0*%i) then pause,end
if or(prod(a+0,'c')<>[2;-12;-30]+0*%i) then pause,end
n='c';
if or(prod(a,n)<>[2;-12;-30]+0*%i) then pause,end
if or(prod(a+0,n)<>[2;-12;-30]+0*%i) then pause,end

a=[]
if prod(a)<>1 then pause,end
if prod([])<>1 then pause,end
if prod(a,'r')<>[] then pause,end
if prod([],'r')<>[] then pause,end
n='r';
if prod(a,n)<>[] then pause,end
if prod([],n)<>[] then pause,end
if prod(a,'r')<>[] then pause,end
if prod([],'r')<>[] then pause,end
n='r';
if prod(a,n)<>[] then pause,end
if prod([],n)<>[] then pause,end

if prod(a,'c')<>[] then pause,end
if prod([],'c')<>[] then pause,end
n='c';
if prod(a,n)<>[] then pause,end
if prod([],n)<>[] then pause,end
if prod(a,'c')<>[] then pause,end
if prod([],'c')<>[] then pause,end
n='c';
if prod(a,n)<>[] then pause,end
if prod([],n)<>[] then pause,end

// Prod sur les sparses ? 
// TOBEDONE 


//cumsum
a=[1 2;-3 4;5,-6];
if or(cumsum(a)<>[1,5;-2,9;3,3]) then pause,end
if or(cumsum(a+0)<> [1,5;-2,9;3,3]) then pause,end
if or(cumsum(a,'r')<>[1,2;-2,6;3 0]) then pause,end
if or(cumsum(a+0,'r')<>[1,2;-2,6;3 0]) then pause,end
n='r';
if or(cumsum(a,n)<>[1,2;-2,6;3 0]) then pause,end
if or(cumsum(a+0,n)<>[1,2;-2,6;3 0]) then pause,end
if or(cumsum(a,'r')<>[1,2;-2,6;3 0]) then pause,end
if or(cumsum(a+0,'r')<>[1,2;-2,6;3 0]) then pause,end
n='r';
if or(cumsum(a,n)<>[1,2;-2,6;3 0]) then pause,end
if or(cumsum(a+0,n)<>[1,2;-2,6;3 0]) then pause,end

if or(cumsum(a,'c')<>[1,3;-3,1;5,-1]) then pause,end
if or(cumsum(a+0,'c')<>[1,3;-3,1;5,-1]) then pause,end
n='c';
if or(cumsum(a,n)<>[1,3;-3,1;5,-1]) then pause,end
if or(cumsum(a+0,n)<>[1,3;-3,1;5,-1]) then pause,end
if or(cumsum(a,'c')<>[1,3;-3,1;5,-1]) then pause,end
if or(cumsum(a+0,'c')<>[1,3;-3,1;5,-1]) then pause,end
n='c';
if or(cumsum(a,n)<>[1,3;-3,1;5,-1]) then pause,end
if or(cumsum(a+0,n)<>[1,3;-3,1;5,-1]) then pause,end

a=[1 2;-3 4;5,-6]+0*%i;
if cumsum(a)<>3+0*%i then pause,end
if cumsum(a+0)<>3+0*%i then pause,end
if or(cumsum(a,'r')<>[1,2;-2,6;3 0]+0*%i) then pause,end
if or(cumsum(a+0,'r')<>[1,2;-2,6;3 0]+0*%i) then pause,end
n='r';
if or(cumsum(a,n)<>[1,2;-2,6;3 0]+0*%i) then pause,end
if or(cumsum(a+0,n)<>[1,2;-2,6;3 0]+0*%i) then pause,end
if or(cumsum(a,'r')<>[1,2;-2,6;3 0]+0*%i) then pause,end
if or(cumsum(a+0,'r')<>[1,2;-2,6;3 0]+0*%i) then pause,end
n='r';
if or(cumsum(a,n)<>[1,2;-2,6;3 0]+0*%i) then pause,end
if or(cumsum(a+0,n)<>[1,2;-2,6;3 0]+0*%i) then pause,end

if or(cumsum(a,'c')<>[1,3;-3,1;5,-1]+0*%i) then pause,end
if or(cumsum(a+0,'c')<>[1,3;-3,1;5,-1]+0*%i) then pause,end
n='c';
if or(cumsum(a,n)<>[1,3;-3,1;5,-1]+0*%i) then pause,end
if or(cumsum(a+0,n)<>[1,3;-3,1;5,-1]+0*%i) then pause,end
if or(cumsum(a,'c')<>[1,3;-3,1;5,-1]+0*%i) then pause,end
if or(cumsum(a+0,'c')<>[1,3;-3,1;5,-1]+0*%i) then pause,end
n='c';
if or(cumsum(a,n)<>[1,3;-3,1;5,-1]+0*%i) then pause,end
if or(cumsum(a+0,n)<>[1,3;-3,1;5,-1]+0*%i) then pause,end

a=[];
if cumsum(a)<>[] then pause,end
if cumsum([])<>[] then pause,end
if cumsum(a,'r')<>[] then pause,end
if cumsum([],'r')<>[] then pause,end
n='r';
if cumsum(a,n)<>[] then pause,end
if cumsum([],n)<>[] then pause,end
if cumsum(a,'r')<>[] then pause,end
if cumsum([],'r')<>[] then pause,end
n='r';
if cumsum(a,n)<>[] then pause,end
if cumsum([],n)<>[] then pause,end

if cumsum(a,'c')<>[] then pause,end
if cumsum([],'c')<>[] then pause,end
n='c';
if cumsum(a,n)<>[] then pause,end
if cumsum([],n)<>[] then pause,end
if cumsum(a,'c')<>[] then pause,end
if cumsum([],'c')<>[] then pause,end
n='c';
if cumsum(a,n)<>[] then pause,end
if cumsum([],n)<>[] then pause,end


a=sparse([1 2;-3 4;5,-6]);
//if cumsum(a)<> [1,5;-2,9;3,3] then pause,end
//if cumsum(a+0*a)<> [1,5;-2,9;3,3] then pause,end
a=sparse([1 2;-3 4;5,-6]+0*%i);
//if cumsum(a)<>3+0*%i then pause,end
//if cumsum(a+0*a)<>3+0*%i then pause,end




//cumprod
a=[1 2;-3 4;5,-6];
if or(cumprod(a)<>[1, -30;-3, -120;-15 720]) then pause,end
if or(cumprod(a+0)<> [1, -30;-3, -120;-15 720]) then pause,end
if or(cumprod(a,'r')<>[1 2;-3, 8;-15 ,-48]) then pause,end
if or(cumprod(a+0,'r')<>[1 2;-3, 8;-15, -48]) then pause,end
n='r';
if or(cumprod(a,n)<>[1 2;-3 8;-15, -48]) then pause,end
if or(cumprod(a+0,n)<>[1 2;-3 8;-15 ,-48]) then pause,end
if or(cumprod(a,'r')<>[1 2;-3 8;-15 ,-48]) then pause,end
if or(cumprod(a+0,'r')<>[1 2;-3 8;-15, -48]) then pause,end
n='r';
if or(cumprod(a,n)<>[1 2;-3 8;-15, -48]) then pause,end
if or(cumprod(a+0,n)<>[1 2;-3 8;-15, -48]) then pause,end

if or(cumprod(a,'c')<>[1 2;-3 ,-12;5, -30]) then pause,end
if or(cumprod(a+0,'c')<>[1 2;-3, -12;5, -30]) then pause,end
n='c';
if or(cumprod(a,n)<>[1 2;-3, -12;5, -30]) then pause,end
if or(cumprod(a+0,n)<>[1 2;-3 ,-12;5, -30]) then pause,end
if or(cumprod(a,'c')<>[1 2;-3, -12;5, -30]) then pause,end
if or(cumprod(a+0,'c')<>[1 2;-3, -12;5, -30]) then pause,end
n='c';
if or(cumprod(a,n)<>[1 2;-3, -12;5 ,-30]) then pause,end
if or(cumprod(a+0,n)<>[1 2;-3, -12;5 ,-30]) then pause,end

a=[1 2;-3 4;5,-6]+0*%i;
if cumprod(a)<>[1, -30;-3, -120;-15 720]+0*%i then pause,end
if cumprod(a+0)<>[1, -30;-3, -120;-15 720]+0*%i then pause,end
if or(cumprod(a,'r')<>[1 2;-3 8;-15, -48]+0*%i) then pause,end
if or(cumprod(a+0,'r')<>[1 2;-3 8;-15, -48]+0*%i) then pause,end
n='r';
if or(cumprod(a,n)<>[1 2;-3 8;-15, -48]+0*%i) then pause,end
if or(cumprod(a+0,n)<>[1 2;-3 8;-15, -48]+0*%i) then pause,end
if or(cumprod(a,'r')<>[1 2;-3 8;-15, -48]+0*%i) then pause,end
if or(cumprod(a+0,'r')<>[1 2;-3 8;-15, -48]+0*%i) then pause,end
n='r';
if or(cumprod(a,n)<>[1 2;-3 8;-15, -48]+0*%i) then pause,end
if or(cumprod(a+0,n)<>[1 2;-3 8;-15 ,-48]+0*%i) then pause,end

if or(cumprod(a,'c')<>[1 2;-3, -12;5, -30]+0*%i) then pause,end
if or(cumprod(a+0,'c')<>[1 2;-3, -12;5, -30]+0*%i) then pause,end
n='c';
if or(cumprod(a,n)<>[1 2;-3, -12;5, -30]+0*%i) then pause,end
if or(cumprod(a+0,n)<>[1 2;-3, -12;5, -30]+0*%i) then pause,end
if or(cumprod(a,'c')<>[1 2;-3, -12;5 ,-30]+0*%i) then pause,end
if or(cumprod(a+0,'c')<>[1 2;-3, -12;5, -30]+0*%i) then pause,end
n='c';
if or(cumprod(a,n)<>[1 2;-3, -12;5, -30]+0*%i) then pause,end
if or(cumprod(a+0,n)<>[1 2;-3, -12;5 ,-30]+0*%i) then pause,end

a=[];
if cumprod(a)<>[] then pause,end
if cumprod([])<>[] then pause,end
if cumprod(a,'r')<>[] then pause,end
if cumprod([],'r')<>[] then pause,end
n='r';
if cumprod(a,n)<>[] then pause,end
if cumprod([],n)<>[] then pause,end
if cumprod(a,'r')<>[] then pause,end
if cumprod([],'r')<>[] then pause,end
n='r';
if cumprod(a,n)<>[] then pause,end
if cumprod([],n)<>[] then pause,end

if cumprod(a,'c')<>[] then pause,end
if cumprod([],'c')<>[] then pause,end
n='c';
if cumprod(a,n)<>[] then pause,end
if cumprod([],n)<>[] then pause,end
if cumprod(a,'c')<>[] then pause,end
if cumprod([],'c')<>[] then pause,end
n='c';
if cumprod(a,n)<>[] then pause,end
if cumprod([],n)<>[] then pause,end


a=sparse([1 2;-3 4;5,-6]);
//if cumprod(a)<> [1 -30;-3 -120;-15 720] then pause,end
//if cumprod(a+0*a)<> [1 -30;-3 -120;-15 720] then pause,end
a=sparse([1 2;-3 4;5,-6]+0*%i);
//if cumprod(a)<>[1 -30;-3 -120;-15 720]+0*%i then pause,end
//if cumprod(a+0*a)<>[1 -30;-3 -120;-15 720]+0*%i then pause,end

//max
a=[1 2;-3 4;5,-6];
if or(max(a)<>5) then pause,end
if or(max(a+0)<>5) then pause,end
if or(max(a,'r')<>[5 4]) then pause,end
if or(max(a+0,'r')<>[5 4]) then pause,end
n='r';
if or(max(a,n)<>[5 4]) then pause,end
if or(max(a+0,n)<>[5 4]) then pause,end

if or(max(a,'c')<>[2;4;5]) then pause,end
if or(max(a+0,'c')<>[2;4;5]) then pause,end
n='c';
if or(max(a,n)<>[2;4;5]) then pause,end
if or(max(a+0,n)<>[2;4;5]) then pause,end

//a=[1 2;-3 4;5,-6]+0*%i;
//if max(a)<>[1 -30;-3 -120;-15 720]+0*%i then pause,end
//if max(a+0)<>[1 -30;-3 -120;-15 720]+0*%i then pause,end
//if or(max(a,'r')<>[5 4]+0*%i) then pause,end
//if or(max(a+0,'r')<>[5 4]+0*%i) then pause,end
//n='r';
//if or(max(a,n)<>[5 4]+0*%i) then pause,end
//if or(max(a+0,n)<>[5 4]+0*%i) then pause,end

//if or(max(a,'c')<>[2;4;5]+0*%i) then pause,end
//if or(max(a+0,'c')<>[2;4;5]+0*%i) then pause,end
//n='c';
//if or(max(a,n)<>[2;4;5]+0*%i) then pause,end
//if or(max(a+0,n)<>[2;4;5]+0*%i) then pause,end

a=[];
if max(a)<>[] then pause,end
if max([])<>[] then pause,end

if max(a,'r')<>[] then pause,end
if max([],'r')<>[] then pause,end
n='r';
if max(a,n)<>[] then pause,end
if max([],n)<>[] then pause,end

if max(a,'c')<>[] then pause,end
if max([],'c')<>[] then pause,end
n='c';
if max(a,n)<>[] then pause,end
if max([],n)<>[] then pause,end

a=sparse([1 2;-3 4;5,-6]);
//if max(a)<>5 then pause,end
//if max(a+0*a)<>5 then pause,end
a=sparse([1 2;-3 4;5,-6]+0*%i);
//if max(a)<>[1 -30;-3 -120;-15 720]+0*%i then pause,end
//if max(a+0*a)<>[1 -30;-3 -120;-15 720]+0*%i then pause,end

//min
a=[1 2;-3 4;5,-6];
if or(min(a)<>-6) then pause,end
if or(min(a+0)<>-6) then pause,end
if or(min(a,'r')<>[-3,-6]) then pause,end
if or(min(a+0,'r')<>[-3,-6]) then pause,end
n='r';
if or(min(a,n)<>[-3,-6]) then pause,end
if or(min(a+0,n)<>[-3,-6]) then pause,end

if or(min(a,'c')<>[1;-3;-6]) then pause,end
if or(min(a+0,'c')<>[1;-3;-6]) then pause,end
n='c';
if or(min(a,n)<>[1;-3;-6]) then pause,end
if or(min(a+0,n)<>[1;-3;-6]) then pause,end

//a=[1 2;-3 4;5,-6]+0*%i;
//if min(a)<>[1 -30;-3 -120;-15 720]+0*%i then pause,end
//if min(a+0)<>[1 -30;-3 -120;-15 720]+0*%i then pause,end
//if or(min(a,'r')<>[-3,-6]+0*%i) then pause,end
//if or(min(a+0,'r')<>[-3,-6]+0*%i) then pause,end
//n='r';
//if or(min(a,n)<>[-3,-6]+0*%i) then pause,end
//if or(min(a+0,n)<>[-3,-6]+0*%i) then pause,end

//if or(min(a,'c')<>[1;-3;-6]+0*%i) then pause,end
//if or(min(a+0,'c')<>[1;-3;-6]+0*%i) then pause,end
//n='c';
//if or(min(a,n)<>[1;-3;-6]+0*%i) then pause,end
//if or(min(a+0,n)<>[1;-3;-6]+0*%i) then pause,end

a=[];
if min(a)<>[] then pause,end
if min([])<>[] then pause,end

if min(a,'r')<>[] then pause,end
if min([],'r')<>[] then pause,end
n='r';
if min(a,n)<>[] then pause,end
if min([],n)<>[] then pause,end

if min(a,'c')<>[] then pause,end
if min([],'c')<>[] then pause,end
n='c';
if min(a,n)<>[] then pause,end
if min([],n)<>[] then pause,end

a=sparse([1 2;-3 4;5,-6]);
//if min(a)<>-6 then pause,end
//if min(a+0*a)<>-6 then pause,end
a=sparse([1 2;-3 4;5,-6]+0*%i);
//if min(a)<>[1 -30;-3 -120;-15 720]+0*%i then pause,end
//if min(a+0*a)<>[1 -30;-3 -120;-15 720]+0*%i then pause,end

//sort
a=[5 1 3 2 4]
if or(sort(a)<>[5 4 3 2 1]) then pause,end
if or(sort(a+0)<>[5 4 3 2 1]) then pause,end
[s,k]=sort(a);
if or(k<>[1 5 3 4 2]) then pause,end
if or(s<>[5 4 3 2 1]) then pause,end
[s,k]=sort(a+0);
if or(k<>[1 5 3 4 2]) then pause,end
if or(s<>[5 4 3 2 1]) then pause,end

a=string([5 1 3 2 4])
//if or(sort(a)<>string(1:5)) then pause,end
//if or(sort(string([5 1 3 2 4]))<>string(1:5)) then pause,end
[s,k]=sort(a);
//if or(k<>[2 4 3 5 1]) then pause,end
//if or(s<>string(1:5)) then pause,end
[s,k]=sort(string([5 1 3 2 4]));
//if or(k<>[2 4 3 5 1]) then pause,end
//if or(s<>string(1:5)) then pause,end

a=[]
if sort(a)<>[] then pause,end
[s,k]=sort(a);if s<>[]|k<>[] then pause,end

if sort([])<>[] then pause,end
[s,k]=sort([]);if s<>[]|k<>[] then pause,end

//kron
a=[1 2];b=[3;4];
if or(kron(a,b)<>[3 6;4 8]) then pause,end
if or(kron(a+0,b)<>[3 6;4 8]) then pause,end
if or(kron(a,b+0)<>[3 6;4 8]) then pause,end
if or(kron(a+0,b+0)<>[3 6;4 8]) then pause,end
if kron([],b)<>[] then pause,end
if kron([],b+0)<>[] then pause,end
a=[];
if kron(a,b)<>[] then pause,end
if kron(a,b+0)<>[] then pause,end
a=[1 2];b=[]
if kron(a,b)<>[] then pause,end
if kron(a+0,b)<>[] then pause,end
if kron(a,[])<>[] then pause,end
if kron(a+0,[])<>[] then pause,end
a=[];b=[];
if kron(a,b)<>[] then pause,end
if kron(a,[])<>[] then pause,end
if kron([],b)<>[] then pause,end
if kron([],[])<>[] then pause,end

//matrix
a=[1 2 3 4 5 6];
n=1;m=6;
if or(matrix(a,1,6)<>a) then pause,end
if or(matrix(a,n,6)<>a) then pause,end
if or(matrix(a,1,m)<>a) then pause,end
if or(matrix(a,n,m)<>a) then pause,end
if or(matrix(a+0,1,6)<>a) then pause,end
if or(matrix(a+0,n,6)<>a) then pause,end
if or(matrix(a+0,1,m)<>a) then pause,end
if or(matrix(a+0,n,m)<>a) then pause,end

n=3;m=2; b=[1 4;2 5;3 6];
if or(matrix(a,3,2)<>b) then pause,end
if or(matrix(a,n,2)<>b) then pause,end
if or(matrix(a,3,m)<>b) then pause,end
if or(matrix(a,n,m)<>b) then pause,end
if or(matrix(a+0,3,2)<>b) then pause,end
if or(matrix(a+0,n,2)<>b) then pause,end
if or(matrix(a+0,3,m)<>b) then pause,end
if or(matrix(a+0,n,m)<>b) then pause,end

a=[1+%i 2 3 4 5 6];
n=1;m=6;
if or(matrix(a,1,6)<>a) then pause,end
if or(matrix(a,n,6)<>a) then pause,end
if or(matrix(a,1,m)<>a) then pause,end
if or(matrix(a,n,m)<>a) then pause,end
if or(matrix(a+0,1,6)<>a) then pause,end
if or(matrix(a+0,n,6)<>a) then pause,end
if or(matrix(a+0,1,m)<>a) then pause,end
if or(matrix(a+0,n,m)<>a) then pause,end

n=3;m=2; b=[1+%i 4;2 5;3 6];
if or(matrix(a,3,2)<>b) then pause,end
if or(matrix(a,n,2)<>b) then pause,end
if or(matrix(a,3,m)<>b) then pause,end
if or(matrix(a,n,m)<>b) then pause,end
if or(matrix(a+0,3,2)<>b) then pause,end
if or(matrix(a+0,n,2)<>b) then pause,end
if or(matrix(a+0,3,m)<>b) then pause,end
if or(matrix(a+0,n,m)<>b) then pause,end

a=string([1 2 3 4 5 6]);n=1;m=6;
//TOBEDONE if or(matrix(a,1,6)<>a) then pause,end
//TOBEDONE if or(matrix(a,n,6)<>a) then pause,end
//TOBEDONE if or(matrix(a,1,m)<>a) then pause,end
//TOBEDONE if or(matrix(a,n,m)<>a) then pause,end
//TOBEDONE if or(matrix(a+a,1,6)<>a+a) then pause,end
//TOBEDONE if or(matrix(a+a,n,6)<>a+a) then pause,end
//TOBEDONE if or(matrix(a+a,1,m)<>a+a) then pause,end
//TOBEDONE if or(matrix(a+a,n,m)<>a+a) then pause,end

n=3;m=2; b=string([1 4;2 5;3 6]);
//TOBEDONE if or(matrix(a,3,2)<>b) then pause,end
//TOBEDONE if or(matrix(a,n,2)<>b) then pause,end
//TOBEDONE if or(matrix(a,3,m)<>b) then pause,end
//TOBEDONE if or(matrix(a,n,m)<>b) then pause,end
//TOBEDONE if or(matrix(a+a,3,2)<>b+b) then pause,end
//TOBEDONE if or(matrix(a+a,n,2)<>b+b) then pause,end
//TOBEDONE if or(matrix(a+a,3,m)<>b+b) then pause,end
//TOBEDONE if or(matrix(a+a,n,m)<>b+b) then pause,end

//TOBEDONE a=[1 2 3 4 5 6]+%s;
//TOBEDONE n=1;m=6;
//TOBEDONE if or(matrix(a,1,6)<>a) then pause,end
//TOBEDONE if or(matrix(a,n,6)<>a) then pause,end
//TOBEDONE if or(matrix(a,1,m)<>a) then pause,end
//TOBEDONE if or(matrix(a,n,m)<>a) then pause,end
//TOBEDONE if or(matrix(a+0,1,6)<>a) then pause,end
//TOBEDONE if or(matrix(a+0,n,6)<>a) then pause,end
//TOBEDONE if or(matrix(a+0,1,m)<>a) then pause,end
//TOBEDONE if or(matrix(a+0,n,m)<>a) then pause,end

//TOBEDONE n=3;m=2; b=[1 4;2 5;3 6]+%s;
//TOBEDONE if or(matrix(a,3,2)<>b) then pause,end
//TOBEDONE if or(matrix(a,n,2)<>b) then pause,end
//TOBEDONE if or(matrix(a,3,m)<>b) then pause,end
//TOBEDONE if or(matrix(a,n,m)<>b) then pause,end
//TOBEDONE if or(matrix(a+0,3,2)<>b) then pause,end
//TOBEDONE if or(matrix(a+0,n,2)<>b) then pause,end
//TOBEDONE if or(matrix(a+0,3,m)<>b) then pause,end
//TOBEDONE if or(matrix(a+0,n,m)<>b) then pause,end
//TOBEDONE 
//TOBEDONE a=[1+%i 2 3 4 5 6]+%s;
//TOBEDONE n=1;m=6;
//TOBEDONE if or(matrix(a,1,6)<>a) then pause,end
//TOBEDONE if or(matrix(a,n,6)<>a) then pause,end
//TOBEDONE if or(matrix(a,1,m)<>a) then pause,end
//TOBEDONE if or(matrix(a,n,m)<>a) then pause,end
//TOBEDONE if or(matrix(a+0,1,6)<>a) then pause,end
//TOBEDONE if or(matrix(a+0,n,6)<>a) then pause,end
//TOBEDONE if or(matrix(a+0,1,m)<>a) then pause,end
//TOBEDONE if or(matrix(a+0,n,m)<>a) then pause,end
//TOBEDONE 
//TOBEDONE n=3;m=2; b=[1+%i 4;2 5;3 6]+%s;
//TOBEDONE if or(matrix(a,3,2)<>b) then pause,end
//TOBEDONE if or(matrix(a,n,2)<>b) then pause,end
//TOBEDONE if or(matrix(a,3,m)<>b) then pause,end
//TOBEDONE if or(matrix(a,n,m)<>b) then pause,end
//TOBEDONE if or(matrix(a+0,3,2)<>b) then pause,end
//TOBEDONE if or(matrix(a+0,n,2)<>b) then pause,end
//TOBEDONE if or(matrix(a+0,3,m)<>b) then pause,end
//TOBEDONE if or(matrix(a+0,n,m)<>b) then pause,end

//clean
a=[1 1.d-12 1.d-5 2d8];
b=[1 0 0 2d8];
if or(clean(a)<>b) then pause,end
if or(clean(a+0)<>b) then pause,end
epsa=1.d-10;
if or(clean(a,epsa)<>b) then pause,end
if or(clean(a+0,epsa)<>b) then pause,end
if or(clean(a,epsa+0)<>b) then pause,end
if or(clean(a+0,epsa+0)<>b) then pause,end
epsr=1.d-5;b=[0 0 0 2d8];
if or(clean(a,epsa,epsr)<>b) then pause,end
if or(clean(a+0,epsa,epsr)<>b) then pause,end
if or(clean(a,epsa+0,epsr)<>b) then pause,end
if or(clean(a+0,epsa,epsr+0)<>b) then pause,end
if or(clean(a,epsa+0,epsr)<>b) then pause,end
if or(clean(a+0,epsa,epsr+0)<>b) then pause,end
if or(clean(a,epsa+0,epsr)<>b) then pause,end
if or(clean(a+0,epsa+0,epsr+0)<>b) then pause,end

a=[1+%i 1.d-12 1.d-5 2d8];
b=[1+%i 0 0 2d8];
if or(clean(a)<>b) then pause,end
if or(clean(a+0)<>b) then pause,end
epsa=1.d-10;
if or(clean(a,epsa)<>b) then pause,end
if or(clean(a+0,epsa)<>b) then pause,end
if or(clean(a,epsa+0)<>b) then pause,end
if or(clean(a+0,epsa+0)<>b) then pause,end
epsr=1.d-5;b=[0+0*%i 0 0 2d8];
if or(clean(a,epsa,epsr)<>b) then pause,end
if or(clean(a+0,epsa,epsr)<>b) then pause,end
if or(clean(a,epsa+0,epsr)<>b) then pause,end
if or(clean(a+0,epsa,epsr+0)<>b) then pause,end
if or(clean(a,epsa+0,epsr)<>b) then pause,end
if or(clean(a+0,epsa,epsr+0)<>b) then pause,end
if or(clean(a,epsa+0,epsr)<>b) then pause,end
if or(clean(a+0,epsa+0,epsr+0)<>b) then pause,end
