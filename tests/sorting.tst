// -*- Mode: scilab -*- 

function [t,ok]= test(A,type)
  ok=%t;
  timer();[a,b]=new_sort(A,'g','i');t=timer()
  if A(b)<> a then ok=%f;end
  if or(a(2:$)-a(1:$-1)<0) then ok=%f;end
  if ok == %f then pause,end;
endfunction

A = rand(1e6,1);
t = [test(A,'g'), test(A,'gb'), test(A,'gd'),test(A,'gm'),test(A,'gs')]

A = grand(1e6,1,"uin",1,10);
t = [test(A,'g'), test(A,'gb'), test(A,'gd'),test(A,'gm'),test(A,'gs')]

function x=vec_test_sort16(n)
   if modulo(n,2) == 0 then
      im = n/2;
      x = [1+grand(im,1,"def") ; 0 ; -1-grand(im-1,1,"def")]';
   else
      im = (n+1)/2
      x = [1+grand(im-1,1,"def") ; 0 ; -1-grand(im-1,1,"def")]';
   end
endfunction

A = vec_test_sort16(400000);
t = [test(A,'g'), test(A,'gb'), test(A,'gd'),test(A,'gm'),test(A,'gs')]


