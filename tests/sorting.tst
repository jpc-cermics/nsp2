// -*- Mode: scilab -*- 

function [t,ok]= test(A,meth)
  ok=%t;
  timer();[a,b]=gsort(A,meth,'i');t=timer()
  if A(b)<> a then ok=%f;end
  if or(a(2:$)-a(1:$-1)<0) then ok=%f;end
  if ok == %f then pause,end;
endfunction;

function [t,ok]= testd(A,meth)
  ok=%t;
  timer();[a,b]=gsort(A,meth,'d');t=timer()
  if A(b)<> a then ok=%f;end
  if or(a(2:$)-a(1:$-1)>0) then ok=%f;end
  if ok == %f then pause,end;
endfunction;

A = rand(10000,1);
t1 = [test(A,'g'), test(A,'gb'), test(A,'gd'),test(A,'gm'),test(A,'gs')];
t2 = [testd(A,'g'), testd(A,'gb'), testd(A,'gd'),testd(A,'gm'),testd(A,'gs')];

A = grand(10000,1,"uin",1,10);
t3 = [test(A,'g'), test(A,'gb'), test(A,'gd'),test(A,'gm'),test(A,'gs')];

function x=vec_test_sort16(n)
   if modulo(n,2) == 0 then
      im = n/2;
      x = [1+grand(im,1,"def") ; 0 ; -1-grand(im-1,1,"def")]';
   else
      im = (n+1)/2
      x = [1+grand(im-1,1,"def") ; 0 ; -1-grand(im-1,1,"def")]';
   end
endfunction

A = vec_test_sort16(40000);
t4 = [test(A,'g'), test(A,'gb'), test(A,'gd'),test(A,'gm'),test(A,'gs')];

A = vec_test_sort16(10000);
t5 = [testd(A,'g'), testd(A,'gb'), testd(A,'gd'),testd(A,'gm'),testd(A,'gs')];
t6 = [test(A,'g'), test(A,'gb'), test(A,'gd'),test(A,'gm'),test(A,'gs')];



