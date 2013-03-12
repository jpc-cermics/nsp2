// behaviour of exec with break and return and continue;

// propagate a break;
// -----------------

function y=f2()
  for i=1:5 
    exec(g2)
  end
endfunction

function g2()
  y=i;
  if i==4 then break;end 
endfunction

// the break is propagated when using exec(g2) without 
if f2() <> 4 then pause;end 

// propagate a return 
// ------------------

function y=f1()
  exec(g1);
  y=5;
endfunction

function y=g1()
  y=3;
  return;
endfunction

if f1()<>3 then pause;end 

// propagate a continue 
// ------------------

function y=f3()
  y=0;
  for i=1:5 
    exec(g3)
  end
endfunction

function g3()
  if mod(i,2)==0 then continue;end 
  y = y+ i;
endfunction

// the continue is propagated when using exec(g3) without 
if f3() <> 1+3+5 then pause;end 


