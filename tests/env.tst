// -*- Mode: scilab -*- 
// an object of Classa 
// inserting variables from a hash 
// table in an environment.

function h=my_env() 
  h=hash_create(a=67,b=67); 
  h.scilab= %f; 
  h.foo = 8*ones(4,5)
  function y=poo(x); y=sin(x);endfunction 
  h.poo = poo;
endfunction;

h=my_env() ;

insert_env(h);

if foo<>8*ones(4,5) then pause;end 






