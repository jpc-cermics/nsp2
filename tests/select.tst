// -*- Mode: scilab -*- 

function y=f(x)
  z=56
  global w
  w=34
  select x
   case z then y=1;
   case w then y=1.2;
   case 'foo' then y=2;
   case {'a','b'} then y=3;
  else
    y=6;
  end	
endfunction

x=34;
if f(x)<> 1.2 then pause;end 
if f(34)<> 1.2 then pause;end 

x=56;
if f(x)<> 1 then pause;end 
if f(56)<> 1 then pause;end 

if f('foo')<> 2 then pause;end 
x='foo';
if f(x)<> 2 then pause;end 

if f('a')<> 3 then pause;end 
x='a';
if f(x)<> 3 then pause;end 

if f('b')<> 3 then pause;end 
x='b';
if f(x)<> 3 then pause;end 


if f(560)<> 6 then pause;end 

