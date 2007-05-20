// test global frame 

clearglobal z;

function y=f();global z;z=rand(rand(1,1)*5+1,4);y=z;endfunction;
y=f();
if exists('z','local') then pause;end 
if ~exists('z','global') then pause;end 

global z;
if ~exists('z','local') then pause;end 
if ~z.equal[y] then pause;end 

clearglobal z;
if exists('z','global') then pause;end 


