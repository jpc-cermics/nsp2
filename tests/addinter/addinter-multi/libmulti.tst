// load the shared library 

exec loader.sce ;

A=f1(89);
if A<>89+2 then pause,end
A=f2(89);
if A<>89+2 then pause,end
A=f3(89);
if A<>89 then pause,end
A=f4(89);
if A<>89+2 then pause,end






