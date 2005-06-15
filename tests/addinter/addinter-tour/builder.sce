// builder file used to build shared library 
// must be run from this directory 


ilib_name  = 'libtour' 	   // interface library name 
files = ['Interf-tour.o']  // objects files 
libs  = [] 		   // other libs needed for linking
table = ['ex1c','int_ex1c';
	 'ex2c','int_ex2c';
	 'ex3c_1','int_ex_3c_1';
	 'ex4c_1','int_ex_4c_1';
	 'ex4c_2','int_ex_4c_2';
	 'ex4c_3','int_ex_4c_3';
	 'ex4c_4','int_ex_4c_4';
	 'ex5c_1','int_ex_5c_1';
	 'ex5c_2','int_ex_5c_2';
	 'ex6c_1','int_ex_6c_1';
	 'ex7c_1','int_ex_7c_1';
	 'ex18c_1','int_ex_18c_1'];


ilib_build(ilib_name,table,files,libs);


