// Demo file for ext1c example 

// builder code for ext1c.c 
link_name = [] ;    	// functions to be added to the call table 
			// the first entry point gives the name of the library 
			// or the library name can be given by libname= 
flag  = "c";			// ext1c is a C function 
files = ['foo.o';'bar.o' ];   	// objects files for ext1c 
libs  = [];			// other libs needed for linking 

libname='util';

// the next call generates files (Makelib,loader.sce) used
// for compiling and loading ext1c and performs the compilation

ilib_for_link(link_name,files,libs,flag,libname=libname);






