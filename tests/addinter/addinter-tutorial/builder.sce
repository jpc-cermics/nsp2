// This is the builder.sce 
// must be run from this directory 

ilib_name  = 'libtutorial' 		// interface library name 
files = ['first.o','second.o']     // objects files 
					// 
libs  = [] 				// other libs needed for linking
table = [ 'first', 'int_first';		// table of (scilab_name,interface-name) 
          'second','int_second'];	// for fortran coded interface use 'C2F(name)'

// do not modify below 
// ----------------------------------------------
ilib_build(ilib_name,table,files,libs)

