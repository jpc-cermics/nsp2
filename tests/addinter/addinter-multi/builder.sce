// This is the builder.sce 
// must be run from this directory 

// [1] generate Path.incl 

ilib_path_incl()
  
// [2] build lib/libutil.xx 

chdir('lib');
exec builder.sce 
chdir('../');

// [3] the part devoted to shared lib generation 

ilib_name  = 'libmulti' 		// interface library name 
files = ['f4.o']; // objects files (but do not give mexfiles here)

// other libs needed for linking (must be shared library names)

libs  = ['lib/libutil']; 				

// table of (scilab_name,interface-name or mexfile-name, type) 

table1 =['f1',		'fmex1',	'cmex';
	 'f4',		'int_f4',	'csci'];

table2 =['f2',		'fmex2',	'cmex';
	 'f3',		'foof',		'Fmex'];
	 
table = list(table1,table2);

ilib_build(ilib_name,table,files,libs)


