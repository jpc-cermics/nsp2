// This is the builder.sce 
// must be run from this directory 

// [1] generate Path.incl 

ilib_path_incl()

// [2] building lib/libutil.xx 

chdir('lib');
exec builder.sce 
chdir('../')

// [3] the part devoted to shared lib generation 

ilib_name  = 'libmex' 		// interface library name 

// objects files (but do not give mexfiles here)

files = ['f4.o'];

// other libs needed for linking (must be shared library names)

libs  = ['lib/libutil']; 				

// table of (scilab_name,interface-name or mexfile-name, type) 

table =['f1',		'fmex1',	'cmex';
	'f2',		'fmex2',	'cmex';
	'f3',		'foof',		'Fmex';
	'f4',		'int_f4'        'csci'];

// do not modify below 
// ----------------------------------------------
ilib_build(ilib_name,table,files,libs)
