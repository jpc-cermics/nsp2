prefix=NSP_DIRECTORY
exec_prefix=${prefix}/bin
libdir=${prefix}/libs
includedir=${prefix}/include

nsp_dir=NSP_DIRECTORY

Name: Nsp
Description: 
Version: 1.0
Libs: -L${libdir} -lnsp
Cflags: -I${includedir}/nsp
