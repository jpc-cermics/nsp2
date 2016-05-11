dnl -*- mode: autoconf -*-
dnl 
dnl Author: J.Ph Chancelier 
dnl Copyright (C) 2013-2013 Enpc 
dnl
dnl Distributed under the GPL(GNU Public License):
dnl This program is free software; you can redistribute it and/or modify
dnl it under the terms of the GNU General Public License as published by
dnl the Free Software Foundation; either version 2 of the License, or
dnl (at your option) any later version.
dnl 
dnl This program is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.
dnl 
dnl You should have received a copy of the GNU General Public License
dnl along with this program; if not, write to the Free Software
dnl Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
dnl
dnl In some recent versions of suitesparse we need 
dnl suitesparseconfig and check umfpack and cholmod together

# check for suitesparse: 
# you must have performed a AC_CHECK_AMD() before.
# you must have performed a AC_CHECK_COLAMD() before.
#------------------------------------------------------------------

AC_DEFUN([AC_CHECK_SUITESPARSE],
[
 ac_save_cppflags=${CPPFLAGS}
 ac_save_libs=${LIBS}
 ac_save_ldflags=${LDFLAGS}
 LIBS="$amd_libs $colamd_libs -lsuitesparseconfig ${LAPACK_LIBS} ${BLAS_LIBS} ${LIBS}"
 AC_MSG_WARN([check with a mix of umfpack and cholmod])
 # check umfpack includes 
 #-------------------
 AC_MSG_CHECKING([for umfpack include file directory])
 AC_FIND_FILE("umfpack.h", $ac_ssparse_includedirs, umfpack_includedir)
 if test "x${umfpack_includedir}" != "x" -a "x${umfpack_includedir}" != "xNO"; then
    CPPFLAGS="-I${umfpack_includedir} ${CPPFLAGS}"
 fi
 AC_MSG_RESULT([${umfpack_includedir}])  

 AC_MSG_CHECKING([for cholmod include file directory])
 AC_FIND_FILE("cholmod.h", $ac_ssparse_includedirs, cholmod_includedir)
 if test "x${cholmod_includedir}" != "x" -a "x${cholmod_includedir}" != "xNO"; then
   CPPFLAGS="-I${cholmod_includedir} ${CPPFLAGS}"
 fi
 AC_MSG_RESULT([${cholmod_includedir}])  
 
 umfpack_library=no
 AC_MSG_CHECKING([umfpack library presence])
 AC_FIND_FILE("libumfpack.a", $ac_ssparse_libdirs, ac_umfpack_libdir)
 if test "x${ac_umfpack_libdir}" != "x" -a "x${ac_umfpack_libdir}" != "xNO"; then
     umfpack_library=$ac_umfpack_libdir/libumfpack.a
 fi
 AC_MSG_RESULT([$umfpack_library])
 if test "xx$umfpack_library" != "xxno";then 
   if test "${ac_umfpack_libdir}" = "/usr/lib"; then 
      LUMFPACK=""
   else 
      LUMFPACK="-L${ac_umfpack_libdir}"
   fi
 else 
   LUMFPACK=""
 fi
 # check for umfpack and cholmod together 
 #-------------------
 umfpack_fail=no
 # AC_MSG_CHECKING([umfpack link one])
 ac_save_ldflags_one=${LDFLAGS}
 LDFLAGS="${LUMFPACK} ${LDFLAGS}"
 LIBS="${LUMFPACK} -lcholmod ${LIBS}"
 UMFPACKLIBS="${LUMFPACK} -lumfpack -lcholmod $amd_libs $colamd_libs -lsuitesparseconfig"
 ## Invalidate the cache 
 $as_unset ac_cv_lib_umfpack_umfpack_di_solve
 ## global try with 
 AC_CHECK_LIB(umfpack,umfpack_di_solve,[umfpack_libs="${UMFPACKLIBS}"],[umfpack_fail=yes]) 
 if test "xx$umfpack_fail" == "xxno";then 
   AC_SUBST(umfpack_libs)
   cholmod_libs=
   AC_SUBST(cholmod_libs)
 fi
 CPPFLAGS=${ac_save_cppflags}
 LIBS=${ac_save_libs}
 LDFLAGS=${ac_save_ldflags}
])

