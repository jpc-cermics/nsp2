dnl -*- mode: autoconf -*-
dnl
dnl This file is part of the LifeV Libraries.
dnl Author: Christophe Prud'homme (christophe.prudhomme@epfl.ch)
dnl Copyright (C) 2004 EPFL, INRIA, Politecnico di Milano
dnl Modified for nsp Chancelier 2005-2006
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

# typical places where we search for suitesparse 

AC_DEFUN([AC_SUITESPARSE_PATH],
[
  ac_ssparse_includedirs="/usr/include /usr/include/suitesparse /usr/include/umfpack /usr/include/ufsparse"
  ac_ssparse_includedirs="$ac_ssparse_includedirs /usr/local/include/suitesparse /usr/local/include/ufsparse /usr/local/include"
  ac_ssparse_includedirs="$ac_ssparse_includedirs /opt/local/include/suitesparse /opt/local/include/ufsparse /opt/local/include"
  ac_ssparse_libdirs="/usr/lib /usr/lib/umfpack /usr/lib/suitesparse"
  ac_ssparse_libdirs="$ac_ssparse_libdirs /usr/local/lib /usr/local/lib/umfpack /usr/local/lib/suitesparse"
  ac_ssparse_libdirs="$ac_ssparse_libdirs /opt/local/lib /opt/local/lib/umfpack /opt/local/lib/suitesparse"

 case "$host" in
    i686-w64-mingw32)
     # cross-compiling for windows 32bits 
     ac_amd_includedirs="/usr/i686-w64-mingw32/include/suitesparse /mingw32/include"
     ac_amd_libdirs="/usr/i686-w64-mingw32/lib /mingw32/lib"
     ac_ssparse_includedirs="/usr/i686-w64-mingw32/include/suitesparse /mingw32/include $ac_ssparse_includedirs"
     ;;
    x86_64-w64-mingw32)
     # cross-compiling for windows 64bits
     ac_amd_includedirs="/usr/x86_64-w64-mingw32/include/suitesparse /mingw64/include"
     ac_amd_libdirs="/usr/x86_64-w64-mingw32/lib /mingw64/lib"
     ac_ssparse_includedirs="/usr/x86_64-w64-mingw32/include/suitesparse /mingw64/include $ac_ssparse_includedirs"
     ;;
    *-*-darwin*)
     if test $with_macports = yes; then
       # do not search in /usr/local 
       ac_ssparse_includedirs="/usr/include /usr/include/suitesparse /usr/include/umfpack /usr/include/ufsparse"
       ac_ssparse_includedirs="$ac_ssparse_includedirs /opt/local/include/suitesparse /opt/local/include/ufsparse /opt/local/include"
       ac_ssparse_libdirs="/usr/lib /usr/lib/umfpack /usr/lib/suitesparse"
       ac_ssparse_libdirs="$ac_ssparse_libdirs /opt/local/lib /opt/local/lib/umfpack /opt/local/lib/suitesparse"
     fi
     if test $with_brew = yes; then
       # do not search in /opt/local 
       ac_ssparse_includedirs="/usr/include /usr/include/suitesparse /usr/include/umfpack /usr/include/ufsparse"
       ac_ssparse_includedirs="$ac_ssparse_includedirs /usr/local/include/suitesparse /usr/local/include/ufsparse /usr/local/include"
       ac_ssparse_libdirs="/usr/lib /usr/lib/umfpack /usr/lib/suitesparse"
       ac_ssparse_libdirs="$ac_ssparse_libdirs /usr/local/lib /usr/local/lib/umfpack /usr/local/lib/suitesparse"
     fi
     ;;
  esac 
])

AC_DEFUN([AC_CHECK_AMD],
[
 ac_save_cppflags=${CPPFLAGS}
 ac_save_libs=${LIBS}
 ac_save_ldflags=${LDFLAGS}
 # check amd includes
 #-------------------
 AC_MSG_CHECKING([for amd include file directory])
 AC_FIND_FILE("amd.h", $ac_ssparse_includedirs, amd_includedir)
 if test "x${amd_includedir}" != "x" -a "x${amd_includedir}" != "xNO"; then
  CPPFLAGS="-I${amd_includedir} ${CPPFLAGS}"
 fi
 AC_MSG_RESULT([${amd_includedir}])
 # check amd_library
 #-------------------
 amd_library=no
 AC_MSG_CHECKING([amd library presence])
 AC_FIND_FILE("libamd.so", $ac_ssparse_libdirs, ac_amd_libdir)
 if test "x${ac_amd_libdir}" != "x" -a "x${ac_amd_libdir}" != "xNO"; then
  amd_library=$ac_amd_libdir/libamd.so
 fi
 AC_MSG_RESULT([$amd_library])
 # check for amd_postorder in amd library
 #----------------------------------
 if test "xx$amd_library" != "xxno";then
    if test "${ac_amd_libdir}" = "/usr/lib"; then
      AC_CHECK_LIB(amd,amd_postorder,[amd_libs="-lamd"])
    else
      LDFLAGS="-L${ac_amd_libdir} ${LDFLAGS}"
      AC_CHECK_LIB(amd,amd_postorder,[amd_libs="-L${ac_amd_libdir} -lamd"])
    fi
 else
    # maybe we just have shared libraries in standard path
    AC_CHECK_LIB(amd,amd_postorder,[amd_libs="-lamd"])
 fi
 CPPFLAGS=${ac_save_cppflags}
 LIBS=${ac_save_libs}
 LDFLAGS=${ac_save_ldflags}
])

# check for umfpack: you must have performed a AC_CHECK_AMD() before.
#------------------------------------------------------------------

AC_DEFUN([AC_CHECK_UMFPACK],
[
 ac_save_cppflags=${CPPFLAGS}
 ac_save_libs=${LIBS}
 ac_save_ldflags=${LDFLAGS}
 LIBS="$amd_libs ${LIBS} ${BLAS_LIBS}"
 # check umfpack includes
 #-------------------
 AC_MSG_CHECKING([for umfpack include file directory])
 AC_FIND_FILE("umfpack.h", $ac_ssparse_includedirs, umfpack_includedir)
 if test "x${umfpack_includedir}" != "x" -a "x${umfpack_includedir}" != "xNO"; then
    CPPFLAGS="-I${umfpack_includedir} ${CPPFLAGS}"
 fi
 AC_MSG_RESULT([${umfpack_includedir}])
 # check for umfpack
 #-------------------
 umfpack_library=no
 AC_MSG_CHECKING([umfpack library presence])
 AC_FIND_FILE("libumfpack.so", $ac_ssparse_libdirs, ac_umfpack_libdir)
 if test "x${ac_umfpack_libdir}" != "x" -a "x${ac_umfpack_libdir}" != "xNO"; then
    umfpack_library=$ac_umfpack_libdir/libumfpack.so
 fi
 AC_MSG_RESULT([$umfpack_library])
 if test "xx$umfpack_library" != "xxno";then
   if test "${ac_umfpack_libdir}" = "/usr/lib"; then
      AC_CHECK_LIB(umfpack,umfpack_di_solve,[umfpack_libs="-lumfpack ${amd_libs}"])
   else
      umfpack_fail=no
      # AC_MSG_CHECKING([umfpack link one])
      ac_save_ldflags_one=${LDFLAGS}
      LDFLAGS="-L${ac_umfpack_libdir} ${LDFLAGS} -lSuiteSparse"
      ## Invalidate the cache and try again without -lSuiteSparse
      $as_unset ac_cv_lib_umfpack_umfpack_di_solve
      AC_CHECK_LIB(umfpack,umfpack_di_solve,[umfpack_libs="-L${ac_umfpack_libdir} -lumfpack -lSuiteSparse ${amd_libs}"],[umfpack_fail=yes])
      if test "xx$umfpack_fail" == "xxyes";then
        ## Invalidate the cache and try again without -lSuiteSparse
	$as_unset ac_cv_lib_umfpack_umfpack_di_solve
	LDFLAGS=${ac_save_ldflags_one}
	#  AC_MSG_CHECKING([umfpack link two])
        LDFLAGS="-L${ac_umfpack_libdir} ${LDFLAGS}"
        AC_CHECK_LIB(umfpack,umfpack_di_solve,[umfpack_libs="-L${ac_umfpack_libdir} -lumfpack ${amd_libs}"])
      fi
   fi
   AC_SUBST(umfpack_libs)
 else
    # maybe we just have shared libraries in standard path
    AC_CHECK_LIB(umfpack,umfpack_di_solve,[umfpack_libs="-lumfpack ${amd_libs}"])
 fi
 CPPFLAGS=${ac_save_cppflags}
 LIBS=${ac_save_libs}
 LDFLAGS=${ac_save_ldflags}
])
