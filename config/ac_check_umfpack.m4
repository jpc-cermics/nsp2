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

AC_DEFUN([AC_CHECK_AMD],
[
 ac_save_cppflags=${CPPFLAGS}
 ac_save_libs=${LIBS}
 ac_save_ldflags=${LDFLAGS}
 # check amd includes 
 #-------------------
 AC_MSG_CHECKING([for amd include file directory])
 ac_amd_includedirs=" /usr/include/amd /usr/include/umfpack /usr/include/ufsparse /usr/include /usr/local/include/amd /usr/local/include/umfpack /usr/local/include/ufsparse /usr/local/include"
 AC_FIND_FILE("amd.h", $ac_amd_includedirs, amd_includedir)
 if test "x${amd_includedir}" != "x" -a "x${amd_includedir}" != "xNO"; then
  CPPFLAGS="-I${amd_includedir} ${CPPFLAGS}"
 fi
 AC_MSG_RESULT([${amd_includedir}])  

 ac_save_ldflags=${LDFLAGS}

 # check amd_library 
 #-------------------
 amd_library=no
 AC_MSG_CHECKING([amd library presence])
 ac_amd_libdirs="/usr/lib /usr/local/lib /usr/lib/amd /usr/local/lib/amd /usr/lib/umfpack /usr/local/lib/umfpack"
 AC_FIND_FILE("libamd.a", $ac_amd_libdirs, ac_amd_libdir)
 if test "x${ac_amd_libdir}" != "x" -a "x${ac_amd_libdir}" != "xNO"; then
  amd_library=$ac_amd_libdir/libamd.a
 fi
 AC_MSG_RESULT([$amd_library])

 # check for umfpack if amd was found 
 #----------------------------------
 if test "xx$amd_library" != "xxno";then 
    if test "${ac_amd_libdir}" = "/usr/lib"; then 
      AC_CHECK_LIB(amd,amd_postorder,[amd_libs="-lamd"])
    else 
      LDFLAGS="-L${ac_amd_libdir} ${LDFLAGS}"
      AC_CHECK_LIB(amd,amd_postorder,[amd_libs="-L${ac_amd_libdir} -lamd"])
    fi
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
 ac_umf_includedirs="/usr/include/umfpack /usr/include/ufsparse /usr/include /usr/local/include/umfpack /usr/local/include/ufsparse /usr/local/include"
 AC_FIND_FILE("umfpack.h", $ac_umf_includedirs, umfpack_includedir)
 if test "x${umfpack_includedir}" != "x" -a "x${umfpack_includedir}" != "xNO"; then
    if test "x${umfpack_includedir}" != "x${amd_includedir}"; then 
      CPPFLAGS="-I${umfpack_includedir} ${CPPFLAGS}"
    fi
 fi
 AC_MSG_RESULT([${umfpack_includedir}])  
 # check for umfpack 
 #-------------------
 umfpack_library=no
 AC_MSG_CHECKING([umfpack library presence])
 ac_umfpack_libdirs="/usr/lib /usr/local/lib /usr/lib/umfpack /usr/local/lib/umfpack"
 AC_FIND_FILE("libumfpack.a", $ac_umfpack_libdirs, ac_umfpack_libdir)
 if test "x${ac_umfpack_libdir}" != "x" -a "x${ac_umfpack_libdir}" != "xNO"; then
     umfpack_library=$ac_umfpack_libdir/libumfpack.a
 fi
 AC_MSG_RESULT([$umfpack_library])
 if test "xx$umfpack_library" != "xxno";then 
   if test "${ac_umfpack_libdir}" = "/usr/lib"; then 
      AC_CHECK_LIB(umfpack,umfpack_di_solve,[umfpack_libs="-lumfpack ${amd_libs}"])
   else 
      LDFLAGS="-L${ac_umfpack_libdir} ${LDFLAGS}"
      AC_CHECK_LIB(umfpack,umfpack_di_solve,[umfpack_libs="-L${ac_umfpack_libdir} -lumfpack ${amd_libs}"])
   fi
   AC_SUBST(umfpack_libs)
 fi
 CPPFLAGS=${ac_save_cppflags}
 LIBS=${ac_save_libs}
 LDFLAGS=${ac_save_ldflags}

])

