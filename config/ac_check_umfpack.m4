dnl -*- mode: autoconf -*-
dnl
dnl This file is part of the LifeV Libraries.
dnl
dnl Author: Christophe Prud'homme (christophe.prudhomme@epfl.ch)
dnl
dnl Copyright (C) 2004 EPFL, INRIA, Politecnico di Milano
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
AC_DEFUN([AC_CHECK_UMFPACK],
[
 if test -z "$1"; then
  umfpack_ver=4
  umfpack_subver=3
  umfpack_minorrel=0
 else
  umfpack_minor=`echo "$1" | sed -e 's#[0-9]\+\.[0-9]\+\.\([0-9]\+\).*#\1#'`
  umfpack_subver=`echo "$1" | sed -e 's#[0-9]\+\.\([0-9]\+\).*#\1#'`
  umfpack_ver=`echo "$1" | sed -e 's#^\([0-9]\+\)\..*#\1#'`
 fi
 umfpack_version="${umfpack_ver}.${umfpack_subver}.${umfpack_minor}"
 # check amd includes 
 #-------------------
 AC_MSG_CHECKING([for amd include file directory])
 ac_mpi_includedirs="/usr/include/amd /usr/include/umfpack /usr/include/ufsparse  $REPOSITORY/$ARCH/include/UMFPACK /usr/include /usr/local/include/amd /usr/local/include/umfpack /usr/local/include/ufsparse /usr/local/include"
 AC_FIND_FILE("amd.h", $ac_mpi_includedirs, amd_includedir)
 if test "x${amd_includedir}" != "x" -a "x${amd_includedir}" != "xNO"; then
  CPPFLAGS="-I${amd_includedir} ${CPPFLAGS}"
 fi
 AC_MSG_RESULT([${amd_includedir}])  

 ac_save_cppflags=${CPPFLAGS}

 # check umfpack includes 
 #-------------------
 AC_MSG_CHECKING([for umfpack include file directory])
 ac_mpi_includedirs="/usr/include/umfpack /usr/include/ufsparse $REPOSITORY/$ARCH/include/UMFPACK /usr/include /usr/local/include/umfpack /usr/local/include/ufsparse /usr/local/include"
 AC_FIND_FILE("umfpack.h", $ac_mpi_includedirs, umfpack_includedir)
 if test "x${umfpack_includedir}" != "x" -a "x${umfpack_includedir}" != "xNO"; then
    if test "x${umfpack_includedir}" != "x${amd_includedir}"; then 
      CPPFLAGS="-I${umfpack_includedir} ${CPPFLAGS}"
    fi
 fi
 # check amd_library 
 #-------------------
 amd_library=no
 AC_MSG_CHECKING([amd library presence])
 ac_amd_libdirs="$REPOSITORY/$ARCH/lib$AFFIX/UMFPACK /usr/lib /usr/local/lib /usr/lib/amd /usr/local/lib/amd /usr/lib/umfpack /usr/local/lib/umfpack"
 AC_FIND_FILE("libamd.a", $ac_amd_libdirs, ac_amd_libdir)
 if test "x${ac_amd_libdir}" != "x" -a "x${ac_amd_libdir}" != "xNO"; then
  amd_library=$ac_amd_libdir/libamd.a
  LDFLAGS="${LDFLAGS} -L${ac_amd_libdir}"
 fi
 AC_MSG_RESULT([$amd_library])
 # check for umfpack if amd was found 
 #----------------------------------
 if test "xx$amd_library" != "xxno";then 
  umfpack_library=no
  AC_MSG_CHECKING([umfpack library presence])
  ac_umfpack_libdirs="$REPOSITORY/$ARCH/lib$AFFIX/UMFPACK /usr/lib /usr/local/lib /usr/lib/umfpack /usr/local/lib/umfpack"
  AC_FIND_FILE("libumfpack.a", $ac_umfpack_libdirs, ac_umfpack_libdir)
  if test "x${ac_umfpack_libdir}" != "x" -a "x${ac_umfpack_libdir}" != "xNO"; then
    umfpack_library=$ac_umfpack_libdir/libumfpack.a
    if test "x${ac_umfpack_libdir}" != "x${ac_amd_libdir}"; then
      LDFLAGS="${LDFLAGS} -L${ac_umfpack_libdir}"
    fi
  fi
  AC_MSG_RESULT([$umfpack_library])
#  AC_MSG_WARN([up to here LDFLAGS="$LDFLAGS"])
  if test "xx$umfpack_library" != "xxno";then 
    if test "$umfpack_ver" = "4"; then
     if test "${ac_amd_libdir}" = "/usr/lib"; then 
      AC_CHECK_LIB(amd,amd_postorder,[amd_libs="-lamd"])
     else 
      AC_CHECK_LIB(amd,amd_postorder,[amd_libs="-L${ac_amd_libdir} -lamd"])
     fi
#     AC_MSG_WARN([amd_postorder found with $amd_libs])
     ac_save_ldflags=${LDFLAGS}
     LDFLAGS="${LDFLAGS} $amd_libs"
#     AC_MSG_WARN([up to here LDFLAGS="$LDFLAGS" and amd_libs="$amd_libs"])
     if test "${ac_umfpack_libdir}" = "/usr/lib"; then 
      AC_CHECK_LIB(umfpack,umfpack_di_solve,[umfpack_libs="-lumfpack ${amd_libs}"])
     else 
      AC_CHECK_LIB(umfpack,umfpack_di_solve,[umfpack_libs="-L${ac_umfpack_libdir} -lumfpack ${amd_libs}"])
     fi
     LDFLAGS=${ac_save_ldflags}
#     AC_MSG_WARN([up to here LDFLAGS="$LDFLAGS" umfpack_libs="$umfpack_libs"])
    else
     ac_save_ldflags=${LDFLAGS}
     LDFLAGS="${LDFLAGS} -L${ac_amd_libdir} -lamd"
     AC_CHECK_LIB(umfpack,umfpack_solve,[umfpack_libs="-L${ac_umfpack_libdir} -lumfpack -lamd "])
     LDFLAGS=${ac_save_ldflags}
    fi
    AC_SUBST(umfpack_libs)
  fi
 fi
])
