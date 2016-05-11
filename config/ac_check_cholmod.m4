dnl -*- mode: autoconf -*-
dnl
dnl detect colamd and cholmod Jean-Philippe Chancelier 2007
dnl adapted from ac_check_umfpack.m4
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


# call AC_SUITESPARSE_PATH() before

AC_DEFUN([AC_CHECK_COLAMD],
[
 ac_save_cppflags=${CPPFLAGS}
 ac_save_libs=${LIBS}
 ac_save_ldflags=${LDFLAGS}
 # check colamd includes
 #-------------------
 AC_MSG_CHECKING([for colamd include file directory])
 AC_FIND_FILE("colamd.h", $ac_ssparse_includedirs,colamd_includedir)
 if test "x${colamd_includedir}" != "x" -a "x${colamd_includedir}" != "xNO"; then
  CPPFLAGS="-I${colamd_includedir} ${CPPFLAGS}"
 fi
 AC_MSG_RESULT([${colamd_includedir}])
 # check colamd_library
 #-------------------
 colamd_library=no
 AC_MSG_CHECKING([colamd library presence])
 AC_FIND_FILE("libcolamd.so", $ac_ssparse_libdirs, ac_colamd_libdir)
 if test "x${ac_colamd_libdir}" != "x" -a "x${ac_colamd_libdir}" != "xNO"; then
  colamd_library=$ac_colamd_libdir/libcolamd.so
 fi
 AC_MSG_RESULT([$colamd_library])
 # check for colcolamd
 #----------------------------------
 if test "xx$colamd_library" != "xxno";then
    if test "${ac_colamd_libdir}" = "/usr/lib"; then
      AC_CHECK_LIB(colamd,colamd,[colamd_libs="-lcolamd"])
    else
      LDFLAGS="-L${ac_colamd_libdir} ${LDFLAGS}"
      AC_CHECK_LIB(colamd,colamd,[colamd_libs="-L${ac_colamd_libdir} -lcolamd"])
    fi
 else
    # maybe we just have shared libraries in standard path
    AC_CHECK_LIB(colamd,colamd,[colamd_libs="-lcolamd"])
 fi
 CPPFLAGS=${ac_save_cppflags}
 LIBS=${ac_save_libs}
 LDFLAGS=${ac_save_ldflags}
])

# check for cholmod: you must have performed a AC_CHECK_COLAMD() before.
# and AC_CHECK_AMD()
# Note that lapack / blas are needed here so it won't work if scilab
# uses its own lapack/blas sources
#------------------------------------------------------------------

AC_DEFUN([AC_CHECK_CHOLMOD],
[
 ac_save_cppflags=${CPPFLAGS}
 ac_save_libs=${LIBS}
 ac_save_ldflags=${LDFLAGS}
 LIBS="$colamd_libs $amd_libs $LAPACK_LIBS $BLAS_LIBS ${LIBS}"
 # check cholmod includes
 #-------------------
 AC_MSG_CHECKING([for cholmod include file directory])
 AC_FIND_FILE("cholmod.h", $ac_ssparse_includedirs, cholmod_includedir)
 if test "x${cholmod_includedir}" != "x" -a "x${cholmod_includedir}" != "xNO"; then
    CPPFLAGS="-I${cholmod_includedir} ${CPPFLAGS}"
 fi
 AC_MSG_RESULT([${cholmod_includedir}])
 # check for cholmod
 #-------------------
 AC_MSG_CHECKING([cholmod library presence])
 AC_FIND_FILE("libcholmod.so", $ac_ssparse_libdirs, ac_cholmod_libdir)
 if test "x${ac_cholmod_libdir}" != "x" -a "x${ac_cholmod_libdir}" != "xNO"; then
     cholmod_library=$ac_cholmod_libdir/libcholmod.so
 else
     cholmod_library=no
 fi
 AC_MSG_RESULT([$cholmod_library])
 if test "xx$cholmod_library" != "xxno";then
   # cholmod library was found
   ac_save_libs1=${LIBS}
   LIBS="-lsuitesparseconfig ${LIBS}"
   if test "${ac_cholmod_libdir}" = "/usr/lib"; then
      AC_CHECK_LIB(cholmod,cholmod_analyze,[cholmod_libs="-lcholmod -lsuitesparseconfig ${colamd_libs} "])
   else
      LDFLAGS="-L${ac_cholmod_libdir} ${LDFLAGS}"
      AC_CHECK_LIB(cholmod,cholmod_analyze,[cholmod_libs="-L${ac_cholmod_libdir} -lcholmod -lsuitesparseconfig ${colamd_libs} "])
   fi
   LIBS=${ac_save_libs1}
   if test "xx$cholmod_libs" = "xx";then
     # Try without suitesparseconfig
     ## Invalidate the cache 
     $as_unset ac_cv_lib_cholmod_cholmod_analyze
     if test "${ac_cholmod_libdir}" = "/usr/lib"; then
        AC_CHECK_LIB(cholmod,cholmod_analyze,[cholmod_libs="-lcholmod ${colamd_libs} "])
     else
        LDFLAGS="-L${ac_cholmod_libdir} ${LDFLAGS}"
        AC_CHECK_LIB(cholmod,cholmod_analyze,[cholmod_libs="-L${ac_cholmod_libdir} -lcholmod ${colamd_libs} "])
     fi
   fi
   # Try with c versions and metis (for macports)
   if test "xx$cholmod_libs" = "xx" ; then
      LDFLAGS="-L${ac_cholmod_libdir} ${LDFLAGS} -lccolamd -lcamd -lmetis"
      AC_CHECK_LIB(cholmod,cholmod_metis,[cholmod_libs="-lcholmod -lccolamd -lcamd ${colamd_libs} -lmetis "])
   fi
   AC_SUBST(cholmod_libs)
 else
    # maybe we just have shared libraries in standard path
    # first try with suitesparseconfig
    ac_save_libs1=${LIBS}
    LIBS="-lsuitesparseconfig ${LIBS}"
    AC_CHECK_LIB(cholmod,cholmod_analyze,[cholmod_libs="-lcholmod -lsuitesparseconfig ${colamd_libs} "])
    AC_MSG_RESULT([$cholmod_libs])
    LIBS=${ac_save_libs1}
    if test "xx$cholmod_libs" = "xx";then
     # Try without suitesparseconfig
     $as_unset ac_cv_lib_cholmod_cholmod_analyze
     AC_CHECK_LIB(cholmod,cholmod_analyze,[cholmod_libs="-lcholmod ${colamd_libs} "])
     AC_MSG_RESULT([$cholmod_libs])
    fi
 fi
 CPPFLAGS=${ac_save_cppflags}
 LIBS=${ac_save_libs}
 LDFLAGS=${ac_save_ldflags}
])

# check for spqr: you must have performed a AC_CHECK_CHOLMOD() before
#------------------------------------------------------------------

AC_DEFUN([AC_CHECK_SPQR],
[
 ac_save_cppflags=${CPPFLAGS}
 ac_save_libs=${LIBS}
 ac_save_ldflags=${LDFLAGS}
 LIBS="$cholmod_libs $LAPACK_LIBS $BLAS_LIBS ${LIBS}"
 # check spqr includes
 #-------------------
 AC_MSG_CHECKING([for spqr include file directory])
 AC_FIND_FILE("SuiteSparseQR_C.h", $ac_ssparse_includedirs, spqr_includedir)
 if test "x${spqr_includedir}" != "x" -a "x${spqr_includedir}" != "xNO"; then
    CPPFLAGS="-I${spqr_includedir} ${CPPFLAGS}"
 fi
 AC_MSG_RESULT([${spqr_includedir}])
 # check for spqr
 #-------------------
 AC_MSG_CHECKING([spqr library presence])
 AC_FIND_FILE("libspqr.so", $ac_ssparse_libdirs, ac_spqr_libdir)
 if test "x${ac_spqr_libdir}" != "x" -a "x${ac_spqr_libdir}" != "xNO"; then
     spqr_library=$ac_spqr_libdir/libspqr.so
 else
     spqr_library=no
 fi
 AC_MSG_RESULT([$spqr_library])
 if test "xx$spqr_library" != "xxno";then
   if test "${ac_spqr_libdir}" = "/usr/lib" -o "${ac_spqr_libdir}" = "/usr/$host/lib"; then
      AC_CHECK_LIB(spqr,SuiteSparseQR_C ,[spqr_libs="-lspqr"])
   else
      LDFLAGS="-L${ac_spqr_libdir} ${LDFLAGS}"
      AC_CHECK_LIB(spqr,SuiteSparseQR_C ,[spqr_libs="-L${ac_spqr_libdir} -lspqr"])
   fi
 else
    # maybe we just have shared libraries in standard path
    AC_CHECK_LIB(spqr,SuiteSparseQR_C ,[spqr_libs="-lspqr"])
 fi
 AC_SUBST(spqr_libs)
 CPPFLAGS=${ac_save_cppflags}
 LIBS=${ac_save_libs}
 LDFLAGS=${ac_save_ldflags}
])

# Rajouter metis au besoin
#
# AC_ARG_WITH(cholmod,
#   [AS_HELP_STRING([--without-cholmod],
#      [don't use CHOLMOD, disable some sparse functionality])],
#   with_cholmod=$withval, with_cholmod=yes)

# warn_cholmod="CHOLMOD not found. This will result in some lack of functionality for sparse matrices."
# if test "$with_cholmod" = yes && test "$with_colamd" = yes &&
#         test "$with_ccolamd" = yes && test "$with_amd" = yes; then
#   with_cholmod=no
#   AC_CHECK_HEADERS([ufsparse/cholmod.h cholmod/cholmod.h cholmod.h], [
#     AC_CHECK_HEADERS([ufsparse/metis.h metis/metis.h metis.h], [
#       AC_CHECK_LIB(metis, METIS_NodeND, with_metis=yes, with_metis=no)
#       break],
#       with_metis=no)

#     if test "$with_metis" = yes; then
#       AC_DEFINE(HAVE_METIS, 1, [Define if the METIS library is used.])
#       AC_CHECK_LIB(cholmod, cholmod_start, [CHOLMOD_LIBS="-lcholmod -lmetis";
#         with_cholmod=yes], [
#         AC_CHECK_LIB(cholmod, cholmod_start,
#           [CHOLMOD_LIBS="-lcholmod -cblas -lmetis"; with_cholmod=yes], [],
#           $AMD_LIBS $COLAMD_LIBS $CCOLAMD_LIBS $BLAS_LIBS $FLIBS -lmetis)],
#         $AMD_LIBS $COLAMD_LIBS $CCOLAMD_LIBS $BLAS_LIBS $FLIBS -lmetis)
#     else
#       AC_CHECK_LIB(cholmod, cholmod_start, [CHOLMOD_LIBS="-lcholmod";
#         with_cholmod=yes], [
#         AC_CHECK_LIB(cholmod, cholmod_start, [CHOLMOD_LIBS="-lcholmod -cblas";
#           with_cholmod=yes], [],
#           $AMD_LIBS $COLAMD_LIBS $CCOLAMD_LIBS $BLAS_LIBS $FLIBS)],
#         $AMD_LIBS $COLAMD_LIBS $CCOLAMD_LIBS $BLAS_LIBS $FLIBS)
#     fi

#     if test "$with_cholmod" = yes; then
#       AC_DEFINE(HAVE_CHOLMOD, 1, [Define if the CHOLMOD library is used.])
#       TEXINFO_CHOLMOD="@set HAVE_CHOLMOD"
#       warn_cholmod=
#     fi
#     break])
# fi

# AC_SUBST(TEXINFO_CHOLMOD)
