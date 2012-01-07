# glpk.m4 - checks for glpk  -*-Autoconf-*-

AC_DEFUN([CHECK_GLPK],
[
  AC_ARG_WITH(glpk,
    [  --without-glpk          do not compile with glpk support ])

  AC_ARG_WITH([glpk-includedir],
    AS_HELP_STRING([--with-glpk-includedir=DIR], [search for GLPK headers in DIR]),
              [], [with_glpk_includedir=no])
  AC_ARG_WITH([glpk-libdir],
    AS_HELP_STRING([--with-glpk-libdir=DIR], [search for GLPK libraries in DIR]),
	              [], [with_glpk_libdir=no])

  GLPK_LIB=libglpk
  HAVE_GLPK="no"

  if test "$with_glpk" != no; then
    glpk_found=no
    AC_MSG_CHECKING([for GLPK])
    GLPK_CFLAGS=
    if test x"$with_glpk_includedir" != x"no"; then
      GLPK_CFLAGS="-I$with_glpk_includedir"
    fi
    GLPK_LDFLAGS=
    if test x"$with_glpk_libdir" != x"no"; then
      GLPK_LDFLAGS="-L$with_glpk_libdir"
    fi
    GLPK_LIBS="$GLPK_LDFLAGS -lglpk"
    save_cflags="$CFLAGS"
    save_libs="$LIBS"
    CFLAGS="$GLPK_CFLAGS"
    LIBS="$GLPK_LIBS"
    glpk_test_prog='
        #include <glpk.h>
        #if (GLP_MAJOR_VERSION < 4) \
           || (GLP_MAJOR_VERSION == 4 && GLP_MINOR_VERSION < 33)
        #error Supported GLPK versions: 4.33 or above
        #endif
        int main(int argc, char** argv)
        {
          LPX *lp;
          lp = lpx_create_prob();
          lpx_delete_prob(lp);
          return 0;
        }'
    AC_LANG_PUSH(C)
    AC_LINK_IFELSE([$glpk_test_prog], [glpk_found=yes], [glpk_found=no])
    AC_LANG_POP(C)
    CFLAGS="$save_cflags"
    LIBS="$save_libs"
    if test x"$glpk_found" = x"yes"; then
       AC_DEFINE([WITH_GLPK], [], [Define to 1 if you have GLPK.])
       HAVE_GLPK="yes"
       AC_MSG_RESULT([yes])
    else
       GLPK_CFLAGS=""
       GLPK_LIBS=""
       GLPK_LIB=libglpk
       AC_MSG_RESULT([no])
    fi
  fi 

  AC_SUBST(GLPK_CFLAGS)
  AC_SUBST(GLPK_LIBS)
  AC_SUBST(GLPK_LIB)
])

