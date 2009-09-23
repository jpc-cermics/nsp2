#===============================pointer size =============================

AC_DEFUN([AC_CHECK_POINTER_SIZE], [
dnl INPUTS :
dnl OUTPUTS
dnl  1 if sizeof(int*) == sizeof(int)  0 otherwise
dnl  Check pointer size 

# fro cross compilation 
if test "$build_alias" != "$host_alias" ; then
     AC_MSG_WARN([cannot run executable when cross-compiling: assuming true for next test ])
fi 


AC_MSG_CHECKING([if sizeof(int*) == sizeof(int)])

cat > conftest.$ac_ext <<EOF
#include "confdefs.h"
#include <stdio.h>

int main(int argc,char **argv) {
        FILE *maj = fopen("sizeofintp","w");
        fprintf(maj,"%d",sizeof(int*) == sizeof(int));
        fclose(maj);
        return 0;
}
EOF

eval $ac_link
if test -s conftest$ac_exeext && (./conftest$ac_exeext; exit) 2>/dev/null; then
  SIZEOF_INTP=`cat sizeofintp`
  rm -f sizeofintp
else
  SIZEOF_INTP="cannot_happen"
  # can happen when cross compiling and cross compiled programs do not run 
  if test "$build_alias" != "$host_alias" ; then
     SIZEOF_INTP=1;
  fi 
fi

if test $SIZEOF_INTP = 1; then 
	AC_MSG_RESULT([yes])
	AC_DEFINE(POINTER_INT,[],[sizeof pointer is sizeof int])
else 
	AC_MSG_RESULT([no])
	if test $SIZEOF_INTP = "cannot_happen" ; then
		AC_MSG_ERROR([cannot happen])
	fi
fi
CFLAGS=$saved_cflags
CPPFLAGS=$saved_cppflags
]) dnl End of AC_CHECK_POINTER_SIZE 

