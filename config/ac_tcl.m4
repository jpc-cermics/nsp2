dnl Copyright Jean-Philippe Chancelier Enpc 

AC_DEFUN( AC_CHECK_TCL_VERSION, [
dnl INPUTS :
dnl  $2 : Major version number ( 8 f. ex)
dnl  $3 : Minor version number (0 f. ex.)
dnl
dnl OUTPUTS
dnl  TCL_VERSION_OK : 1 if OK, 0 otherwise
dnl  Check the version of tcl associated to header file tcl.h 
 AC_MSG_CHECKING([if tcl is version $1.$2 or later])
 TCL_VERSION_OK=0
 if test $TCL_MAJOR_VERSION==$1 ; then 
     if test $TCL_MINOR_VERSION==$2 ; then 
	TCL_VERSION_OK=1
        AC_MSG_RESULT(yes)
     else 
        AC_MSG_RESULT(no)
     fi
 else 
     AC_MSG_RESULT(no)
 fi 
]) dnl End of AC_CHECK_TCL_VERSION


AC_DEFUN( AC_CHECK_TK_VERSION, [
dnl INPUTS :
dnl  $2 : Major version number ( 8 f. ex)
dnl  $3 : Minor version number (0 f. ex.)
dnl
dnl OUTPUTS
dnl  TK_VERSION_OK : 1 if OK, 0 otherwise
dnl  Check the version of tcl associated to header file tcl.h 
 AC_MSG_CHECKING([if tcl is version $1.$2 or later])
 TK_VERSION_OK=0
 if test $TK_MAJOR_VERSION==$1 ; then 
     if test $TK_MINOR_VERSION==$2 ; then 
	TK_VERSION_OK=1
        AC_MSG_RESULT(yes)
     else 
        AC_MSG_RESULT(no)
     fi
 else 
     AC_MSG_RESULT(no)
 fi 
]) dnl End of AC_CHECK_TK_VERSION


