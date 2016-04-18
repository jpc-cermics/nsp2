dnl autoconf macros for ocaml
dnl
dnl Copyright 2016 Jean-Philippe Chancelier 
dnl used for companion tools of nsp/scicos 
dnl which are to be compiled or cross-compiled
dnl
dnl Original file: ocaml.m4 from  ocaml-autoconf-1.1
dnl I have just modified AC_PROG_OCAML
dnl since I want ocamlc and ocalopt 
dnl for compiling or cross-compiling 
dnl and I want to have the possibility to give then 
dnl in env 
dnl 
dnl Copyright 2009      Richard W.M. Jones
dnl Copyright 2009      Stefano Zacchiroli
dnl Copyright 2000-2005 Olivier Andrieu
dnl Copyright 2000-2005 Jean-Christophe Filliâtre
dnl Copyright 2000-2005 Georges Mariano

AN_MAKEVAR([OCAMLC], [AC_PROG_OCAML])
AN_MAKEVAR([OCAMLOPT], [AC_PROG_OCAML])
AN_MAKEVAR([OCAMLLIB], [AC_PROG_OCAML])

AC_DEFUN([AC_PROG_OCAML],
[dnl
  # checking for ocamlc

  AC_ARG_VAR([OCAMLC],     [Ocaml, ocamlc compiler command])dnl
  AC_ARG_VAR([OCAMLOPT],   [Ocaml, ocamlopt compiler command])dnl
  AC_ARG_VAR([OCAMLIB],    [Ocaml library path])dnl
  
  AC_CHECK_TOOL([OCAMLC],[ocamlc],[no])
  AC_CHECK_TOOL([OCAMLOPT],[ocamlopt],[no])
  
  if test "$OCAMLC" != "no"; then
     # OCAMLVERSION: 
     AC_MSG_CHECKING([ocamlc version])
     OCAMLVERSION=`$OCAMLC -v | sed -n -e 's|.*version* *\(.*\)$|\1|p'`
     AC_MSG_RESULT([$OCAMLVERSION])

     OCAML_VERSION_MAJOR=`echo $OCAMLVERSION | cut -d . -f 1`
     OCAML_VERSION_MINOR=`echo $OCAMLVERSION | cut -d . -f 2`
     
     # OCAMLLIB: use given value or compute one 
     AC_MSG_CHECKING([OCaml library path])
     if test "$OCAMLLIB" = ""; then
        OCAMLLIB=`$OCAMLC -where 2>/dev/null || $OCAMLC -v|tail -1|cut -d ' ' -f 4`
     fi
     AC_MSG_RESULT([$OCAMLLIB])
     
     AC_SUBST([OCAMLVERSION])
     AC_SUBST([OCAML_VERSION_MAJOR])
     AC_SUBST([OCAML_VERSION_MINOR])
     AC_SUBST([OCAMLLIB])
  fi	

  AC_CHECK_TOOL([OCAMLOPT],[ocamlopt],[no])

  # sanity check 
  if test "$OCAMLOPT" != "no"; then
    AC_MSG_CHECKING([checking if ocamlc and ocamlopt share same version])
    TMPVERSION=`$OCAMLOPT -v | sed -n -e 's|.*version* *\(.*\)$|\1|p' `
    if test "$TMPVERSION" != "$OCAMLVERSION" ; then
       AC_MSG_ERROR([versions differs from ocamlc; ocamlopt discarded.])
    else 
       AC_MSG_RESULT([yes])
    fi
  fi 
  
  AC_SUBST([OCAMLOPT])
  AC_SUBST([OCAMLC])

  # checking for ocamldep
  AC_CHECK_TOOL([OCAMLDEP],[ocamldep],[no])
  # 
  OCAMLDEP_FLAGS="-slash"
  AC_SUBST(OCAMLDEP_FLAGS)

  # checking for ocamlmktop
  AC_CHECK_TOOL([OCAMLMKTOP],[ocamlmktop],[no])
  # checking for ocamlmklib
  AC_CHECK_TOOL([OCAMLMKLIB],[ocamlmklib],[no])
  # checking for ocamldoc
  AC_CHECK_TOOL([OCAMLDOC],[ocamldoc],[no])
  # checking for ocamlbuild
  AC_CHECK_TOOL([OCAMLBUILD],[ocamlbuild],[no])
])


# Check for the existence of FILE.
# even when cross-compiling 

AC_DEFUN([AC_CHECK_FILE_ALWAYS],
[AS_VAR_PUSHDEF([ac_File], [ac_cv_file_$1])dnl
AC_CACHE_CHECK([for $1], [ac_File],
[if test -r "$1"; then
  AS_VAR_SET([ac_File], [yes])
else
  AS_VAR_SET([ac_File], [no])
fi])
AS_VAR_IF([ac_File], [yes], [$2], [$3])
AS_VAR_POPDEF([ac_File])dnl
])# AC_CHECK_FILE_ALWAYS

AC_DEFUN([AC_PROG_HTMLC],
[dnl
  AC_CHECK_TOOL([HTMLC_COMMAND],[htmlc],[no])
  AC_SUBST([HTMLC_COMMAND])
  AC_SUBST(HTMLC_FLAGS)
  HTMLC_CONF_INCLUDES=
  HTMLC_CONF_ENV=
  HTMLC_FLAGS="-lang en -honor_line_continuation true"
  AC_SUBST(HTMLC_CONF_INCLUDES)
  AC_SUBST(HTMLC_CONF_ENV)
  AC_SUBST(HTMLC_FLAGS)
])

AC_DEFUN([AC_PROG_OCAMLPRINTC],
[dnl
  AC_CHECK_TOOL([OCAMLPRINTC],[ocamlprintc],[no])
  AC_SUBST(OCAMLPRINTC)
  OCAMLPRINTC_FLAGS="-c"
  AC_SUBST(OCAMLPRINTC_FLAGS)
])


AC_DEFUN([AC_PROG_OCAMLLEX],
[dnl
  # checking for ocamllex
  AC_CHECK_TOOL([OCAMLLEX],[ocamllex],[no])
  if test "$OCAMLLEX" != "no"; then
    AC_CHECK_TOOL([OCAMLLEXDOTOPT],[ocamllex.opt],[no])
    if test "$OCAMLLEXDOTOPT" != "no"; then
	OCAMLLEX=$OCAMLLEXDOTOPT
    fi
  fi
  AC_SUBST([OCAMLLEX])
])

AC_DEFUN([AC_PROG_OCAMLYACC],
[dnl
  AC_CHECK_TOOL([OCAMLYACC],[ocamlyacc],[no])
  AC_SUBST([OCAMLYACC])
])

AC_DEFUN([AC_PROG_CAMLP4],
[dnl
  AC_REQUIRE([AC_PROG_OCAML])dnl

  # checking for camlp4
  AC_CHECK_TOOL([CAMLP4],[camlp4],[no])
  if test "$CAMLP4" != "no"; then
     TMPVERSION=`$CAMLP4 -v 2>&1| sed -n -e 's|.*version *\(.*\)$|\1|p'`
     if test "$TMPVERSION" != "$OCAMLVERSION" ; then
	AC_MSG_RESULT([versions differs from ocamlc])
        CAMLP4=no
     fi
  fi
  AC_SUBST([CAMLP4])

  # checking for companion tools
  AC_CHECK_TOOL([CAMLP4BOOT],[camlp4boot],[no])
  AC_CHECK_TOOL([CAMLP4O],[camlp4o],[no])
  AC_CHECK_TOOL([CAMLP4OF],[camlp4of],[no])
  AC_CHECK_TOOL([CAMLP4OOF],[camlp4oof],[no])
  AC_CHECK_TOOL([CAMLP4ORF],[camlp4orf],[no])
  AC_CHECK_TOOL([CAMLP4PROF],[camlp4prof],[no])
  AC_CHECK_TOOL([CAMLP4R],[camlp4r],[no])
  AC_CHECK_TOOL([CAMLP4RF],[camlp4rf],[no])
  AC_SUBST([CAMLP4BOOT])
  AC_SUBST([CAMLP4O])
  AC_SUBST([CAMLP4OF])
  AC_SUBST([CAMLP4OOF])
  AC_SUBST([CAMLP4ORF])
  AC_SUBST([CAMLP4PROF])
  AC_SUBST([CAMLP4R])
  AC_SUBST([CAMLP4RF])
])

AC_DEFUN([AC_PROG_FINDLIB],
[dnl
  AC_REQUIRE([AC_PROG_OCAML])dnl

  # checking for ocamlfind
  AC_CHECK_TOOL([OCAMLFIND],[ocamlfind],[no])
  AC_SUBST([OCAMLFIND])
])

dnl Thanks to Jim Meyering for working this next bit out for us.
dnl XXX We should define AS_TR_SH if it's not defined already
dnl (eg. for old autoconf).
AC_DEFUN([AC_CHECK_OCAML_PKG],
[dnl
  AC_REQUIRE([AC_PROG_FINDLIB])dnl

  AC_MSG_CHECKING([for OCaml findlib package $1])

  unset found
  unset pkg
  found=no
  for pkg in $1 $2 ; do
    if $OCAMLFIND query $pkg >/dev/null 2>/dev/null; then
      AC_MSG_RESULT([found])
      AS_TR_SH([OCAML_PKG_$1])=$pkg
      found=yes
      break
    fi
  done
  if test "$found" = "no" ; then
    AC_MSG_RESULT([not found])
    AS_TR_SH([OCAML_PKG_$1])=no
  fi

  AC_SUBST(AS_TR_SH([OCAML_PKG_$1]))
])

AC_DEFUN([AC_CHECK_OCAML_MODULE],
[dnl
  AC_MSG_CHECKING([for OCaml module $2])

  cat > conftest.ml <<EOF
open $3
EOF
  unset found
  for $1 in $$1 $4 ; do
    if $OCAMLC -c -I "$$1" conftest.ml >&5 2>&5 ; then
      found=yes
      break
    fi
  done

  if test "$found" ; then
    AC_MSG_RESULT([$$1])
  else
    AC_MSG_RESULT([not found])
    $1=no
  fi
  AC_SUBST([$1])
])


dnl XXX Cross-compiling
AC_DEFUN([AC_CHECK_OCAML_WORD_SIZE],
[dnl
  AC_REQUIRE([AC_PROG_OCAML])dnl
  AC_MSG_CHECKING([for OCaml compiler word size])
  cat > conftest.ml <<EOF
  print_endline (string_of_int Sys.word_size)
  EOF
  OCAML_WORD_SIZE=`$OCAML conftest.ml`
  AC_MSG_RESULT([$OCAML_WORD_SIZE])
  AC_SUBST([OCAML_WORD_SIZE])
])

AC_DEFUN([AC_CHECK_OCAML_OS_TYPE],
[dnl
  AC_REQUIRE([AC_PROG_OCAML])dnl
  AC_MSG_CHECKING([OCaml Sys.os_type])

  cat > conftest.ml <<EOF
  print_string(Sys.os_type);;
EOF

  OCAML_OS_TYPE=`$OCAML conftest.ml`
  AC_MSG_RESULT([$OCAML_OS_TYPE])
  AC_SUBST([OCAML_OS_TYPE])
])
