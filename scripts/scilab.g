#!/bin/SCILABGS
# Warning : some old versions of sh dont allow inline function definitions
# like do_scilex()... . In this case use a system V sh (sh5)

# Copyright INRIA

if test "$PRINTERS" = ""; then
  PRINTERS="lp"
fi
#############################################################################
#                                                                           #
#                       DO NOT MODIFY BELOW THIS LINE                       #
#                                                                           #
#############################################################################
if test "$SCI" = ""; then
  SCI="SCILAB_DIRECTORY"
fi
export SCI
if test "$DISPLAY" = ""; then
  DISPLAY="unix:0.0"
fi
export DISPLAY

export PRINTERS
VERSION="SCILAB_VERSION"
export VERSION

if test "$PVM_ROOT" = ""; then
  PVM_ROOT="$SCI/pvm3"
fi
if test "$PVM_ARCH" = ""; then
  PVM_ARCH=`$PVM_ROOT/lib/pvmgetarch`
fi
export PVM_ROOT PVM_ARCH

#TCL_LIBRARY

#TK_LIBRARY

do_scilex()
{
    PATH=$PATH:$SCI:$SCI/util
    export PATH
    XAPPLRESDIR=$SCI/X11_defaults
    export XAPPLRESDIR
    tty -s && stty kill '^U' intr '^C' erase '^H' quit '^\' eof '^D' susp '^Z'
    $SCI/bin/scilex $* 
}

do_scilex_now()
{
    PATH=$PATH:$SCI:$SCI/util
    export PATH
    XAPPLRESDIR=$SCI/X11_defaults
    export XAPPLRESDIR
    $SCI/bin/scilex $* 
}

do_help()
{
echo "Usage:"
echo     "	scilab [-ns -nw -display display -f file  -l lang -args arguments]"
echo     "	scilab [-ns -nw -display display -e expression]"
echo     "	scilab -link <objects>"
exit
}


do_compile()
{
	umask 002
	rm -f report
	name=`basename $1 .sci`
	echo generating $name.bin
	echo "predef();getf('$name.sci');save('$name.bin');quit"\
	      | $SCI/bin/scilex -ns -nw | sed 1,8d 1>report 2>&1
	if (grep error report 1> /dev/null  2>&1);
	then cat report;echo " " 
	   echo see `pwd`/report for more informations
	   grep libok report>/dev/null; 
	else rm -f report;
	fi
	umask 022
	exit 0
}

do_lib()
{
	umask 002
	rm -f report
	echo "$1=lib('$2/');save('$2/lib',$1);quit"\
	      | $SCI/bin/scilex -ns -nw |sed 1,/--\>/d 1>report 2>&1
	if (grep error report 1> /dev/null  2>&1);
	then cat report;echo " " 
		echo see `pwd`/report for more informations
		grep libok report>/dev/null; 
	else rm -f report;
	fi
	umask 022
	exit 0
}


do_print() 
{
	$SCI/bin/BEpsf $1 $2 
	lpr -P$3 $2.eps
	rm -f $2 $2.eps

}

do_save() 
{
	case $3 in 
          Postscript)
		$SCI/bin/BEpsf $1 $2 
          	 ;;
          Postscript-Latex)
		$SCI/bin/Blatexpr $1 1.0 1.0 $2 
	   	;;
	  Xfig)
		case $1 in
		-portrait)
			mv $2 $2.fig
		;;
		-landscape)
			sed -e "2s/Portrait/Landscape/" $2 >$2.fig
			rm -f $2
		;;
		esac
           	;;
          Gif)
		case $1 in
		-portrait)
			mv $2 $2.gif
		;;
		-landscape)
			mv $2 $2.gif
		;;
		esac
           	;;
	esac
}

# calling Scilab with no argument or special cases of calling Scilab
rest="no"
case $# in
    0)
	do_scilex &
        ;;
    2)
        case $1 in
            -comp)
		do_compile $2
                ;;
            -link)
                shift
		$SCI/bin/scilink $SCI $*
                ;;
            -function)
		$SCI/bin/minfopr $SCI $2
		;;
            *)
		rest="yes"
                ;;
        esac
        ;;
    3)
        case $1 in
            -lib)
		do_lib $2 $3
                ;;
            -print_l)
                do_print -landscape $2 $3
                ;;
            -print_p)
                do_print -portrait $2 $3
                ;;
            -save_l)
                do_save -landscape $2 $3
                ;;
            -save_p)
                do_save -portrait $2 $3
                ;;
            -link)
                shift
		$SCI/bin/scilink $SCI $*
                ;;
            *)
		rest="yes"
                ;;
        esac
        ;;
    *)
        case $1 in
            -link)
                shift
		$SCI/bin/scilink $SCI $*
                ;;
            *)
		rest="yes"
                ;;
        esac
        ;;
esac


# really calling Scilab with arguments

sci_args=

if test "$rest" = "yes"; then
  debug=
  now=
  display=
  start_file=
  prevarg=
  language=
  for sciarg 
  do
    # If the previous argument needs an argument, assign it.
    if test -n "$prevarg"; then
      eval "$prevarg=\$sciarg"
      prevarg=
      continue
    fi
    case $sciarg in
      -ns)
	  sci_args="$sci_args -ns"
          ;;
      -nb)
	  sci_args="$sci_args -nb"
          ;;
      -debug) 
          debug="-debug"
          ;;
      -nw)
          now="-nw"
	  sci_args="$sci_args -nw"
          ;;
      -nwni)
          now="-nw"
	  sci_args="$sci_args -nw"
          ;;
      -display|-d)
          prevarg="display"
          ;;
       -f)
          prevarg="start_file"
          ;;
       -l)
          prevarg="language"
          ;;
       -e)
          prevarg="start_exp"
          ;;
       -args)
           prevarg="arguments"
          ;;

      *)
          do_help
          ;;
    esac
  done

  if test -n "$display"; then
    sci_args="$sci_args -display $display"
  fi

  if test -n "$start_file"; then
    sci_args="$sci_args -f $start_file"
  fi

  if test -n "$start_exp"; then
    sci_args="$sci_args -e $start_exp"
  fi

  if test -n "$language"; then
    sci_args="$sci_args -l $language"
  fi

  if test -n "$now"; then
      sci_exe="do_scilex_now"
  else
      sci_exe="do_scilex"
  fi

  if test -n "$debug"; then 
     gdb  $SCI/bin/scilex
  else	
      $sci_exe $sci_args 
  fi 
fi
