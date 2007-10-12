#!/bin/SCILABGS
#/* -*- Mode: Shell-script -*- */
if test "$PRINTERS" = ""; then
  PRINTERS="lp"
fi
#############################################################################
#                       DO NOT MODIFY BELOW THIS LINE                       #
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

PATH=$PATH:$SCI:$SCI/util
export PATH

do_help()
{
echo "Usage:"
echo     "	nsp [-ns -nw -display display -f file  -l lang -args arguments]"
echo     "	nsp [-ns -nw -display display -e expression]"
echo     "	nsp -link <objects>"
exit
}

# calling Scilab with no argument or special cases of calling Scilab
rest="no"
case $# in
    0)
       $SCI/bin/zterm -e $SCI/bin/scilex $start_file $arguments &
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
      -show)
     	  sci_args="$sci_args $sciarg"
          ;;
      -echo)
     	  sci_args="$sci_args $sciarg"
          ;;
      -errcatch)
     	  sci_args="$sci_args $sciarg"
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
      -texmacs)
          sci_args="$sci_args -texmacs"
          ;;
      -nwni)
          now="-nw"
	      sci_args="$sci_args"
          ;;
      -tv) 
	  now="-nw"
          sci_args="$sci_args -tv"
	  ;;
      -display|-d)
          prevarg="display"
          ;;
       -f)
          prevarg="start_file"
          ;;
       -font)
	  prevarg="fontname"
	  ;;
       -n)
	  prevarg="history"
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

  zterm_arg=
  if test -n "$fontname"; then
    zterm_arg="-f $fontname"
  fi
  if test -n "$history"; then
    zterm_arg="$zterm_arg -n $history"
  fi

  if test -n "$now"; then
      sci_exe="$SCI/bin/scilex "
  else
      sci_exe="$SCI/bin/zterm  $zterm_arg -e  $SCI/bin/scilex"
  fi

  if test -n "$debug"; then 
     if test -n "$now"; then
        gdb $SCI/bin/scilex
     else
        $SCI/bin/zterm  $zterm_arg  -e gdb $SCI/bin/scilex
     fi
  else
      $sci_exe $sci_args 
  fi 
fi

