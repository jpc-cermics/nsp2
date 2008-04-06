#!/bin/sh
# Copyright INRIA
if test "$SCI" = ""; then
  SCI="NSP_DIRECTORY"
fi
export SCI
$SCI/bin/Slatexpr2 $*
