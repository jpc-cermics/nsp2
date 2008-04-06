#!/bin/sh 
# Copyright ENPC
if test "$SCI" = ""; then
  SCI="NSP_DIRECTORY"
fi
export SCI
$SCI/util/Slatdocs $*
