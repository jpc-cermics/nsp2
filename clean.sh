#!/bin/sh
#------------------
find . \( -name CVS -o -name .libs \) -exec \rm -f -r {} \;
find . \( -name '*.o' -o -name '*.so' -o -name '*.a' \)  -exec \rm -f {} \;
find . \( -name '*.bin' -o -name '.nsp_history' \)  -exec \rm -f  {} \;
find . \( -name '*.X' -o -name '*.save' \)  -exec \rm -f  {} \;
find . \( -name 'TAGS' -o -name '.#*' \)  -exec \rm -f  {} \;
