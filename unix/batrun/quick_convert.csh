#! /bin/csh -f
source `uff $d0unix/d0local.cshrc`
#------------------------------------------------------------------------------
#
# Name      : Quick_convert
#
# Purpose   : Convert VAX style code to $D0FLAVOR
#
# Usage     :
#
# % quick_convert file_list
#
# Options   :
#
# none
#
# Arguments :
#
# file_list - a list of files to be converted, may contain wildcards.
#
# Created 08-FEB-1994   Michael J. Wendling
#
#------------------------------------------------------------------------------
set flavor = $D0FLAVOR
#
#
foreach file ( $argv ) 
   set out = (`echo $file | /bin/awk -F. '{print $1".f"}' `)
   /bin/echo -n "$out "
   `uff $d0unix/tasteofd0` $flavor < $1 | `uff $d0unix/vmstounix` > $out
end
#
exit:
exit 0
