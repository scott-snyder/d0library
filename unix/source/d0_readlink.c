#include "unix.h"

long d0_readlink_(char *path, char *buf, long pathlen, long buflen)

/*
C----------------------------------------------------------------------
C-
C-   Name: d0_readlink
C-
C-   Purpose: This subroutine is a fortran callable interface to the c 
C-            run-time library routine readlink.  This fuuction returns
C-            the contents of a symbolic link. 
C-
C-   Fortran usage:
C-
C-      ok = d0_readlink(path, buf, bufsiz)
C-
C-   Returned value:  .TRUE. = Success
C-
C-   Arguments:
C-
C-      path (character, read only) - Pathname containing symbolic link
C-      buf (character, write only) - Buffer to receive result.
C-      bufsiz (integer, read only) - Size of buffer.
C-
C-   Created   13-May-1993   Herbert Greenlee
C-
C----------------------------------------------------------------------
*/

{
  int ok;
  char cpath[STRLEN];

  cstring(path, pathlen, cpath, STRLEN);
  fstring("", buf, buflen);
  ok = readlink(cpath, buf, buflen);
  if(ok >= 0)
    return 1;
  else
    return 0;
}
