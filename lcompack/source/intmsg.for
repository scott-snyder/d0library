      SUBROUTINE INTMSG(STRING)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Output string to top part of split screen if
C-                         possible. VAX-specific.
C-
C-   Inputs  : STRING: Characters to be output
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 22-SEP-1988   Jan S. Hoftun
C-   Updated  21-OCT-1988   Jan S. Hoftun   (Take out part never executed)
C-   Updated   7-NOV-1990 Scott Snyder - scrolling viewport stuff
C-   Updated  22-FEB-1991 Scott Snyder - restore cursor position properly.
C-   Updated  31-MAR-1991 Scott Snyder - add call to LIBIND.
C-   Updated  23-APR-1991 Scott Snyder - remove call to LIBCOP. (see LIBPST)
C-   Updated  13-MAY-1991 Scott Snyder - restrict maximum number of lines
C-   Updated   3-OCT-1991 Herbert Greenlee
C-   Modified    13-AUG-1992 sss - compile on ibm
C-   Updated  11-Feb-1993 Herbert Greenlee
C-      Line mode version
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) STRING
      CALL OUTMSG(STRING)
      RETURN
      END
