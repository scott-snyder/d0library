      SUBROUTINE OUTSAV
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Save anything written to unit 6 in a file
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: Uses FILSAV in /COMCHR/ as file name
C-
C-   Documented 22-SEP-1988   Jan S. Hoftun
C-   Updated   8-OCT-1991   Herbert Greenlee
C-
C----------------------------------------------------------------------
      IMPLICIT NONE 
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
C---------------------------------------------------------------------
C&IF VAXVMS
      OPEN(UNIT=6,FILE=FILSAV,STATUS='NEW')
C&ELSE
C&      OPEN(UNIT=6,FILE=FILSAV,STATUS='UNKNOWN')
C&ENDIF
      RETURN
      END
