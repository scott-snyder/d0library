      SUBROUTINE DEFCOL(INUM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : User callable routine to set number of columns
C-                         in menu display.
C-
C-   Inputs  : INUM: Number of columns to become the default
C-   Outputs : None
C-   Controls: NUMCOL in /COMNUM/ set to INUM if valid
C-
C-   Documnetyed 22-SEP-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER INUM
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
C----------------------------------------------------------------------
      IF(INUM.GT.0.AND.INUM.LT.11) THEN
        NUMCOL=INUM
      ELSE
        CALL OUTMSG('0Illegal # of columns in DEFCOL'//CHAR(7))
        CALL OUTMSG(' ')
      ENDIF
      RETURN
      END
