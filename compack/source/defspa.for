      SUBROUTINE DEFSPA(INUM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set default line spacing for menu display
C-
C-   Inputs  : INUM: Line spacing to use
C-   Outputs : None
C-   Controls: LINSPA in /COMNUM/ set to INUM if valid
C-
C-   Documneted 22-SEP-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER INUM
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
C----------------------------------------------------------------------
      IF(INUM.GT.0.AND.INUM.LT.4) THEN
        LINSPA=INUM
      ELSE
        CALL OUTMSG('0Illegal line spacing in DEFSPA'//CHAR(7))
        CALL OUTMSG(' ')
      ENDIF
      RETURN
      END
