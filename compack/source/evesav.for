      SUBROUTINE EVESAV
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Edit what was written to UNIT 6
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  20-APR-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
C&IF VAXVMS
      INTEGER ISTAT,LIB$DELETE_FILE
C---------------------------------------------------------------------
      CLOSE(6)
      CALL EVEFIL(FILSAV//'/READONLY')
      ISTAT=LIB$DELETE_FILE(FILSAV//';*')
C&ENDIF
      RETURN
      END
