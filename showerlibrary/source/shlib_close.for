      SUBROUTINE SHLIB_CLOSE(IUNIT,TOP_DIRECTORY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CLOSE SHOWERLIBRARY
C-
C-   Inputs  :IUNIT = TOP UNIT
C-            TOP_DIRECTORY = NAME OF TOP DIRECTORY
C-   Outputs :
C-   Controls:
C-
C-   Created  22-FEB-1990   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IUNIT
      CHARACTER*(*) TOP_DIRECTORY
      CHARACTER*80 MSG
C----------------------------------------------------------------------
C        CALL RZEND(TOP_DIRECTORY)
        CLOSE(IUNIT)
        WRITE(MSG,1)IUNIT,TOP_DIRECTORY
    1   FORMAT(' CLOSED SHOWERLIBRARY: UNIT ',I5,
     &    ' TOP DIR: ',A25)
        CALL ERRMSG('SHOWERLIBRARY','SHLIB_CLOSE',MSG,'W')
  999 RETURN
      END
