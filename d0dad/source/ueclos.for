      SUBROUTINE UECLOS(LUN,IERR)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Close an unsorted event catalog.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  11-Jan-1994   John D. Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  LUN,IERR
C-----------------------------------------------------------------------
C
      CALL UELSET(LUN,IERR)
C
      IERR=0
      CLOSE(LUN,ERR=998)
  999 RETURN
C
 998  CONTINUE
      IERR = -1
      RETURN
      END
