      SUBROUTINE FCCLOS(ILUN,IERR)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Close a file catalog
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  11-Jan-1994   John D. Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  ILUN,IERR
C-----------------------------------------------------------------------
C
      CALL FCLSET(ILUN,IERR)
C
      IERR=0
      CLOSE(ILUN,ERR=998)
  999 RETURN
C
 998  CONTINUE
      IERR = -1
      RETURN
      END
