      SUBROUTINE DFCLOS_OLD(ILUN,IERR)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Close a d0dad file
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  11-Jan-1994   John D. Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ILUN,IERR
C-----------------------------------------------------------------------
      IERR=0
      CALL CFCLOS(ILUN,0)
  999 RETURN
      END
