      SUBROUTINE HMATRIX_RZ_CLOSE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Close an existing RZ file.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  17-DEC-1990   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:HMATRIX_PARS.INC'
C----------------------------------------------------------------------
      CALL RZEND(TOP_DIRECTORY)
      CLOSE(UNIT=RZ_UNIT)
      CALL INTMSG(' RZ file is now closed')
  999 RETURN
      END
