      SUBROUTINE SAMUS_ZX_S
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Display Z-X View of SAMUS SOUTH (+Z)
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  14-MAY-1991 Sharon Hagopian
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INTEGER IVIEW,IFLAG
C______________________________________________________________________
      DATA IVIEW/3/
      DATA IFLAG/1/
C----------------------------------------------------------------------
      CALL PSAMAG(IVIEW,IFLAG)
      CALL PSAMDETC(IVIEW,IFLAG)
      CALL PSAMUD1_eV(IVIEW,IFLAG)
      CALL PSAM_TK(IVIEW,IFLAG)
  999 RETURN
      END
