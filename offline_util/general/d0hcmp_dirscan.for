      SUBROUTINE D0HCMP_DIRSCAN(CHPATH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : userexit routine for ZEBRA RZSCAN
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   5-MAY-1992   Krzysztof L. Genser
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      CHARACTER*(*) CHPATH
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:D0HCMP.DEF/LIST'
      INCLUDE 'D0$INC:D0HCMP_HST_RCP.INC/LIST'
C----------------------------------------------------------------------
      NDIRN = NDIRN + 1
      HSUBDIRS(NDIRN)=CHPATH(1:LENOCC(CHPATH))
C      PRINT *,HSUBDIRS(NDIRN)(1:LENOCC(HSUBDIRS(NDIRN)))
      RETURN
      END
