      FUNCTION FGEAN_CHK()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check if running D0GEANT.
C-
C-   ENTRY FGEAN_SET: Set status of D0GEANT
C-
C-   Inputs  : none
C-   Outputs : 
C-   Controls: 
C-
C-   Created   7-JAN-1992   Susan K. Blessing
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL FGEAN_CHK,FGEAN_SET
      LOGICAL GEAN
C
      DATA GEAN/.FALSE./
C
C----------------------------------------------------------------------
C
      FGEAN_CHK = GEAN
C
      RETURN
C
C----------------------------------------------------------------------
      ENTRY FGEAN_SET()
C
      GEAN = .TRUE.
      FGEAN_SET = .TRUE.
C
  999 RETURN
      END
