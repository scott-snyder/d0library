      FUNCTION FTRAKS_EXM()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Control FTRAKS Examine2 analysis
C-
C-   Created   8-FEB-1991  Robert E. Avery 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL FTRAKS_EXM
      LOGICAL FTRAKS
      LOGICAL OK
C
C----------------------------------------------------------------------
C
      OK = FTRAKS()
      FTRAKS_EXM = .TRUE.
C
  999 CONTINUE
      RETURN
      END
