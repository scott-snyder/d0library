      LOGICAL FUNCTION BOKLV0
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book all LV0 Histograms - # 5000-5999
C-
C-   Inputs  : Logical Flags
C-   Outputs : None
C-
C-   Created  6-DEC-1988   A.M.Jonckheere
C-
C-   Updated   2-JUN-1989   Harrison B. Prosper
C-   Made into a program-builder interface function
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
C----------------------------------------------------------------------
      BOKLV0 = .TRUE.
      IF ( DLV0 .LT. 2 ) GOTO 999
  999 RETURN
      END
