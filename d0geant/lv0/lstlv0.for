      LOGICAL FUNCTION LSTLV0
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Lv0 Detector rap up
C-                                 Called from UGLAST
C-
C-   Inputs  : NONE
C-   Outputs : NONE as of now
C-
C-   Created   6-DEC-1988   A.M.Jonckheere
C-   Updated   2-JUN-1989   Harrison B. Prosper
C-   Made into a program-builder interface function
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
C----------------------------------------------------------------------
      LSTLV0 = .TRUE.
      IF ( DLV0 .LT. 2 ) GOTO 999
C
  999 RETURN
      END
