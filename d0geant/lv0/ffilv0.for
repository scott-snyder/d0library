      LOGICAL FUNCTION FFILV0()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Lv0 detector FFREAD cards defined here
C-
C-   Inputs  :
C-   Outputs : SLV0
C-
C-   Created   6-DEC-1988   A.M.Jonckheere
C-   Updated   2-JUN-1989   Harrison B. Prosper
C-   Made into a program-builder interface function
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
C
      INTEGER I
      LOGICAL PRT_FFILV0
C----------------------------------------------------------------------
      FFILV0 = .TRUE.
      IF ( DLV0 .LE. 0 ) GOTO 999
C
      DO I = 1, 10
        SLV0(I) = 0.0
      ENDDO
      SLV0(2) = 0.598
      SLV0(3) = 2.0
C
      CALL FFKEY('SLV0',SLV0,10,'REAL')
C
      ENTRY PRT_FFILV0
C
      PRT_FFILV0 = .TRUE.
      WRITE (LOUT,9000) SLV0
 9000 FORMAT(' FFILV0 ** SLV0 ',10F6.1)
C
  999 RETURN
      END
