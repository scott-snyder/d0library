      REAL FUNCTION MUJUMP()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return full jumper length for WMAUS.
C_                         This fuction identify real data or MC event
C-                         automatically and return right value. 
C-
C-   Inputs  : None
C-   Outputs : None
C-   Return  : full jumper length for WMAUS
C-
C-   Created   7-MAY-1992   Atsushi Taketani
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE  'D0$INC:ZEBCOM.INC'
      INCLUDE  'D0$INC:ZEBSTP.INC'
      REAL     JUMPER
      SAVE     JUMPER               ! save jumper length for UNIX
      INTEGER  KMGEH, GZMGEH
      LOGICAL   FIRST
      DATA      FIRST /.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        IF ( IQ(LHEAD+1).LE.999 ) THEN
          JUMPER = 15.64     ! Jumper length by tape measured
        ELSE
          JUMPER = 10.0      ! Hard wired jumper length in MSTVC (GEANT)
          KMGEH = GZMGEH(1)
          IF ( KMGEH.NE.0 ) THEN
            IF ( IC(KMGEH+1).GE.3 ) THEN
              JUMPER = C(KMGEH+23)
            END IF
          END IF
        END IF
        FIRST = .FALSE.
      END IF
C
      MUJUMP = JUMPER
C
  999 RETURN
      END
