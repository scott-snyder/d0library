      FUNCTION QSTEP(PART_ID)

C       This step function returns 1 if the squark PART_ID is coupled, 0
C       if not. (Coupled meaning Q>Msquark).

      IMPLICIT NONE
      INTEGER PART_ID
      REAL*8 QSTEP
      INCLUDE 'D0$SPYTHIA$INC:DIFFEQ.INC'

      IF (SQLOG(PART_ID)) THEN
        QSTEP=1.0
      ELSE
        QSTEP=0.0
      ENDIF

C       Qstep=1.0

      RETURN
      END
