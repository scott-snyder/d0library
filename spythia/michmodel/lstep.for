      FUNCTION LSTEP(PART_ID)

C       This step function returns 1 if the slepton PART_ID is coupled, 0
C       if not. (Coupled meaning Q>Mslepton).

      IMPLICIT NONE
      INTEGER PART_ID
      REAL*8 LSTEP
      INCLUDE 'D0$SPYTHIA$INC:DIFFEQ.INC'

      IF (SLLOG(PART_ID)) THEN
        LSTEP=1.0
      ELSE
        LSTEP=0.0
      ENDIF

C       Lstep=1.0

      RETURN
      END
