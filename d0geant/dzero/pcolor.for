      SUBROUTINE PCOLOR(PID)
C*******************************************************************************
C
C      DRAWS PARTICLES IN COLOR ON ENVISION TERMINAL
C      3 NOV 1985 SLL
C
C******************************************************************************
      IMPLICIT NONE
      INTEGER PID
      CHARACTER*12 COLOR(44)
      DATA COLOR/'YELLOW',2*'GREEN','WHITE',2*'BLUE',38*'RED'/
      CALL GDCOLE(COLOR(PID))
      END
