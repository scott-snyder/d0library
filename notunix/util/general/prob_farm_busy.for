      FUNCTION PROB_FARM_BUSY(C,U)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate the Erlang B function, the probability that
C-   a queue no buffers is busy.  Arrival assumed Poisson
C-
C-   Inputs  : C  INTEGER # servers
C-             U = intensity in Erlangs = Arrival rate * <Service Time>
C-   Outputs : The probability
C-   Controls:
C-
C-   Created  12-JUN-1993   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER C,I
      REAL    PROB_FARM_BUSY,U
      DOUBLE PRECISION TERM,SUM,BIG
      PARAMETER( BIG  = 1.0E20 )
C----------------------------------------------------------------------
      TERM = 1
      SUM = 1
      DO I = 1,C
        IF (TERM.GT.BIG.OR.SUM.GT.BIG) GO TO 100
        TERM = TERM*(C-I+1)/U
        SUM = SUM + TERM
      ENDDO
  100 CONTINUE
      PROB_FARM_BUSY = 1/SUM
  999 RETURN
      END
