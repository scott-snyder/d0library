      FUNCTION PROB_BUFFER_BUSY(N,U)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Evaluate deadtime fraction in a buffered system
C-
C-   Returned value  : dead fraction (probability queue is full)
C-   Inputs  : N  number of buffers (0 means a simple server w/o queue)
C-             U  traffic intensity in erlangs: Input Hz * <Tservice>
C-   Outputs :
C-   Controls:
C-
C-   Created  21-JUN-1993   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    PROB_BUFFER_BUSY,U,W,BIG
      PARAMETER( BIG = 20.  )
      INTEGER N
C----------------------------------------------------------------------
      IF (U.EQ.1.0) THEN
        PROB_BUFFER_BUSY = 1/FLOAT(N+2)
      ELSE
        IF (U.GT.1.0) THEN
          IF (((N+1)*LOG10(U)).GT.BIG) THEN
            PROB_BUFFER_BUSY = 1 - 1/U    !avoid overflow: W large limit
            GO TO 999
          ENDIF
        ENDIF
        W = U**(N+1)
        IF (W.LE.100.) THEN
          PROB_BUFFER_BUSY = (1-U)*W/(1-U*W)        !small W (standard) case
        ELSE
          PROB_BUFFER_BUSY = (1-1/U)/(1-1/(U*W))    !large W case
        ENDIF
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
