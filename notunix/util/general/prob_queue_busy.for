      FUNCTION PROB_QUEUE_BUSY(C,K,R)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : get busy fraction for a simple buffer or farm queue
C-
C-                M/M/C/K queues (table 8, p 360 of A.O. Allen,
C-                      Probability, Statistics, and Queueing Theory)
C-
C-      1 event per processor, plus possibly K-C more buffers for waiting
C-
C-   Returned value  : dead fraction (probability queue is full and all
C-                        processors are busy)
C-   Inputs  : C    number of processors
C-             K    total number of events system can hold
C-             R    traffic intensity/processor = U/C
C-             R    intensity in Erlangs/processor
C-                    = Arrival rate/C * <Service Time>
C-   Outputs :
C-   Controls:
C-
C-   Created 16-AUG-1995   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER C,B,K,L
      REAL    U,R,PROB_QUEUE_BUSY,ARG
      DOUBLE PRECISION RB,TERM,SUM,BIG,SUMB
      PARAMETER( BIG  = 1.0E06 )
      LOGICAL EXACT
      COMMON/IF_EXACT/EXACT !set in q_SOLVE main
C----------------------------------------------------------------------
      U = C*R
      B = K - C   !number of extra buffers in system
      IF (B*LOG10(R).GT.37) THEN
        PROB_QUEUE_BUSY=1.0
      ELSE
        RB = R**B   !common part of deadtime between all terms
        IF (R.LT.1.0) THEN
          IF (RB.EQ.0) RB = 1/BIG
          SUM = (1 - RB*R)/(1-R)/RB   !this is the part due to extra buffers
        ELSE
          SUM = 0 !can't use summation formula
          TERM = 1.0/RB
          DO L = 0,B
            SUM = SUM + TERM
            TERM = TERM*R
          ENDDO
        ENDIF
        TERM = 1/RB !L=0 term
C
C...calculate 1/deadtime; if it gets big, forget it, since all terms positive
        DO L = C,1,-1     !adding part due to # processors
          IF (TERM.GT.BIG.OR.SUM.GT.BIG) GO TO 100
          TERM = TERM*L/U
          SUM = SUM + TERM
        ENDDO
  100   CONTINUE
        PROB_QUEUE_BUSY = 1/SUM
      ENDIF
      IF(.NOT.EXACT) THEN
C        PROB_QUEUE_BUSY = (R**K) * EXP(C*(1-R))/SQRT(TWOPI*C) !really crude
        IF(R.LT.1/BIG) THEN
          PROB_QUEUE_BUSY = 1/BIG
        ELSEIF(R.LE. 1.2)  THEN
          SUMB = R*B/RB
          ARG = (1 - R + LOG(R))*C
          IF (ARG.LT.-LOG(BIG)) THEN
            PROB_QUEUE_BUSY = 1.0/BIG
          ELSE
            TERM =  SQRT(TWOPI*C)/ (RB* EXP(ARG)) !Theta = 1
            SUM = MIN(BIG,SUMB + TERM)
            PROB_QUEUE_BUSY = 1.0/SUM
          ENDIF
        ELSE
          PROB_QUEUE_BUSY = 1 - 1/R
        ENDIF
      ENDIF
  999 RETURN
      END
