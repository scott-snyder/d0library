      FUNCTION R_ERLANG_SOLVE(R,I)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Function for CERNLIB 205 RZERO zero finder
C-
C-   Returned value  : values of (desired deadtime - actual deadtime)
C-   Inputs  : R, traffic intensity/processor in erlangs
C-             I = 1: first call, 2 = 2nd
C-   Controls: /QUEUE_SOLVE/  FARM_QUEUE controls queue type to solve
C-
C-   Created  21-JUN-1993   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    R,PROB_QUEUE_BUSY,R_ERLANG_SOLVE
      INTEGER I
      INCLUDE 'D0$INC:QUEUE_SOLVE.INC'
C----------------------------------------------------------------------
       R_ERLANG_SOLVE =
     &  P_DEAD_IN - PROB_QUEUE_BUSY(P_IN,K_IN,R)
  999 RETURN
      END
