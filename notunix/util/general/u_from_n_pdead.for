      FUNCTION U_FROM_N_PDEAD(U,I)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Function for CERNLIB 205 RZERO zero finder
C-
C-   Returned value  : values of (desired deadtime - actual deadtime)
C-   Inputs  : U, traffic intensity in erlangs
C-             I = 1: first call, 2 = 2nd
C-   Controls: /QUEUE_SOLVE/  FARM_QUEUE controls queue type to solve
C-
C-   Created  21-JUN-1993   James T. Linnemann
C-
C----------------------------------------------------------------------
      REAL    U,PROB_QUEUE_BUSY,U_FROM_N_PDEAD
      INTEGER I
      INCLUDE 'D0$INC:QUEUE_SOLVE.INC'
C----------------------------------------------------------------------
      U_FROM_N_PDEAD = P_DEAD_IN - PROB_QUEUE_BUSY(N_IN,U,FARM_QUEUE)
  999 RETURN
      END
