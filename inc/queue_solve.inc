C----------------------------------------------------------------------
C-
C-   Created  21-JUN-1993   James T. Linnemann
C-        for passing arguments into function needed by RZERO to solve for u
C----------------------------------------------------------------------
      REAL    P_DEAD_IN
      INTEGER P_IN,K_IN
      COMMON /QUEUE_SOLVE/ P_IN,K_IN,P_DEAD_IN
