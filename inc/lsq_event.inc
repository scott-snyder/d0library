C----------------------------------------------------------------------
C-
C-   Created   3-MAR-1992   Rajendran Raja
C-   EVENT RELATED QUANTITIES
C-
C----------------------------------------------------------------------
      INTEGER NLIVE
      PARAMETER( NLIVE = 5 )
      COMMON /LSQ_EVENT/E0,ETOT,LYR_LIVE,LYR_DEAD,ON,ELIVE,EDEAD,NEV,
     &  EVINV
      REAL ETOT(25),E0
      INTEGER NEV
      REAL    EVINV
C----------------------------------------------------------------------
      INTEGER LYR_LIVE(8),LYR_DEAD(8),ON(25)
      REAL    ELIVE(NLIVE),EDEAD(NLIVE)
