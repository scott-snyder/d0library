C----------------------------------------------------------------------
C-                                                      L2JETS_ALGO.INC
C-   Created  14-MAY-1990   Richard V. Astur
C-
C-  Common block for algorithm parameters - common block L2JETS_ALGO
C----------------------------------------------------------------------
      INTEGER IEVT_ALGO                 
C      ICELL_USED is used to keep track of which cells have been claimed
C      by a jet an hence are unavailable to other jet candidates.
      INTEGER ICELL_USED(-NETAL1:NETAL1,NPHIL1,NPAR_IND_MAX)

      COMMON /L2JETS_ALGO/ IEVT_ALGO,ICELL_USED
