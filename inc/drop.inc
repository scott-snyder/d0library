C----------------------------------------------------------------------
C-
C-   Created   5-AUG-1991   K. Wyatt Merritt
C-   Contains storage array of energies for weighting used in N. Amos'
C-   pruning algorithm.  Also some sums for debugging the algorithm.
C-
C----------------------------------------------------------------------
      REAL EDROP,LEFTOVER,PRUNED
      REAL UNWEIGHTED,EPRUNED,KEPRUNED
      INTEGER NSTORED,NPRUNED,NSEC_SHWR,NPR
      COMMON /DROP/ EDROP(13,-50:15),LEFTOVER,PRUNED,
     &  UNWEIGHTED,EPRUNED,KEPRUNED,NSTORED,NPRUNED,NSEC_SHWR,
     &  NPR(-50:15)
