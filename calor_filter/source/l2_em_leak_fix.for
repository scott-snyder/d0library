      SUBROUTINE L2_EM_LEAK_FIX(IETA_CAND, DEPTH, LEAK_COR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Correct for ELECTRON leakage outside of 
C-                         3x3 window at high IETA 
C-
C-   Inputs  : IETA_CAND    detector IETA of L2_EM candidate 
C-   Outputs : LEAK_COR     correction factor to boost candidate Et
C-   Controls: NONE         who needs control?
C-
C-   Created  22-MAR-1993   Michael A. Tartaglia
C-   Modified 29-MAR-1993   James T. McKinley - Add DEPTH to parameter list
C-                          and warning message if non-standard depth is used
C-                          for EM cluster (can happen in L2_EM_ISOL)
C-   Modified 29-MAR-1993   Michael A. Tartaglia - fix correction function
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IETA_CAND,DEPTH
      REAL FRAC3X3, LEAK_COR, ETA, A0,A1,A2,A3
      PARAMETER (A0= 0.8121)
      PARAMETER (A1= 0.2342)
      PARAMETER (A2=-0.08401)
      PARAMETER (A3= 0.004919)
C----------------------------------------------------------------------
C- CORRECT ONLY IF ABS(IETA).GT.15
C
      LEAK_COR = 1.0
      IF(IABS(IETA_CAND).LT.15) GO TO 999
C
C- CALCULATE THE CORRECTION FACTOR:
C-  BASED UPON PARAMETRIZATION OF 3X3 CONTAINED FRACTION (FRAC3X3)
C-      (parametrized as a function of IETA/10... not real ETA)
C-  FROM PLATE-LEVEL MONTE CARLO (AGREES WITH W,Z TIGHT ELECS)
C
      IF(DEPTH .NE. 4)THEN
        CALL ERRMSG('L2_EM_LEAK_FIX','NON-STANDARD DEPTH',
     &    'EM shower leakage correction based on 4 EM floors only','W')
      ENDIF
C
      ETA = FLOAT(IABS(IETA_CAND))/10.
      FRAC3X3 = A0 + ETA*(A1 + ETA*(A2 + ETA*A3))
      LEAK_COR = 1./FRAC3X3
C----------------------------------------------------------------------
  999 RETURN
      END
