      FUNCTION NP_HAD()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Concatenate the streams for the New Phen group's
C-                         hadronic searches: TAU,SET,SSY
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  28-JUN-1993   K. Wyatt Merritt
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL NP_HAD,NP_HAD_EOJ
      LOGICAL NP_CLEAN_TAU,NP_SCALAR_TIGHT,NP_SQGL_TIGHT
      LOGICAL L1,L2,L3
      LOGICAL FIRST
      SAVE FIRST
C
      INTEGER I
C
      REAL RSUMMARY(20),RSUM(20)
C
      DATA FIRST / .TRUE. /
C----------------------------------------------------------------------
      IF( FIRST ) THEN
        FIRST = .FALSE.
        CALL VZERO(RSUMMARY,20)
      ENDIF
      L1 = NP_CLEAN_TAU()
      L2 = NP_SCALAR_TIGHT()
      L3 = NP_SQGL_TIGHT()
      NP_HAD = L1 .OR. L2 .OR. L3
      IF (L1) RSUMMARY(1) = RSUMMARY(1) + 1
      IF (L2) RSUMMARY(2) = RSUMMARY(2) + 1
      IF (L3) RSUMMARY(3) = RSUMMARY(3) + 1
      IF (NP_HAD) RSUMMARY(4) = RSUMMARY(4) + 1
C
  999 RETURN
C
      ENTRY NP_HAD_EOJ(RSUM)
      DO I = 1,20
        RSUM(I) = RSUMMARY(I)
      ENDDO
      RETURN
      END
