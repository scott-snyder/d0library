      FUNCTION NP_LEP()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Concatenate the streams for the New Phen group's
C-                         leptonic searches: 2EM,ENU,2NU,LSS,MSP
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
      LOGICAL NP_LEP,NP_LEP_EOJ
      LOGICAL NP_LQ_2EM_TIGHT,LQNUE,LQNN,NP_LSS_TIGHT,NP_MSP
      LOGICAL NP_WRIGHT_TIGHT,NP_MULQ_TIGHT
      LOGICAL L1,L2,L3,L4,L5,L6,L7
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
      L1 = NP_LQ_2EM_TIGHT()
      L2 = LQNUE()
      L3 = LQNN()
      L4 = NP_LSS_TIGHT()
      L5 = NP_MSP()
      L6 = NP_WRIGHT_TIGHT()
      L7 = NP_MULQ_TIGHT()
      NP_LEP = L1 .OR. L2 .OR. L3 .OR. L4 .OR. L5 .OR. L6 .OR. L7
      IF (L1) RSUMMARY(1) = RSUMMARY(1) + 1
      IF (L2) RSUMMARY(2) = RSUMMARY(2) + 1
      IF (L3) RSUMMARY(3) = RSUMMARY(3) + 1
      IF (L4) RSUMMARY(4) = RSUMMARY(4) + 1
      IF (L5) RSUMMARY(5) = RSUMMARY(5) + 1
      IF (L6) RSUMMARY(6) = RSUMMARY(6) + 1
      IF (L7) RSUMMARY(7) = RSUMMARY(7) + 1
      IF (NP_LEP) RSUMMARY(8) = RSUMMARY(8) + 1
C
  999 RETURN
C
      ENTRY NP_LEP_EOJ(RSUM)
      DO I = 1,20
        RSUM(I) = RSUMMARY(I)
      ENDDO
      RETURN
      END
