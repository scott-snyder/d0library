      LOGICAL FUNCTION MATCHEM_MC_DATA(ETA_LEP,PHI_LEP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  28-JUN-1994   Meenakshi Narain
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      REAL    ETA_LEP,PHI_LEP,ETA_ISA,PHI_ISA
      REAL    DRMIN,DR,ETA_DIFF,PHI_DIFF,DIFF_PHI
      INTEGER ID,GZISAL,LISAL
C----------------------------------------------------------------------
      DRMIN=999.
      MATCHEM_MC_DATA = .FALSE.
C
C ** Get Link to first ISAL bank
C
      LISAL=GZISAL()
C
C ** Loop over all ISAL banks
C
      DO WHILE (LISAL.GT.0)
        ID  = IQ(LISAL+1)
        IF(ABS(ID).EQ.12)THEN
          PHI_ISA = Q(LISAL+7)
          ETA_ISA = Q(LISAL+9)
          PHI_DIFF = DIFF_PHI(PHI_ISA,PHI_LEP)
          ETA_DIFF = ETA_ISA-ETA_LEP
          DR = PHI_DIFF**2+ETA_DIFF**2
          IF(DR.GT.0)DR=SQRT(DR)
          IF(DR.LT.DRMIN)DRMIN=DR
        ENDIF
        LISAL = LQ(LISAL)
      ENDDO
      IF(DRMIN.LE.0.25)THEN
        MATCHEM_MC_DATA = .TRUE.
      ENDIF
C
  999 RETURN
      END
