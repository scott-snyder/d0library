      LOGICAL FUNCTION TRDFIX()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  23-JUN-1995   Lewis Taylor Goss
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LTRDT,GZTRDT
      REAL EPSILON_T
      LOGICAL ACCEPTANCE,FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL TRD_ANALYSIS_INI
      ENDIF
C
      LTRDT = GZTRDT()
      TRDFIX = .TRUE.
C
      DO WHILE (LTRDT.NE.0)
        CALL TRD_ANALYSIS(LTRDT,EPSILON_T,ACCEPTANCE)
        LTRDT = LQ(LTRDT)
      ENDDO
  999 RETURN
      END
