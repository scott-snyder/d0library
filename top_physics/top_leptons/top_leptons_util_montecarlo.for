      LOGICAL FUNCTION TOP_LEPTONS_UTIL_MONTECARLO()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Test on Run No to look for Monte Carlo Data
C-                         Assumes that 'data' with Run nos below 40,000
C-                         are MonteCarlo (D0 standard -> Run 1)
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   8-JUL-1993   Stephen J. Wimpenny
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      INTEGER REC_TYPE,GZHEAD
C
      TOP_LEPTONS_UTIL_MONTECARLO=.FALSE.
      REC_TYPE=5
C
      LHEAD=GZHEAD()
      IF(LHEAD.NE.0) THEN
        REC_TYPE=IQ(LHEAD+1)
        IF(REC_TYPE.GE.1005) TOP_LEPTONS_UTIL_MONTECARLO=.TRUE.
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
