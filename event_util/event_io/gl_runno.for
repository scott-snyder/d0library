      FUNCTION GL_RUNNO()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-      Return GLOBAL run value for last event in memory
C-
C-   Inputs  : none
C-   Outputs : GL_RUNNO [I] the global run number
C-   Controls: 
C-
C-
C-   Created 13-APR-1992   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER GL_RUNNO,LASTRN
      SAVE LASTRN
      DATA LASTRN/0/
C----------------------------------------------------------------------
      IF(LHEAD.NE.0) THEN
        LASTRN=IQ(LHEAD+12)
      ENDIF
      GL_RUNNO=LASTRN
  999 RETURN
      END
