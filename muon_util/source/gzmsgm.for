      INTEGER FUNCTION GZMSGM(MODULE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return MSGM bank address
C-
C-   Inputs  : Module  : muon module ID
C-   Outputs : None
C-   Controls: None
C-
C-   Created   1-NOV-1992   Atsushi Taketani
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER  LMSGH, GZMSGH
      INTEGER  MODULE
C----------------------------------------------------------------------
      GZMSGM = 0
C
      LMSGH = GZMSGH(1)
      IF ( LMSGH.LE.0 ) GOTO 999
C
      IF ( (MODULE.GE.1).AND.(MODULE.LE.307) ) THEN
        GZMSGM = LC( LMSGH-MODULE)
      END IF
C
  999 RETURN
      END
