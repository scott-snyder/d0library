      INTEGER FUNCTION GZMSGE( IMOD, ISC )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Pointer to MSGE
C-
C-   Inputs  : IMOD : muon module number
C-             ISC    scintillator number
C-   Outputs : None
C-   Controls: None
C-
C-   Created   2-JAN-1992   Atsushi Taketani
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  GZMSGM, LMSGM
      INTEGER  IMOD, ISC
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
      GZMSGE = 0
      LMSGM = GZMSGM(IMOD)
      IF ( LMSGM.EQ.0 ) GOTO 999
      IF ( (ISC.LE.IC(LMSGM+10)).AND.(ISC.GE.1) ) THEN
        GZMSGE = LC(LMSGM-ISC)
      END IF
C
  999 RETURN
      END
