      FUNCTION GZMSEG(ITRAK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return first MSEG bank address which hung
C-                         to MUON bank
C-
C-   Inputs  : ITRAK    MUON track number
C-   Outputs : none
C-   Controle: none
C-
C-   Created   7-OCT-1992   Atsushi Taketani
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE      'D0$INC:ZEBCOM.INC'
      INCLUDE      'D0$LINKS:IZMSEG.LINK'
      INTEGER  GZMSEG
      INTEGER  ITRAK
      INTEGER  LMUON
      INTEGER GZMUON
C----------------------------------------------------------------------
      GZMSEG = 0
C
      LMUON = GZMUON(ITRAK)
      IF ( LMUON.EQ.0 ) GOTO 999
C
      GZMSEG = LQ(LMUON-IZMSEG)
C
  999 RETURN
      END
