      FUNCTION GZMUKF(IT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to MUKF bank
C-
C-   Returned value  : Link to 1st element of MUKF linear structure
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   5-MAY-1994 21:41:17.29  Igor V. Mandrichenko
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZMUKF,IT
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZMUKF.LINK'
C----------------------------------------------------------------------
      INTEGER LMUON,GZMUON
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZMUKF = 0
C
C--   GET LINK TO SUPPORTING MUON BANK
      LMUON = GZMUON(IT)
C
C--   CHECK LMUON
      IF ( LMUON .LE. 0 ) THEN
        CALL ERRMSG('MUON-NOT-FOUND','GZMUKF',
     &    'MUON BANK DOES NOT EXIST ' ,'W')
        GOTO 999
      ENDIF
C
C--   FIND LINK TO MUKF
      GZMUKF = LQ(LMUON-IZMUKF)
C
  999 RETURN
      END
