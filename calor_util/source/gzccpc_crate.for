      FUNCTION GZCCPC_CRATE(CRATE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to CCPC bank
C-
C-   Returned value  : Link to 1st element of CCPC linear structure
C-   Inputs  : CRATE - ADC crate number, 0 point to first crate
C-   Outputs :
C-   Controls:
C-
C-   Created  31-JUL-1991  Jan Guida, Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCCPC.LINK/LIST'
      INTEGER GZCCPC_CRATE,GZCCPH,LCCPH,LZFIND,LCCPC
      INTEGER CRATE
      CHARACTER*20 ACRATE
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZCCPC_CRATE=0
C
C--   GET LINK TO SUPPORTING CCPH BANK
      LCCPH=GZCCPH()
C
C--   CHECK LCCPH
      IF(LCCPH.LE.0)THEN
        CALL ERRMSG('CALORIMETER','GZCCPC',
     &    'CCPH BANK DOES NOT EXIST ' ,'W')
        GO TO 999
      ENDIF
C
C--   FIND LINK TO CCPC
      LCCPC=LC(LCCPH-IZCCPC)
      IF (CRATE.NE.0) THEN
        GZCCPC_CRATE = LZFIND(IDVSTP,LCCPC,CRATE,6)
        IF (GZCCPC_CRATE.EQ.0) THEN
          WRITE(ACRATE,'(I12)')CRATE
          CALL ERRMSG('CALORIMETER','GZCCPC',
     &    'CCPC BANK DOES NOT EXIST FOR CRATE'//ACRATE ,'W')
        ENDIF
      ENDIF
C
  999 RETURN
      END
