      FUNCTION GZCCPT_CRATE(CRATE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to CCPT bank
C-
C-   Returned value  : Link to 1st element of CCPT linear structure
C-   Inputs  : CRATE - ADC crate number, 0 point to first crate
C-   Outputs :
C-   Controls:
C-
C-   Created  31-JUL-1991  Jan Guida, Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCCPT.LINK/LIST'
      INTEGER GZCCPT_CRATE,GZCCPH,LCCPH,LZFIND,LCCPT
      INTEGER CRATE
      CHARACTER*20 ACRATE
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZCCPT_CRATE=0
C
C--   GET LINK TO SUPPORTING CCPH BANK
      LCCPH=GZCCPH()
C
C--   CHECK LCCPH
      IF(LCCPH.LE.0)THEN
        CALL ERRMSG('CALORIMETER','GZCCPT',
     &    'CCPH BANK DOES NOT EXIST ' ,'W')
        GO TO 999
      ENDIF
C
C--   FIND LINK TO CCPT
      LCCPT=LC(LCCPH-IZCCPT)
      IF (CRATE.NE.0) THEN
        GZCCPT_CRATE = LZFIND(IDVSTP,LCCPT,CRATE,6)
        IF (GZCCPT_CRATE.EQ.0) THEN
          WRITE(ACRATE,'(I12)')CRATE
          CALL ERRMSG('CALORIMETER','GZCCPT',
     &    'CCPT BANK DOES NOT EXIST FOR CRATE'//ACRATE ,'W')
        ENDIF
      ENDIF
C
  999 RETURN
      END
