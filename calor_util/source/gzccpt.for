      FUNCTION GZCCPT()
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
      INTEGER GZCCPT,GZCCPH,LCCPH,LZFIND,LCCPT
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZCCPT=0
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
      GZCCPT=LC(LCCPH-IZCCPT)
C
  999 RETURN
      END
