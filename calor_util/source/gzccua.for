      FUNCTION GZCCUA()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to CCUA bank
C-
C-   Returned value  : Link to 1st element of CCUA linear structure
C-   Inputs  : CRATE - ADC crate number, 0 point to first crate
C-   Outputs :
C-   Controls:
C-
C-   Created  31-JUL-1991  Jan Guida, Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCCUA.LINK/LIST'
      INTEGER GZCCUA,GZCCPH,LCCPH,LZFIND,LCCUA
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZCCUA=0
C
C--   GET LINK TO SUPPORTING CCPH BANK
      LCCPH=GZCCPH()
C
C--   CHECK LCCPH
      IF(LCCPH.LE.0)THEN
        CALL ERRMSG('CALORIMETER','GZCCUA',
     &    'CCPH BANK DOES NOT EXIST ' ,'W')
        GO TO 999
      ENDIF
C
C--   FIND LINK TO CCUA
      GZCCUA=LC(LCCPH-IZCCUA)
C
  999 RETURN
      END
