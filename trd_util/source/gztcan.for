      INTEGER FUNCTION GZTCAN()
C----------------------------------------------------------------------
C-   Purpose and Methods : Returns the Link to TCAN bank
C-   Returned value  : Link to 1st element of TCAN linear structure
C-   Created  23-JUN-1994 16:43:26 Alain PLUQUET
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTCAN.LINK/LIST'
      INTEGER GZSTRD
      GZTCAN=0
      LSTRD=GZSTRD()
      IF(LSTRD.LE.0)THEN
        CALL ERRMSG('TRD','GZTCAN','STRD BANK DOES NOT EXIST','W')
        GOTO 999
      ENDIF
      GZTCAN=LC(LSTRD-IZTCAN)
  999 RETURN
      END
