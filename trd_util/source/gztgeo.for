      INTEGER FUNCTION GZTGEO()
C----------------------------------------------------------------------
C-   Purpose and Methods : Returns the Link to TGEO bank
C-   Returned value  : Link to 1st element of TGEO linear structure
C-   Created  12-JUL-1991 16:05:46.32  Alain PLUQUET
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTGEO.LINK/LIST'
      INTEGER GZSTRD
C----------------------------------------------------------------------
      GZTGEO=0
      LSTRD=GZSTRD()
      IF(LSTRD.LE.0)THEN
        CALL ERRMSG('TRD','GZTGEO','STRD BANK DOES NOT EXIST','W')
        GOTO 999
      ENDIF
      GZTGEO=LC(LSTRD-IZTGEO)
  999 RETURN
      END
