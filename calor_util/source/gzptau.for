      FUNCTION GZPTAU()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-          return pointer to first PTAU (tau candidate) bank
C-   Returned value  : pointer
C-
C-   Created  26-SEP-1990   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  GZPTAU
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZPTAU.LINK/LIST'
      INTEGER LPARH,GZPARH
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZPTAU=0
C
C--   get link to supporting PARH bank
      LPARH=GZPARH()
C
C--   CHECK LPARH
      IF(LPARH.LE.0)THEN
        CALL ERRMSG('CALORIMETER','GZPTAU',
     &    'PARH BANK DOES NOT EXIST ' ,'W')
      ELSE
C
C--   find link to PTAU
        GZPTAU=LQ(LPARH-IZPTAU)
      ENDIF
C
  999 RETURN
      END
