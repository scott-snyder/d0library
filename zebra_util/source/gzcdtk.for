      INTEGER FUNCTION GZCDTK()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return link to CDTK bank
C-
C-   Inputs  :  NONE
C-   Outputs :  CDTK address
C-   Controls:  NONE
C-
C-   Created   3-OCT-1995   Ian Adam
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCDTK.LINK'
      INTEGER LANLS,GZANLS
C----------------------------------------------------------------------
      GZCDTK = 0

      LANLS = GZANLS()
      IF (LANLS.LE.0) THEN
        CALL ERRMSG('NO ANLS','GZCDTK',' ','W')
        GOTO 999
      ENDIF

      IF (IQ(LANLS-3).LT.IZCDTK) THEN
        CALL ERRMSG('WRONG ANLS VERSION','GZCDTK','CANNOT FIND CDTK',
     &    'W')
        GOTO 999
      ENDIF

      GZCDTK = LQ(LANLS-IZCDTK)

  999 RETURN
      END
