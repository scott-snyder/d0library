      FUNCTION GZVCOR()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to VCOR bank
C-
C-   Returned value  : Link to 1st element of VCOR linear structure
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  17-NOV-1992   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZVCOR
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZVCOR.LINK'
C----------------------------------------------------------------------
      INTEGER LPROC,GZPROC
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZVCOR = 0
C
C--   GET LINK TO SUPPORTING PROC BANK
      LPROC = GZPROC()
C
C--   CHECK LPROC
      IF ( LPROC .LE. 0 ) THEN
        CALL ERRMSG('PROC-NOT-FOUND','GZVCOR',
     &    'PROC BANK DOES NOT EXIST ' ,'W')
        GOTO 999
      ENDIF
C
C--   FIND LINK TO VCOR
      GZVCOR = LQ(LPROC-IZVCOR)
C
  999 RETURN
      END
