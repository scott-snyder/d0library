      FUNCTION GZGLOB()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the address of the GLOB bank.
C-
C-   Returned value  : Address.
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  15-DEC-1992 Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZGLOB
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZGLOB.LINK'
C----------------------------------------------------------------------
      INTEGER LPROC,GZPROC
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZGLOB = 0
C
C--   GET LINK TO SUPPORTING PROC BANK
      LPROC = GZPROC()
C
C--   CHECK LPROC
      IF ( LPROC .LE. 0 ) THEN
        CALL ERRMSG('PROC-NOT-FOUND','GZGLOB',
     &    'PROC BANK DOES NOT EXIST ' ,'W')
        GOTO 999
      ENDIF
C
C--   FIND LINK TO GLOB
      GZGLOB = LQ(LPROC-IZGLOB)
C
  999 RETURN
      END
