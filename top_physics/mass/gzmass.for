      FUNCTION GZMASS()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to MASS bank
C-
C-   Returned value  : Link to 1st element of MASS linear structure
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  22-JUN-1993 10:51:16.15  Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZMASS
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZMASS.LINK'
C----------------------------------------------------------------------
      INTEGER LPROC,GZPROC
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZMASS = 0
C
C--   GET LINK TO SUPPORTING PROC BANK
      LPROC = GZPROC()
C
C--   CHECK LPROC
      IF ( LPROC .LE. 0 ) THEN
        CALL ERRMSG('PROC-NOT-FOUND','GZMASS',
     &    'PROC BANK DOES NOT EXIST ' ,'W')
        GOTO 999
      ENDIF
C
C--   FIND LINK TO MASS
      GZMASS = LQ(LPROC-IZMASS)
C
  999 RETURN
      END
