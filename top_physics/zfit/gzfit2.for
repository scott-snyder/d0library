      FUNCTION GZFIT2()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to FIT2 bank
C-
C-   Returned value  : Link to 1st element of FIT2 linear structure
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   3-SEP-1993 23:29:14.64  Pushpa C. Bhat
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZFIT2
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZFIT2.LINK'
C----------------------------------------------------------------------
      INTEGER LPROC,GZPROC
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZFIT2 = 0
C
C--   GET LINK TO SUPPORTING PROC BANK
      LPROC = GZPROC()
C
C--   CHECK LPROC
      IF ( LPROC .LE. 0 ) THEN
        CALL ERRMSG('PROC-NOT-FOUND','GZFIT2',
     &    'PROC BANK DOES NOT EXIST ' ,'W')
        GOTO 999
      ENDIF
C
C--   FIND LINK TO FIT2
      GZFIT2 = LQ(LPROC-IZFIT2)
C
  999 RETURN
      END
