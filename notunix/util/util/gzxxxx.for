      FUNCTION GZXXXX()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to XXXX bank
C-
C-   Returned value  : Link to 1st element of XXXX linear structure
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  XDATE  XAUTHOR
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZXXXX
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZXXXX.LINK'
C----------------------------------------------------------------------
      INTEGER LXPARENT,GZXPARENT
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZXXXX = 0
C
C--   GET LINK TO SUPPORTING XPARENT BANK
      LXPARENT = GZXPARENT()
C
C--   CHECK LXPARENT
      IF ( LXPARENT .LE. 0 ) THEN
        CALL ERRMSG('XPARENT-NOT-FOUND','GZXXXX',
     &    'XPARENT BANK DOES NOT EXIST ' ,'W')
        GOTO 999
      ENDIF
C
C--   FIND LINK TO XXXX
      GZXXXX = LQ(LXPARENT-IZXXXX)
C
  999 RETURN
      END
